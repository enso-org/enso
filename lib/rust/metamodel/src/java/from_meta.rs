//! Lowering a `meta` data model to a `crate::java` data model.

use crate::java::*;
use crate::meta;



// ======================
// === Java from Meta ===
// ======================

/// Lower a data model in the meta representation to a data model in the Java typesystem.
pub fn from_meta(
    graph: &meta::TypeGraph,
    either_type: impl Into<String>,
) -> (TypeGraph, BTreeMap<meta::TypeId, ClassId>) {
    let primitives = Default::default();
    let mut java = TypeGraph::default();
    let mut class_promises: BTreeMap<_, _> =
        graph.types.keys().map(|id| (id, java.classes.allocate_key())).collect();
    let meta_to_java = class_promises.iter().map(|(key, value)| (*key, value.into())).collect();
    let either_type = either_type.into();
    let mut from_meta = FromMeta { java, meta_to_java, primitives, either_type };
    // Translate primitives first, because in Java we need to know whether a type is primitive when
    // we reference the type.
    let mut unbound_ids: Vec<_> = class_promises.keys().copied().collect();
    for &id_ in &unbound_ids {
        if let meta::Data::Primitive(ty) = &graph[id_].data {
            match from_meta.primitive(ty) {
                Ok(prim) => {
                    from_meta.primitives.insert(id_, prim);
                }
                Err(class) => {
                    from_meta.java.classes.bind_key(class_promises.remove(&id_).unwrap(), class);
                }
            }
        }
    }
    unbound_ids.clear();
    unbound_ids.extend(class_promises.keys().copied());
    // Translate structs.
    for id_ in unbound_ids {
        let ty = &graph[id_];
        let fields_ = match &ty.data {
            meta::Data::Primitive(_) => continue,
            meta::Data::Struct(fields_) => fields_,
        };
        let class = from_meta.class(ty, fields_);
        from_meta.java.classes.bind_key(class_promises.remove(&id_).unwrap(), class);
    }
    let FromMeta { java, meta_to_java, .. } = from_meta;
    (java, meta_to_java)
}

#[derive(Debug)]
struct FromMeta {
    java:         TypeGraph,
    meta_to_java: BTreeMap<meta::TypeId, ClassId>,
    primitives:   BTreeMap<meta::TypeId, Primitive>,
    either_type:  String,
}

impl FromMeta {
    fn primitive(&self, ty: &meta::Primitive) -> Result<Primitive, Class> {
        match ty {
            meta::Primitive::Bool => Ok(Primitive::Bool),
            meta::Primitive::U64 => Ok(Primitive::Long { unsigned: true }),
            meta::Primitive::U32 => Ok(Primitive::Int { unsigned: true }),
            meta::Primitive::String => Err(Class::builtin("String", vec![])),
            meta::Primitive::Option(t0) => {
                let t0_ = self.meta_to_java[t0];
                Err(Class::builtin("java.util.Optional", vec![t0_]))
            }
            meta::Primitive::Sequence(t0) => {
                let t0_ = self.meta_to_java[t0];
                Err(Class::builtin("java.util.List", vec![t0_]))
            }
            meta::Primitive::Result(t0, t1) => {
                let t0_ = self.meta_to_java[t0];
                let t1_ = self.meta_to_java[t1];
                Err(Class::builtin(&self.either_type, vec![t1_, t0_]))
            }
        }
    }

    fn class<'f>(
        &self,
        ty: &meta::Type,
        fields_: impl IntoIterator<Item = &'f meta::Field>,
    ) -> Class {
        let name = ty.name.to_pascal_case();
        let abstract_ = ty.abstract_;
        let sealed = ty.closed;
        let parent = ty.parent.as_ref().map(|id| self.meta_to_java[id]);
        let mut methods = match abstract_ {
            true => abstract_methods(),
            false => standard_methods(),
        };
        let fields_ = fields_.into_iter();
        let mut fields = Vec::with_capacity(fields_.size_hint().0);
        for field in fields_ {
            let meta::Field { name, type_, hide, .. } = field;
            let name = name.to_camel_case().expect("Unimplemented: Tuples.");
            let field = match self.primitives.get(type_) {
                Some(primitive) => Field::primitive(name, *primitive),
                None => Field::object(name, self.meta_to_java[type_], true),
            };
            if !hide {
                methods.push(Method::Dynamic(Dynamic::Getter(field.id())));
            }
            fields.push(field);
        }
        let discriminants =
            ty.discriminants.iter().map(|(key, id)| (*key, self.meta_to_java[id])).collect();
        let child_field = ty.child_field;
        Class {
            name,
            parent,
            abstract_,
            sealed,
            fields,
            methods,
            discriminants,
            child_field,
            ..Default::default()
        }
    }
}
