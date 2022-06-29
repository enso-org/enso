use crate::generic;
use crate::java::*;



// =========================
// === Java from Generic ===
// =========================

/// Lower a data model in the generic representation to a data model in the Java typesystem.
pub fn from_generic(graph: &generic::TypeGraph) -> (TypeGraph, BTreeMap<generic::TypeId, TypeId>) {
    let primitives = Default::default();
    let mut java = TypeGraph::default();
    let generic_to_java: BTreeMap<_, _> =
        graph.type_ids().map(|id| (id, java.reserve_type_id())).collect();
    let mut from_generic = FromGeneric { java, generic_to_java, primitives };
    // Translate primitives first, because in Java we need to know whether a type is primitive when
    // we reference the type.
    for (&id_, &id) in &from_generic.generic_to_java {
        if let generic::Data::Primitive(ty) = &graph[id_].data {
            match from_generic.primitive(ty) {
                Ok(prim) => {
                    from_generic.primitives.insert(id_, prim);
                }
                Err(class) => from_generic.java.set(id, class),
            }
        }
    }
    // Translate structs.
    for (&id_, &id) in &from_generic.generic_to_java {
        let ty = &graph[id_];
        let fields_ = match &ty.data {
            generic::Data::Primitive(_) => continue,
            generic::Data::Struct(fields_) => fields_,
        };
        let class = from_generic.class(ty, fields_);
        from_generic.java.set(id, class);
    }
    let FromGeneric { java, generic_to_java, .. } = from_generic;
    (java, generic_to_java)
}

#[derive(Debug)]
struct FromGeneric {
    java:            TypeGraph,
    generic_to_java: BTreeMap<generic::TypeId, TypeId>,
    primitives:      BTreeMap<generic::TypeId, Primitive>,
}

impl FromGeneric {
    fn primitive(&self, ty: &generic::Primitive) -> Result<Primitive, Class> {
        match ty {
            generic::Primitive::Bool => Ok(Primitive::Bool),
            // FIXME the right way to handle this varies; needs to be configurable
            generic::Primitive::Usize => Ok(Primitive::Long { unsigned: true }),
            generic::Primitive::U32 => Ok(Primitive::Int { unsigned: true }),
            generic::Primitive::String => Err(Class::builtin(&self.java, "String", vec![])),
            generic::Primitive::Option(t0) => {
                let t0_ = self.generic_to_java[t0];
                Err(Class::builtin(&self.java, "java.util.Optional", vec![t0_]))
            }
            generic::Primitive::Sequence(t0) => {
                let t0_ = self.generic_to_java[t0];
                Err(Class::builtin(&self.java, "java.util.List", vec![t0_]))
            }
            generic::Primitive::Result(t0, t1) => {
                let t0_ = self.generic_to_java[t0];
                let t1_ = self.generic_to_java[t1];
                Err(Class::builtin(&self.java, "utils.Either", vec![t1_, t0_]))
            }
        }
    }

    fn class<'f>(
        &self,
        ty: &generic::Type,
        fields_: impl IntoIterator<Item = &'f generic::Field>,
    ) -> Class {
        let name = ty.name.to_pascal_case();
        let abstract_ = ty.abstract_;
        let sealed = ty.closed;
        let parent = ty.parent.as_ref().map(|id| self.generic_to_java[id]);
        let mut methods = match abstract_ {
            true => abstract_methods(),
            false => standard_methods(),
        };
        let fields_ = fields_.into_iter();
        let mut fields = Vec::with_capacity(fields_.size_hint().0);
        for field in fields_ {
            let (field, getter) = self.field(field);
            if getter {
                methods.push(Method::Dynamic(Dynamic::Getter(field.id)));
            }
            fields.push(field);
        }
        let discriminants =
            ty.discriminants.iter().map(|(key, id)| (*key, self.generic_to_java[id])).collect();
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

    fn field(&self, field: &generic::Field) -> (Field, bool) {
        let generic::Field { name, type_, hide, .. } = field;
        let name = name.clone();
        let data = if let Some(primitive) = self.primitives.get(&type_) {
            FieldData::Primitive(primitive.clone())
        } else {
            let type_ = self.generic_to_java[type_];
            FieldData::Object { type_, nonnull: true }
        };
        let name = name.to_camel_case().expect("Unimplemented: Tuples.");
        let field = self.java.field(name, data);
        (field, !*hide)
    }
}
