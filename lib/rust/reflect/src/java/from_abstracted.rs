use crate::abstracted;
use crate::java::*;



// ============================
// === Java from Abstracted ===
// ============================

/// Lower a data model in the abstracted representation to a data model in the Java typesystem.
pub fn from_abstracted(
    graph: &abstracted::TypeGraph,
    either_type: impl Into<String>,
) -> (TypeGraph, BTreeMap<abstracted::TypeId, TypeId>) {
    let primitives = Default::default();
    let mut java = TypeGraph::default();
    let abstracted_to_java: BTreeMap<_, _> =
        graph.type_ids().map(|id| (id, java.reserve_type_id())).collect();
    let either_type = either_type.into();
    let mut from_abstracted = FromAbstracted { java, abstracted_to_java, primitives, either_type };
    // Translate primitives first, because in Java we need to know whether a type is primitive when
    // we reference the type.
    for (&id_, &id) in &from_abstracted.abstracted_to_java {
        if let abstracted::Data::Primitive(ty) = &graph[id_].data {
            match from_abstracted.primitive(ty) {
                Ok(prim) => {
                    from_abstracted.primitives.insert(id_, prim);
                }
                Err(class) => from_abstracted.java.set(id, class),
            }
        }
    }
    // Translate structs.
    for (&id_, &id) in &from_abstracted.abstracted_to_java {
        let ty = &graph[id_];
        let fields_ = match &ty.data {
            abstracted::Data::Primitive(_) => continue,
            abstracted::Data::Struct(fields_) => fields_,
        };
        let class = from_abstracted.class(ty, fields_);
        from_abstracted.java.set(id, class);
    }
    let FromAbstracted { java, abstracted_to_java, .. } = from_abstracted;
    (java, abstracted_to_java)
}

#[derive(Debug)]
struct FromAbstracted {
    java:               TypeGraph,
    abstracted_to_java: BTreeMap<abstracted::TypeId, TypeId>,
    primitives:         BTreeMap<abstracted::TypeId, Primitive>,
    either_type:        String,
}

impl FromAbstracted {
    fn primitive(&self, ty: &abstracted::Primitive) -> Result<Primitive, Class> {
        match ty {
            abstracted::Primitive::Bool => Ok(Primitive::Bool),
            abstracted::Primitive::U64 => Ok(Primitive::Long { unsigned: true }),
            abstracted::Primitive::U32 => Ok(Primitive::Int { unsigned: true }),
            abstracted::Primitive::String => Err(Class::builtin("String", vec![])),
            abstracted::Primitive::Option(t0) => {
                let t0_ = self.abstracted_to_java[t0];
                Err(Class::builtin("java.util.Optional", vec![t0_]))
            }
            abstracted::Primitive::Sequence(t0) => {
                let t0_ = self.abstracted_to_java[t0];
                Err(Class::builtin("java.util.List", vec![t0_]))
            }
            abstracted::Primitive::Result(t0, t1) => {
                let t0_ = self.abstracted_to_java[t0];
                let t1_ = self.abstracted_to_java[t1];
                Err(Class::builtin(&self.either_type, vec![t1_, t0_]))
            }
        }
    }

    fn class<'f>(
        &self,
        ty: &abstracted::Type,
        fields_: impl IntoIterator<Item = &'f abstracted::Field>,
    ) -> Class {
        let name = ty.name.to_pascal_case();
        let abstract_ = ty.abstract_;
        let sealed = ty.closed;
        let parent = ty.parent.as_ref().map(|id| self.abstracted_to_java[id]);
        let mut methods = match abstract_ {
            true => abstract_methods(),
            false => standard_methods(),
        };
        let fields_ = fields_.into_iter();
        let mut fields = Vec::with_capacity(fields_.size_hint().0);
        for field in fields_ {
            let (field, getter) = self.field(field);
            if getter {
                methods.push(Method::Dynamic(Dynamic::Getter(field.id())));
            }
            fields.push(field);
        }
        let discriminants =
            ty.discriminants.iter().map(|(key, id)| (*key, self.abstracted_to_java[id])).collect();
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

    fn field(&self, field: &abstracted::Field) -> (Field, bool) {
        let abstracted::Field { name, type_, hide, .. } = field;
        let name = name.to_camel_case().expect("Unimplemented: Tuples.");
        let field = match self.primitives.get(type_) {
            Some(primitive) => Field::primitive(name, *primitive),
            None => Field::object(name, self.abstracted_to_java[type_], true),
        };
        (field, !*hide)
    }
}
