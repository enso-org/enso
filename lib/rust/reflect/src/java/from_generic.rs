use crate::generic;
use crate::java::*;
use std::collections::BTreeSet;


// =========================
// === Java from Generic ===
// =========================

/// Lower a data model in the generic representation to a data model in the Java typesystem.
pub fn from_generic(graph: &generic::TypeGraph) -> (TypeGraph, BTreeMap<generic::TypeId, TypeId>) {
    let mut primitives = BTreeMap::new();
    let mut java = TypeGraph::default();
    let mut generic_to_java: BTreeMap<_, _> =
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
    let mut getters_wanted = vec![];
    for (&id_, &id) in &from_generic.generic_to_java {
        let ty = &graph[id_];
        let fields_ = match &ty.data {
            generic::Data::Primitive(_) => continue,
            generic::Data::Struct(fields_) => fields_,
        };
        let (class, getters) = from_generic.class(ty, fields_);
        getters_wanted.push((id, getters));
        from_generic.java.set(id, class);
    }
    // Generate getters. We do this after translating structs because we need field type info.
    for (id, mut fields) in getters_wanted {
        let mut methods = vec![];
        for field in &from_generic.java[id].fields {
            if fields.remove(&field.id()) {
                methods.push(implementation::getter(&from_generic.java, &field));
            }
        }
        assert!(fields.is_empty());
        from_generic.java[id].methods.extend(methods.into_iter().map(Method::Raw))
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
            generic::Primitive::Option(t0) =>
                Err(Class::builtin(&self.java, "java.util.Optional", vec![
                    self.generic_to_java[t0],
                ])),
            generic::Primitive::Sequence(t0) =>
                Err(Class::builtin(&self.java, "java.util.ArrayList", vec![
                    self.generic_to_java[t0],
                ])),
            generic::Primitive::Result(t0, t1) => {
                let t0_ = self.generic_to_java[t0];
                let t1_ = self.generic_to_java[t1];
                Err(Class::builtin(&self.java, "utils.Either", vec![t0_, t1_]))
            }
        }
    }

    fn class<'f>(
        &self,
        ty: &generic::Type,
        fields_: impl IntoIterator<Item = &'f generic::Field>,
    ) -> (Class, BTreeSet<FieldId>) {
        let name = ty.name.to_pascal_case();
        let abstract_ = ty.abstract_;
        let sealed = ty.closed;
        let parent = ty.parent.as_ref().map(|id| self.generic_to_java[id]);
        let builtin = false;
        let mut methods = match abstract_ {
            true => abstract_methods(),
            false => standard_methods(),
        };
        let mut getters = BTreeSet::new();
        let fields = fields_.into_iter().map(|field| self.field(field, &mut getters)).collect();
        let params = vec![];
        let discriminants =
            ty.discriminants.iter().map(|(key, id)| (*key, self.generic_to_java[id])).collect();
        let class = Class {
            name,
            params,
            parent,
            abstract_,
            builtin,
            sealed,
            fields,
            methods,
            discriminants,
        };
        (class, getters)
    }

    fn field(&self, field: &generic::Field, getters: &mut BTreeSet<FieldId>) -> Field {
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
        if !hide {
            getters.insert(field.id());
        }
        field
    }
}
