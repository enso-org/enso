use crate::generic;
use crate::java::*;



// =========================
// === Java from Generic ===
// =========================

/// Lower a data model in the generic representation to a data model in the Java typesystem.
pub fn from_generic(root: generic::TypeId, graph: &generic::TypeGraph) -> (TypeId, TypeGraph) {
    let mut java_of_generic = std::collections::HashMap::new();
    let mut primitives = std::collections::HashMap::new();
    let mut java = TypeGraph::default();
    for (id, _) in graph.types.iter().enumerate() {
        let id = generic::TypeId(id);
        let id_ = java.reserve_type_id();
        java_of_generic.insert(id, id_);
    }
    for (id, ty) in graph.types.iter().enumerate() {
        let id = generic::TypeId(id);
        let id_ = java_of_generic[&id];
        let ty = match ty.as_ref() {
            Some(generic::Type { data: generic::Data::Primitive(ty), .. }) => ty,
            _ => continue,
        };
        let primitive = match ty {
            generic::Primitive::Bool => Primitive::Bool,
            // FIXME the right way to handle this varies; needs to be configurable
            generic::Primitive::Usize => Primitive::Long { unsigned: true },
            generic::Primitive::U32 => Primitive::Int { unsigned: true },
            generic::Primitive::String => {
                java.set(id_, Class::builtin("String", vec![]));
                continue;
            }
            generic::Primitive::Option(t0) => {
                java.set(id_, Class::builtin("java.util.Optional", vec![java_of_generic[t0]]));
                continue;
            }
            generic::Primitive::Sequence(t0) => {
                java.set(id_, Class::builtin("java.util.ArrayList", vec![java_of_generic[t0]]));
                continue;
            }
            generic::Primitive::Result(t0, t1) => {
                let t0_ = java_of_generic[t0];
                let t1_ = java_of_generic[t1];
                java.set(id_, Class::builtin("utils.Either", vec![t0_, t1_]));
                continue;
            }
        };
        primitives.insert(id, primitive);
    }
    for (id, ty) in graph.types.iter().enumerate() {
        let ty = match ty.as_ref() {
            Some(ty) => ty,
            None => continue,
        };
        let id = generic::TypeId(id);
        let id_ = java_of_generic[&id];
        let name = ty.name.to_pascal_case();
        let abstract_ = ty.abstract_;
        let sealed = ty.closed;
        let parent = ty.parent.as_ref().map(|id| java_of_generic[id]);
        let builtin = false;
        let fields;
        match &ty.data {
            generic::Data::Primitive(_) => continue,
            generic::Data::Struct(generic::Struct::Unit) => fields = vec![],
            generic::Data::Struct(generic::Struct::Named(fields_)) => {
                fields = fields_
                    .iter()
                    .map(|generic::Named { name, value: generic::Field { type_, hide } }| {
                        let name = name.clone();
                        let data = if let Some(primitive) = primitives.get(type_) {
                            FieldData::Primitive(primitive.clone())
                        } else {
                            let type_ = java_of_generic[type_];
                            FieldData::Object { type_, nonnull: true }
                        };
                        let name = name.to_camel_case();
                        let getter = !hide;
                        Field { name, data, getter }
                    })
                    .collect();
            }
            generic::Data::Struct(generic::Struct::Unnamed(_fields)) =>
                unimplemented!("Promotion of tuples to structs with named fields."),
        }
        let params = vec![];
        let methods = match abstract_ {
            true => abstract_methods(),
            false => standard_methods(),
        };
        let discriminants = if let Some(attrs) = &ty.child_attrs {
            attrs.discriminants.iter().map(|(key, id)| (*key, java_of_generic[id])).collect()
        } else {
            Default::default()
        };
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
        java.set(id_, class);
    }
    let root = java_of_generic[&root];
    (root, java)
}
