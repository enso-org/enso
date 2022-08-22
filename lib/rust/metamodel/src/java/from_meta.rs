//! Translating a data model in the highly-abstracted `meta` representation to a data model in the
//! `crate::java` representation.
//!
//! As the `meta` and `java` models are similar, this is a straightforward translation. The main
//! differences are:
//! - In Java, there is a distinction between a few types that are unboxed primitives and all other
//!   types, which are reference types.
//! - In Java, all classes are expected to implement certain methods. These methods are attached in
//!   this stage, although [`Dynamic`] methods are used so that if any classes are modified before
//!   the model is rendered to syntax, the generated methods will reflect the changes.

use crate::java::*;

use crate::meta;



// ======================
// === Java from Meta ===
// ======================

/// Translate a data model in the [`meta`] representation to a data model in the Java typesystem.
pub fn from_meta(
    graph: &meta::TypeGraph,
    either_type: impl Into<String>,
) -> (TypeGraph, BTreeMap<meta::TypeId, ClassId>) {
    let primitives = Default::default();
    let mut java = TypeGraph::default();
    let mut class_promises: BTreeMap<_, _> =
        graph.types.keys().map(|id| (id, java.classes.unbound_key())).collect();
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
                    from_meta.java.classes.bind(class_promises.remove(&id_).unwrap(), class);
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
        from_meta.java.classes.bind(class_promises.remove(&id_).unwrap(), class);
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
    /// Translate a primitive in the [`meta`] model to either a Java primitive, or a Java class.
    fn primitive(&self, ty: &meta::Primitive) -> Result<Primitive, Class> {
        match ty {
            meta::Primitive::Bool => Ok(Primitive::Bool),
            meta::Primitive::U64 => Ok(Primitive::Long { unsigned: true }),
            meta::Primitive::U32 => Ok(Primitive::Int { unsigned: true }),
            meta::Primitive::String => Err(Class::string()),
            meta::Primitive::Option(t0_) => Err(Class::optional(self.meta_to_java[t0_])),
            meta::Primitive::Sequence(t0_) => Err(Class::list(self.meta_to_java[t0_])),
            meta::Primitive::Result(t0_, t1_) => {
                let t0 = self.meta_to_java[t0_];
                let t1 = self.meta_to_java[t1_];
                Err(Class::builtin(&self.either_type, vec![t1, t0]))
            }
        }
    }

    /// Translate a type in the [`meta`] model to a Java class.
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
            let mut prefixed_name = meta::FieldName::from_snake_case("field");
            prefixed_name.append(name.clone());
            let prefixed_name = prefixed_name.to_camel_case().unwrap();
            let field = match self.primitives.get(type_) {
                Some(primitive) => Field::primitive(prefixed_name, *primitive),
                None => Field::object(prefixed_name, self.meta_to_java[type_], true),
            };
            if !hide {
                let mut getter_name = meta::FieldName::from_snake_case("get");
                getter_name.append(name.clone());
                let getter_name = getter_name.to_camel_case().unwrap();
                methods.push(Method::Dynamic(Dynamic::GetterNamed(field.id(), getter_name)));
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_converting_graph() {
        let mut meta = meta::TypeGraph::new();
        let u32_name = meta::TypeName::from_pascal_case("U32");
        let u32_ty = meta::Type::new(u32_name, meta::Data::Primitive(meta::Primitive::U32));
        let u32_ = meta.types.insert(u32_ty);
        let inner_field_name = meta::FieldName::from_snake_case("inner_field");
        let inner_fields = vec![meta::Field::named(inner_field_name, u32_)];
        let inner_name = meta::TypeName::from_pascal_case("Inner");
        let inner =
            meta.types.insert(meta::Type::new(inner_name, meta::Data::Struct(inner_fields)));
        let outer_field_inner_name = meta::FieldName::from_snake_case("inner");
        let outer_name = meta::TypeName::from_pascal_case("Outer");
        let outer_fields = vec![meta::Field::named(outer_field_inner_name, inner)];
        let outer_ty = meta::Type::new(outer_name, meta::Data::Struct(outer_fields));
        let outer = meta.types.insert(outer_ty);
        let (java, meta_to_java) = from_meta(&meta, "Either");
        let outer_ = meta_to_java[&outer];
        let inner_ = meta_to_java[&inner];
        assert_eq!(java[outer_].name, "Outer");
        assert_eq!(java[inner_].name, "Inner");
        assert_eq!(java[outer_].fields[0].data, FieldData::Object {
            type_:    inner_,
            non_null: true,
        });
        assert_eq!(
            java[inner_].fields[0].data,
            FieldData::Primitive(Primitive::Int { unsigned: true })
        );
    }
}
