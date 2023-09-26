//! Supports generation of TypeScript types corresponding to `enso-parser`'s AST types, and testing
//! and debugging the translation process.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use enso_metamodel::meta;
use enso_metamodel::rust;
use enso_reflect::Reflect;



// ===================
// === Entry Point ===
// ===================

/// Return a serializable [`Schema`] describing the parser types.
pub fn types() -> impl serde::Serialize {
    let (graph, _) = rust::to_meta(enso_parser::syntax::Tree::reflect());
    schema(&graph)
}



// ==============
// === Schema ===
// ==============

#[derive(serde::Serialize)]
struct Schema {
    types: std::collections::HashMap<String, Type>,
}

#[derive(serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct Type {
    name:          String,
    fields:        Vec<(String, Field)>,
    parent:        Option<String>,
    discriminants: std::collections::BTreeMap<usize, String>,
    size:          usize,
}

#[derive(serde::Serialize)]
struct Field {
    r#type: TypeRef,
    offset: usize,
}

#[derive(serde::Serialize, Clone, Hash, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "class")]
enum TypeRef {
    Type { id: String },
    Primitive { r#type: Primitive },
    Sequence { r#type: Box<TypeRef> },
    Option { r#type: Box<TypeRef> },
    Result { r#type0: Box<TypeRef>, r#type1: Box<TypeRef> },
}

#[derive(serde::Serialize, Copy, Clone, Hash, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum Primitive {
    Bool,
    U32,
    U64,
    I32,
    I64,
    Char,
    String,
}

fn schema(graph: &meta::TypeGraph) -> Schema {
    let mut next_type_id = 0;
    let mut next_type_id = || {
        let result = format!("type_{next_type_id}");
        next_type_id += 1;
        result
    };
    let mut type_refs = std::collections::HashMap::new();
    let mut type_ids = std::collections::HashMap::new();
    let mut primitives = vec![];
    // Map struct types; gather primitive types.
    for (key, ty) in &graph.types {
        match &ty.data {
            meta::Data::Struct(_) => {
                let id = next_type_id();
                type_ids.insert(key, id.clone());
                type_refs.insert(key, TypeRef::Type { id });
            }
            meta::Data::Primitive(prim) => {
                primitives.push((key, prim));
            }
        }
    }
    // Convert primitive types, handling dependencies in a topological order.
    while !primitives.is_empty() {
        primitives.retain(|(key, prim)| {
            let ty = match prim {
                meta::Primitive::Bool => Some(TypeRef::Primitive { r#type: Primitive::Bool }),
                meta::Primitive::U32 => Some(TypeRef::Primitive { r#type: Primitive::U32 }),
                meta::Primitive::U64 => Some(TypeRef::Primitive { r#type: Primitive::U64 }),
                meta::Primitive::I32 => Some(TypeRef::Primitive { r#type: Primitive::I32 }),
                meta::Primitive::I64 => Some(TypeRef::Primitive { r#type: Primitive::I64 }),
                meta::Primitive::Char => Some(TypeRef::Primitive { r#type: Primitive::Char }),
                meta::Primitive::String => Some(TypeRef::Primitive { r#type: Primitive::String }),
                meta::Primitive::Sequence(id) =>
                    type_refs.get(id).cloned().map(|ty| TypeRef::Sequence { r#type: Box::new(ty) }),
                meta::Primitive::Option(id) =>
                    type_refs.get(id).cloned().map(|ty| TypeRef::Option { r#type: Box::new(ty) }),
                meta::Primitive::Result(id0, id1) => type_refs.get(id0).cloned().and_then(|ty0| {
                    type_refs.get(id1).cloned().map(|ty1| TypeRef::Result {
                        r#type0: Box::new(ty0),
                        r#type1: Box::new(ty1),
                    })
                }),
            };
            if let Some(ty) = ty {
                type_refs.insert(*key, ty);
                false
            } else {
                true
            }
        });
    }
    let mut types: std::collections::HashMap<_, _> = graph
        .types
        .iter()
        .filter_map(|(key, ty)| {
            ty.data.as_struct().map(|fields| {
                let key_to_id = |id| type_ids[id].clone();
                (key_to_id(&key), Type {
                    name:          ty.name.to_snake_case(),
                    fields:        fields
                        .iter()
                        .map(|f| {
                            let name = f.name.to_snake_case().expect("Tuples not supported.");
                            let r#type = type_refs[&f.type_].clone();
                            let field = Field { r#type, offset: 0 };
                            (name, field)
                        })
                        .collect(),
                    parent:        ty.parent.as_ref().map(key_to_id),
                    discriminants: ty
                        .discriminants
                        .iter()
                        .map(|(k, v)| (*k, key_to_id(v)))
                        .collect(),
                    size: 0,
                })
            })
        })
        .collect();
    let mut data_size = std::collections::HashMap::new();
    let mut reference_size = std::collections::HashMap::new();
    for key in types.keys() {
        let reference = TypeRef::Type { id: key.to_owned() };
        let ref_size = compute_size(reference.clone(), &types, &mut data_size);
        if !types[key].discriminants.is_empty() {
            reference_size.insert(reference, ref_size);
        }
    }
    for (id, ty) in types.iter_mut() {
        let mut offset = ty
            .parent
            .as_ref()
            .map_or(0, |parent| data_size[&TypeRef::Type { id: parent.to_owned() }]);
        for (_, field) in &mut ty.fields {
            field.offset = offset;
            offset += reference_size
                .get(&field.r#type)
                .copied()
                .unwrap_or_else(|| data_size[&field.r#type]);
        }
        let reference = TypeRef::Type { id: id.to_owned() };
        ty.size = reference_size
            .get(&reference)
            .copied()
            .unwrap_or_else(|| data_size[&reference]);
    }
    Schema { types }
}

const POINTER_BYTES: usize = 4;

/// Writes the size of the object's fields to `data_size`. Returns the size of a reference to the
/// object.
fn compute_size(
    ty: TypeRef,
    types: &std::collections::HashMap<String, Type>,
    data_size: &mut std::collections::HashMap<TypeRef, usize>,
) -> usize {
    let mut reference = None;
    let size = match &ty {
        // May be pointer or inline fields.
        TypeRef::Type { id } => {
            let ty = &types[id];
            if !ty.discriminants.is_empty() {
                reference = Some(POINTER_BYTES);
            }
            let key = TypeRef::Type { id: id.to_owned() };
            if let Some(size) = data_size.get(&key) {
                *size
            } else {
                let mut size =
                    if let Some(id) = ty.parent.clone() {
                        let type_ref = TypeRef::Type { id };
                        compute_size(type_ref.clone(), types, data_size);
                        data_size[&type_ref]
                    } else {
                        0
                    };
                for (_, field) in &ty.fields {
                    let field_size = compute_size(field.r#type.to_owned(), types, data_size);
                    size += field_size;
                }
                size
            }
        }
        // Pointer.
        TypeRef::Primitive { r#type: Primitive::String } => POINTER_BYTES,
        TypeRef::Sequence { .. } => POINTER_BYTES,
        TypeRef::Result { .. } => POINTER_BYTES,
        // Discriminant + pointer.
        TypeRef::Option { .. } => 1 + POINTER_BYTES,
        // Scalars.
        TypeRef::Primitive { r#type: Primitive::Bool } => 1,
        TypeRef::Primitive { r#type: Primitive::U32 | Primitive::I32 | Primitive::Char } => 4,
        TypeRef::Primitive { r#type: Primitive::U64 | Primitive::I64 } => 8,
    };
    data_size.insert(ty, size);
    reference.unwrap_or(size)
}
