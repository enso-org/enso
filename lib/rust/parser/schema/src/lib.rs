//! Supports generation of a schema describing `enso-parser`'s AST types.

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
use enso_metamodel::meta::Data;
use enso_reflect::Reflect;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::rc::Rc;



// ===================
// === Entry Point ===
// ===================

/// Return a serializable [`Schema`] describing the parser types.
pub fn schema() -> impl serde::Serialize {
    let (graph, _) = enso_metamodel::rust::to_meta(enso_parser::syntax::Tree::reflect());
    let Types { types, ids } = types(&graph);
    let serialization = serialization(&graph)
        .filter_map(|(k, v)| ids.get(&k).map(|k| (k.clone(), v.map_ids(|k| ids[&k].clone()))))
        .collect();
    Schema { types, serialization }
}



// ==============
// === Schema ===
// ==============

/// Describes a set of types and their serialization.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Schema {
    /// The type definitions.
    pub types:         HashMap<TypeId, Type>,
    /// Serialization information for the types.
    pub serialization: HashMap<TypeId, Layout>,
}


// === Type graph ===

/// Describes a type.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Type {
    /// The type's name, in snake case.
    pub name:   Rc<str>,
    /// The type's fields.
    pub fields: HashMap<FieldName, TypeRef>,
    /// The type's parent, if any.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent: Option<TypeId>,
}

/// Arbitrary key uniquely identifying a type within the schema.
#[derive(Debug, serde::Serialize, serde::Deserialize, Clone, Hash, PartialEq, Eq)]
pub struct TypeId(Rc<str>);

/// The name of a field, in snake case.
#[derive(Debug, serde::Serialize, serde::Deserialize, Clone, Hash, PartialEq, Eq)]
pub struct FieldName(Rc<str>);

/// A reference to a type, which may be a [`Type`] defined in the schema, a primitive, or a
/// parameterized generic type.
#[derive(Debug, serde::Serialize, serde::Deserialize, Clone, Hash, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "class")]
pub enum TypeRef {
    /// A type defined in the schema.
    Type {
        /// Identifies the type.
        id: TypeId,
    },
    /// A [`Primitive`].
    Primitive {
        /// A [`Primitive`].
        r#type: Primitive,
    },
    /// A sequence of values of a type.
    Sequence {
        /// The type of the elements of the sequence.
        r#type: Box<TypeRef>,
    },
    /// An optional value of a type.
    Option {
        /// The type of the value, if present.
        r#type: Box<TypeRef>,
    },
    /// A value that may indicate success or error.
    Result {
        /// The type of the value, in case of success.
        r#type0: Box<TypeRef>,
        /// The type of the value, in case of error.
        r#type1: Box<TypeRef>,
    },
}

/// The base types that all [`Type`]s defined in the schema are ultimately composed of.
#[derive(Debug, serde::Serialize, serde::Deserialize, Copy, Clone, Hash, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Primitive {
    /// A boolean value.
    Bool,
    /// A 32-bit unsigned integer.
    U32,
    /// A 64-bit unsigned integer.
    U64,
    /// A 32-bit signed integer.
    I32,
    /// A 64-bit signed integer.
    I64,
    /// A unicode codepoint, in the range 0x0000..=0x10FFFF.
    Char,
    /// A UTF-8 string.
    String,
}


// === Serialization ===

/// Describes the serialized layout of a type.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Layout<Id = TypeId> {
    /// The fields, in order. Names are references to the fields defined in the [`Type`].
    pub fields:        Vec<(FieldName, usize)>,
    /// Values that encode the possible child types of this type.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub discriminants: Option<BTreeMap<Discriminant, Id>>,
    /// The number of bytes this type's encoding takes as a field of a containing struct, element
    /// of a sequence, or parent of another type.
    pub size:          usize,
}

/// Number distinguishing different possible child types.
#[derive(Debug, serde::Serialize, serde::Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub struct Discriminant(u32);



// ==================
// === Type Graph ===
// ==================

struct Types {
    types: HashMap<TypeId, Type>,
    ids:   HashMap<meta::TypeId, TypeId>,
}

fn types(graph: &meta::TypeGraph) -> Types {
    let mut next_type_id = 0;
    let mut next_type_id = || {
        let result = TypeId(format!("type_{next_type_id}").into());
        next_type_id += 1;
        result
    };
    let mut type_refs = HashMap::new();
    let mut ids = HashMap::new();
    let mut primitives = vec![];
    // Map struct types; gather primitive types.
    for (key, ty) in &graph.types {
        match &ty.data {
            Data::Struct(_) => {
                let id = next_type_id();
                ids.insert(key, id.clone());
                type_refs.insert(key, TypeRef::Type { id });
            }
            Data::Primitive(prim) => {
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
    let types: HashMap<_, _> = graph
        .types
        .iter()
        .filter_map(|(key, ty)| {
            ty.data.fields().map(|fields| {
                let key_to_id = |id| ids[id].clone();
                (key_to_id(&key), Type {
                    name:   ty.name.to_snake_case().into(),
                    fields: fields
                        .iter()
                        .map(|f| {
                            let name = f.name.to_snake_case().expect("Tuples not supported.");
                            let r#type = type_refs[&f.type_].clone();
                            (FieldName(name.into()), r#type)
                        })
                        .collect(),
                    parent: ty.parent.as_ref().map(key_to_id),
                })
            })
        })
        .collect();
    Types { types, ids }
}



// =====================
// === Serialization ===
// =====================

fn serialization(
    graph: &meta::TypeGraph,
) -> impl Iterator<Item = (meta::TypeId, Layout<meta::TypeId>)> + '_ {
    let sizes = solve_sizes(graph);
    layouts(graph, sizes)
}

const POINTER: usize = 4;

/// Returns the inheritance-size of the object, if `sizes` contains all the needed information to
/// compute this type. The inheritance-size is the shallow size of all the type's fields, including
/// fields inherited from ancestor types.
fn compute_size(
    graph: &meta::TypeGraph,
    key: meta::TypeId,
    sizes: &HashMap<meta::TypeId, usize>,
) -> Option<usize> {
    use meta::Primitive;
    let ty = &graph[key];
    Some(match &ty.data {
        Data::Primitive(Primitive::Bool) => 1,
        Data::Primitive(Primitive::U32 | Primitive::I32 | Primitive::Char) => 4,
        Data::Primitive(Primitive::U64 | Primitive::I64) => 8,
        Data::Primitive(Primitive::Option(_)) => 1 + POINTER,
        Data::Primitive(Primitive::String | Primitive::Sequence(_) | Primitive::Result(_, _)) =>
            POINTER,
        Data::Struct(fields) => {
            let inherited_size =
                if let Some(parent) = ty.parent { *sizes.get(&parent)? } else { 0 };
            let mut fields_size = 0;
            for field in fields {
                let ty = &graph[&field.type_];
                fields_size += if !ty.discriminants.is_empty() {
                    POINTER
                } else {
                    *sizes.get(&field.type_)?
                };
            }
            inherited_size + fields_size
        }
    })
}

fn solve_sizes(graph: &meta::TypeGraph) -> HashMap<meta::TypeId, usize> {
    let mut uncomputed: Vec<_> = graph.types.keys().collect();
    let mut sizes = HashMap::new();
    // Termination: Each step will make progress as long as there is no cycle in the type
    // dependencies. Only an unconditional reference to a type creates a dependency. A cycle in
    // unconditional type references would only occur if the input contained an infinite-sized type.
    //
    // Performance: In the worst case, this implementation requires time quadratic in the number of
    // types (for inputs with deep composition graphs). However, it is simpler and more efficient
    // for *reasonable* inputs than an implementation with better worst-case behavior.
    while !uncomputed.is_empty() {
        let uncomputed_before_step = uncomputed.len();
        uncomputed.retain(|key| match compute_size(graph, *key, &sizes) {
            Some(size) => {
                sizes.insert(*key, size);
                false
            }
            None => true,
        });
        assert_ne!(uncomputed.len(), uncomputed_before_step);
    }
    sizes
}

/// Given the sizes of all types in the graph, compute the field offsets and return the layouts for
/// all the types.
fn layouts(
    graph: &meta::TypeGraph,
    sizes: HashMap<meta::TypeId, usize>,
) -> impl Iterator<Item = (meta::TypeId, Layout<meta::TypeId>)> + '_ {
    graph.types.iter().map(move |(key, ty)| {
        (key, {
            let mut offset = ty.parent.map_or(0, |key| sizes[&key]);
            let fields = ty
                .data
                .fields()
                .map(|fields| {
                    fields
                        .iter()
                        .map(|field| {
                            let entry =
                                (FieldName(field.name.to_snake_case().unwrap().into()), offset);
                            offset += if graph[&field.type_].discriminants.is_empty() {
                                sizes[&field.type_]
                            } else {
                                POINTER
                            };
                            entry
                        })
                        .collect()
                })
                .unwrap_or_default();
            if ty.discriminants.is_empty() {
                Layout { fields, discriminants: None, size: sizes[&key] }
            } else {
                let discriminants = ty
                    .discriminants
                    .iter()
                    .map(|(k, v)| (Discriminant((*k).try_into().unwrap()), *v))
                    .collect();
                Layout { fields, discriminants: Some(discriminants), size: POINTER }
            }
        })
    })
}

impl<Id> Layout<Id> {
    fn map_ids<Id2>(self, f: impl Fn(Id) -> Id2) -> Layout<Id2> {
        let Layout { fields, discriminants, size } = self;
        let discriminants = discriminants
            .map(|discriminants| discriminants.into_iter().map(|(k, v)| (k, f(v))).collect());
        Layout { fields, discriminants, size }
    }
}
