//! A language-independent metamodel for representing data models.
//!
//! This is used as an intermediate representation in translation from Rust to Java to:
//! - Decouple the complexities of the source language from those of the target language.
//! - Provide a simple representation in which to apply transformations.
//!
//! It is also used for language-independent analysis of data models.



#[cfg(feature = "graphviz")]
mod graphviz;
pub mod serialization;
pub mod transform;

use crate::data_structures::VecMap;
use derive_more::Index;
use derive_more::IndexMut;
use std::collections::BTreeMap;
use std::collections::BTreeSet;



// ==============================
// === Type Parameterizations ===
// ==============================

/// Globally unique, stable identifier for a `Field`.
pub type FieldId = crate::data_structures::Id<Field>;

/// Identfies a type within a `TypeGraph`.
pub type TypeId = crate::data_structures::vecmap::Key<Type>;
/// Identfies an unbound type within a `TypeGraph`.
pub type UnboundTypeId = crate::data_structures::vecmap::UnboundKey<Type>;



// ======================
// === Datatype Types ===
// ======================

/// A datatype.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Type {
    /// The type's name.
    pub name:          TypeName,
    /// The type's data content.
    pub data:          Data,
    /// The parent type, if any.
    pub parent:        Option<TypeId>,
    /// If true, this type cannot be instantiated.
    pub abstract_:     bool,
    /// If true, this type is not open to extension by children outside those defined with it.
    pub closed:        bool,
    /// When serializing/deserializing, indicates the index of the field in a `Type` before which a
    /// child object's data will be placed/expected.
    pub child_field:   Option<usize>,
    /// When serializing/deserializing, indicates the available concrete types and the values used
    /// to identify them.
    pub discriminants: BTreeMap<usize, TypeId>,
}

impl Type {
    /// Create a new datatype, with defaults for most fields.
    pub fn new(name: TypeName, data: Data) -> Self {
        let parent = Default::default();
        let abstract_ = Default::default();
        let closed = Default::default();
        let child_field = Default::default();
        let discriminants = Default::default();
        Type { name, data, parent, abstract_, closed, child_field, discriminants }
    }
}

/// A datatype's data.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Data {
    /// A type with fields.
    Struct(Vec<Field>),
    /// A builtin type.
    Primitive(Primitive),
}

impl Data {
    /// If this is a [`Data::Struct`], return its fields.
    pub fn fields(&self) -> Option<&[Field]> {
        match self {
            Data::Struct(fields) => Some(&fields[..]),
            _ => None,
        }
    }
}

/// Standard types.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Primitive {
    /// A boolean value.
    Bool,
    /// An unsigned 32-bit integer.
    U32,
    /// An unsigned 64-bit integer.
    U64,
    /// A signed 32-bit integer.
    I32,
    /// A signed 64-bit integer.
    I64,
    /// A unicode code point (in the range 0 to 0x10FFFF).
    Char,
    /// An UTF-8-encoded string.
    String,
    /// Zero or more values of a type.
    Sequence(TypeId),
    /// Zero or one value of a type.
    Option(TypeId),
    /// A value that may be one type in a success case, or another type in a failure case.
    Result(TypeId, TypeId),
}

/// A data field of a `Type`.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Field {
    /// The field's `Type`.
    pub type_: TypeId,
    /// The field's name.
    pub name:  FieldName,
    /// Whether the field should be private in generated code.
    pub hide:  bool,
    id:        FieldId,
}

impl Field {
    /// Create a new named field.
    pub fn named(name: FieldName, type_: TypeId) -> Self {
        let hide = Default::default();
        let id = Default::default();
        Self { type_, name, hide, id }
    }

    /// Create a new unnamed field.
    pub fn unnamed(type_: TypeId) -> Self {
        let name = Default::default();
        let hide = Default::default();
        let id = Default::default();
        Self { name, type_, hide, id }
    }

    /// Get the field's `FieldId`.
    pub fn id(&self) -> FieldId {
        self.id
    }
}



// ===================
// === Identifiers ===
// ===================

/// An identifier, in a naming convention-agnostic representation.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Identifier {
    segments: Vec<String>,
}

impl Identifier {
    fn new(segments: Vec<String>) -> Self {
        for segment in &segments {
            assert!(!segment.is_empty());
        }
        Self { segments }
    }

    fn segments_len(&self) -> usize {
        let mut n = 0;
        for segment in &self.segments {
            n += segment.len();
        }
        n
    }

    /// Render in PascalCase.
    pub fn to_pascal_case(&self) -> String {
        let mut pascal = String::with_capacity(self.segments_len() + self.segments.len());
        for segment in &self.segments {
            let mut chars = segment.chars();
            pascal.push(chars.next().unwrap().to_ascii_uppercase());
            pascal.extend(chars);
        }
        pascal
    }

    /// Render in camelCase.
    pub fn to_camel_case(&self) -> String {
        let mut camel = String::with_capacity(self.segments_len());
        if let Some((head, tail)) = self.segments.split_first() {
            camel.push_str(head);
            for segment in tail {
                let mut chars = segment.chars();
                camel.push(chars.next().unwrap().to_ascii_uppercase());
                camel.extend(chars);
            }
        }
        camel
    }

    /// Render in snake_case.
    pub fn to_snake_case(&self) -> String {
        self.segments.join("_")
    }

    /// Parse an identifier expected to be in snake_case.
    pub fn from_snake_case(s: &str) -> Self {
        let segments = s.split('_').map(|s| s.to_string()).collect();
        Self::new(segments)
    }

    /// Parse an identifier expected to be in camelCase.
    pub fn from_camel_case(s: &str) -> Self {
        Self::from_pascal_case(s)
    }

    /// Parse an identifier expected to be in PascalCase.
    pub fn from_pascal_case(s: &str) -> Self {
        let mut segments = vec![];
        let mut current = String::new();
        for c in s.chars() {
            if c.is_ascii_uppercase() && !current.is_empty() {
                segments.push(std::mem::take(&mut current));
            }
            current.push(c.to_ascii_lowercase());
        }
        segments.push(current);
        Self::new(segments)
    }

    /// Append another `Identifier` to the end of `self`; when rendered, the boundary between the
    /// old and new components will be indicated in a manner determined by the naming convention
    /// chosen at rendering time.
    pub fn append(&mut self, other: Self) {
        self.segments.extend(other.segments)
    }

    /// Return whether this identifier is zero-length.
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
}


// === Type Names ===

/// The name of a type, e.g. a `struct` in Rust or a `class` in Java.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeName(Identifier);

impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.to_pascal_case())
    }
}

impl TypeName {
    /// Parse from PascalCase.
    pub fn from_pascal_case(s: &str) -> Self {
        Self(Identifier::from_pascal_case(s))
    }
    /// Render in PascalCase.
    pub fn to_pascal_case(&self) -> String {
        self.0.to_pascal_case()
    }
    /// Render in snake_case.
    pub fn to_snake_case(&self) -> String {
        self.0.to_snake_case()
    }
    /// Append another `TypeName` to the end of `self`. See `Identifier::append`.
    pub fn append(&mut self, other: Self) {
        self.0.append(other.0)
    }
}


// === Field Names ===

/// The name of a field, e.g. the data members of a Rust struct or Java class.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct FieldName(Identifier);

impl std::fmt::Display for FieldName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0.to_camel_case())
    }
}

impl FieldName {
    /// Parse from snake_case.
    pub fn from_snake_case(s: &str) -> Self {
        Self(Identifier::from_snake_case(s))
    }
    /// Render in camelCase.
    pub fn to_camel_case(&self) -> Option<String> {
        match self.0.to_camel_case() {
            ident if ident.is_empty() => None,
            ident => Some(ident),
        }
    }
    /// Render in snake_case.
    pub fn to_snake_case(&self) -> Option<String> {
        match self.0.to_snake_case() {
            ident if ident.is_empty() => None,
            ident => Some(ident),
        }
    }
    /// Append another `FieldName` to the end of `self`. See `Identifier::append`.
    pub fn append(&mut self, other: Self) {
        self.0.append(other.0)
    }
    /// Return whether this identifier is zero-length.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    /// Return a reference to the underlying identifier.
    pub fn as_identifier(&self) -> &Identifier {
        &self.0
    }
}



// ===========================
// === System of Datatypes ===
// ===========================

/// A collection of [`Type`]s. The [`TypeGraph`] owns its types; they do not refer to each other
/// directly, but through [`TypeId`]s, which must be looked up in the graph (its [`Index`]
/// implementation provides a convenient interface).
#[derive(Debug, Default, Clone, Index, IndexMut)]
pub struct TypeGraph {
    #[index]
    #[index_mut]
    #[allow(missing_docs)]
    pub types: VecMap<Type>,
}

impl TypeGraph {
    /// Create a new, empty [`TypeGraph`].
    pub fn new() -> Self {
        Default::default()
    }

    /// For every `(id0, id1)` pair in the input, replace all occurrences of `id0` in any type
    /// definition with the corresponding `id1`.
    pub fn apply_aliases<'a>(&mut self, aliases: impl IntoIterator<Item = &'a (TypeId, TypeId)>) {
        let mut canonical = BTreeMap::new();
        for (from_, to_) in aliases.into_iter() {
            canonical.insert(*from_, *to_);
        }
        let rewrite = |id: &mut TypeId| {
            if let Some(id_) = canonical.get(id) {
                *id = *id_;
            }
        };
        for ty in self.types.values_mut() {
            if let Some(parent) = &mut ty.parent {
                rewrite(parent);
            }
            match &mut ty.data {
                Data::Struct(fields) =>
                    for field in fields {
                        rewrite(&mut field.type_);
                    },
                Data::Primitive(Primitive::Sequence(t0))
                | Data::Primitive(Primitive::Option(t0)) => rewrite(t0),
                Data::Primitive(Primitive::Result(t0, t1)) => {
                    rewrite(t0);
                    rewrite(t1);
                }
                Data::Primitive(Primitive::U32)
                | Data::Primitive(Primitive::U64)
                | Data::Primitive(Primitive::I32)
                | Data::Primitive(Primitive::I64)
                | Data::Primitive(Primitive::Bool)
                | Data::Primitive(Primitive::Char)
                | Data::Primitive(Primitive::String) => {}
            }
        }
    }

    /// Eliminate types that are not in the referential transitive closure of the given collection
    /// of roots.
    pub fn gc(&mut self, roots: impl IntoIterator<Item = TypeId>) {
        let mut visited = BTreeSet::new();
        let mut to_visit = BTreeSet::new();
        to_visit.extend(roots);
        while let Some(id) = to_visit.pop_last() {
            let Type {
                name: _,
                data,
                parent,
                abstract_: _,
                closed: _,
                child_field: _,
                discriminants,
            } = &self.types[id];
            let already_visited = !visited.insert(id);
            if already_visited {
                continue;
            }
            if let Some(parent) = parent {
                to_visit.insert(*parent);
            }
            to_visit.extend(discriminants.values());
            match data {
                Data::Struct(fields) => to_visit.extend(fields.iter().map(|field| field.type_)),
                Data::Primitive(Primitive::Sequence(t0))
                | Data::Primitive(Primitive::Option(t0)) => {
                    to_visit.insert(*t0);
                }
                Data::Primitive(Primitive::Result(t0, t1)) => {
                    to_visit.insert(*t0);
                    to_visit.insert(*t1);
                }
                Data::Primitive(Primitive::U32)
                | Data::Primitive(Primitive::U64)
                | Data::Primitive(Primitive::I32)
                | Data::Primitive(Primitive::I64)
                | Data::Primitive(Primitive::Bool)
                | Data::Primitive(Primitive::Char)
                | Data::Primitive(Primitive::String) => {}
            }
        }
        let live = |id: &TypeId| visited.contains(id);
        let ids: Vec<_> = self.types.keys().collect();
        for id in ids {
            if !live(&id) {
                self.types.remove(id);
            }
        }
    }
}

impl TypeGraph {
    /// Return the type's hierarchy. The first element of the hierarchy is the type itself; each
    /// element followed by its parent, if it has one.
    pub fn hierarchy(&self, id: TypeId) -> Vec<TypeId> {
        let mut hierarchy = vec![];
        let mut id_ = Some(id);
        while let Some(id) = id_ {
            hierarchy.push(id);
            id_ = self[id].parent;
        }
        hierarchy
    }
}


// === GraphViz support ===

#[cfg(feature = "graphviz")]
impl From<&'_ TypeGraph> for crate::graphviz::Graph {
    fn from(graph: &'_ TypeGraph) -> Self {
        graphviz::graph(graph)
    }
}
