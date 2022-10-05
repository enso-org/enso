//! Representation of data models in the Rust typesystem.
//!
//! Unlike the other metamodels in this crate, the Rust model uses a lazy-evaluation representation
//! of type graphs. While this representation doesn't support analysis as easily as the
//! `crate::data_structures::VecMap` representation, it can be created by a context-free translation
//! from Rust syntax, so it can be built directly by a proc macro, like [`enso_reflect`].


// ==============
// === Export ===
// ==============

pub use to_meta::to_meta;



#[cfg(feature = "graphviz")]
mod graphviz;
mod to_meta;



// ==================
// === Data model ===
// ==================

/// A type.
#[derive(Debug, Clone)]
pub struct TypeData {
    /// A value uniquely identifying the type.
    pub id:             TypeId,
    /// The Rust identifier of the type.
    pub name:           String,
    /// The type's contents.
    pub data:           Data,
    /// A value uniquely-identifying the type up to the type of a certain field.
    pub subtype_erased: GenericTypeId,
}

/// A type's data content.
#[derive(Debug, Clone)]
pub enum Data {
    /// A `struct`.
    Struct(Struct),
    /// An `enum`.
    Enum(Enum),
    /// Builtins, including basic types ands generics.
    Primitive(Primitive),
}

/// An `enum`.
#[derive(Debug, Clone)]
pub struct Enum {
    /// The variants.
    pub variants: Vec<Variant>,
}

/// A possible value of an `enum`.
#[derive(Debug, Clone)]
pub struct Variant {
    /// The variant's name.
    pub ident:  String,
    /// The variant's data.
    pub fields: Fields,
    /// If true, when abstracting to the `meta` representation, rather than generate a type for
    /// this variant, its (sole) field will become a child of the parent enum.
    pub inline: bool,
}

/// A `struct`.
#[derive(Debug, Clone)]
pub struct Struct {
    /// The fields.
    pub fields:      Fields,
    /// If true, this field should be passed-through to its (sole) field when abstracting to the
    /// `meta` representation.
    pub transparent: bool,
}

/// A field with a name.
#[derive(Debug, Clone)]
pub struct NamedField {
    #[allow(missing_docs)]
    pub name:    String,
    /// The abstract identifier of the type, if different from its name in Rust.
    pub rename:  Option<String>,
    #[allow(missing_docs)]
    pub type_:   LazyType,
    /// If true, this type should become the parent of the type in this field.
    pub subtype: bool,
    /// If true, the fields of this field should be inserted in place of it.
    pub flatten: bool,
    /// If true, this field should be hidden in generated code, in a target-language-dependent
    /// manner.
    pub hide:    bool,
}

/// A field in a tuple struct or tuple variant.
#[derive(Debug, Copy, Clone)]
pub struct UnnamedField {
    #[allow(missing_docs)]
    pub type_: LazyType,
}

/// The data of a struct or variant.
#[derive(Debug, Clone)]
pub enum Fields {
    #[allow(missing_docs)]
    Named(Vec<NamedField>),
    #[allow(missing_docs)]
    Unnamed(Vec<UnnamedField>),
    #[allow(missing_docs)]
    Unit,
}

/// Rust standard types.
#[derive(Debug, Clone, Copy)]
pub enum Primitive {
    /// A `bool`.
    Bool,
    /// A `usize`.
    Usize,
    /// A `u32`.
    U32,
    /// An `i32`.
    I32,
    /// A `char`.
    Char,
    /// A `String`.
    String,
    /// A `Vec<_>`.
    Vec(LazyType),
    /// An `Option<_>`.
    Option(LazyType),
    /// A `Result<_, _>`.
    Result(LazyType, LazyType),
}



// =======================
// === Type references ===
// =======================

/// Uniquely identifies a type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct TypeId(std::any::TypeId);

impl TypeId {
    #[allow(missing_docs)]
    pub fn new(id: std::any::TypeId) -> Self {
        Self(id)
    }
}

/// Distinguishes a type, irrespective of any sole type parameter present in the field marked
/// `#[reflect(subtype)]`, if any. Used in the implementation of the `subtype` transform.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct GenericTypeId(std::any::TypeId);

impl GenericTypeId {
    #[allow(missing_docs)]
    pub fn new(id: std::any::TypeId) -> Self {
        Self(id)
    }
}

/// Identifies a type, and can be evaluated to obtain the type's definition.
///
/// This is used for the fields of a type's `TypeData` because type graphs may contain cycles.
#[derive(Copy, Clone, Debug)]
pub struct LazyType {
    #[allow(missing_docs)]
    pub id:   TypeId,
    evaluate: Thunk<TypeData>,
}

impl LazyType {
    #[allow(missing_docs)]
    pub fn new(id: TypeId, evaluate: Thunk<TypeData>) -> Self {
        Self { id, evaluate }
    }

    /// Obtain the type's definition.
    pub fn evaluate(&self) -> TypeData {
        (self.evaluate)()
    }
}

type Thunk<T> = fn() -> T;


// === Reference Traversal ===

/// Type reference traversal.
pub trait ReferencedTypes {
    /// Identify all the types this type contains references to.
    fn referenced_types(&self) -> Vec<LazyType>;
}

impl ReferencedTypes for Primitive {
    fn referenced_types(&self) -> Vec<LazyType> {
        match self {
            Primitive::Bool
            | Primitive::Usize
            | Primitive::String
            | Primitive::U32
            | Primitive::I32
            | Primitive::Char => vec![],
            Primitive::Vec(ty) | Primitive::Option(ty) => vec![*ty],
            Primitive::Result(ty0, ty1) => vec![*ty0, *ty1],
        }
    }
}

impl ReferencedTypes for TypeData {
    fn referenced_types(&self) -> Vec<LazyType> {
        self.data.referenced_types()
    }
}

impl ReferencedTypes for Data {
    fn referenced_types(&self) -> Vec<LazyType> {
        match self {
            Data::Struct(struct_) => struct_.referenced_types(),
            Data::Enum(enum_) => enum_.referenced_types(),
            Data::Primitive(primitive) => primitive.referenced_types(),
        }
    }
}

impl ReferencedTypes for Enum {
    fn referenced_types(&self) -> Vec<LazyType> {
        let mut referenced = vec![];
        for variant in &self.variants {
            referenced.extend(variant.referenced_types());
        }
        referenced
    }
}

impl ReferencedTypes for Struct {
    fn referenced_types(&self) -> Vec<LazyType> {
        self.fields.referenced_types()
    }
}

impl ReferencedTypes for Variant {
    fn referenced_types(&self) -> Vec<LazyType> {
        self.fields.referenced_types()
    }
}

impl ReferencedTypes for Fields {
    fn referenced_types(&self) -> Vec<LazyType> {
        match self {
            Fields::Named(fields) => fields.iter().map(|field| field.type_).collect(),
            Fields::Unnamed(fields) => fields.iter().map(|field| field.type_).collect(),
            Fields::Unit => vec![],
        }
    }
}

impl ReferencedTypes for NamedField {
    fn referenced_types(&self) -> Vec<LazyType> {
        vec![self.type_]
    }
}

impl ReferencedTypes for UnnamedField {
    fn referenced_types(&self) -> Vec<LazyType> {
        vec![self.type_]
    }
}



// ====================================
// === Abstractions over data model ===
// ====================================

/// Categorizes types by the nature of their composition operators.
#[derive(Copy, Clone, Debug)]
pub enum TypeType {
    /// A type like an `enum`, that only contains data for one of its constituent types.
    Sum,
    /// A type like a `struct` or tuple, that contains data for all of its constituent types.
    Product,
}

impl TypeData {
    /// Get information about the composition operator relating the types this type is composed of.
    pub fn type_type(&self) -> TypeType {
        match &self.data {
            Data::Struct(_) => TypeType::Product,
            Data::Enum(_) => TypeType::Sum,
            Data::Primitive(primitive) => primitive.type_type(),
        }
    }
}

impl Primitive {
    /// Get information about the composition operator relating the types this type is composed of.
    pub fn type_type(&self) -> TypeType {
        match self {
            Primitive::Bool
            | Primitive::Usize
            | Primitive::U32
            | Primitive::I32
            | Primitive::Char
            | Primitive::String
            | Primitive::Vec(_) => TypeType::Product,
            Primitive::Option(_) | Primitive::Result(_, _) => TypeType::Sum,
        }
    }
}



// ================================
// === Operations on data model ===
// ================================

impl Fields {
    /// Get the sole field this type contains, if it has exactly one.
    pub fn as_wrapped_type(&self) -> Option<LazyType> {
        match self {
            Fields::Named(fields) if fields.len() == 1 => Some(fields[0].type_),
            Fields::Unnamed(fields) if fields.len() == 1 => Some(fields[0].type_),
            _ => None,
        }
    }
}

impl TypeData {
    /// Return whether this type is a `Primitive`.
    pub fn is_primitive(&self) -> bool {
        matches!(&self.data, Data::Primitive(_))
    }
}

impl NamedField {
    #[allow(missing_docs)]
    pub fn type_id(&self) -> TypeId {
        self.type_.id
    }

    #[allow(missing_docs)]
    pub fn type_(&self) -> TypeData {
        self.type_.evaluate()
    }
}

impl UnnamedField {
    #[allow(missing_docs)]
    pub fn type_id(&self) -> TypeId {
        self.type_.id
    }

    #[allow(missing_docs)]
    pub fn type_(&self) -> TypeData {
        self.type_.evaluate()
    }
}



// ========================
// === GraphViz support ===
// ========================

#[cfg(feature = "graphviz")]
impl From<LazyType> for crate::graphviz::Graph {
    fn from(root: LazyType) -> Self {
        graphviz::graph(root)
    }
}
