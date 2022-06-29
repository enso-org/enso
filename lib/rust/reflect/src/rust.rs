use derivative::Derivative;
pub use to_abstracted::to_abstracted;

pub mod graphviz;
mod reflect;
mod to_abstracted;

pub use reflect::Reflect;



// ==================
// === Data model ===
// ==================

/// A type.
#[derive(Debug, Clone)]
pub struct TypeData {
    // data
    pub id:             TypeId,
    pub name:           String,
    pub data:           Data,
    pub subtype_erased: GenericTypeId,
}

/// A type's data content.
#[derive(Debug, Clone)]
pub enum Data {
    Struct(Struct),
    Enum(Enum),
    Primitive(Primitive),
}

/// An `enum`.
#[derive(Debug, Clone)]
pub struct Enum {
    pub variants: Vec<Variant>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    // data
    pub ident:       String,
    pub fields:      Fields,
    // attributes
    pub transparent: bool,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub fields:      Fields,
    // attributes
    pub transparent: bool,
}

#[derive(Debug, Clone)]
pub struct NamedField {
    // data
    pub name:    String,
    pub type_:   LazyType,
    // attributes
    pub subtype: bool,
    pub flatten: bool,
    pub hide:    bool,
}

impl NamedField {
    pub fn flatten(&self, inner: &Self) -> Self {
        let no_flatten_subtype = "Unsupported: flatten with subtype";
        assert!(!self.subtype, "{}", no_flatten_subtype);
        assert!(!inner.subtype, "{}", no_flatten_subtype);
        let name = format!("{}_{}", &self.name, &inner.name);
        let type_ = inner.type_;
        let subtype = false;
        let flatten = inner.flatten;
        let hide = self.hide || inner.hide;
        Self { name, type_, subtype, flatten, hide }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct UnnamedField {
    pub type_: LazyType,
}

#[derive(Debug, Clone)]
pub enum Fields {
    Named(Vec<NamedField>),
    Unnamed(Vec<UnnamedField>),
    Unit,
}

impl Fields {
    pub fn as_wrapped_type(&self) -> Option<LazyType> {
        match self {
            Fields::Named(fields) if fields.len() == 1 => Some(fields[0].type_),
            Fields::Unnamed(fields) if fields.len() == 1 => Some(fields[0].type_),
            _ => None,
        }
    }
}

/// Rust standard types.
#[derive(Debug, Clone, Copy)]
pub enum Primitive {
    Bool,
    Usize,
    U32,
    String,
    Vec(LazyType),
    Option(LazyType),
    Result(LazyType, LazyType),
}



// =======================
// === Type references ===
// =======================

/// Uniquely identifies a type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct TypeId(std::any::TypeId);

impl TypeId {
    /// Obtain a unique identifier for a type.
    pub fn of<T: ?Sized + Reflect>() -> Self {
        Self(std::any::TypeId::of::<T::Static>())
    }
}

/// Distinguishes a type, irrespective of any sole type parameter present in the field marked
/// `#[reflect(subtype)]`, if any. Used in the implementation of the `subtype` transform.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct GenericTypeId(std::any::TypeId);

/// Identifies a type, and can be evaluated to obtain the type's definition.
///
/// This is used for the fields of a type's `TypeData` because type graphs may contain cycles.
#[derive(Copy, Clone, Debug)]
pub struct LazyType {
    pub id:   TypeId,
    evaluate: Thunk<TypeData>,
}

impl LazyType {
    pub fn of<T: ?Sized + Reflect>() -> Self {
        let id = TypeId::of::<T>();
        let evaluate = <T as Reflect>::reflect;
        Self { id, evaluate }
    }

    /// Obtain the type's definition.
    pub fn evaluate(&self) -> TypeData {
        (self.evaluate)()
    }
}

type Thunk<T> = fn() -> T;



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

/// Type reference traversal.
pub trait ReferencedTypes {
    /// Identify all the types this type contains references to.
    fn referenced_types(&self) -> Vec<LazyType>;
}

impl TypeData {
    /// Return whether this type is a `Primitive`.
    pub fn is_primitive(&self) -> bool {
        matches!(&self.data, Data::Primitive(_))
    }

    /// Get information about the composition operator relating the types this type is composed of.
    pub fn type_type(&self) -> TypeType {
        match &self.data {
            Data::Struct(_) => TypeType::Product,
            Data::Enum(_) => TypeType::Sum,
            Data::Primitive(primitive) => primitive.type_type(),
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

impl ReferencedTypes for Variant {
    fn referenced_types(&self) -> Vec<LazyType> {
        self.fields.referenced_types()
    }
}

impl ReferencedTypes for Struct {
    fn referenced_types(&self) -> Vec<LazyType> {
        self.fields.referenced_types()
    }
}

impl NamedField {
    pub fn type_id(&self) -> TypeId {
        self.type_.id
    }

    pub fn type_(&self) -> TypeData {
        self.type_.evaluate()
    }
}

impl ReferencedTypes for NamedField {
    fn referenced_types(&self) -> Vec<LazyType> {
        vec![self.type_]
    }
}

impl UnnamedField {
    pub fn type_id(&self) -> TypeId {
        self.type_.id
    }

    pub fn type_(&self) -> TypeData {
        self.type_.evaluate()
    }
}

impl ReferencedTypes for UnnamedField {
    fn referenced_types(&self) -> Vec<LazyType> {
        vec![self.type_]
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
