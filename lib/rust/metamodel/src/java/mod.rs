//! Representation of datatype definitions in the Java typesystem.

pub mod bincode;
mod from_meta;
#[cfg(feature = "graphviz")]
pub mod graphviz;
mod implementation;
pub mod syntax;
pub mod transform;

pub use from_meta::from_meta;
pub use implementation::implement as to_syntax;
use std::collections::BTreeMap;



// ======================
// === Datatype Types ===
// ======================

/// Identifies a type in a `TypeGraph`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

/// Globally unique, stable identifier for a `Field`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldId(u32);

/// A Java class.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Class {
    /// The name of the class, not including package.
    pub name:      String,
    /// Parameters of a generic class.
    pub params:    Vec<TypeId>,
    /// The parent class, if any.
    pub parent:    Option<TypeId>,
    /// Whether this class is `abstract`.
    pub abstract_: bool,
    /// Whether this class is `sealed`.
    pub sealed:    bool,
    /// The data fields.
    pub fields:    Vec<Field>,
    /// The class's methods.
    pub methods:   Vec<Method>,
    builtin:       bool,
    // Attributes
    discriminants: BTreeMap<usize, TypeId>,
    child_field:   Option<usize>,
}

impl Class {
    /// Create a new "builtin" class.
    pub fn builtin(name: &str, fields: impl IntoIterator<Item = TypeId>) -> Self {
        let params: Vec<_> = fields.into_iter().collect();
        let name = name.to_owned();
        let builtin = true;
        let fields = params.iter().map(|&type_| Field::object("data", type_, true)).collect();
        Class { name, params, builtin, fields, ..Default::default() }
    }

    /// Get a field by name.
    pub fn find_field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|field| field.name == name)
    }
}

/// A method of a class.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Method {
    /// A `Dynamic` method.
    Dynamic(Dynamic),
    /// A literal method implementation.
    Raw(syntax::Method),
}

/// A method that is rendered to syntax on demand.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Dynamic {
    /// A constructor.
    Constructor,
    /// `hashCode` method.
    HashCode,
    /// `equals` method.
    Equals,
    /// `toString` method.
    ToString,
    /// A read-accessor for a field.
    Getter(FieldId),
}

impl From<Dynamic> for Method {
    fn from(method: Dynamic) -> Self {
        Method::Dynamic(method)
    }
}

fn abstract_methods() -> Vec<Method> {
    vec![Dynamic::Constructor.into()]
}

fn standard_methods() -> Vec<Method> {
    vec![
        Dynamic::Constructor.into(),
        Dynamic::HashCode.into(),
        Dynamic::Equals.into(),
        Dynamic::ToString.into(),
    ]
}

/// A data field of a class.
#[derive(Debug, PartialEq, Eq)]
pub struct Field {
    #[allow(missing_docs)]
    pub name: String,
    #[allow(missing_docs)]
    pub data: FieldData,
    id:       FieldId,
}

impl Field {
    /// Create a field referencing a `Class` of a specified type.
    pub fn object(name: impl Into<String>, type_: TypeId, nonnull: bool) -> Self {
        let name = name.into();
        let data = FieldData::Object { type_, nonnull };
        let id = Self::new_id();
        Self { name, data, id }
    }

    /// Create a field holding primitive data.
    pub fn primitive(name: impl Into<String>, primitive: Primitive) -> Self {
        let name = name.into();
        let data = FieldData::Primitive(primitive);
        let id = Self::new_id();
        Self { name, data, id }
    }

    #[allow(missing_docs)]
    pub fn id(&self) -> FieldId {
        self.id
    }

    fn new_id() -> FieldId {
        use std::sync::atomic;
        static NEXT_ID: atomic::AtomicU32 = atomic::AtomicU32::new(0);
        FieldId(NEXT_ID.fetch_add(1, atomic::Ordering::Relaxed))
    }
}

/// A field's data contents.
#[derive(Debug, Clone, PartialEq, Eq, Copy, PartialOrd, Ord, Hash)]
pub enum FieldData {
    /// A reference to an object.
    Object {
        #[allow(missing_docs)]
        type_:   TypeId,
        /// If `true`, this field should be subject to null-checking in constructors, and can be
        /// assumed always to be present.
        nonnull: bool,
    },
    /// An unboxed primitive.
    Primitive(Primitive),
}

impl FieldData {
    fn fmt_equals(&self, a: &str, b: &str) -> String {
        match self {
            FieldData::Object { .. } => format!("{}.equals({})", a, b),
            FieldData::Primitive(_) => format!("({} == {})", a, b),
        }
    }
}

/// An unboxed type; i.e. a type that is not a subtype of `java.lang.Object`.
#[derive(Debug, Clone, PartialEq, Eq, Copy, PartialOrd, Ord, Hash)]
pub enum Primitive {
    /// Java's `boolean`
    Bool,
    /// Java's `int`
    Int {
        /// If `true`, arithmetic on this value is to be performed with unsigned operations.
        unsigned: bool,
    },
    /// Java's `long`
    Long {
        /// If `true`, arithmetic on this value is to be performed with unsigned operations.
        unsigned: bool,
    },
}



// ============================
// === Systems of Datatypes ===
// ============================

/// A system of Java `Class`es.
#[derive(Debug, Default)]
pub struct TypeGraph {
    types:      Vec<Option<Class>>,
    next_field: std::cell::Cell<u32>,
}

impl std::ops::Index<TypeId> for TypeGraph {
    type Output = Class;
    fn index(&self, TypeId(i): TypeId) -> &Self::Output {
        self.types.get(i).unwrap().as_ref().unwrap()
    }
}

impl std::ops::Index<&TypeId> for TypeGraph {
    type Output = Class;
    fn index(&self, id: &TypeId) -> &Self::Output {
        &self[*id]
    }
}

impl std::ops::IndexMut<TypeId> for TypeGraph {
    fn index_mut(&mut self, TypeId(i): TypeId) -> &mut Self::Output {
        self.types.get_mut(i).unwrap().as_mut().unwrap()
    }
}

impl std::ops::IndexMut<&TypeId> for TypeGraph {
    fn index_mut(&mut self, id: &TypeId) -> &mut Self::Output {
        &mut self[*id]
    }
}

impl TypeGraph {
    fn reserve_type_id(&mut self) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(None);
        id
    }

    fn set(&mut self, id: TypeId, value: Class) {
        self.types[id.0] = Some(value);
    }

    /// Add a `Class` to the graph; return its assigned ID.
    pub fn insert(&mut self, value: Class) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(Some(value));
        id
    }

    /// Remove a `Class` from the graph; its ID becomes unbound.
    pub fn remove(&mut self, TypeId(i): TypeId) -> Class {
        self.types[i].take().unwrap()
    }

    /// Get a `Class` if the ID is bound.
    pub fn get(&self, TypeId(i): TypeId) -> Option<&Class> {
        self.types[i].as_ref()
    }

    /// Iterate all defined `Class`es by `TypeId`.
    pub fn type_ids(&self) -> impl Iterator<Item = TypeId> + '_ {
        self.types.iter().enumerate().filter_map(|(i, ty)| ty.as_ref().map(|_| TypeId(i)))
    }

    /// Iterate all defined `Class`es.
    pub fn classes(&self) -> impl Iterator<Item = &Class> {
        self.types.iter().filter_map(|ty| ty.as_ref())
    }

    /// Iterate all defined `Class`es mutably.
    pub fn classes_mut(&mut self) -> impl Iterator<Item = &mut Class> {
        self.types.iter_mut().filter_map(|ty| ty.as_mut())
    }

    /// Look up a `Class` by name.
    pub fn find_by_name(&self, name: &str) -> Option<TypeId> {
        let mut class = None;
        for id in self.type_ids() {
            if self[id].name == name {
                assert_eq!(class, None);
                class = Some(id);
            }
        }
        class
    }
}
