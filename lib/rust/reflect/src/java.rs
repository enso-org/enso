//! Models datatype definitions in the Java typesystem.

pub mod bincode;
mod from_generic;
pub mod graphviz;
pub mod implementation;
mod syntax;
pub mod transform;

pub use from_generic::from_generic;
use std::collections::BTreeMap;



// ======================
// === Datatype Types ===
// ======================

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeId(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    name:          String,
    params:        Vec<TypeId>,
    parent:        Option<TypeId>,
    abstract_:     bool,
    builtin:       bool,
    sealed:        bool,
    fields:        Vec<Field>,
    methods:       Vec<Method>,
    // Attributes
    discriminants: BTreeMap<usize, TypeId>,
}

impl Class {
    fn builtin(name: &str, fields: impl IntoIterator<Item = TypeId>) -> Self {
        let params: Vec<_> = fields.into_iter().collect();
        let name = name.to_owned();
        let parent = None;
        let abstract_ = false;
        let builtin = true;
        let fields = params
            .iter()
            .map(|&type_| Field {
                name:   "data".to_owned(),
                data:   FieldData::Object { type_, nonnull: true },
                getter: Default::default(),
            })
            .collect();
        let methods = Default::default();
        let discriminants = Default::default();
        let sealed = Default::default();
        Class { name, params, parent, abstract_, builtin, sealed, fields, methods, discriminants }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Method {
    Dynamic(Dynamic),
    Raw(syntax::Method),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Dynamic {
    Constructor,
    HashCode,
    Equals,
    ToString,
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

/// Definition of a field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    name:   String,
    data:   FieldData,
    getter: bool,
}

/// Typeinfo for a field.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum FieldData {
    Object { type_: TypeId, nonnull: bool },
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
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Primitive {
    /// Java's `boolean`
    Bool,
    /// Java's `int`
    Int { unsigned: bool },
    /// Java's `long`
    Long { unsigned: bool },
}



// ============================
// === Systems of Datatypes ===
// ============================

#[derive(Debug, Default)]
pub struct TypeGraph {
    types: Vec<Option<Class>>,
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

    fn insert(&mut self, value: Class) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(Some(value));
        id
    }

    pub fn remove(&mut self, TypeId(i): TypeId) -> Class {
        self.types[i].take().unwrap()
    }

    pub fn implement(&self) -> Vec<syntax::Class> {
        implementation::implement(self)
    }

    pub fn type_ids(&self) -> impl Iterator<Item=TypeId> + '_ {
        self.types.iter().enumerate().filter_map(|(i, ty)| ty.as_ref().map(|_| TypeId(i)))
    }

    pub fn classes(&self) -> impl Iterator<Item=&Class> {
        self.types.iter().filter_map(|ty| ty.as_ref())
    }

    pub fn classes_mut(&mut self) -> impl Iterator<Item=&mut Class> {
        self.types.iter_mut().filter_map(|ty| ty.as_mut())
    }
}
