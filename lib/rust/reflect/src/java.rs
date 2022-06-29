//! Models datatype definitions in the Java typesystem.

pub mod bincode;
mod from_generic;
pub mod graphviz;
pub mod implementation;
pub mod syntax;
pub mod transform;

pub use from_generic::from_generic;
use std::collections::BTreeMap;



// ======================
// === Datatype Types ===
// ======================

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldId(u32);

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Class {
    pub name:      String,
    pub params:    Vec<TypeId>,
    pub parent:    Option<TypeId>,
    pub abstract_: bool,
    pub sealed:    bool,
    pub fields:    Vec<Field>,
    pub methods:   Vec<Method>,
    builtin:       bool,
    // Attributes
    discriminants: BTreeMap<usize, TypeId>,
    child_field:   Option<usize>,
}

impl Class {
    pub fn builtin(
        graph: &TypeGraph,
        name: &str,
        fields: impl IntoIterator<Item = TypeId>,
    ) -> Self {
        let params: Vec<_> = fields.into_iter().collect();
        let name = name.to_owned();
        let builtin = true;
        let fields = params
            .iter()
            .map(|&type_| {
                graph.field("data".to_owned(), FieldData::Object { type_, nonnull: true })
            })
            .collect();
        Class { name, params, builtin, fields, ..Default::default() }
    }

    pub fn find_field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|field| &field.name == name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Method {
    Dynamic(Dynamic),
    Raw(syntax::Method),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Dynamic {
    Constructor,
    HashCode,
    Equals,
    ToString,
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

/// Definition of a field.
#[derive(Debug, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub data: FieldData,
    pub id:   FieldId,
}

impl Field {
    pub fn id(&self) -> FieldId {
        self.id
    }
}

/// Typeinfo for a field.
#[derive(Debug, Clone, PartialEq, Eq, Copy, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Copy, PartialOrd, Ord, Hash)]
pub enum Primitive {
    /// Java's `boolean`
    Bool,
    /// Java's `int`
    Int { unsigned: bool },
    /// Java's `long`
    Long { unsigned: bool },
}



// =========================
// === Datatype Builders ===
// =========================


// === Field ===

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldBuilder {
    name: String,
    data: FieldData,
}

impl FieldBuilder {
    pub fn object(name: impl Into<String>, type_: TypeId, nonnull: bool) -> Self {
        let name = name.into();
        let data = FieldData::Object { type_, nonnull };
        Self { name, data }
    }
}



// ============================
// === Systems of Datatypes ===
// ============================

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

    pub fn insert(&mut self, value: Class) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(Some(value));
        id
    }

    pub fn remove(&mut self, TypeId(i): TypeId) -> Class {
        self.types[i].take().unwrap()
    }

    pub fn get(&self, TypeId(i): TypeId) -> Option<&Class> {
        self.types[i].as_ref()
    }

    fn field(&self, name: String, data: FieldData) -> Field {
        let id = self.next_field.get();
        self.next_field.set(id + 1);
        let id = FieldId(id);
        Field { name, data, id }
    }

    pub fn add_field(&mut self, id: TypeId, field: FieldBuilder) -> FieldId {
        let field = self.field(field.name, field.data);
        let field_id = field.id;
        self[id].fields.push(field);
        field_id
    }

    pub fn implement(&self, package: &str) -> Vec<syntax::Class> {
        implementation::implement(self, package)
    }

    pub fn type_ids(&self) -> impl Iterator<Item = TypeId> + '_ {
        self.types.iter().enumerate().filter_map(|(i, ty)| ty.as_ref().map(|_| TypeId(i)))
    }

    pub fn classes(&self) -> impl Iterator<Item = &Class> {
        self.types.iter().filter_map(|ty| ty.as_ref())
    }

    pub fn classes_mut(&mut self) -> impl Iterator<Item = &mut Class> {
        self.types.iter_mut().filter_map(|ty| ty.as_mut())
    }

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
