use derivative::Derivative;
pub use genericize::to_generic;

mod genericize;

// =======================
// === Type references ===
// =======================

type Thunk<T> = fn() -> T;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct TypeId(std::any::TypeId);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct GenericTypeId(std::any::TypeId);

impl TypeId {
    pub fn of<T: ?Sized + Reflect>() -> Self {
        Self(std::any::TypeId::of::<T::Static>())
    }
}

#[derive(Clone, Debug)]
pub struct LazyType {
    pub id:       TypeId,
    pub evaluate: Thunk<TypeData>,
}



// ==================
// === Data model ===
// ==================

/// A type.
#[derive(Debug)]
pub struct TypeData {
    // data
    pub id:             TypeId,
    pub name:           String,
    pub data:           Data,
    pub subtype_erased: GenericTypeId,
    // attributes
    pub transparent:    bool,
}

/// A type's data content.
#[derive(Debug)]
pub enum Data {
    Struct(Struct),
    Enum(Enum),
    Primitive(Primitive),
}

/// An `enum`.
#[derive(Debug)]
pub struct Enum {
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    // data
    pub ident:       String,
    pub fields:      Fields,
    // attributes
    pub transparent: bool,
}

/// A `struct` with named fields.
#[derive(Debug)]
pub struct Struct {
    pub fields: Fields,
}

#[derive(Debug)]
pub struct NamedField {
    // data
    pub name:    String,
    pub type_:   LazyType,
    // attributes
    pub subtype: bool,
}

#[derive(Debug)]
pub struct UnnamedField {
    pub type_: LazyType,
}

#[derive(Debug)]
pub enum Fields {
    Named(Vec<NamedField>),
    Unnamed(Vec<UnnamedField>),
    Unit,
}

impl Fields {
    pub fn as_wrapped_type(&self) -> Option<&LazyType> {
        match self {
            Fields::Named(fields) if fields.len() == 1 => Some(&fields[0].type_),
            Fields::Unnamed(fields) if fields.len() == 1 => Some(&fields[0].type_),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Bool,
    Usize,
    U32,
    String,
    Vec(LazyType),
    Option(LazyType),
    Result(LazyType, LazyType),
}



// ====================================
// === Abstractions over data model ===
// ====================================

pub enum TypeType {
    Sum,
    Product,
}

pub trait ReferencedTypes {
    fn referenced_types(&self) -> Vec<LazyType>;
}

impl TypeData {
    pub fn is_primitive(&self) -> bool {
        matches!(&self.data, Data::Primitive(_))
    }

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
        (self.type_.evaluate)()
    }
}

impl ReferencedTypes for NamedField {
    fn referenced_types(&self) -> Vec<LazyType> {
        let type_ = self.type_.clone();
        vec![type_]
    }
}

impl UnnamedField {
    pub fn type_id(&self) -> TypeId {
        self.type_.id
    }

    pub fn type_(&self) -> TypeData {
        (self.type_.evaluate)()
    }
}

impl ReferencedTypes for UnnamedField {
    fn referenced_types(&self) -> Vec<LazyType> {
        let type_ = self.type_.clone();
        vec![type_]
    }
}

impl ReferencedTypes for Fields {
    fn referenced_types(&self) -> Vec<LazyType> {
        match self {
            Fields::Named(fields) => fields.iter().map(|field| field.type_.clone()).collect(),
            Fields::Unnamed(fields) => fields.iter().map(|field| field.type_.clone()).collect(),
            Fields::Unit => vec![],
        }
    }
}



// ===============
// === Reflect ===
// ===============

pub trait Reflect {
    type Static: 'static;
    type SubtypeErased: 'static;
    fn reflect() -> TypeData;
    fn reflect_lazy() -> LazyType {
        LazyType { id: TypeId::of::<Self>(), evaluate: Self::reflect }
    }
    fn reflect_type(&self) -> TypeData {
        Self::reflect()
    }
    fn subtype_erased_type() -> GenericTypeId {
        GenericTypeId(std::any::TypeId::of::<Self::SubtypeErased>())
    }
}


// === Implementations for standard types ===

impl Reflect for std::borrow::Cow<'_, str> {
    type Static = String;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        // FIXME: Should be a transparent wrapper.
        <String as Reflect>::reflect()
    }
}

impl<T> Reflect for std::rc::Rc<T>
where T: Reflect
{
    type Static = T::Static;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        // FIXME: Should be a transparent wrapper.
        T::reflect()
    }
}

impl<T> Reflect for Box<T>
where T: Reflect
{
    type Static = T::Static;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        // FIXME: Should be a transparent wrapper.
        T::reflect()
    }
}

impl<T> Reflect for Option<T>
where T: Reflect
{
    type Static = Option<T::Static>;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        let id = TypeId::of::<Self>();
        let name = "Option".to_owned();
        let data = Data::Primitive(Primitive::Option(T::reflect_lazy()));
        let transparent = false;
        let subtype_erased = Self::subtype_erased_type();
        TypeData { id, name, data, subtype_erased, transparent }
    }
}

impl<T, E> Reflect for Result<T, E>
where
    T: Reflect,
    E: Reflect,
{
    type Static = Result<T::Static, E::Static>;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        let id = TypeId::of::<Self>();
        let name = "Result".to_owned();
        let ok = T::reflect_lazy();
        let err = E::reflect_lazy();
        let data = Data::Primitive(Primitive::Result(ok, err));
        let transparent = false;
        let subtype_erased = Self::subtype_erased_type();
        TypeData { id, name, data, subtype_erased, transparent }
    }
}

impl Reflect for &'_ str {
    type Static = String;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        // FIXME: Should be a transparent wrapper.
        <String as Reflect>::reflect()
    }
}

impl<T> Reflect for Vec<T>
where T: Reflect
{
    type Static = Vec<T::Static>;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        let id = TypeId::of::<Vec<T>>();
        let name = "Vec".to_owned();
        let data = Data::Primitive(Primitive::Vec(T::reflect_lazy()));
        let transparent = false;
        let subtype_erased = Self::subtype_erased_type();
        TypeData { id, name, data, subtype_erased, transparent }
    }
}

macro_rules! reflect_primitive {
    ($ty: ty, $primitive: expr) => {
        impl Reflect for $ty {
            type Static = Self;
            type SubtypeErased = Self::Static;
            fn reflect() -> TypeData {
                let id = TypeId::of::<$ty>();
                let name = stringify!($ty).to_owned();
                let data = Data::Primitive($primitive);
                let transparent = false;
                let subtype_erased = Self::subtype_erased_type();
                TypeData { id, name, data, subtype_erased, transparent }
            }
        }
    };
}

impl ReferencedTypes for Primitive {
    fn referenced_types(&self) -> Vec<LazyType> {
        match self {
            Primitive::Bool | Primitive::Usize | Primitive::String | Primitive::U32 => vec![],
            Primitive::Vec(ty) | Primitive::Option(ty) => vec![ty.clone()],
            Primitive::Result(ty0, ty1) => vec![ty0.clone(), ty1.clone()],
        }
    }
}

impl Primitive {
    pub fn type_type(&self) -> TypeType {
        match &self {
            Primitive::Bool
            | Primitive::Usize
            | Primitive::U32
            | Primitive::String
            | Primitive::Vec(_) => TypeType::Product,
            Primitive::Option(_) | Primitive::Result(_, _) => TypeType::Sum,
        }
    }
}

reflect_primitive!(bool, Primitive::Bool);
reflect_primitive!(usize, Primitive::Usize);
reflect_primitive!(u32, Primitive::U32);
reflect_primitive!(String, Primitive::String);



// ==============
// === Graphs ===
// ==============

pub mod graphviz {
    use super::*;
    use crate::graphviz::EdgeType;
    use crate::graphviz::Graph;
    use crate::graphviz::Node;
    use crate::graphviz::NodeType;

    #[derive(Derivative)]
    #[derivative(Default(bound = ""))]
    struct Number<T> {
        map:     std::collections::HashMap<T, u32>,
        next_id: u32,
    }

    impl<T: Eq + std::hash::Hash> Number<T> {
        fn get(&mut self, index: T) -> u32 {
            *self.map.entry(index).or_insert_with(|| {
                let id = self.next_id;
                self.next_id += 1;
                id
            })
        }
    }

    pub fn graph<T: Reflect>() {
        let mut to_visit = vec![T::reflect_lazy()];
        let mut types = std::collections::HashMap::new();
        while let Some(type_) = to_visit.pop() {
            let id = type_.id;
            if types.contains_key(&id) {
                continue;
            }
            let type_ = (type_.evaluate)();
            to_visit.extend(type_.referenced_types().into_iter());
            types.insert(id, type_);
        }
        let mut graph = Graph::default();
        let mut number = Number::default();
        for type_ in types.values() {
            let sname = format!("{}{}", type_.name, number.get(type_.id));
            let primitive = type_.is_primitive();
            let node_type = match type_.type_type() {
                TypeType::Sum => NodeType::Enum,
                TypeType::Product => NodeType::Struct,
            };
            let label = type_.name.clone();
            graph.nodes.insert(sname.clone(), Node { primitive, node_type, label });
            if let Data::Enum(enum_) = &type_.data {
                for variant in &enum_.variants {
                    let svariant = format!("{}_{}", sname, variant.ident);
                    let primitive = false;
                    let node_type = NodeType::Variant;
                    let label = variant.ident.clone();
                    graph.nodes.insert(svariant.clone(), Node { primitive, node_type, label });
                    graph.edges.push((sname.clone(), svariant.clone(), EdgeType::Variant));
                    for ty in variant.fields.referenced_types() {
                        let ty = &types[&ty.id];
                        let sname2 = format!("{}{}", ty.name, number.get(ty.id));
                        graph.edges.push((svariant.clone(), sname2, EdgeType::Field));
                    }
                }
            } else {
                for ty in type_.referenced_types() {
                    let ty = &types[&ty.id];
                    let sname2 = format!("{}{}", ty.name, number.get(ty.id));
                    graph.edges.push((sname.clone(), sname2, EdgeType::Field));
                }
            }
        }
        println!("{}", graph);
    }
}
