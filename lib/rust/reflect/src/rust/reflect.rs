use crate::rust::*;



// ===============
// === Reflect ===
// ===============

/// Supports obtaining descriptions of the definitions of types at runtime.
pub trait Reflect {
    /// This must be a type that uniquely identifies `Self`, ignoring any lifetime parameters.
    type Static: 'static;
    /// This must be a type that uniquely identifies `Self`, ignoring any lifetime parameters, and
    /// invariant to any one generic parameter that may occur in the definition of a field marked
    /// `#[reflect(subtype)]`, if present. The type used for the erased parameter can be any type
    /// that satisfies `Self`'s bounds.
    type SubtypeErased: 'static;
    /// Get information about the type's definition.
    fn reflect() -> TypeData;
    /// Get an object that can be used to obtain information about the type's definition.
    fn reflect_lazy() -> LazyType {
        LazyType { id: TypeId::of::<Self>(), evaluate: Self::reflect }
    }
    /// Get information about type, identified by a reference.
    fn reflect_type(&self) -> TypeData {
        Self::reflect()
    }
    /// Get an identifier for the type. If the type does not contain a field marked
    /// `#[reflect(subtype)]`, this will be unique to the type, just like its `TypeId`; however, if
    /// this type does contain such a field, and this type is generic, and exactly one generic
    /// parameter occurs in the type of the given field, this ID will compare equal to another ID
    /// if and only if that ID corresponds to a type that is an instantiation of the same generic
    /// with all the same parameters *ignoring* the parameter that occurs in the `subtype` field.
    ///
    /// This is used to implement the `subtype` transform.
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
        let subtype_erased = Self::subtype_erased_type();
        TypeData { id, name, data, subtype_erased }
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
        let subtype_erased = Self::subtype_erased_type();
        TypeData { id, name, data, subtype_erased }
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
        let subtype_erased = Self::subtype_erased_type();
        TypeData { id, name, data, subtype_erased }
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
                let subtype_erased = Self::subtype_erased_type();
                TypeData { id, name, data, subtype_erased }
            }
        }
    };
}

impl ReferencedTypes for Primitive {
    fn referenced_types(&self) -> Vec<LazyType> {
        match self {
            Primitive::Bool | Primitive::Usize | Primitive::String | Primitive::U32 => vec![],
            Primitive::Vec(ty) | Primitive::Option(ty) => vec![*ty],
            Primitive::Result(ty0, ty1) => vec![*ty0, *ty1],
        }
    }
}

impl Primitive {
    /// Get information about the composition operator relating the types this type is composed of.
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
