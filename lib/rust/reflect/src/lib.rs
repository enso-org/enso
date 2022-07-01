//! Rust reflection for datatype definitions.

// === Features ===
#![feature(map_first_last)]
#![feature(associated_type_defaults)]
#![feature(option_get_or_insert_default)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
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

pub use enso_metamodel as metamodel;
use enso_metamodel::rust::*;

/// Imports for crates that `#[derive(Reflect)]`.
pub mod prelude {
    pub use enso_reflect_macros::Reflect;
}



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
    /// Get information about type, identified by a reference.
    fn reflect_type(&self) -> TypeData {
        Self::reflect()
    }
}


// === Implementations for standard types ===

impl Reflect for std::borrow::Cow<'_, str> {
    type Static = String;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        <String as Reflect>::reflect()
    }
}

impl<T> Reflect for std::rc::Rc<T>
where T: Reflect
{
    type Static = T::Static;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        T::reflect()
    }
}

impl<T> Reflect for Box<T>
where T: Reflect
{
    type Static = T::Static;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        T::reflect()
    }
}

impl<T> Reflect for Option<T>
where T: Reflect
{
    type Static = Option<T::Static>;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        let id = type_id::<Self>();
        let name = "Option".to_owned();
        let data = Data::Primitive(Primitive::Option(reflect_lazy::<T>()));
        let subtype_erased = generic_id::<Self>();
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
        let id = type_id::<Self>();
        let name = "Result".to_owned();
        let ok = reflect_lazy::<T>();
        let err = reflect_lazy::<E>();
        let data = Data::Primitive(Primitive::Result(ok, err));
        let subtype_erased = generic_id::<Self>();
        TypeData { id, name, data, subtype_erased }
    }
}

impl Reflect for &'_ str {
    type Static = String;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        <String as Reflect>::reflect()
    }
}

impl<T> Reflect for Vec<T>
where T: Reflect
{
    type Static = Vec<T::Static>;
    type SubtypeErased = Self::Static;
    fn reflect() -> TypeData {
        let id = type_id::<Vec<T>>();
        let name = "Vec".to_owned();
        let data = Data::Primitive(Primitive::Vec(reflect_lazy::<T>()));
        let subtype_erased = generic_id::<Self>();
        TypeData { id, name, data, subtype_erased }
    }
}

macro_rules! reflect_primitive {
    ($ty: ty, $primitive: expr) => {
        impl Reflect for $ty {
            type Static = Self;
            type SubtypeErased = Self::Static;
            fn reflect() -> TypeData {
                let id = type_id::<$ty>();
                let name = stringify!($ty).to_owned();
                let data = Data::Primitive($primitive);
                let subtype_erased = generic_id::<Self>();
                TypeData { id, name, data, subtype_erased }
            }
        }
    };
}

reflect_primitive!(bool, Primitive::Bool);
reflect_primitive!(usize, Primitive::Usize);
reflect_primitive!(u32, Primitive::U32);
reflect_primitive!(String, Primitive::String);



// ==================
// === Reflectors ===
// ==================

/// Return a value that can be used to obtain type information.
pub fn reflect_lazy<T: ?Sized + Reflect>() -> LazyType {
    let id = type_id::<T>();
    let evaluate = <T as Reflect>::reflect;
    LazyType::new(id, evaluate)
}

/// Get an identifier that uniquely identifies the type, up to the instantiation of the parameter
/// of any field marked with the attribute `#[reflect(subtype)]`
pub fn generic_id<T: ?Sized + Reflect>() -> GenericTypeId {
    GenericTypeId::new(std::any::TypeId::of::<T::SubtypeErased>())
}

/// Obtain a unique identifier for a type.
pub fn type_id<T: ?Sized + Reflect>() -> TypeId {
    TypeId::new(std::any::TypeId::of::<T::Static>())
}



// ================
// === GraphViz ===
// ================

/// Generate a graph of the given type's relationships with other types.
#[cfg(feature = "graphviz")]
pub fn graph<T: Reflect>() -> metamodel::graphviz::Graph {
    graphviz::graph(reflect_lazy::<T>())
}
