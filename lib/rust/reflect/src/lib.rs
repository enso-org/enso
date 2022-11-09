//! Runtime support crate for [`enso_reflect_macros`].
//!
//! For data producers: See the docs of [`enso_reflect_macros`] for how to derive [`Reflect`]. It is
//! not recommended to explicitly `impl Reflect`; the derived implementation can be extensively
//! customized through attributes. The meanings of the associated types required by the trait are
//! rather obtuse, and the trait itself should not be considered a stable interface--expect that
//! new, even more obtuse associated types will be added to the trait in the future.
//!
//! For data consumers: The `Reflect` trait can be used to obtain reflection data; after that, the
//! [`enso_metamodel`] crate supports working with it--see the docs there.
//!
//! # Implementation
//!
//! The functionality exposed by the `Reflect` trait is to report information about type
//! relationships--what fields compose a `struct`, what variants compose an `enum`, etc.
//!
//! The chief design constraint of the `Reflect` trait is: It must be possible for a pure function
//! from Rust syntax to Rust syntax, operating on each data type in isolation (e.g. a proc macro)
//! to generate a `Reflect` implementation for any type.
//!
//! ## Producing a type graph from syntax
//!
//! Because Rust doesn't have value-level type information (i.e. it doesn't have a native reflection
//! mechanism), and a `Reflect` implementation must be generatable from syntax, when the `Reflect`
//! implementation needs to refer to another type, it does so by creating an invocation of the
//! `Reflect` method of the type being referred to. However, it cannot call these functions
//! directly--type graphs often contain cycles. To address this, the [`enso_metamodel::rust`]
//! representation is based on *lazy* graphs: A reference to a type contains a thunk that can be
//! evaluated to obtain type information.
//!
//! ## Associating unique identifiers with types
//!
//! This solves the problem of producing references between types, potentially cyclic, from syntax.
//! However, the consumer of the data needs more information not to be stymied by the cyclic nature
//! of type graphs; without attaching some notion of identity to the type references, it would be
//! impossible for a data consumer to tell whether they are following a cycle (repeatedly visiting
//! types they've encountered before), or encountering new, similarly-shaped types.
//!
//! Assigning IDs is not straightforward: How does a pure function from the syntax representing a
//! type name to the syntax representing an expression produce something that will evaluate to a
//! value uniquely identifying a type?
//!
//! Referring to the address of the type's `Reflect::reflect` function might seem like a solution,
//! but that isn't reliable--if two `reflect` function bodies compiled to the same code, LLVM might
//! implement them both with one function; conversely, one function in the source code can have
//! multiple addresses, for example if a generic type is instantiated with the same parameters in
//! different compilation units.
//!
//! Fortunately, there is an answer: `std::TypeId::of::<T>()` returns a value uniquely identifying
//! any type `T`. "Wait," you ask--"std::TypeId::of::<T> has a `T: 'static` bound! How could we
//! possibly use it to implement a trait for types that may be non-`'static`?" And so, we have come
//! to the motivation for the first obtuse associated type, `Reflect::Static`:
//! ```
//! pub trait Reflect {
//!     type Static: 'static;
//!     // ...
//! }
//! ```
//! While a function operating on syntax has extremely limited ability to reason about types, one
//! thing it can do is tell a `'static` type from a non-`'static` type: A type is always `'static`
//! unless it is parameterized with some type parameter other than `'static`. Thus, a proc macro is
//! able to, for any type, name a type that is the same except with only `'static` lifetime
//! parameters, and therefore a `'static` type. [`enso_reflect_macros`] uses this life-extension
//! approach to provide `Reflect::Static` types, and `Reflect` uses the `std::any::TypeId` of its
//! associated `Static` to attach an identifier to a type, or to a lazy reference to a type.
//!
//! ## Adding a little parametric polymorphism
//!
//! Due to its syntax-transformation implementation, `reflect` sees types post-monomorphization: For
//! example, in the following type definition:
//! ```text
//! #[derive(Reflect)]
//! struct Foo<T> {
//!     field: T,
//! }
//! ```
//! Syntactically, the reflect implementation for `Foo<T>` will refer to the type of `field` by the
//! name of its parameter `T`--but when its `reflect` function is run to collect the data,
//! monomorphization has already occurred; the resulting data will not be able to distinguish
//! between a field with a parameteric type `T` that has been instantiated with, e.g. `u32`, and a
//! field with a concrete type that is always `u32`.
//!
//! However, to support a Rust pattern in [`enso_parser`], it was necessary for
//! [`enso_reflect_macros`] to provide the `#[reflect(subtype)]` attribute. If you refer to the
//! documentation for that field, you may notice that its implementation requires identifying when a
//! generic type with a field whose type is a type parameter is instantiated with different types
//! for that parameter. A certain amount of type-erasure is called for.
//!
//! And so, we come to the second obtuse assocatied type of `Reflect`:
//! ```
//! pub trait Reflect {
//!     // ...
//!     type SubtypeErased: 'static;
//!     // ...
//! }
//! ```
//! The `SubtypeErased` type is used to obtain a `TypeId` that does not depend on the parameter of
//! the type of the field annotated with `#[reflect(subtype)]`, if any. This is accomplished by a
//! similar approach to the way lifetimes are erased: Starting with the lifetime-erased type used to
//! for `Reflect::Static`, identify the relevant parameter instantiation within the type, and
//! replace it with a constant type, to obtain a type that is invariant in one parameter, and
//! covariant in all the others. The implementation uses `()` for the constant type. (Thus, it is
//! not currently supported for types to apply the `subtype` transform to a field with a parameter
//! that has arbitrary bounds. While this could be achieved to some extent by using a `Box<dyn ..>`
//! for the invariant parameter, that has other complications--we'd need to identify associated
//! types of the trait in question used in other fields--and [`enso_parser`] hasn't had a need for
//! it, and probably never will.)

// === Features ===
#![feature(map_first_last)]
#![feature(associated_type_defaults)]
#![feature(option_get_or_insert_default)]
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

use enso_metamodel::rust::*;


// ==============
// === Export ===
// ==============

pub use enso_metamodel as metamodel;



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
reflect_primitive!(i32, Primitive::I32);
reflect_primitive!(char, Primitive::Char);
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
    reflect_lazy::<T>().into()
}
