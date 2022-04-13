//! This crate defines a custom derive macro `Iterator`. Should not be used
//! directly, but only through `enso-shapely` crate, as it provides utilities
//! necessary for the generated code to compile.

// === Features ===
#![feature(bool_to_option)]
#![feature(exact_size_is_empty)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]



extern crate proc_macro;

mod derive_clone_ref;
mod derive_entry_point;
mod derive_iterator;
mod derive_no_clone;
mod overlappable;

mod prelude {
    pub use enso_macro_utils::repr;
    pub use proc_macro2::Span;
    pub use proc_macro2::TokenStream;
    pub use quote::quote;
}

use crate::derive_iterator::IsMut;

/// For `struct Foo<T>` or `enum Foo<T>` provides:
/// * `IntoIterator` implementations for `&'t Foo<T>`, `iter` and `into_iter`
/// methods.
///
/// The iterators will:
/// * for structs: go over each field that declared type is same as the struct's last type
///   parameter.
/// * enums: delegate to current constructor's nested value's iterator.
///
/// Enums are required to use only a single element tuple-like variant. This
/// limitation should be lifted in the future.
///
/// Any dependent type stored in struct, tuple or wrapped in enum should have
/// dependency only in its last type parameter. All dependent types that are not
/// tuples nor directly the yielded type, are required to provide `iter` method
/// that returns a compatible iterator (possible also derived).
///
/// Caller must have the following features enabled:
/// ```
/// #![feature(generators)]
/// #![feature(type_alias_impl_trait)]
/// ```
///
/// When used on type that takes no type parameters, like `struct Foo`, does
/// nothing but yields no errors.
#[proc_macro_derive(Iterator)]
pub fn derive_iterator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_iterator::derive(input, IsMut::Immutable)
}

/// Same as `derive(Iterator)` but generates mutable iterator.
///
/// It is separate, as some types allow deriving immutable iterator but ont the
/// mutable one.
#[proc_macro_derive(IteratorMut)]
pub fn derive_iterator_mut(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_iterator::derive(input, IsMut::Mutable)
}

/// Derives `CloneRef` implementation for given type. It performs `clone_ref` on every member
/// field. The input type must implement `Clone` and its every field must implement `CloneRef`.
///
/// For generic types no bounds are introduced in the generated implementation. To customize this
/// behavior user might add `#[clone_ref(bound="…")]` attribute. Then the generated implementation
/// will use the provided bounds.
///
/// Moreover, for a given struct `X` this macro generates also `impl From<&X> for X` which uses
/// `CloneRef` under the hood. The semantics of `CloneRef` makes each object to naturally provide
/// transformation from reference to an owned type.
///
/// Supported inputs are structs (unit, named, unnamed), enums (with unit, named, unnamed and no
/// variants at all). Unions are currently not supported.
#[proc_macro_derive(CloneRef, attributes(clone_ref))]
pub fn derive_clone_ref(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_clone_ref::derive(input)
}

/// Makes sure that the structure does not derive [`Clone`] and that it implements custom [`Drop`]
/// implementation.
///
/// For the given input
/// ```ignore
/// #[derive(NoCloneBecauseOfCustomDrop)]
/// struct Test {}
/// ```
///
/// The following output will be generated:
/// ```ignore
/// struct Test {}
/// impl !Clone for Test {}
//  impl ImplementsDrop for Test {}
/// ```
#[proc_macro_derive(NoCloneBecauseOfCustomDrop)]
pub fn derive_no_clone(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_no_clone::derive(input)
}

/// Exposes the function as an application entry point. Entry points are alternative application
/// running modes that you can access by adding `?entry=` to the end of the application URL.
#[proc_macro_attribute]
pub fn entry_point(
    _: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    derive_entry_point::derive(item)
}

#[allow(missing_docs)]
#[proc_macro_attribute]
pub fn overlappable(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    overlappable::overlappable(attrs, input)
}
