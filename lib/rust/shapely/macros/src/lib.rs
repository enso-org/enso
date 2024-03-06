//! This crate defines a custom derive macro `Iterator`. Should not be used
//! directly, but only through `enso-shapely` crate, as it provides utilities
//! necessary for the generated code to compile.

// === Features ===
#![feature(exact_size_is_empty)]
#![feature(proc_macro_span)]
#![feature(proc_macro_def_site)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]


extern crate proc_macro;

mod before_main;
mod derive_clone_ref;
mod derive_for_each_variant;
mod derive_iterator;
mod derive_no_clone;
mod overlappable;
mod root_call_path;
mod tagged_enum;

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
/// ```text
/// #[derive(NoCloneBecauseOfCustomDrop)]
/// struct Test {}
/// ```
///
/// The following output will be generated:
/// ```text
/// struct Test {}
/// impl !Clone for Test {}
//  impl ImplementsDrop for Test {}
/// ```
#[proc_macro_derive(NoCloneBecauseOfCustomDrop)]
pub fn derive_no_clone(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_no_clone::derive(input)
}

/// Implements the `ForEachVariant` derive macro which creates a helper for iterating over each
/// variant of an enum at compile time. The derive panics if used on non-enum types.
///
/// The derive creates a macro (hereafter called loop-macro) named `for_each_NAME_variant` where
/// `NAME` is replaced with the name of the enum converted to snake case. The loop-macro takes a
/// name of another macro (hereafter called iterator-macro) as an argument followed by a
/// parenthesized list of extra arguments. The loop-macro expands to a call of the iterator-macro
/// with a list of comma-separated names of the enum variants wrapped in square brackets, followed
/// by the extra arguments defined above.
///
/// For example, the following code:
/// ```no_compile
/// #[derive(ForEachVariant)]
/// pub enum FooBar {
///     Foo,
///     Bar,
/// }
/// ```
/// results in the following macro being defined:
/// ```
/// #[macro_export]
/// macro_rules! for_each_foo_bar_variant {
///     ( $f:ident($( $args:tt )*) ) => { $f!([Foo, Bar] $($args)*) }
/// }
///
/// pub(crate) use for_each_foo_bar_variant;
/// ```
#[proc_macro_derive(ForEachVariant)]
pub fn derive_for_each_variant(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_for_each_variant::derive(input)
}

#[allow(missing_docs)]
#[proc_macro_attribute]
pub fn overlappable(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    overlappable::overlappable(attrs, input)
}

/// Transforms Rust enums into enums where each variant is a separate type. It also implements
/// several traits (such as conversions between variants and the enum type) and defines utility
/// functions, such as constructors. See [`tagged_enum::run`] to learn more.
#[proc_macro_attribute]
pub fn tagged_enum(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    tagged_enum::run(attr, input)
}

/// A macro allowing running functions after WASM initialization, before the main function. In order
/// to run a function before main, simply use this attribute (please note that the function has to
/// be public, as otherwise it can't be exported to WASM):
///
/// ```text
/// #[before_main]
/// pub fn any_name {
///     println!("I'm running before main!");
/// }
/// ```
#[proc_macro_attribute]
pub fn before_main(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    before_main::run(attr, input)
}

/// Macro reporting the root call path of itself. If it was used inside another macro "A", the
/// reported path will be the place where "A" was called.
#[proc_macro]
pub fn root_call_path(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    root_call_path::run(input)
}
