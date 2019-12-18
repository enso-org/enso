//! This crate defines a custom derive macro `Iterator`. Should not be used
//! directly, but only through `shapely` crate, as it provides utilities
//! necessary for the generated code to compile.

extern crate proc_macro;

mod derive_iterator;

use prelude::*;

use proc_macro2::TokenStream;

/// For `struct Foo<T>` or `enum Foo<T>` provides:
/// * `IntoIterator` implementations for `&'t Foo<T>` and `&mut 't Foo<T>`;
/// * `iter` and `into_iter` methods.
///
/// The iterators will:
/// * for structs: go over each field that declared type is same as the
///   struct's last type parameter.
/// * enums: delegate to current constructor's nested value if it is takes `T`
///   type argument; or return empty iterator otherwise.
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
pub fn derive_iterator
(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl   = syn::parse_macro_input!(input as syn::DeriveInput);
    let params = &decl.generics.params.iter().collect::<Vec<_>>();
    let output = match params.last() {
        Some(last_param) => derive_iterator::derive(&decl, &last_param),
        None             => TokenStream::new(),
    };
    proc_macro::TokenStream::from(output)
}
