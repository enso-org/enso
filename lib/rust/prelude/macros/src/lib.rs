//! This module defines a set of procedural macros which are useful across different projects.
//!
//! Procedural macros must be defined in a separate crate, which can only export procedural macros.
//! If not for that, macros defined in this crate would be defined in the [`enso-prelude`] crate.

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

use inflector::cases::snakecase::to_snake_case;
use proc_macro2::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::Token;



// ======================
// === ForEachVariant ===
// ======================

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
/// enum FooBar {
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
    let decl = syn::parse_macro_input!(input as syn::DeriveInput);
    let ret = match decl.data {
        syn::Data::Enum(ref e) => derive_for_each_variant_for_enum(&decl, e),
        _ => panic!("The `ForEachVariant` derive macro only works on enums."),
    };
    proc_macro::TokenStream::from(ret)
}

fn derive_for_each_variant_for_enum(decl: &syn::DeriveInput, data: &syn::DataEnum) -> TokenStream {
    let enum_name = &decl.ident;
    let enum_snake_name = to_snake_case(&enum_name.to_string());
    let macro_name = quote::format_ident!("for_each_{}_variant", enum_snake_name);
    let variant_names: Punctuated<_, Token![,]> = data.variants.iter().map(|v| &v.ident).collect();
    quote! {
        #[macro_export]
        macro_rules! #macro_name {
            ( $f:ident($( $args:tt )*) ) => { $f!([ #variant_names ] $($args)*) }
        }

        pub(crate) use #macro_name;
    }
}

