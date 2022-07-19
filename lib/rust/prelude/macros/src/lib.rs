
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

use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::Token;



#[proc_macro_derive(ForEachVariant)]
pub fn derive_for_each_variant(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl = syn::parse_macro_input!(input as syn::DeriveInput);
    let ret = match decl.data {
        syn::Data::Enum(ref e) => derive_for_enum(&decl, e),
        _ => quote! {},
    };
    proc_macro::TokenStream::from(ret)
}

fn derive_for_enum(decl: &syn::DeriveInput, data: &syn::DataEnum) -> TokenStream {
    let ret = quote! {
        #[macro_export]
        macro_rules! for_each_kind_variant {
            ($f:ident($($args:tt)*)) => { $f!([Atom, Function, Local, Method, Module] $($args)*) }
        }

        pub(crate) use for_each_kind_variant;
    };
    ret
}

