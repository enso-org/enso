//! This module contains the [`derive`] function (implementing the [`crate::ForEachVariant`] derive
//! macro) as well as its helper functions.

use inflector::cases::snakecase::to_snake_case;
use proc_macro2::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::Token;



// ======================
// === ForEachVariant ===
// ======================

/// Implementation of the `ForEachVariant` derive macro. For details, see the documentation of the
/// [`crate::derive_for_each_variant`] function.
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl = syn::parse_macro_input!(input as syn::DeriveInput);
    let ret = match decl.data {
        syn::Data::Enum(ref e) => derive_for_enum(&decl, e),
        _ => panic!("The `ForEachVariant` derive macro only works on enums."),
    };
    proc_macro::TokenStream::from(ret)
}

fn derive_for_enum(decl: &syn::DeriveInput, data: &syn::DataEnum) -> TokenStream {
    let enum_name = &decl.ident;
    let enum_snake_name = to_snake_case(&enum_name.to_string());
    let macro_name = quote::format_ident!("for_each_{}_variant", enum_snake_name);
    let variant_names: Punctuated<_, Token![,]> = data.variants.iter().map(|v| &v.ident).collect();
    quote! {
        /// Calls `f!` passing to it a comma-separated list of names of variants of [`#enum_name`]
        /// enclosed in square brackets. The extra `args` are passed to `f!` verbatim after the
        /// closing square bracket. For more details, see the documentation of the
        /// [`ForEachVariant`] derive macro.
        #[macro_export]
        macro_rules! #macro_name {
            ( $f:ident($( $args:tt )*) ) => { $f!([ #variant_names ] $($args)*) }
        }

        pub(crate) use #macro_name;
    }
}
