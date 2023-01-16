//! Information specific to each profiling level.

use inflector::Inflector;
use quote::ToTokens;
use std::env;
use syn::parse::Parser;
use syn::punctuated;



// =============
// === Level ===
// =============

/// Information about a profiling level.
pub struct Level {
    pub obj_ident: syn::Ident,
    pub fn_ident:  syn::Ident,
    pub name:      String,
    pub enabled:   bool,
}

/// Given syntax representing a sequence of profiling levels, produce [`Level`] objects describing
/// the levels.
pub fn parse_levels(var: &str, ts: proc_macro::TokenStream) -> Vec<Level> {
    let parser = punctuated::Punctuated::<syn::Ident, syn::Token![,]>::parse_terminated;
    let obj_idents: Vec<_> = parser.parse(ts).unwrap().into_iter().collect();
    let level_names: Vec<_> = obj_idents.iter().map(|id| id.to_string().to_snake_case()).collect();
    let max_enabled = level_from_env_var(var, &level_names);
    obj_idents
        .into_iter()
        .enumerate()
        .zip(level_names)
        .map(|((i, obj_ident), name)| Level {
            obj_ident,
            fn_ident: syn::Ident::new(&name, proc_macro2::Span::call_site()),
            name: name.clone(),
            enabled: i <= max_enabled,
        })
        .collect()
}

/// Return the numeric Profiling/Log Level (counting from 0 = top-level only).
fn level_from_env_var(var: &str, levels: &[impl AsRef<str>]) -> usize {
    let enabled = match env::var(var) {
        Ok(level) => level,
        // If the variable isn't set, we default to the minimum.
        Err(_) => return 0,
    };
    for (i, name) in levels.iter().enumerate() {
        if &enabled[..] == name.as_ref() {
            return i;
        }
    }
    panic!("{var} set to unknown level: {enabled}")
}



// =========================
// === `enum` Generation ===
// =========================

/// Given a collection of variant identifiers, produce syntax defining a data-less enum.
pub fn make_enum<'a>(
    ident: syn::Ident,
    variants: impl IntoIterator<Item = &'a syn::Ident>,
) -> proc_macro::TokenStream {
    let ident_to_variant = |ident| syn::Variant {
        ident,
        fields: syn::Fields::Unit,
        attrs: Default::default(),
        discriminant: Default::default(),
    };
    let variants: punctuated::Punctuated<syn::Variant, syn::Token![,]> =
        variants.into_iter().cloned().map(ident_to_variant).collect();
    (quote::quote! {
        #[allow(missing_docs)]
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, serde::Serialize)]
        pub enum #ident {
            #[default]
            #variants
        }
    })
    .to_token_stream()
    .into()
}
