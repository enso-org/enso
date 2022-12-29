//! A macro allowing running functions after WASM initialization, before the main function.

use crate::definition_path::definition_path;
use crate::prelude::*;


// ========================
// === Main entry point ===
// ========================

fn mangle_name(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c.to_string() } else { format!("_{}_", c as u32) })
        .collect()
}

// FIXME!!!!!!!!!!!!!!!!!!!!
// FIXME: wrongly computed!

/// Functions exposed in WASM have to have unique names. This utility creates a name based on the
/// location (module path, line number, column number) the function was defined.
fn unique_name() -> String {
    mangle_name(&definition_path())
}

pub fn run(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut input_fn = syn::parse_macro_input!(input as syn::ImplItemMethod);
    let name = format!("before_main_entry_point_{}", unique_name());
    input_fn.sig.ident = quote::format_ident!("{name}");
    let output = quote! {
        #[wasm_bindgen]
        #input_fn
    };
    output.into()
}
