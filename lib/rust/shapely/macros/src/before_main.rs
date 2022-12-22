//! A macro allowing running functions after WASM initialization, before the main function.

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
    let span = proc_macro::Span::call_site();
    let source = span.source_file();
    let start = span.start();
    let path = source.path().to_str().unwrap_or_default().to_string();
    mangle_name(&format!("{path}_line_{}_column_{}", start.line, start.column))
}

pub fn run(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut input_fn = syn::parse_macro_input!(input as syn::ImplItemMethod);
    let name = format!("before_main_{}", unique_name());
    input_fn.sig.ident = quote::format_ident!("{name}");
    let output = quote! {
        #[wasm_bindgen]
        #input_fn
    };
    output.into()
}
