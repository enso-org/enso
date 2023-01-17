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

const DEFAULT_PRIORITY: usize = 100;

pub fn run(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut args_iter = args.into_iter();
    let priority = match args_iter.next() {
        None => DEFAULT_PRIORITY,
        Some(token) => {
            if args_iter.next().is_some() {
                panic!("Expected maximum one argument, the entry point priority. If missing, the default priority is used ({DEFAULT_PRIORITY}).");
            }
            match token.to_string().parse::<usize>() {
                Ok(priority) => priority,
                Err(_) => panic!("The priority must be a number."),
            }
        }
    };
    let mut input_fn = syn::parse_macro_input!(input as syn::ImplItemMethod);
    let name = format!("before_main_entry_point_{priority}_{}", unique_name());
    input_fn.sig.ident = quote::format_ident!("{name}");
    let output = quote! {
        #[wasm_bindgen::prelude::wasm_bindgen]
        #input_fn
    };
    output.into()
}
