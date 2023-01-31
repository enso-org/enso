//! A macro allowing running functions after WASM initialization, before the main function.

use crate::prelude::*;

use crate::root_call_path::root_call_path;



// ========================
// === Main entry point ===
// ========================

/// Mangle the name replacing `_` with `__` and all non-ASCII characters with `_<charcode>_`. The
/// JS code contains a counterpart function that allows un-mangling the files, thus, after changing
/// this code, the JS one has to be updated as well.
fn mangle_name(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c.to_string() } else { format!("_{}_", c as u32) })
        .collect()
}

/// Functions exposed in WASM have to have unique names. This utility creates a name based on the
/// location the function was defined at (module path, line number, column number).
fn unique_name() -> String {
    mangle_name(&root_call_path())
}

/// The prefix of the before-main entry point function in WASM. The JS code contains a code
/// referring to that name as well, so if you change it, you have to update the JS code as well.
const BEFORE_MAIN_ENTRY_POINT_PREFIX: &str = "before_main_entry_point";

/// The default priority of the before-main entry point. This number will be used as part of the
/// function name. Before-main entry points are sorted by name before being run.
const DEFAULT_PRIORITY: usize = 100;

/// Convert the function to a before-main entry point. Please note that the [`wasm-bindgen`] macro
/// has to be in scope for the result to compile.
pub fn run(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut args_iter = args.into_iter();
    let priority = match args_iter.next() {
        None => DEFAULT_PRIORITY,
        Some(token) => {
            if args_iter.next().is_some() {
                panic!(
                    "Expected maximum one argument, the entry point priority. If missing, the \
                default priority will be used ({DEFAULT_PRIORITY})."
                );
            }
            match token.to_string().parse::<usize>() {
                Ok(priority) => priority,
                Err(_) => panic!("The priority must be a number."),
            }
        }
    };
    let mut input_fn = syn::parse_macro_input!(input as syn::ImplItemMethod);
    let name = format!("{BEFORE_MAIN_ENTRY_POINT_PREFIX}_{priority}_{}", unique_name());
    input_fn.sig.ident = quote::format_ident!("{name}");
    let output = quote! {
        #[wasm_bindgen::prelude::wasm_bindgen]
        #input_fn
    };
    output.into()
}
