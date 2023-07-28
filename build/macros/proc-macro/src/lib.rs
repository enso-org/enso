// === Features ===
#![feature(result_option_inspect)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use syn_1 as syn;



#[proc_macro_derive(Arg, attributes(arg))]
pub fn derive_arg_fn(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    enso_build_macros_lib::program_args::derive(input)
        .unwrap_or_else(|err| panic!("Failed to derive program argument: {err:?}"))
        .into()
}

/// This macro takes a string literal with YAML description of file tree and generates wrapper
/// classes. See the tests for this crate for usage examples.
#[proc_macro]
pub fn make_paths(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::LitStr);
    let input = input.value();
    enso_build_macros_lib::paths::process(input.as_bytes())
        // .inspect(|tt| println!("Generated: {:}", tt))
        .unwrap_or_else(|err| panic!("Failed to generate path types: {err:?}"))
        .into()
}
