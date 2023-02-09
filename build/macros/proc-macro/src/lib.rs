#![feature(result_option_inspect)]

#[proc_macro_derive(Arg, attributes(arg))]
pub fn derive_arg_fn(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    enso_build_macros_lib::program_args::derive(input)
        .unwrap_or_else(|err| panic!("Failed to derive program argument: {err:?}"))
        .into()
}

#[proc_macro]
pub fn make_paths(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::LitStr);
    let input = input.value();
    enso_build_macros_lib::paths::process(input.as_bytes())
        // .inspect(|tt| println!("Generated: {:}", tt))
        .unwrap_or_else(|err| panic!("Failed to generate path types: {err:?}"))
        .into()
}
