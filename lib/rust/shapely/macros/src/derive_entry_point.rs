use crate::prelude::*;



// ===================
// === Entry Point ===
// ===================

fn crate_name_to_base_name(name: &str) -> String {
    let name = name.replace("debug-scene-", "");
    let name = name.replace("ensogl-example-", "");
    let name = name.replace("enso-example-", "");
    let name = name.replace("enso-", "");
    let name = name.replace("example-", "");
    name.replace('-', "_")
}

fn base_name_to_fn_name(name: &str) -> String {
    format!("entry_point_{}", name)
}

pub fn derive(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut args_iter = args.into_iter();
    let fn_name_str = match args_iter.next() {
        None => {
            let crate_name = std::env::var("CARGO_PKG_NAME").unwrap();
            crate_name_to_base_name(&crate_name)
        }
        Some(token) => {
            if args_iter.next().is_some() {
                panic!("Expected maximum one argument, the entry point name. If missing, the crate name will be used.");
            }
            token.to_string()
        }
    };
    let fn_name_str = base_name_to_fn_name(&fn_name_str);
    let fn_name = quote::format_ident!("{}", fn_name_str);
    let decl = syn::parse_macro_input!(input as syn::Item);
    match decl {
        syn::Item::Fn(f) => {
            let name = f.sig.ident.to_string();
            if &name != "main" {
                panic!("The function should be named 'main'.");
            }
            let mut fn_sig = f.sig.clone();
            fn_sig.ident = fn_name;
            let attrs = &f.attrs;
            let block = &f.block;
            let output = quote! {
                #(#attrs)*
                #[wasm_bindgen]
                pub #fn_sig {
                    init_global();
                    #block
                }
            };
            output.into()
        }
        _ => panic!("This macro is intended to be used on functions only."),
    }
}
