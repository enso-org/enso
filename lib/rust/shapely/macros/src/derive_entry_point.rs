use crate::prelude::*;



// ===================
// === Entry Point ===
// ===================

fn crate_name_to_fn_name(name: &str) -> String {
    let name = name.replace("ensogl-example-", "");
    let name = name.replace("enso-example-", "");
    let name = name.replace("enso-", "");
    let name = name.replace("example-", "");
    let name = name.replace('-', "_");
    format!("entry_point_{}", name)
}

pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let crate_name = std::env::var("CARGO_PKG_NAME").unwrap();
    let decl = syn::parse_macro_input!(input as syn::Item);
    match decl {
        syn::Item::Fn(f) => {
            let name = f.sig.ident.to_string();
            if &name != "main" {
                panic!("The function should be named 'main'.");
            }
            let fn_name = quote::format_ident!("{}", crate_name_to_fn_name(&crate_name));
            let mut fn_sig = f.sig.clone();
            fn_sig.ident = fn_name;
            let attrs = &f.attrs;
            let block = &f.block;
            let output = quote! {
                #(#attrs)*
                #[wasm_bindgen]
                pub #fn_sig #block
            };
            output.into()
        }
        _ => panic!("This macro is intended to be used on functions only."),
    }
}
