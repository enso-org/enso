use crate::prelude::*;



// ========================
// === Main entry point ===
// ========================

// FIXME!!!!!!!!!!!!!!!!!!!!
// FIXME: fix the unwraps and parents

/// Functions exposed in WASM have to have unique names. This utility creates a name based on the
/// location (module path, line number, column number) the function was defined.
pub fn definition_path() -> String {
    let mut span = proc_macro::Span::call_site();
    while let Some(parent) = span.parent() {
        span = parent;
    }
    let source = span.source_file();
    let start = span.start();
    let path = source.path().to_str().unwrap_or_default().to_string();
    format!("{path}:{}:{}", start.line, start.column)
}

pub fn run(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let path = definition_path();
    let output = quote! {
        #path
    };
    output.into()
}
