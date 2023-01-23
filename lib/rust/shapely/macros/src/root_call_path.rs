//! Macro reporting the root call path of itself. If it was used inside another macro "A", the
//! reported path will be the place where "A" was called.

use crate::prelude::*;



// ========================
// === Main entry point ===
// ========================

/// Get the root call path of the call side at compile time.
pub fn root_call_path() -> String {
    let mut span = proc_macro::Span::call_site();
    while let Some(parent) = span.parent() {
        span = parent;
    }
    let source = span.source_file();
    let start = span.start();
    let path = source.path().to_str().unwrap_or_default().to_string();
    format!("{path}:{}:{}", start.line, start.column)
}

/// Macro reporting the root call path of itself. If it was used inside another macro "A", the
/// reported path will be the place where "A" was called.
pub fn run(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let path = root_call_path();
    let output = quote! {
        #path
    };
    output.into()
}
