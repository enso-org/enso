#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports an AST API macro.

use ast_generation::ast;
use ast_generation::api;

extern crate proc_macro;
use quote::quote;
use syn;



/// Generates constructors for the Rust and Scala AST with same API.
#[proc_macro]
pub fn generate_ast_api(input:proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::File);
    let file  = ast::File::new("Ast", "org.enso.ast", input.clone());
    let api   = api::Source::new(file.into()).ast_api();

    quote!(#input #api).into()
}
