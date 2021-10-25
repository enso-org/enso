#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports the implementation of parser for the Enso language.

mod jni;

pub use crate::jni::*;

use ast::AnyAst;
use ast::Ast;



// =======================
// === Parser Rust API ===
// =======================

/// Parse a content of a single source file.
pub fn parse_str(input:String) -> AnyAst {
    Ast::new(ast::txt::Text{text:input})
}

/// Parse a single source file.
pub fn parse_file(filename:String) -> AnyAst {
    parse_str(filename)
}


// === Tokens ===

/// Parse a content of single source file.
pub fn lexe_str(input:String) -> AnyAst {
    parse_str(input)
}

/// Parse a single source file.
pub fn lexe_file(filename:String) -> AnyAst {
    parse_str(filename)
}
