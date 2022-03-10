//! This module exports the implementation of parser for the Enso language.

// === Features ===
#![feature(test)]

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]



mod jni;

pub use crate::jni::*;

use ast_new::AnyAst;
use ast_new::Ast;



// =======================
// === Parser Rust API ===
// =======================

/// Parse a content of a single source file.
pub fn parse_str(input: String) -> AnyAst {
    Ast::new(ast_new::txt::Text { text: input })
}

/// Parse a single source file.
pub fn parse_file(filename: String) -> AnyAst {
    parse_str(filename)
}


// === Tokens ===

/// Parse a content of single source file.
pub fn lexe_str(input: String) -> AnyAst {
    parse_str(input)
}

/// Parse a single source file.
pub fn lexe_file(filename: String) -> AnyAst {
    parse_str(filename)
}
