//! This module exports the implementation of the enso abstract syntax tree.

// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

// === Features ===
#![feature(test)]



mod ast;
pub mod generation;

pub use crate::ast::*;
