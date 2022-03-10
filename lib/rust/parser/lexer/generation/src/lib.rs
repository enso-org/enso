//! This module exports the interface to the generated Enso lexer.

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


// ==============
// === Export ===
// ==============

#[rustfmt::skip]
pub mod generated;
pub mod lexer;

pub use crate::lexer::*;



/// Support libraries for the lexer definition.
///
/// This is an intentional re-export in this crate's namespace.
pub mod library {
    pub use lexer_definition::library::*;
}


/// A library of commonly useful functionality.
mod prelude {
    pub use lexer_definition::prelude::*;
}
