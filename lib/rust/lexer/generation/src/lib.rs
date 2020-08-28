#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports the interface to the generated Enso lexer.

pub mod generated;

/// Support libraries for the lexer definition.
///
/// This is an intentional re-export in this crate's namespace.
mod library {
    pub use lexer_definition::library::*;
}

/// A library of commonly useful functionality.
mod prelude {
    pub use lexer_definition::prelude::*;
}
