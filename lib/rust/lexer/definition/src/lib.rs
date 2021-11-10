#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This library defines the lexer for the syntax of the Enso language.

pub mod escape;
pub mod lexeme;
pub mod lexer;
pub mod rule;
pub mod token;

/// A module that can be re-exported under the same name in the generation crate.
///
/// This is necessary to avoid issues with paths getting wonky when the code is generated from the
/// Enso lexer definition. In this project, imports should _not_ be made from the crate root
/// _except_ through use of this `library` module.
pub mod library {
    pub use crate::escape;
    pub use crate::lexeme;
    pub use crate::rules;
    pub use crate::token;
}

/// A collection of functionality for working with the lexer definition.
pub mod prelude {
    pub use enso_flexer::prelude::logger::*;
    pub use enso_flexer::prelude::*;
}
