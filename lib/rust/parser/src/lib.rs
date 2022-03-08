//! This library contains the implementation of the Enso parser.

// === Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub mod macros;
pub mod operator;
pub mod parser;

pub use crate::parser::*;



/// The prelude for the parser.
pub mod prelude {
    pub use enso_logger::AnyLogger;
    pub use enso_prelude::*;

    /// The Enso logging library.
    pub mod logger {
        pub use enso_logger::Logger;
        pub use enso_logger::*;
    }

    /// The lexer types.
    pub mod lexer {
        pub use ::lexer::*;

        /// The lexer tokens.
        pub mod token {
            pub use lexer::library::token::*;
        }
    }
}
