//! Math utilities with focus on computer graphics.

// === Features ===
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub mod algebra;
pub mod num;
pub mod topology;
pub mod unit;

pub use algebra::*;
pub use topology::*;



/// Common traits.
pub mod traits {
    pub use super::topology::traits::*;
}
