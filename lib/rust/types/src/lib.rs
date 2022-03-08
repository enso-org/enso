//! Math utilities with focus on computer graphics.

// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
// === Features ===
#![feature(trait_alias)]


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
