//! EnsoGL GUI Component Abstraction.
//!
//! A collection of utilities for easy implementing new GUI components.

#![recursion_limit = "512"]

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

// === Non-Standard Linter Configuration ===

// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]


// ==============
// === Export ===
// ==============

pub mod component;



/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}
