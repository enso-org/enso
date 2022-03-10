//! Ensogl text rendering implementation.
//!
//! To properly understand the implementation and its assumptions, please read the documentation
//! of [`enso_text`] crate carefully.

#![recursion_limit = "1024"]
// === Features ===
#![feature(trait_alias)]
#![feature(type_ascription)]
#![feature(option_zip)]
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

pub mod buffer;
pub mod component;
pub mod typeface;



/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

pub use buffer::*;
pub use component::Area;
pub use ensogl_core::data;
