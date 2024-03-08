//! Library of general data structures.

// === Features ===
#![feature(associated_type_bounds)]
#![feature(test)]
#![feature(trait_alias)]
#![feature(core_intrinsics)]
#![feature(cell_update)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
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

pub mod im_list;

pub use enso_prelude as prelude;
