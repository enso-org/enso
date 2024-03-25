//! Library of general data structures.

// === Features ===
#![feature(associated_type_bounds)]
#![feature(test)]
#![feature(trait_alias)]
#![feature(cell_update)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]


// ==============
// === Export ===
// ==============

pub mod im_list;

pub use enso_prelude as prelude;
