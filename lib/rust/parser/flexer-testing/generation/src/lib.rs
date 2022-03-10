//! This library exposes the specialized version of the Enso lexer.
//!
//! Its sole purpose is to avoid the lexer definition getting out of sync with its implementation
//! (the generated engine), which requires the engine to live in a separate crate.
//!
//! This separation enables generation of the enso lexer source code with `build.rs` during
//! compilation. Its output is then stored in a new file `engine.rs`and exported by `lexer.rs`.

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



