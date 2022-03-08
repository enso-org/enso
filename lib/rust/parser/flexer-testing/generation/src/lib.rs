//! This library exposes the specialized version of the Enso lexer.
//!
//! Its sole purpose is to avoid the lexer definition getting out of sync with its implementation
//! (the generated engine), which requires the engine to live in a separate crate.
//!
//! This separation enables generation of the enso lexer source code with `build.rs` during
//! compilation. Its output is then stored in a new file `engine.rs`and exported by `lexer.rs`.

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



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



//
// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-standard linter configuration ===



// === Standard linter configuration ===
#![warn(missing_copy_implementations)]#![warn(missing_debug_implementations)]#![warn(missing_docs)]#![warn(trivial_casts)]#![warn(trivial_numeric_casts)]#![warn(unsafe_code)]#![warn(unused_import_braces)]#![warn(unused_qualifications)]
// === Non-standard linter configuration ===

// === Features ===
#![feature(test)]


// ==============
// === Export ===
// ==============

#[rustfmt::skip]
pub mod generated;



