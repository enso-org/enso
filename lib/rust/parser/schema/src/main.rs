//! Generate a schema representing `enso-parser`'s AST types. This schema can be used to generate
//! AST representations and (de)serialization.
//!
//! The JSON schema data will be emitted to standard output.

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
#![warn(unused_qualifications)]



// =========================
// === Schema Generation ===
// =========================

fn main() {
    println!("{}", serde_json::to_string(&enso_parser_schema::types()).unwrap());
}
