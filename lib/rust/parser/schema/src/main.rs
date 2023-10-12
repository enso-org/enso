//! Generate a schema representing `enso-parser`'s AST types. This schema can be used to generate
//! AST representations and deserialization in other languages, such as TypeScript.
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
    serde_json::to_writer_pretty(std::io::stdout(), &enso_parser_schema::schema()).unwrap()
}
