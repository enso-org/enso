//! Generate a schema representing `enso-parser`'s AST types. This schema can be used to generate
//! AST representations and deserialization in other languages, such as TypeScript.
//!
//! The JSON schema data will be emitted to standard output.

// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]



// =========================
// === Schema Generation ===
// =========================

fn main() {
    serde_json::to_writer_pretty(std::io::stdout(), &enso_parser_schema::schema()).unwrap()
}
