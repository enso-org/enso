//! Generate a GraphViz graph of parser datatype relationships in the Rust type system.
//!
//! Usage:
//! ```console
//! graph-rust > rust.dot
//! dot -Tx11 rust.dot
//! ```

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]



// ===========================
// === Graphing Rust types ===
// ===========================

fn main() {
    let rendered = enso_reflect::graph::<enso_parser::syntax::tree::Tree>();
    println!("{}", rendered);
}
