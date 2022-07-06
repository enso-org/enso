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



// ===========================
// === Graphing Rust types ===
// ===========================

fn main() {
    let rendered = enso_reflect::graph::<enso_parser::syntax::tree::Tree>();
    println!("{}", rendered);
}
