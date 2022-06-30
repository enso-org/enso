//! Generate a GraphViz graph of parser datatype relationships in the Rust type system.
//!
//! Usage:
//! ```console
//! graph-rust > rust.dot
//! dot -Tx11 rust.dot
//! ```



// ============================
// === Graph for Rust types ===
// ============================

fn main() {
    let rendered = enso_metamodel::rust::graphviz::graph::<enso_parser::syntax::tree::Tree>();
    println!("{}", rendered);
}
