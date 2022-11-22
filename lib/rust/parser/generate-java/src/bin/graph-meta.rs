//! Generate a GraphViz graph of parser datatype relationships in the `meta` metamodel.
//!
//! Usage:
//! ```console
//! graph-meta > meta.dot
//! dot -Tx11 meta.dot
//! ```

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_reflect::Reflect;



// =============================
// === Graphing `meta` types ===
// =============================

fn main() {
    let (graph, _) = enso_metamodel::rust::to_meta(enso_parser::syntax::Tree::reflect());
    let rendered = enso_metamodel::graphviz::Graph::from(&graph);
    println!("{}", rendered);
}
