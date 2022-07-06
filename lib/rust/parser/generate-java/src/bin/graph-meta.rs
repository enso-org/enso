//! Generate a GraphViz graph of parser datatype relationships in the `meta` metamodel.
//!
//! Usage:
//! ```console
//! graph-meta > meta.dot
//! dot -Tx11 meta.dot
//! ```



// =======================================
// === Graph for `meta` representation ===
// =======================================

use enso_reflect::Reflect;

fn main() {
    let (graph, _) = enso_metamodel::rust::to_meta(enso_parser::syntax::Tree::reflect());
    let rendered = enso_metamodel::graphviz::Graph::from(&graph);
    println!("{}", rendered);
}
