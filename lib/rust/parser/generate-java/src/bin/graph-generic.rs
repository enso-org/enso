//! Generate a GraphViz graph of parser datatype relationships in the `generic` metamodel.
//!
//! Usage:
//! ```console
//! graph-generic > generic.dot
//! dot -Tx11 generic.dot
//! ```



// ==========================================
// === Graph for `generic` representation ===
// ==========================================

use enso_reflect::Reflect;

fn main() {
    let (graph, _) = enso_reflect::rust::to_generic(enso_parser::syntax::Tree::reflect());
    let rendered = enso_reflect::generic::graphviz::graph(&graph);
    println!("{}", rendered);
}
