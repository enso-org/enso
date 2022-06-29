//! Generate a GraphViz graph of parser datatype relationships in the `abstracted` metamodel.
//!
//! Usage:
//! ```console
//! graph-abstracted > abstracted.dot
//! dot -Tx11 abstracted.dot
//! ```



// =============================================
// === Graph for `abstracted` representation ===
// =============================================

use enso_reflect::Reflect;

fn main() {
    let (graph, _) = enso_reflect::rust::to_abstracted(enso_parser::syntax::Tree::reflect());
    let rendered = enso_reflect::abstracted::graphviz::graph(&graph);
    println!("{}", rendered);
}
