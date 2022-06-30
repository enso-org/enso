//! Generate a GraphViz graph of parser datatype relationships in the Java type system.
//!
//! Usage:
//! ```console
//! graph-java > java.dot
//! dot -Tx11 java.dot
//! ```



// ============================
// === Graph for Java types ===
// ============================

use enso_metamodel::java;
use enso_metamodel::rust;
use enso_reflect::Reflect;

fn main() {
    let (graph, _) = rust::to_meta(enso_parser::syntax::Tree::reflect());
    let (graph, _) = java::from_meta(&graph, enso_parser_generate_java::EITHER_TYPE);
    let graph = java::transform::optional_to_null(graph);
    let rendered = java::graphviz::graph(&graph);
    println!("{}", rendered);
}
