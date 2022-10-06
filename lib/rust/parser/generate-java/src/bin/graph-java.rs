//! Generate a GraphViz graph of parser datatype relationships in the Java type system.
//!
//! Usage:
//! ```console
//! graph-java > java.dot
//! dot -Tx11 java.dot
//! ```

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]

use enso_metamodel::graphviz;
use enso_metamodel::java;
use enso_metamodel::rust;
use enso_reflect::Reflect;



// ===========================
// === Graphing Java types ===
// ===========================

fn main() {
    let (graph, _) = rust::to_meta(enso_parser::syntax::Tree::reflect());
    let (graph, _) = java::from_meta(&graph, enso_parser_generate_java::EITHER_TYPE);
    let graph = java::transform::optional_to_null(graph);
    let rendered = graphviz::Graph::from(&graph);
    println!("{}", rendered);
}
