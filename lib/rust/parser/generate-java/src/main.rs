//! Generate the Java types corresponding to `enso-parser`'s AST types.
//!
//! # Usage
//!
//! Generated files will be placed in the directory given as an argument:
//! ```console
//! generate-java org/enso/syntax2/
//! ```

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use enso_metamodel::java;
use enso_metamodel::rust;
use enso_parser_generate_java::serialization;
use enso_reflect::Reflect;



// =======================
// === Java Generation ===
// =======================

fn main() {
    let ast = enso_parser::syntax::Tree::reflect();
    let tree = enso_parser::syntax::Tree::reflect().id;
    let token = enso_parser::syntax::Token::<enso_parser::syntax::token::Variant>::reflect().id;
    let unsupported = enso_parser::syntax::tree::Unsupported::reflect().id;
    let (graph, rust_to_meta) = rust::to_meta(ast);
    let (graph, meta_to_java) = java::from_meta(&graph, enso_parser_generate_java::EITHER_TYPE);
    let mut graph = java::transform::optional_to_null(graph);
    let rust_to_java = |id| meta_to_java[&rust_to_meta[&id]];
    let (tree, token, unsupported) =
        (rust_to_java(tree), rust_to_java(token), rust_to_java(unsupported));
    serialization::derive(&mut graph, tree, token, unsupported);
    let graph = java::to_syntax(&graph, enso_parser_generate_java::PACKAGE);
    let mut args = std::env::args();
    args.next().unwrap();
    let dir = args.next().expect("Usage: generate-java <output-dir>");
    for class in graph {
        let code = class.to_string();
        std::fs::write(format!("{}/{}.java", &dir, &class.name), &code).unwrap();
    }
}
