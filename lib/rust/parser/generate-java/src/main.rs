//! Generate the Java types corresponding to `enso-parser`'s AST types.
//!
//! # Usage
//!
//! Generated files will be placed in the directory given as an argument:
//! ```console
//! generate-java org/enso/syntax2/
//! ```

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
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
    let (graph, rust_to_meta) = rust::to_meta(ast);
    let (graph, meta_to_java) = java::from_meta(&graph, enso_parser_generate_java::EITHER_TYPE);
    let mut graph = java::transform::optional_to_null(graph);
    let rust_to_java = |id| meta_to_java[&rust_to_meta[&id]];
    let (tree, token) = (rust_to_java(tree), rust_to_java(token));
    serialization::derive(&mut graph, tree, token);
    let graph = java::to_syntax(&graph, enso_parser_generate_java::PACKAGE);
    let mut args = std::env::args();
    args.next().unwrap();
    let dir = std::path::PathBuf::from(args.next().expect("Usage: generate-java <output-dir>"));
    if !dir.exists() {
        std::fs::create_dir_all(&dir).unwrap();
    }
    for class in graph {
        let code = class.to_string();
        let path = dir.join(format!("{}.java", &class.name));
        std::fs::write(path, code).unwrap();
    }
}
