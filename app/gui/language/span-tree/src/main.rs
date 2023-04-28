//! Command-line debug tool for `SpanTree`. Accepts a single line of Enso source code as an
//! argument, and prints a debug representation of the resulting `SpanTree`.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use span_tree::generate;
use span_tree::generate::SpanTreeGenerator;
use span_tree::SpanTree;



// ===================
// === Entry point ===
// ===================

#[allow(missing_docs)]
pub fn main() {
    let mut args = std::env::args();
    let _ = args.next().unwrap();
    let code = args.next().unwrap();

    let parser = parser::Parser::new();
    let ast = parser.parse_line_ast(&code).unwrap();
    let tree: SpanTree = ast.generate_tree(&generate::context::Empty).unwrap();
    let tree = tree.debug_print(&code);
    println!("{tree}");
}
