//! Tests for [`enso_parser`].

#![recursion_limit = "256"]
// === Features ===
#![allow(incomplete_features)]
#![feature(allocator_api)]
#![feature(test)]
#![feature(specialization)]
#![feature(let_chains)]
#![feature(if_let_guard)]
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

use enso_parser::prelude::*;



// =============
// === Tests ===
// =============

fn main() {
    init_tracing(TRACE);
    let ast = enso_parser::Parser::new().run("foo = 23");
    println!("\n\n==================\n\n");
    println!("{:#?}", ast);
}
