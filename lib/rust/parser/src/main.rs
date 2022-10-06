//! Tests for [`enso_parser`].

#![recursion_limit = "256"]
// === Features ===
#![allow(incomplete_features)]
#![feature(allocator_api)]
#![feature(exact_size_is_empty)]
#![feature(test)]
#![feature(specialization)]
#![feature(let_chains)]
#![feature(if_let_guard)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]
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
    init_wasm();
    let args = std::env::args().skip(1);
    if args.is_empty() {
        use std::io::Read;
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).unwrap();
        check_file("<stdin>", input.as_str());
    } else {
        args.for_each(|path| check_file(&path, &std::fs::read_to_string(&path).unwrap()));
    }
}

fn check_file(path: &str, mut code: &str) {
    if let Some((_meta, code_)) = enso_parser::metadata::parse(code) {
        code = code_;
    }
    let ast = enso_parser::Parser::new().run(code);
    let errors = RefCell::new(vec![]);
    ast.map(|tree| {
        if let enso_parser::syntax::tree::Variant::Invalid(err) = &*tree.variant {
            errors.borrow_mut().push((err.clone(), tree.span.clone()));
        }
    });
    for (error, span) in &*errors.borrow() {
        let whitespace = &span.left_offset.code.repr;
        let start = whitespace.as_ptr() as usize + whitespace.len() - code.as_ptr() as usize;
        let mut line = 1;
        let mut char = 0;
        for (i, c) in code.char_indices() {
            if i >= start {
                break;
            }
            if c == '\n' {
                line += 1;
                char = 0;
            } else {
                char += 1;
            }
        }
        eprintln!("{path}:{line}:{char}: {}", &error.error.message);
    }
    for (parsed, original) in ast.code().lines().zip(code.lines()) {
        assert_eq!(parsed, original, "Bug: dropped tokens, while parsing: {}", path);
    }
}
