//! Show debug-representation of AST of input sources.

// === Features ===
#![feature(exact_size_is_empty)]
#![feature(let_chains)]
#![feature(if_let_guard)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
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



// ===================
// === Debug-parse ===
// ===================

fn main() {
    use std::io::Read;
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    check_file("<stdin>", input.as_str());
}

fn check_file(path: &str, mut code: &str) {
    if let Some((_meta, code_)) = enso_parser::metadata::parse(code) {
        code = code_;
    }
    let ast = enso_parser::Parser::new().run(code);
    let expected_span = 0..(code.encode_utf16().count() as u32);
    enso_parser_debug::validate_spans(&ast, expected_span);
    for (parsed, original) in ast.code().lines().zip(code.lines()) {
        assert_eq!(parsed, original, "Bug: dropped tokens, while parsing: {path}");
    }
    let s_expr = enso_parser_debug::to_s_expr(&ast, code);
    println!("{s_expr}");
}
