//! Run the lexer from the command line, for understanding the early stages of the parser.

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]



/// Lexer main function used for ad-hoc testing during development.
pub fn main() {
    use std::io::Read;
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    println!("{:#?}", enso_parser::lexer::debug::lex_and_validate_spans(&input));
}
