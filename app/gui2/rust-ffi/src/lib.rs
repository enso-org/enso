// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use wasm_bindgen::prelude::*;

use enso_parser::Parser;



thread_local! {
    pub static PARSER: Parser = Parser::new();
}

#[wasm_bindgen]
pub fn parse_to_json(code: &str) -> String {
    let ast = PARSER.with(|parser| parser.run(code));
    serde_json::to_string(&ast).expect("Failed to serialize AST to JSON")
}
