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
pub fn parse_doc_to_json(docs: &str) -> String {
    let docs = enso_doc_parser::parse(docs);
    serde_json::to_string(&docs).expect("Failed to serialize Doc Sections to JSON")
}

#[wasm_bindgen]
pub fn parse(code: &str) -> Vec<u8> {
    let ast = PARSER.with(|parser| parser.run(code));
    enso_parser::format::serialize(&ast).expect("Failed to serialize AST to binary format")
}

#[wasm_bindgen]
pub fn is_ident_or_operator(code: &str) -> u32 {
    let parsed = enso_parser::lexer::run(code);
    if parsed.internal_error.is_some() {
        return 0;
    }
    let token = match &parsed.value[..] {
        [token] => token,
        _ => return 0,
    };
    match &token.variant {
        enso_parser::syntax::token::Variant::Ident(_) => 1,
        enso_parser::syntax::token::Variant::Operator(_) => 2,
        _ => 0,
    }
}

#[wasm_bindgen(start)]
fn main() {
    console_error_panic_hook::set_once();
}
