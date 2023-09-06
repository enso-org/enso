use enso_parser::Parser;
use wasm_bindgen::prelude::*;


thread_local! {
    pub static PARSER: Parser = Parser::new();
}

#[wasm_bindgen]
pub fn parse_to_json(code: &str) -> String {
    let ast = PARSER.with(|parser| parser.run(code));
    serde_json::to_string(&ast).expect("Failed to serialize AST to JSON")
}
