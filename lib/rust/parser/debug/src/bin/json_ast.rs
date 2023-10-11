//! Run the parser from the command line, and output the a JSON serialization of the AST for
//! debugging.

// === Features ===
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===

fn main() {
    use std::io::Read;
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let mut code = input.as_str();
    if let Some((_meta, code_)) = enso_parser::metadata::parse(code) {
        code = code_;
    }
    let ast = enso_parser::Parser::new().run(code);
    serde_json::to_writer(std::io::stdout(), &ast).unwrap();
}
