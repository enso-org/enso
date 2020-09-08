use std::fs::File;
use std::io::prelude::*;
use ast_generation::api;


/// Generates the AST API  and saves the result into the file `src/api.rs`.
///
/// The content of the generated file can be used with the `include!` macro.
fn generate_api() {
    let ast_path     = "src/ast.rs";
    let api_path     = "src/api.rs";
    let ast_error    = format!("The AST definition should exist at {}.", ast_path);
    let api_error    = format!("Cannot open API file at {}.", api_path);
    let mut ast_file = File::open(ast_path).expect(ast_error.as_str());
    let mut api_file = File::create(api_path).expect(api_error.as_str());
    let mut ast_str  = String::new();

    ast_file.read_to_string(&mut ast_str).expect("Unable to read AST definition.");
    let api = api::Source::api(ast_str.as_str());
    api_file.write_all(api.as_bytes()).expect("Unable to write API definition.");
}

fn main() {
    generate_api()
}
