use ast_generation::api;

use std::fs::File;
use std::io::prelude::*;
use std::process::Command;


/// Generates the AST API and saves the result into the file `src/api.rs`.
///
/// Should be replaced by proc macro once IDE gets proper support for them (autocomplete etc.).
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
    if let Ok(result) = Command::new("rustfmt").args(&["src/api.rs"]).output() {
        if !result.status.success() {
            println!("cargo:warning=Couldn't format generated AST API.");
            if let Ok(err_message) = String::from_utf8(result.stderr) {
                println!("cargo:warning={}", err_message);
            }
        }
    } else {
        println!("cargo:warning=Couldn't format generated AST API - rustfmt isn't installed.");
    }
}

fn main() {
    println!("cargo:rerun-if-changed=src/ast.rs");
    generate_api()
}
