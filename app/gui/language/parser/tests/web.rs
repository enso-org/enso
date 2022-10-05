// === Non-Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

use enso_prelude::*;

use ast::Ast;
use parser_scala::api::ParsedSourceFile;
use parser_scala::Parser;
use uuid::Uuid;
use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;



wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn web_test() {
    let uuid = Uuid::parse_str("00000000-0000-0000-0000-000000000000").unwrap();

    let parser = Parser::new_or_panic();

    let parse = |input| parser.parse_with_metadata(input).unwrap();
    let file = |term| ParsedSourceFile {
        metadata: serde_json::json!({}),
        ast:      ast::known::KnownAst::new_no_id(term),
    };


    let line = |term| ast::Module { lines: vec![ast::BlockLine { elem: term, off: 0 }] };

    let app = ast::Prefix { func: Ast::var("x"), off: 3, arg: Ast::var("y") };
    let var = ast::Var { name: "x".into() };

    let ast = file(line(None));
    assert_eq!(parse(String::try_from(&ast).unwrap()), ast);

    let ast = file(line(Some(Ast::new(var, Some(uuid)))));
    assert_eq!(parse(String::try_from(&ast).unwrap()), ast);

    let ast = file(line(Some(Ast::new(app, Some(uuid)))));
    assert_eq!(parse(String::try_from(&ast).unwrap()), ast);
}
