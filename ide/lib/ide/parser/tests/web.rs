use enso_prelude::*;

use ast::Ast;
use ast::IdMap;
use data::text::*;
use parser::Parser;
use parser::api::IsParser;

use std::rc::Rc;
use uuid::Uuid;
use wasm_bindgen_test::wasm_bindgen_test_configure;
use wasm_bindgen_test::wasm_bindgen_test;



wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn web_test() {
    let uuid = Uuid::parse_str("00000000-0000-0000-0000-000000000000").unwrap();

    let mut parser = Parser::new_or_panic();

    let mut parse = |input| {
        let span = Span::new(default(), Size::new(5));
        let ids  = IdMap(vec![(span,uuid)]);
        let ast  = parser.parse(String::from(input), ids).unwrap().wrapped;

        match Rc::try_unwrap(ast).unwrap().wrapped.wrapped {
            ast::Shape::Module(ast) => ast,
            _                       => panic!("Expected module."),
        }
    };

    let line = |term| {
        ast::Module {lines: vec![ast::BlockLine {elem: term, off: 0}]}
    };


    let app_x_y = ast::Prefix {func: Ast::var("x"), off: 3, arg: Ast::var("y")};


    assert_eq!(parse(""),       line(None));
    assert_eq!(parse("xy"),     line(Some(Ast::var("xy"))));
    assert_eq!(parse("x   y"),  line(Some(Ast::new(app_x_y, Some(uuid)))));

}
