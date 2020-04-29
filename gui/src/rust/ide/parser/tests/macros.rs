use parser::prelude::*;

use parser::Parser;
use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn recognizing_lambdas() {
    let parser = Parser::new_or_panic();

    let expect_lambda = |code:&str, arg:&str, body:&str| {
        let ast = parser.parse_line(code).unwrap();
        let lambda = ast::macros::as_lambda(&ast).expect("failed to recognize lambda");
        assert_eq!(lambda.arg.repr(), arg);
        assert_eq!(lambda.body.repr(), body);
        assert_eq!(*lambda.arg, ast.get_traversing(&lambda.arg.crumbs).unwrap());
        assert_eq!(*lambda.body, ast.get_traversing(&lambda.body.crumbs).unwrap());
    };
    let expect_not_lambda = |code:&str| {
        let ast = parser.parse_line(code).unwrap();
        assert!(ast::macros::as_lambda_match(&ast).is_none(), "wrongly recognized a lambda");
    };

    expect_lambda("a->b",       "a",    "b");
    expect_lambda("foo->4+(4)", "foo",  "4+(4)");
    expect_lambda("a->b->c",    "a",    "b->c");
    // expect_lambda("(a->b)->c"); // TODO: Failing due to internal parser error: java.lang.NullPointerException

    expect_not_lambda("(a->b)");
    expect_not_lambda("a+b");
    expect_not_lambda("'a+b'");
    expect_not_lambda("497");
}
