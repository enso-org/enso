use parser::prelude::*;

use parser::Parser;
use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn import_utilities() {
    use ast::macros::ast_as_import_match;
    use ast::macros::is_ast_import;
    use ast::macros::is_match_import;

    let parser = Parser::new_or_panic();
    let expect_import = |code: &str| {
        let ast = parser.parse_line_ast(code).unwrap();
        assert!(is_ast_import(&ast), "Not Ast import: {:?}", ast);
        let ast_match = ast_as_import_match(&ast).unwrap();
        assert_eq!(&ast, ast_match.ast());
        assert!(is_match_import(&ast_match));
    };

    let expect_not_import = |code: &str| {
        let ast = parser.parse_line_ast(code).unwrap();
        assert!(!is_ast_import(&ast));
        assert!(ast_as_import_match(&ast).is_none());
    };

    expect_import("import");
    expect_import("import Foo");
    expect_import("import foo.Foo.Bar");
    expect_import("import foo.Foo.Bar");
    expect_import("import Foo.Bar");
    expect_import("import Foo.Bar.Baz");
    expect_import("from Foo import Bar");
    expect_import("from foo.Foo import all hiding Bar");
    expect_import("from Base.Data.List import all hiding Cons, Nil");

    expect_not_import("type Foo");
    expect_not_import("type Foo as Bar");
    expect_not_import("if Foo then Bar else Baz");
    expect_not_import("Foo.Bar.Baz");
    expect_not_import("->");
    expect_not_import("export");
    expect_not_import("export Foo");
    expect_not_import("from Foo export all hiding Bar");
}

#[wasm_bindgen_test]
fn recognizing_lambdas() {
    let parser = Parser::new_or_panic();

    let expect_lambda = |code: &str, arg: &str, body: &str| {
        let ast = parser.parse_line_ast(code).unwrap();
        let lambda = ast::macros::as_lambda(&ast).expect("failed to recognize lambda");
        assert_eq!(lambda.arg.repr(), arg);
        assert_eq!(lambda.body.repr(), body);
        assert_eq!(*lambda.arg, ast.get_traversing(&lambda.arg.crumbs).unwrap());
        assert_eq!(*lambda.body, ast.get_traversing(&lambda.body.crumbs).unwrap());
    };
    let expect_not_lambda = |code: &str| {
        let ast = parser.parse_line_ast(code).unwrap();
        assert!(ast::macros::as_lambda_match(&ast).is_none(), "wrongly recognized a lambda");
    };

    expect_lambda("a->b", "a", "b");
    expect_lambda("foo->4+(4)", "foo", "4+(4)");
    expect_lambda("a->b->c", "a", "b->c");
    expect_lambda("(a->b)->c", "(a->b)", "c");

    expect_not_lambda("(a->b)");
    expect_not_lambda("a+b");
    expect_not_lambda("'a+b'");
    expect_not_lambda("497");
}
