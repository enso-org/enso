use enso_prelude::*;

use ast::crumbs::Crumbable;
use ast::HasRepr;
use parser::Parser;

use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;



wasm_bindgen_test_configure!(run_in_browser);


#[wasm_bindgen_test]
fn macro_crumb_test() {
    let ast = Parser::new_or_panic().parse_line_ast("foo -> bar").unwrap();
    let crumbs = ast.iter_subcrumbs().collect_vec();

    assert_eq!(ast.get(&crumbs[0]).unwrap().repr(), "foo");
    assert_eq!(ast.get(&crumbs[1]).unwrap().repr(), "->");
    assert_eq!(ast.get(&crumbs[2]).unwrap().repr(), "bar");

    let ast = Parser::new_or_panic().parse_line_ast("(  foo bar )").unwrap();
    let crumbs = ast.iter_subcrumbs().collect_vec();

    assert_eq!(ast.get(&crumbs[0]).unwrap().repr(), "(");
    assert_eq!(ast.get(&crumbs[1]).unwrap().repr(), "foo bar");
    assert_eq!(ast.get(&crumbs[2]).unwrap().repr(), ")");
}
