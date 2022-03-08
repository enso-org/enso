use parser::prelude::*;

use ast::HasIdMap;
use parser::Parser;
use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;



wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn id_map_round_tripping() {
    let cases = [
        "main =\n    2 + 2",
        "main =   \n \n    2 + 2\n    foo = bar \n    baz",
        "main = \n    foo\n\n    bar",
        "main = \n    foo\n  \n    bar",
        "main = \n    foo\n     \n    bar",
        "main = \n    foo\n    baz \n    bar",
    ];

    let parser = Parser::new().unwrap();
    for case in cases.iter().copied() {
        let id_map = default();
        let ast1 = parser.parse_module(case, id_map).unwrap();
        let id_map = ast1.id_map();
        let ast2 = parser.parse_module(case, id_map).unwrap();
        assert_eq!(ast1, ast2)
    }
}
