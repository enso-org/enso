use parser::DocParser;

use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;



wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn doc_gen_test() {
    // Case of pure documentation code.
    let input = String::from("Foo *Bar* Baz");
    let program = std::env::args().nth(1).unwrap_or(input);
    let parser = DocParser::new_or_panic();
    let gen_code = parser.generate_html_doc_pure(program).unwrap();
    assert_ne!(gen_code.len(), 0);

    let input = String::from("##\n  foo\ntype Foo\n");
    let program = std::env::args().nth(1).unwrap_or(input);
    let parser = DocParser::new_or_panic();
    let gen_code = parser.generate_html_docs(program).unwrap();
    assert_ne!(gen_code.len(), 0);

    let input = String::from("##\n  DEPRECATED\n  Foo bar baz\ntype Foo\n  type Bar");
    let program = std::env::args().nth(1).unwrap_or(input);
    let parser = DocParser::new_or_panic();
    let gen_code = parser.generate_html_docs(program).unwrap();
    assert_ne!(gen_code.len(), 0);
}
