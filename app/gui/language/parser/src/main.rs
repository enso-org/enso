// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]

use enso_prelude::*;



/// Simple interactive tester - calls parser with its argument (or a
/// hardcoded default) and prints the result, then calls doc parser
/// and prints the HTML code or an error message.
fn main() {
    let default_input = String::from("import Foo.Bar\nfoo = a + 2");
    let program = std::env::args().nth(1).unwrap_or(default_input);
    DEBUG!("Will parse: " program);

    let parser = parser::Parser::new_or_panic();
    let output = parser.parse(program, default());
    match output {
        Ok(result) => DEBUG!("Parser responded with: {result:?}"),
        Err(e) => DEBUG!("Failed to obtain a response: {e:?}"),
    }


    let default_input = String::from("##\n  DEPRECATED\n  Foo bar baz\ntype Foo\n  type Bar");
    let program = std::env::args().nth(1).unwrap_or(default_input);
    DEBUG!("Will parse: " program);

    let parser = parser::DocParser::new_or_panic();
    let output = parser.generate_html_docs(program);
    match output {
        Ok(result) => DEBUG!("Doc parser responded with: {result:?}"),
        Err(e) => DEBUG!("Failed to obtain a response: {e:?}"),
    }


    let default_input = String::from("Computes the _logical_ conjunction of *two* booleans");
    let program = std::env::args().nth(1).unwrap_or(default_input);
    DEBUG!("Will parse: " program);

    let parser = parser::DocParser::new_or_panic();
    let output = parser.generate_html_doc_pure(program);
    match output {
        Ok(result) => DEBUG!("Doc parser responded with: {result:?}"),
        Err(e) => DEBUG!("Failed to obtain a response: {e:?}"),
    }
}
