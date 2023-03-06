#![recursion_limit = "256"]
// === Features ===
#![allow(incomplete_features)]
#![feature(assert_matches)]
#![feature(allocator_api)]
#![feature(exact_size_is_empty)]
#![feature(test)]
#![feature(specialization)]
#![feature(let_chains)]
#![feature(if_let_guard)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


use enso_parser::prelude::*;



fn main() {
    init_global();
    let args = std::env::args().skip(1);
    if args.is_empty() {
        use std::io::Read;
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).unwrap();
        check_doc_parse("<stdin>", input.as_str());
    } else {
        args.for_each(|path| check_doc_parse(&path, &std::fs::read_to_string(&path).unwrap()));
    }
}

fn check_doc_parse(filename: &str, code: &str) {
    println!("{filename}");
    let docs = extract_docs(filename, code);
    for doc in &docs {
        let doc = enso_doc_parser::token_collector::parse(doc);
        for token in doc.tokens {
            println!("{token:?}");
        }
    }
}

fn extract_docs(_filename: &str, mut code: &str) -> Vec<String> {
    if let Some((_meta, code_)) = enso_parser::metadata::parse(code) {
        code = code_;
    }
    let ast = enso_parser::Parser::new().run(code);
    let docs = RefCell::new(vec![]);
    ast.map(|tree| match &*tree.variant {
        enso_parser::syntax::tree::Variant::Documented(doc) => {
            docs.borrow_mut().push(doc.documentation.clone());
        }
        enso_parser::syntax::tree::Variant::CaseOf(case_of) => {
            for case in case_of.cases.iter().filter_map(|c| c.case.as_ref()) {
                docs.borrow_mut().extend(case.documentation.clone());
            }
        }
        _ => {}
    });
    docs.take().into_iter().map(|node| node.content()).collect()
}
