//! Parses Enso sources and reports any syntax errors, while performing internal consistency checks.
//! Source files may be specified as command line arguments; if none a provided, source code will be
//! read from standard input.

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]

use enso_parser::prelude::*;



// =============
// === Tests ===
// =============

fn main() {
    let args = std::env::args().skip(1);
    let mut parser = enso_parser::Parser::new();
    if args.len() == 0 {
        use std::io::Read;
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).unwrap();
        check_file("<stdin>", input.as_str(), &mut parser);
    } else {
        args.for_each(|path| {
            check_file(&path, &std::fs::read_to_string(&path).unwrap(), &mut parser)
        });
    }
}

fn check_file(path: &str, mut code: &str, parser: &mut enso_parser::Parser) {
    if let Some((_meta, code_)) = enso_parser::metadata::parse(code) {
        code = code_;
    }
    let ast = parser.run(code);
    let errors = RefCell::new(vec![]);
    ast.visit_trees(|tree| {
        if let enso_parser::syntax::tree::Variant::Invalid(err) = &*tree.variant {
            let error = format!("{}: {}", err.error.message, tree.code());
            errors.borrow_mut().push((error, tree.span.clone()));
        } else if let enso_parser::syntax::tree::Variant::TextLiteral(text) = &*tree.variant {
            for element in &text.elements {
                if let enso_parser::syntax::tree::TextElement::Escape { token } = element {
                    if token.variant.value.is_none() {
                        let escape = token.code.to_string();
                        let error = format!("Invalid escape sequence: {escape}");
                        errors.borrow_mut().push((error, tree.span.clone()));
                    }
                }
            }
        }
    });
    for (error, span) in &*errors.borrow() {
        let whitespace = &span.left_offset.code.repr;
        let start = whitespace.as_ptr() as usize + whitespace.len() - code.as_ptr() as usize;
        let mut line = 1;
        let mut char = 0;
        for (i, c) in code.char_indices() {
            if i >= start {
                break;
            }
            if c == '\n' {
                line += 1;
                char = 0;
            } else {
                char += 1;
            }
        }
        eprintln!("{path}:{line}:{char}: {}", &error);
    }
    for (parsed, original) in ast.code().lines().zip(code.lines()) {
        assert_eq!(parsed, original, "Bug: dropped tokens, while parsing: {path}");
    }
}
