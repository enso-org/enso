//! Parses Enso sources and reports any syntax errors or warnings.
//!
//! Source files may be specified as command line arguments; if none are provided, source code will
//! be read from standard input.

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]

use enso_parser::prelude::*;

use clap::Parser;



use std::path::Path;
use std::path::PathBuf;



struct WithSourcePath<T> {
    path:  PathBuf,
    value: T,
}

#[derive(Parser)]
struct Cli {
    /// Files to check. If none specified, code will be read from standard input.
    files: Vec<PathBuf>,

    /// Only check if the parser fails to parse the input.
    #[arg(short, long)]
    smoke_test: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let mut to_read = vec![];
    let mut to_parse = vec![];
    if cli.files.is_empty() {
        use std::io::Read;
        let mut data = String::new();
        std::io::stdin().read_to_string(&mut data).unwrap();
        to_parse.push(WithSourcePath { path: "<stdin>".into(), value: data });
    } else {
        to_read.extend(cli.files);
    };
    let cores = std::thread::available_parallelism()
        .unwrap_or(std::num::NonZeroUsize::new(1).unwrap())
        .get();
    let io_threads = std::cmp::min(cores, to_read.len());
    let cpu_threads = std::cmp::min(cores, to_read.len() + to_parse.len());
    let to_read = std::sync::Arc::new(std::sync::Mutex::new(to_read));
    let to_parse = std::sync::Arc::new((
        std::sync::Mutex::new((to_parse, io_threads)),
        std::sync::Condvar::new(),
    ));
    for _ in 0..io_threads {
        let to_read = std::sync::Arc::clone(&to_read);
        let to_parse = std::sync::Arc::clone(&to_parse);
        std::thread::spawn(move || {
            let (to_parse, condvar) = &*to_parse;
            while let Some(path) = to_read.lock().unwrap().pop() {
                let data = std::fs::read_to_string(&path).unwrap();
                to_parse.lock().unwrap().0.push(WithSourcePath { path, value: data });
                condvar.notify_one();
            }
            let io_threads = &mut to_parse.lock().unwrap().1;
            *io_threads -= 1;
            if *io_threads == 0 {
                condvar.notify_all();
            }
        });
    }
    let to_print = std::sync::Arc::new(std::sync::Mutex::new(vec![]));
    let mut parsers = vec![];
    for _ in 0..cpu_threads {
        let to_parse = std::sync::Arc::clone(&to_parse);
        let to_print = std::sync::Arc::clone(&to_print);
        parsers.push(std::thread::spawn(move || {
            let mut parser = enso_parser::Parser::new();
            let (to_parse, condvar) = &*to_parse;
            loop {
                let source = {
                    let mut to_parse = to_parse.lock().unwrap();
                    while to_parse.0.is_empty() && to_parse.1 != 0 {
                        to_parse = condvar.wait(to_parse).unwrap();
                    }
                    match to_parse.0.pop() {
                        Some(source) => source,
                        None => break,
                    }
                };
                let results = check_file(source, &mut parser, cli.smoke_test);
                to_print.lock().unwrap().push(results);
            }
        }));
    }
    for parser in parsers {
        parser.join().unwrap();
    }
    let mut to_print = to_print.lock().unwrap();
    let mut to_print = mem::take(&mut *to_print);
    to_print.sort_unstable_by(|a, b| a.path.cmp(&b.path));
    let mut files_with_bugs = 0;
    for source in to_print {
        if !source.value.is_empty() {
            files_with_bugs += 1;
        }
        for line in source.value {
            eprintln!("{}", line);
        }
    }
    if files_with_bugs != 0 {
        Err(format!("Errors or warnings found in {files_with_bugs} files.").into())
    } else {
        Ok(())
    }
}

fn check_file(
    file: WithSourcePath<String>,
    parser: &mut enso_parser::Parser,
    smoke_test: bool,
) -> WithSourcePath<Vec<String>> {
    let mut code = file.value.as_str();
    if let Some((_meta, code_)) = enso_parser::metadata::parse(code) {
        code = code_;
    }
    let ast = parser.parse_module(code);
    let mut messages = if smoke_test { vec![] } else { collect_messages(&ast, &file.path) };
    if ast.code() != code {
        messages.push(format!(
            "Internal error: AST does not match source code. File: {}",
            file.path.display()
        ));
    }
    WithSourcePath { path: file.path, value: messages }
}

fn collect_messages(ast: &enso_parser::syntax::Tree, path: impl AsRef<Path>) -> Vec<String> {
    let errors = RefCell::new(vec![]);
    let warnings = RefCell::new(vec![]);
    ast.visit_trees(|tree| {
        match &tree.variant {
            enso_parser::syntax::tree::Variant::Invalid(err) => {
                let error = format!("{}: {}", err.error.message, tree.code());
                errors.borrow_mut().push((error, tree.span.clone()));
            }
            enso_parser::syntax::tree::Variant::OprApp(app) =>
                if let enso_parser::syntax::tree::OprApp { opr: Err(e), .. } = &**app {
                    let error = format!("Consecutive operators: {:?}", e.operators);
                    errors.borrow_mut().push((error, tree.span.clone()));
                },
            enso_parser::syntax::tree::Variant::TextLiteral(text) =>
                for element in &text.elements {
                    if let enso_parser::syntax::tree::TextElement::Escape { token } = element {
                        if token.variant.value.is_none() {
                            let escape = token.code.to_string();
                            let error = format!("Invalid escape sequence: {escape}");
                            errors.borrow_mut().push((error, tree.span.clone()));
                        }
                    }
                },
            _ => {}
        }
        for warning in tree.warnings.iter() {
            warnings.borrow_mut().push((warning.clone(), tree.span.clone()));
        }
    });
    let sort_key = |span: &enso_parser::source::Span| {
        (
            span.left_offset.code.start,
            span.left_offset.code.position_after().start + span.code_length,
        )
    };
    errors.borrow_mut().sort_unstable_by_key(|(_, span)| sort_key(span));
    warnings.borrow_mut().sort_unstable_by_key(|(_, span)| sort_key(span));
    let mut messages = vec![];
    for (message, span) in &*errors.borrow() {
        messages.push(format!("E {}: {}", fmt_location(path.as_ref().display(), span), &message));
    }
    for (warning, span) in &*warnings.borrow() {
        messages.push(format!(
            "W {}: {}",
            fmt_location(path.as_ref().display(), span),
            warning.message()
        ));
    }
    messages
}

fn fmt_location(path: impl Display, span: &enso_parser::source::Span) -> String {
    let start = span.left_offset.code.position_after().start;
    let end = start + span.code_length;
    format!("{path} {}:{}-{}:{}", start.line + 1, start.col16, end.line + 1, end.col16)
}
