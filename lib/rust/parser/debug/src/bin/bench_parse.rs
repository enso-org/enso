//! Parses Enso sources, measuring time spent in the parser.

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]



// =============
// === Tests ===
// =============

fn main() {
    let args = std::env::args().skip(1);
    let parser = enso_parser::Parser::new();
    let parse_time: std::time::Duration = args
        .map(|path| {
            let code = std::fs::read_to_string(path).unwrap();
            let mut code = code.as_str();
            if let Some((_meta, code_)) = enso_parser::metadata::parse(code) {
                code = code_;
            }
            let start = std::time::Instant::now();
            std::hint::black_box(parser.run(code));
            start.elapsed()
        })
        .sum();
    println!("Parse time: {} ms", parse_time.as_millis());
}
