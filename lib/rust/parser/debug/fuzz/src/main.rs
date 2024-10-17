//! Build:
//! `cargo afl build --profile=fuzz -p enso-parser-fuzz`
//!
//! Run:
//! `cargo afl fuzz -i inputs/ -o outputs/ target/rust/fuzz/enso-parser-fuzz`

use afl::fuzz;



fn main() {
    fuzz!(|code: &[u8]| {
        if let Ok(code) = std::str::from_utf8(code) {
            let parser = enso_parser::Parser::new();
            let ast = parser.parse_module(code);
            assert_eq!(ast.code(), code);
        }
    });
}
