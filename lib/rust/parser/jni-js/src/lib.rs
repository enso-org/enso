//! JavaScript port of Java interface to [`enso_parser`].

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

use enso_prelude::*;
use wasm_bindgen::prelude::*;



// ======================
// === Java Interface ===
// ======================

#[wasm_bindgen]
pub struct ParseResult {
    base: u64,
    output: Vec<u8>,
    metadata: Option<Rc<enso_parser::metadata::Metadata>>,
}

#[wasm_bindgen]
pub fn parse_input(input: &str) -> ParseResult {
    let mut code = input;
    let mut meta = None;
    if let Some((meta_, code_)) = enso_parser::metadata::parse(input) {
        match meta_ {
            Ok(meta_) => meta = Some(meta_),
            Err(e) => error!("Ignoring invalid metadata: {e}."),
        }
        code = code_;
    }
    let base = str::as_ptr(code) as usize as u64;
    let tree = PARSER.with(|parser| parser.run(code));
    let output = match enso_parser::serialization::serialize_tree(&tree) {
        Ok(tree) => tree,
        // `Tree` does not contain any types with fallible `serialize` implementations, so this
        // cannot fail.
        Err(_) => {
            debug_assert!(false);
            default()
        }
    };
    let metadata = meta.map(Rc::new);
    ParseResult { base, output, metadata }
}

#[wasm_bindgen]
pub fn ast(state: &ParseResult) -> Vec<u8> {
    state.output.clone()
}

#[wasm_bindgen]
pub fn base(state: &ParseResult) -> u64 {
    state.base
}

#[wasm_bindgen]
pub fn metadata(state: &ParseResult) -> Option<Metadata> {
    state.metadata.clone().map(Metadata)
}

#[wasm_bindgen]
pub struct Metadata(Rc<enso_parser::metadata::Metadata>);

#[wasm_bindgen]
pub fn uuid_high(metadata: &Metadata, code_offset: u64, code_length: u64) -> u64 {
    metadata.get_uuid(code_offset, code_length).0
}

#[wasm_bindgen]
pub fn uuid_low(metadata: &Metadata, code_offset: u64, code_length: u64) -> u64 {
    metadata.get_uuid(code_offset, code_length).0
}


// === Implementation ===

thread_local! {
    pub static PARSER: enso_parser::Parser = enso_parser::Parser::new();
}

impl Metadata {
    fn get_uuid(&self, code_offset: u64, code_length: u64) -> (u64, u64) {
        let data = self.0.get_uuid(code_offset as usize, code_length as usize);
        match data {
            Some(uuid) => uuid.as_u64_pair(),
            None => (0, 0),
        }
    }
}

#[wasm_bindgen(start)]
fn main() {
    console_error_panic_hook::set_once();
}
