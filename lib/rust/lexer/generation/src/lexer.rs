//! A driver for the Enso lexer.

use crate::prelude::*;

use crate::generated::engine::EnsoLexer;
use crate::library::token;
use crate::prelude::reader::decoder::DecoderUTF8;
use enso_flexer::LexingResult;



// ====================
// === Lexer Driver ===
// ====================

/// Execute the lexer on the provided `input`, assuming utf-8 encoding.
pub fn run(input: impl AsRef<str>) -> LexingResult<token::Stream> {
    let mut lexer = EnsoLexer::new();
    let reader = Reader::new(input.as_ref().as_bytes(), DecoderUTF8());
    lexer.run(reader)
}
