//! A driver for the Enso lexer.

use crate::prelude::*;

use enso_flexer::LexingResult;
use crate::library::token;
use crate::generated::engine::EnsoLexer;
use crate::prelude::reader::decoder::DecoderUTF8;



// ====================
// === Lexer Driver ===
// ====================

/// Execute the lexer on the provided `input`, assuming utf-8 encoding.
pub fn run(input:impl AsRef<str>) -> LexingResult<token::Stream> {
    let mut lexer = EnsoLexer::new();
    let reader    = Reader::new(input.as_ref().as_bytes(),DecoderUTF8());
    lexer.run(reader)
}
