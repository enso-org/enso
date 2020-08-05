//! This module exposes the definition of the Enso lexer.

use flexer::FlexerTemp;



// ========================
// === Lexer Definition ===
// ========================

/// The definition of enso lexer that is responsible for lexing the enso source code.
///
/// It chunks the character stream into a (structured) token stream in order to make later
/// processing faster, and to identify blocks.
#[derive(Debug,Clone,Copy)]
pub struct Lexer {}

impl FlexerTemp for Lexer {
    fn new() -> Self {
        Lexer{}
    }
}
