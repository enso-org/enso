//! Utilities for testing the Enso lexer.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(dead_code)]

use enso_flexer::*;
use lexer_definition::library::*;

use lexer_definition::library::token::Token;



// =================
// === Utilities ===
// =================

/// Assert that `result` is a success with tokens `expected`.
pub fn assert_succeeds_as(result: &LexingResult<token::Stream>, expected: token::Stream) {
    match result.kind {
        ResultKind::Success => assert_eq!(result.tokens, expected),
        _ => panic!("Lexing failed."),
    }
}

/// Assert that the provided input lexes as `expected`.
pub fn assert_lexes(input: impl AsRef<str>, expected: token::Stream) {
    let input_len = input.as_ref().chars().count();
    let result = lex(input);
    assert_succeeds_as(&result, expected);
    let tokens_vec: Vec<_> = result.tokens.into();
    let total_length: usize = tokens_vec.iter().map(|token| token.offset + token.length).sum();
    assert_eq!(total_length, input_len);
}

/// Lex the provided string.
pub fn lex(input: impl AsRef<str>) -> LexingResult<token::Stream> {
    lexer::run(input)
}

/// Asserts that the input is a block and has a length equal to `length`.
pub fn assert_block_has_length(input: impl AsRef<str>, expected_length: usize) {
    let result = lex(input);
    match result.kind {
        ResultKind::Success => {
            let tokens = result.tokens.tokens();
            match tokens.first().expect("Token should be present.") {
                Token { shape: token::Shape::Block { .. }, length, .. } =>
                    assert_eq!(*length, expected_length),
                _ => panic!("Token not a block."),
            }
        }
        _ => panic!("Lexing failed"),
    }
}

/// Makes the test text have unix line endings to ensure consistency regardless of git checkout
/// style.
pub fn make_unix_line_endings(input: &str) -> String {
    let string = String::from(input);
    string.chars().filter(|c| *c != '\r').collect()
}
