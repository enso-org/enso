//! This file contains tests for the generated lexer.

// === Features ===
#![feature(test)]

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]

// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

use enso_flexer::prelude::*;

use enso_flexer::prelude::reader::decoder::DecoderUTF8;
use flexer_test_generation::generated::engine::TestLexer;
use flexer_test_generation::generated::engine::Token;
use flexer_test_generation::generated::engine::TokenStream;



// =============
// === Tests ===
// =============

/// Executes the test on the provided input string slice.
fn run_test_on(str: impl AsRef<str>) -> TokenStream {
    // Hardcoded for ease of use here.
    let reader = Reader::new(str.as_ref().as_bytes(), DecoderUTF8());
    let mut lexer = TestLexer::new();
    let run_result = lexer.run(reader);

    match run_result.kind {
        enso_flexer::ResultKind::Success => run_result.tokens,
        _ => default(),
    }
}

#[test]
fn test_single_a_word() {
    let input = "aaaaa";
    let expected_output = TokenStream::from(vec![Token::word(input)]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_single_b_word() {
    let input = "bbbbb";
    let expected_output = TokenStream::from(vec![Token::word(input)]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_two_word() {
    let input = "aaaaa bbbbb";
    let expected_output = TokenStream::from(vec![Token::word("aaaaa"), Token::word("bbbbb")]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_multi_word() {
    let input = "bbb aa a b bbbbb aa";
    let expected_output = TokenStream::from(vec![
        Token::word("bbb"),
        Token::word("aa"),
        Token::word("a"),
        Token::word("b"),
        Token::word("bbbbb"),
        Token::word("aa"),
    ]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_invalid_single_word() {
    let input = "c";
    let expected_output = TokenStream::from(vec![Token::unrecognized(input)]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_multi_word_invalid() {
    let input = "aaaaaa c bbbbbb";
    let expected_output = TokenStream::from(vec![
        Token::word("aaaaaa"),
        Token::unrecognized(" "),
        Token::unrecognized("c"),
        Token::unrecognized(" "),
        Token::word("bbbbbb"),
    ]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_end_invalid() {
    let input = "bbbbbb c";
    let expected_output = TokenStream::from(vec![
        Token::word("bbbbbb"),
        Token::unrecognized(" "),
        Token::unrecognized("c"),
    ]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}
