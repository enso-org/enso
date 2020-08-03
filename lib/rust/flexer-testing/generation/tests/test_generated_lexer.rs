#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

use flexer::prelude::*;

use flexer::prelude::reader::decoder::DecoderUTF8;
use flexer_test_generation::engine::TestLexer;
use flexer_test_generation::engine::Token;
use flexer_test_generation::engine::TokenStream;



// =============
// === Tests ===
// =============

/// Executes the test on the provided input string slice.
fn run_test_on(str:impl AsRef<str>) -> TokenStream {
    // Hardcoded for ease of use here.
    let reader     = Reader::new(str.as_ref().as_bytes(), DecoderUTF8());
    let mut lexer  = TestLexer::new();
    let run_result = lexer.run(reader);

    match run_result.kind {
        flexer::ResultKind::Success => run_result.tokens,
        _                           => default()
    }
}

#[test]
fn test_single_a_word() {
    let input           = "aaaaa";
    let expected_output = TokenStream::from(vec![Token::Word(String::from(input))]);
    let result          = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_single_b_word() {
    let input           = "bbbbb";
    let expected_output = TokenStream::from(vec![Token::Word(String::from(input))]);
    let result          = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_two_word() {
    let input           = "aaaaa bbbbb";
    let expected_output = TokenStream::from(
        vec![Token::Word(String::from("aaaaa")), Token::Word(String::from("bbbbb"))]
    );
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_multi_word() {
    let input           = "bbb aa a b bbbbb aa";
    let expected_output = TokenStream::from(vec![
        Token::Word(String::from("bbb")),
        Token::Word(String::from("aa")),
        Token::Word(String::from("a")),
        Token::Word(String::from("b")),
        Token::Word(String::from("bbbbb")),
        Token::Word(String::from("aa"))
    ]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_invalid_single_word() {
    let input           = "c";
    let expected_output = TokenStream::from(vec![Token::Unrecognized(String::from(input))]);
    let result          = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_multi_word_invalid() {
    let input           = "aaaaaa c bbbbbb";
    let expected_output = TokenStream::from(vec![
        Token::Word(String::from("aaaaaa")),
        Token::Unrecognized(String::from(" ")),
        Token::Unrecognized(String::from("c")),
        Token::Unrecognized(String::from(" ")),
        Token::Word(String::from("bbbbbb")),
    ]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}

#[test]
fn test_end_invalid() {
    let input           = "bbbbbb c";
    let expected_output = TokenStream::from(vec![
        Token::Word(String::from("bbbbbb")),
        Token::Unrecognized(String::from(" ")),
        Token::Unrecognized(String::from("c")),
    ]);
    let result = run_test_on(input);
    assert_eq!(result, expected_output);
}
