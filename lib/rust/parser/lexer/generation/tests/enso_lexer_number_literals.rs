//! This file contains tests for lexing number literals in the Enso lexer.

// === Features ===
#![feature(test)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]



mod test_utils;

use lexer_definition::library::*;
use test_utils::*;

use lexer_definition::library::token::Token;



// ===============
// === Numbers ===
// ===============

#[test]
fn integer() {
    let input = "13831";
    let expected = token::Stream::from(vec![Token::number("", "13831", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn integer_with_explicit_base() {
    let input = "10_13831";
    let expected = token::Stream::from(vec![Token::number("10", "13831", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn dangling_base() {
    let input = "10_";
    let expected = token::Stream::from(vec![Token::dangling_base("10", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn hex_number() {
    let input = "16_ff";
    let expected = token::Stream::from(vec![Token::number("16", "ff", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn decimal() {
    let input = "2.71828";
    let expected = token::Stream::from(vec![Token::number("", "2.71828", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn decimal_with_explicit_base() {
    let input = "10_2.71828";
    let expected = token::Stream::from(vec![Token::number("10", "2.71828", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn error_base() {
    let input = "10.2_2";
    let expected =
        token::Stream::from(vec![Token::number("", "10.2", 0), Token::invalid_suffix("_2", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn offset_number() {
    let input = "    10.2";
    let expected = token::Stream::from(vec![Token::number("", "10.2", 4)]);
    assert_lexes(input, expected);
}
