//! This file contains tests for lexing operators in the Enso lexer.

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



mod test_utils;

use lexer_definition::library::*;
use test_utils::*;

use lexer_definition::library::token::Token;



// =================
// === Operators ===
// =================

#[test]
fn function_operator() {
    let input = "->";
    let expected = token::Stream::from(vec![Token::operator("->", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn bind_operator() {
    let input = "<-";
    let expected = token::Stream::from(vec![Token::operator("<-", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn left_pipe_operator() {
    let input = "<|";
    let expected = token::Stream::from(vec![Token::operator("<|", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn right_pipe_operator() {
    let input = "|>";
    let expected = token::Stream::from(vec![Token::operator("|>", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn eq_operator() {
    let input = "=";
    let expected = token::Stream::from(vec![Token::operator("=", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn eq_compare_operator() {
    let input = "==";
    let expected = token::Stream::from(vec![Token::operator("==", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn geq_operator() {
    let input = ">=";
    let expected = token::Stream::from(vec![Token::operator(">=", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn neq_operator() {
    let input = "!=";
    let expected = token::Stream::from(vec![Token::operator("!=", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn dot_operator() {
    let input = ".";
    let expected = token::Stream::from(vec![Token::operator(".", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn comma_operator() {
    let input = ",";
    let expected = token::Stream::from(vec![Token::operator(",", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn double_dot_operator() {
    let input = "..";
    let expected = token::Stream::from(vec![Token::operator("..", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn triple_dot_operator() {
    let input = "...";
    let expected = token::Stream::from(vec![Token::operator("...", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn error_operator() {
    let input = "!";
    let expected = token::Stream::from(vec![Token::operator("!", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn type_ascription_operator() {
    let input = ":";
    let expected = token::Stream::from(vec![Token::operator(":", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn in_operator() {
    let input = "in";
    let expected = token::Stream::from(vec![Token::operator("in", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn typeset_union_operator() {
    let input = "|";
    let expected = token::Stream::from(vec![Token::operator("|", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn typeset_intersection_operator() {
    let input = "&";
    let expected = token::Stream::from(vec![Token::operator("&", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn typeset_subtraction_operator() {
    let input = "\\";
    let expected = token::Stream::from(vec![Token::operator("\\", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn arbitrary_left_operator() {
    let input = "<!!-";
    let expected = token::Stream::from(vec![Token::operator("<!!-", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn arbitrary_right_operator() {
    let input = "-->>";
    let expected = token::Stream::from(vec![Token::operator("-->>", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn modifier_plus() {
    let input = "+=";
    let expected = token::Stream::from(vec![Token::modifier("+", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn modifier_minus() {
    let input = "-=";
    let expected = token::Stream::from(vec![Token::modifier("-", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn arbitrary_modifier() {
    let input = "<%=";
    let expected = token::Stream::from(vec![Token::modifier("<%", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn invalid_eq_suffix() {
    let input = "===";
    let expected =
        token::Stream::from(vec![Token::operator("==", 0), Token::invalid_suffix("=", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn invalid_dots_suffix() {
    let input = "....";
    let expected =
        token::Stream::from(vec![Token::operator("...", 0), Token::invalid_suffix(".", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn invalid_modifier_suffix() {
    let input = "+==";
    let expected =
        token::Stream::from(vec![Token::operator("+", 0), Token::invalid_suffix("==", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn dot_call_operator() {
    let input = ".+ .<*>";
    let expected = token::Stream::from(vec![
        Token::operator(".", 0),
        Token::operator("+", 0),
        Token::operator(".", 1),
        Token::operator("<*>", 0),
    ]);
    assert_lexes(input, expected)
}

#[test]
fn dot_eq_operator() {
    let input = ".== .  !=";
    let expected = token::Stream::from(vec![
        Token::operator(".", 0),
        Token::operator("==", 0),
        Token::operator(".", 1),
        Token::operator("!=", 2),
    ]);
    assert_lexes(input, expected);
}
