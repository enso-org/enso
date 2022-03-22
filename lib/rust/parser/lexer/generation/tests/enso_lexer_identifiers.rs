//! This file contains tests for lexing identifiers in the Enso lexer.

// === Features ===
#![feature(test)]
// === Non-Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![deny(unconditional_recursion)]
#![warn(unsafe_code)]
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



// ===================
// === Identifiers ===
// ===================

#[test]
fn variable_ident() {
    let input = "some_variable_name";
    let expected = token::Stream::from(vec![Token::variable("some_variable_name", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn referent_ident() {
    let input = "Some_Referent_Name";
    let expected = token::Stream::from(vec![Token::referent("Some_Referent_Name", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn external_ident() {
    let input = "__camelCaseIdentifier";
    let expected = token::Stream::from(vec![Token::external("__camelCaseIdentifier", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn blank_ident() {
    let input = "_";
    let expected = token::Stream::from(vec![Token::blank(0)]);
    assert_lexes(input, expected)
}

#[test]
fn annotation() {
    let input = "@debug";
    let expected = token::Stream::from(vec![Token::annotation("debug", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn ticked_variable_ident() {
    let input = "some_variable_name'";
    let expected = token::Stream::from(vec![Token::variable("some_variable_name'", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn ticked_referent_ident() {
    let input = "Some_Referent_Name'";
    let expected = token::Stream::from(vec![Token::referent("Some_Referent_Name'", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn ticked_annotation() {
    let input = "@debug'";
    let expected = token::Stream::from(vec![Token::annotation("debug'", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn multi_ticked_variable_ident() {
    let input = "some_variable_name'''";
    let expected = token::Stream::from(vec![Token::variable("some_variable_name'''", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn multi_ticked_referent_ident() {
    let input = "Some_Referent_Name'''";
    let expected = token::Stream::from(vec![Token::referent("Some_Referent_Name'''", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn multi_ticked_annotation() {
    let input = "@debug''";
    let expected = token::Stream::from(vec![Token::annotation("debug''", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn variable_with_numbers() {
    let input = "some0_1";
    let expected = token::Stream::from(vec![Token::variable("some0_1", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn referent_with_numbers() {
    let input = "Some_1821";
    let expected = token::Stream::from(vec![Token::referent("Some_1821", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn annotation_with_numbers() {
    let input = "@debug_1";
    let expected = token::Stream::from(vec![Token::annotation("debug_1", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn tick_not_at_end_variable() {
    let input = "some_var'iable";
    let expected = token::Stream::from(vec![
        Token::variable("some_var'", 0),
        Token::invalid_suffix("iable", 0),
    ]);
    assert_lexes(input, expected)
}

#[test]
fn trailing_underscore() {
    let input = "some_var_";
    let expected = token::Stream::from(vec![Token::external("some_var_", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn trailing_underscore_with_tick() {
    let input = "some_var_'";
    let expected = token::Stream::from(vec![Token::external("some_var_'", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn invalid_suffix() {
    let input = "some_varД";
    let expected =
        token::Stream::from(vec![Token::variable("some_var", 0), Token::invalid_suffix("Д", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn unrecognized_token() {
    let input = "some_var@";
    let expected =
        token::Stream::from(vec![Token::variable("some_var", 0), Token::unrecognized("@", 0)]);
    assert_lexes(input, expected)
}

#[test]
fn chained_identifiers() {
    let input = "my_func A' someJavaValue some_python_value";
    let expected = token::Stream::from(vec![
        Token::variable("my_func", 0),
        Token::referent("A'", 1),
        Token::external("someJavaValue", 1),
        Token::variable("some_python_value", 1),
    ]);
    assert_lexes(input, expected)
}
