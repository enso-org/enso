#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This file contains tests for lexing identifiers in the Enso lexer.

mod test_utils;

use lexer_definition::library::*;
use test_utils::*;

use lexer_definition::library::token::Token;



// ===================
// === Identifiers ===
// ===================

#[test]
fn variable_ident() {
    let input    = "some_variable_name";
    let expected = token::Stream::from(vec![Token::Variable("some_variable_name",0)]);
    assert_lexes(input,expected)
}

#[test]
fn referent_ident() {
    let input    = "Some_Referent_Name";
    let expected = token::Stream::from(vec![Token::Referent("Some_Referent_Name",0)]);
    assert_lexes(input,expected)
}

#[test]
fn external_ident() {
    let input    = "__camelCaseIdentifier";
    let expected = token::Stream::from(vec![Token::External("__camelCaseIdentifier",0)]);
    assert_lexes(input,expected)
}

#[test]
fn blank_ident() {
    let input    = "_";
    let expected = token::Stream::from(vec![Token::Blank(0)]);
    assert_lexes(input,expected)
}

#[test]
fn annotation() {
    let input    = "@debug";
    let expected = token::Stream::from(vec![Token::Annotation("debug",0)]);
    assert_lexes(input,expected);
}

#[test]
fn ticked_variable_ident() {
    let input    = "some_variable_name'";
    let expected = token::Stream::from(vec![Token::Variable("some_variable_name'",0)]);
    assert_lexes(input,expected)
}

#[test]
fn ticked_referent_ident() {
    let input    = "Some_Referent_Name'";
    let expected = token::Stream::from(vec![Token::Referent("Some_Referent_Name'",0)]);
    assert_lexes(input,expected)
}

#[test]
fn ticked_annotation() {
    let input    = "@debug'";
    let expected = token::Stream::from(vec![Token::Annotation("debug'",0)]);
    assert_lexes(input,expected);
}

#[test]
fn multi_ticked_variable_ident() {
    let input    = "some_variable_name'''";
    let expected = token::Stream::from(vec![Token::Variable("some_variable_name'''",0)]);
    assert_lexes(input,expected)
}

#[test]
fn multi_ticked_referent_ident() {
    let input    = "Some_Referent_Name'''";
    let expected = token::Stream::from(vec![Token::Referent("Some_Referent_Name'''",0)]);
    assert_lexes(input,expected)
}

#[test]
fn multi_ticked_annotation() {
    let input    = "@debug''";
    let expected = token::Stream::from(vec![Token::Annotation("debug''",0)]);
    assert_lexes(input,expected);
}

#[test]
fn variable_with_numbers() {
    let input    = "some0_1";
    let expected = token::Stream::from(vec![Token::Variable("some0_1",0)]);
    assert_lexes(input,expected)
}

#[test]
fn referent_with_numbers() {
    let input    = "Some_1821";
    let expected = token::Stream::from(vec![Token::Referent("Some_1821",0)]);
    assert_lexes(input,expected)
}

#[test]
fn annotation_with_numbers() {
    let input    = "@debug_1";
    let expected = token::Stream::from(vec![Token::Annotation("debug_1",0)]);
    assert_lexes(input,expected);
}

#[test]
fn tick_not_at_end_variable() {
    let input    = "some_var'iable";
    let expected = token::Stream::from(vec![
        Token::Variable("some_var'",0),
        Token::InvalidSuffix("iable",0),
    ]);
    assert_lexes(input,expected)
}

#[test]
fn trailing_underscore() {
    let input    = "some_var_";
    let expected = token::Stream::from(vec![Token::External("some_var_",0)]);
    assert_lexes(input,expected)
}

#[test]
fn trailing_underscore_with_tick() {
    let input    = "some_var_'";
    let expected = token::Stream::from(vec![Token::External("some_var_'",0)]);
    assert_lexes(input,expected)
}

#[test]
fn invalid_suffix() {
    let input    = "some_varД";
    let expected = token::Stream::from(vec![
        Token::Variable("some_var",0),
        Token::InvalidSuffix("Д",0),
    ]);
    assert_lexes(input,expected)
}

#[test]
fn unrecognized_token() {
    let input    = "some_var@";
    let expected = token::Stream::from(vec![
        Token::Variable("some_var",0),
        Token::Unrecognized("@",0),
    ]);
    assert_lexes(input,expected)
}

#[test]
fn chained_identifiers() {
    let input    = "my_func A' someJavaValue some_python_value";
    let expected = token::Stream::from(vec![
        Token::Variable("my_func",0),
        Token::Referent("A'",1),
        Token::External("someJavaValue",1),
        Token::Variable("some_python_value",1),
    ]);
    assert_lexes(input,expected)
}
