#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This file contains tests for the Enso Lexer.

// TODO [AA] Tests for error scenarios once it's done.

use flexer::*;
use lexer_definition::library::*;

use flexer::prelude::reader::decoder::DecoderUTF8;
use flexer::prelude::Reader;
use lexer::generated::engine::EnsoLexer;
use lexer_definition::library::token::Token;
use lexer_definition::token::BlockType;
use lexer_definition::token::LineEnding;



// =================
// === Utilities ===
// =================

/// Assert that `result` is a success with tokens `expected`.
fn assert_succeeds_as(result:&LexingResult<token::Stream>, expected:token::Stream) {
    match result.kind {
        ResultKind::Success => assert_eq!(result.tokens,expected),
        _                   => panic!("Lexing failed.")
    }
}

/// Assert that the provided input lexes as `expected`.
fn assert_lexes(input:impl AsRef<str>, expected:token::Stream) {
    let input_len = input.as_ref().chars().count();
    let result    = lex(input);
    assert_succeeds_as(&result,expected);
    let tokens_vec   : Vec<_> = result.tokens.into();
    let total_length : usize  = tokens_vec.iter().map(|token| token.offset + token.length).sum();
    assert_eq!(total_length,input_len);
}

/// Lex the provided string.
fn lex(input:impl AsRef<str>) -> LexingResult<token::Stream> {
    let mut lexer = EnsoLexer::new();
    let reader    = Reader::new(input.as_ref().as_bytes(),DecoderUTF8());
    lexer.run(reader)
}

/// Asserts that the input is a block and has a length equal to `length`.
fn assert_block_has_length(input:impl AsRef<str>, expected_length:usize) {
    let result = lex(input);
    match result.kind {
        ResultKind::Success => {
            let tokens = result.tokens.tokens();
            match tokens.first().expect("Token should be present.") {
                Token{shape:token::Shape::Block{..},length,..} =>
                    assert_eq!(*length,expected_length),
                _ => panic!("Token not a block."),
            }
        },
        _ => panic!("Lexing failed"),
    }
}

/// Makes the test text have unix line endings to ensure consistency regardless of git checkout
/// style.
fn make_unix_line_endings(input:&str) -> String {
    let string = String::from(input);
    string.chars().filter(|c| *c != '\r').collect()
}



// =================
// === Operators ===
// =================

#[test]
fn function_operator() {
    let input    = "->";
    let expected = token::Stream::from(vec![Token::Operator("->",0)]);
    assert_lexes(input,expected);
}

#[test]
fn bind_operator() {
    let input    = "<-";
    let expected = token::Stream::from(vec![Token::Operator("<-",0)]);
    assert_lexes(input,expected);
}

#[test]
fn left_pipe_operator() {
    let input    = "<|";
    let expected = token::Stream::from(vec![Token::Operator("<|",0)]);
    assert_lexes(input,expected);
}

#[test]
fn right_pipe_operator() {
    let input    = "|>";
    let expected = token::Stream::from(vec![Token::Operator("|>",0)]);
    assert_lexes(input,expected);
}

#[test]
fn eq_operator() {
    let input    = "=";
    let expected = token::Stream::from(vec![Token::Operator("=",0)]);
    assert_lexes(input,expected);
}

#[test]
fn eq_compare_operator() {
    let input    = "==";
    let expected = token::Stream::from(vec![Token::Operator("==",0)]);
    assert_lexes(input,expected);
}

#[test]
fn geq_operator() {
    let input    = ">=";
    let expected = token::Stream::from(vec![Token::Operator(">=",0)]);
    assert_lexes(input,expected);
}

#[test]
fn neq_operator() {
    let input    = "!=";
    let expected = token::Stream::from(vec![Token::Operator("!=",0)]);
    assert_lexes(input,expected);
}

#[test]
fn dot_operator() {
    let input    = ".";
    let expected = token::Stream::from(vec![Token::Operator(".",0)]);
    assert_lexes(input,expected);
}

#[test]
fn comma_operator() {
    let input    = ",";
    let expected = token::Stream::from(vec![Token::Operator(",",0)]);
    assert_lexes(input,expected);
}

#[test]
fn double_dot_operator() {
    let input    = "..";
    let expected = token::Stream::from(vec![Token::Operator("..",0)]);
    assert_lexes(input,expected);
}

#[test]
fn triple_dot_operator() {
    let input    = "...";
    let expected = token::Stream::from(vec![Token::Operator("...",0)]);
    assert_lexes(input,expected);
}

#[test]
fn error_operator() {
    let input    = "!";
    let expected = token::Stream::from(vec![Token::Operator("!",0)]);
    assert_lexes(input,expected);
}

#[test]
fn type_ascription_operator() {
    let input    = ":";
    let expected = token::Stream::from(vec![Token::Operator(":",0)]);
    assert_lexes(input,expected);
}

#[test]
fn in_operator() {
    let input    = "in";
    let expected = token::Stream::from(vec![Token::Operator("in",0)]);
    assert_lexes(input,expected);
}

#[test]
fn typeset_union_operator() {
    let input    = "|";
    let expected = token::Stream::from(vec![Token::Operator("|",0)]);
    assert_lexes(input,expected);
}

#[test]
fn typeset_intersection_operator() {
    let input    = "&";
    let expected = token::Stream::from(vec![Token::Operator("&",0)]);
    assert_lexes(input,expected);
}

#[test]
fn typeset_subtraction_operator() {
    let input    = "\\";
    let expected = token::Stream::from(vec![Token::Operator("\\",0)]);
    assert_lexes(input,expected);
}

#[test]
fn disable_comment() {
    let input    = "#";
    let expected = token::Stream::from(vec![Token::Operator("#",0)]);
    assert_lexes(input,expected);
}

#[test]
fn doc_comment() {
    let input    = "##";
    let expected = token::Stream::from(vec![Token::Operator("##",0)]);
    assert_lexes(input,expected);
}

#[test]
fn arbitrary_left_operator() {
    let input    = "<!!-";
    let expected = token::Stream::from(vec![Token::Operator("<!!-",0)]);
    assert_lexes(input,expected);
}

#[test]
fn arbitrary_right_operator() {
    let input    = "-->>";
    let expected = token::Stream::from(vec![Token::Operator("-->>",0)]);
    assert_lexes(input,expected);
}

#[test]
fn modifier_plus() {
    let input    = "+=";
    let expected = token::Stream::from(vec![Token::Modifier("+",0)]);
    assert_lexes(input,expected);
}

#[test]
fn modifier_minus() {
    let input    = "-=";
    let expected = token::Stream::from(vec![Token::Modifier("-",0)]);
    assert_lexes(input,expected);
}

#[test]
fn arbitrary_modifier() {
    let input    = "<%=";
    let expected = token::Stream::from(vec![Token::Modifier("<%",0)]);
    assert_lexes(input,expected);
}

#[test]
fn invalid_eq_suffix() {
    let input    = "===";
    let expected = token::Stream::from(vec![Token::Operator("==",0),Token::InvalidSuffix("=",0)]);
    assert_lexes(input,expected);
}

#[test]
fn invalid_dots_suffix() {
    let input    = "....";
    let expected = token::Stream::from(vec![Token::Operator("...",0),Token::InvalidSuffix(".",0)]);
    assert_lexes(input,expected);
}

#[test]
fn invalid_modifier_suffix() {
    let input    = "+==";
    let expected = token::Stream::from(vec![Token::Operator("+",0),Token::InvalidSuffix("==",0)]);
    assert_lexes(input,expected);
}



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
    let input    = "some_var`";
    let expected = token::Stream::from(vec![
        Token::Variable("some_var",0),
        Token::Unrecognized("`",0),
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



// ===============
// === Numbers ===
// ===============

#[test]
fn integer() {
    let input    = "13831";
    let expected = token::Stream::from(vec![Token::Number("","13831",0)]);
    assert_lexes(input,expected);
}

#[test]
fn integer_with_explicit_base() {
    let input    = "10_13831";
    let expected = token::Stream::from(vec![Token::Number("10","13831",0)]);
    assert_lexes(input,expected);
}

#[test]
fn dangling_base() {
    let input    = "10_";
    let expected = token::Stream::from(vec![Token::DanglingBase("10",0)]);
    assert_lexes(input,expected);
}

#[test]
fn hex_number() {
    let input    = "16_ff";
    let expected = token::Stream::from(vec![Token::Number("16","ff",0)]);
    assert_lexes(input,expected);
}

#[test]
fn decimal() {
    let input    = "2.71828";
    let expected = token::Stream::from(vec![Token::Number("","2.71828",0)]);
    assert_lexes(input,expected);
}

#[test]
fn decimal_with_explicit_base() {
    let input    = "10_2.71828";
    let expected = token::Stream::from(vec![Token::Number("10","2.71828",0)]);
    assert_lexes(input,expected);
}

#[test]
fn error_base() {
    let input    = "10.2_2";
    let expected = token::Stream::from(vec![
        Token::Number("","10.2",0),
        Token::InvalidSuffix("_2",0),
    ]);
    assert_lexes(input,expected);
}

#[test]
fn offset_number() {
    let input    = "    10.2";
    let expected = token::Stream::from(vec![
        Token::Number("","10.2",4),
    ]);
    assert_lexes(input,expected);
}



// ============
// === Text ===
// ============



// ==============
// === Blocks ===
// ==============

#[test]
fn block_function_call() {
    let input = make_unix_line_endings(
r#"f
    argument_1
    argument_2
    fn a1 a2 a3
    argument_4
    argument_5"#);
    let block_fn_args =
        Token::Block(
            BlockType::Continuous,
            4,
            vec![
                Token::Line(
                    vec![Token::Variable("argument_1",0)],
                    0,
                    LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Variable("argument_2",0),
                    ],
                    0,
                    LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Variable("fn",0),
                        Token::Variable("a1",1),
                        Token::Variable("a2",1),
                        Token::Variable("a3",1),
                    ],
                    0,
                    LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Variable("argument_4",0),
                    ],
                    0,
                    LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Variable("argument_5",0),
                    ],
                    0,
                    LineEnding::None
                ),
            ],
            0
        );
    let top_level_first_line = Token::Line(
        vec![
            Token::Variable("f",0),
            block_fn_args
        ],
        0,
        LineEnding::LF
    );
    let top_level_block = token::Stream::from(vec![
        Token::Block(
            BlockType::Continuous,
            0,
            vec![top_level_first_line],
            0
        )
    ]);
    assert_lexes(input,top_level_block);
}


#[test]
fn block_empty_lines() {
    let input        = "f\r\n    a\n\n    b\n";
    let nested_block = Token::Block(
        BlockType::Continuous,
        4,
        vec![
            Token::Line(vec![Token::Variable("a",0)],0,LineEnding::LF),
            Token::BlankLine(0,LineEnding::LF),
            Token::Line(vec![Token::Variable("b",0)],0,LineEnding::LF),
        ],
        0
    );
    let top_line = Token::Line(
        vec![
            Token::Variable("f",0),
            nested_block
        ],
        0,
        LineEnding::CRLF
    );
    let expected = token::Stream::from(vec![
        Token::Block(
            BlockType::Continuous,
            0,
            vec![top_line],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn block_top_level() {
    let input = make_unix_line_endings(
r#"

foo
bar
baz
"#);
    let expected = token::Stream::from(vec![
        Token::Block(
            BlockType::Continuous,
            0,
            vec![
                Token::BlankLine(0,LineEnding::LF),
                Token::BlankLine(0,LineEnding::LF),
                Token::Line(vec![Token::Variable("foo",0)],0,LineEnding::LF),
                Token::Line(vec![Token::Variable("bar",0)],0,LineEnding::LF),
                Token::Line(vec![Token::Variable("baz",0)],0,LineEnding::LF),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn block_with_operator() {
    let input = make_unix_line_endings(
r#"x ->
    foo x 1
"#);
    let nested_block = Token::Block(
        BlockType::Discontinuous,
        4,
        vec![
            Token::Line(vec![
                Token::Variable("foo",0),
                Token::Variable("x",1),
                Token::Number("","1",1),
            ], 0, LineEnding::LF)
        ],
        0
    );
    let expected = token::Stream::from(vec![
        Token::Block(
            BlockType::Continuous,
            0,
            vec![
                Token::Line(vec![
                    Token::Variable("x",0),
                    Token::Operator("->",1),
                    nested_block
                ], 0, LineEnding::LF)
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn block_with_nesting() {
    let input = make_unix_line_endings(r#"
some_long_thing
    foo ->
        Bar
        baz

    quux
"#);
    let function_block = Token::Block(
        BlockType::Discontinuous,
        8,
        vec![
            Token::Line(vec![Token::Referent("Bar",0)],0,LineEnding::LF),
            Token::Line(vec![Token::Variable("baz",0)],0,LineEnding::LF),
            Token::BlankLine(0,LineEnding::LF),
        ],
        0
    );
    let foo_block = Token::Block(
        BlockType::Continuous,
        4,
        vec![
            Token::Line(vec![
                Token::Variable("foo",0),
                Token::Operator("->",1),
                function_block,
            ], 0, LineEnding::LF),
            Token::Line(vec![Token::Variable("quux",0)],0,LineEnding::LF),
        ],
        0
    );
    let expected = token::Stream::from(vec![
        Token::Block(
            BlockType::Continuous,
            0,
            vec![
                Token::BlankLine(0,LineEnding::LF),
                Token::Line(vec![
                    Token::Variable("some_long_thing",0),
                    foo_block
                ], 0, LineEnding::LF),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn block_extra_indented_blank_lines() {
    let input          = "a\n    b\n        \n  \n    c";
    let indented_block = Token::Block(
        BlockType::Continuous,
        4,
        vec![
            Token::Line(vec![Token::Variable("b",0)],0,LineEnding::LF),
            Token::BlankLine(8,LineEnding::LF),
            Token::BlankLine(2,LineEnding::LF),
            Token::Line(vec![Token::Variable("c",0)],0,LineEnding::None),
        ],
        0
    );
    let top_level_line = Token::Line(vec![
        Token::Variable("a",0),
        indented_block
    ],0,LineEnding::LF);
    let expected = token::Stream::from(vec![
        Token::Block(
            BlockType::Continuous,
            0,
            vec![top_level_line],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn block_length_unix() {
    let input = "a\n    b\n    c";
    assert_block_has_length(input,13);
}

#[test]
fn block_length_windows() {
    let input = "a\r\n    b\r\n    c";
    assert_block_has_length(input,15);
}

#[test]
fn block_length_mixed() {
    let input = "a\r\n    b\n    c\n    d";
    assert_block_has_length(input,20);
}



// ================
// === Combined ===
// ================
