#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This file contains tests for lexing blocks in the Enso lexer.

mod test_utils;

use lexer_definition::library::*;
use test_utils::*;

use lexer_definition::library::token::Token;
use lexer_definition::token::BlockType;
use lexer_definition::token::LineEnding;



// ==============
// === Blocks ===
// ==============

#[test]
fn function_call() {
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
fn empty_lines() {
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
fn top_level() {
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
fn with_operator() {
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
fn with_nesting() {
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
fn multiple_dedent() {
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
                Token::Line(vec![Token::Variable("quux",0)],0,LineEnding::LF),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn extra_indented_blank_lines() {
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
fn length_unix() {
    let input = "a\n    b\n    c";
    assert_block_has_length(input,13);
}

#[test]
fn length_windows() {
    let input = "a\r\n    b\r\n    c";
    assert_block_has_length(input,15);
}

#[test]
fn length_mixed() {
    let input = "a\r\n    b\n    c\n    d";
    assert_block_has_length(input,20);
}
