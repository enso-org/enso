#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This file contains tests for lexing text literals in the Enso lexer.

mod test_utils;

use lexer_definition::library::*;
use test_utils::*;

use lexer_definition::library::token::Token;



// ============
// === Text ===
// ============

#[test]
fn invalid_interpolate_quote() {
    let input = "`";
    let expected = token::Stream::from(vec![Token::Unrecognized("`",0)]);
    assert_lexes(input,expected);
}

#[test]
fn invalid_format_quote() {
    let input    = r#"''''"#;
    let expected = token::Stream::from(vec![Token::InvalidQuote(r#"''''"#,0)]);
    assert_lexes(input,expected);
}

#[test]
fn single_line_format_text() {
    let input    = r#"'dearest creature in creation, studying english pronunciation'"#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw(
                    "dearest creature in creation, studying english pronunciation"
                    ,0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn single_line_format_with_one_interpolation() {
    let input    = "'The result is `result.pretty`!'";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("The result is ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Variable("result",0),
                        Token::Operator(".",0),
                        Token::Variable("pretty",0),
                    ],
                    0
                ),
                Token::TextSegmentRaw("!",0),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn single_line_format_with_multiple_interpolations() {
    let input    = "'My_Type: name=`self.name`, suspended=`self.suspended`'";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("My_Type: name=",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Variable("self",0),
                        Token::Operator(".",0),
                        Token::Variable("name",0),
                    ],
                    0
                ),
                Token::TextSegmentRaw(", suspended=",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Variable("self",0),
                        Token::Operator(".",0),
                        Token::Variable("suspended",0),
                    ],
                    0
                ),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn block_format_text() {
    let input = make_unix_line_endings(
r#"'''
    I have a format text block literal.
    It may optionally contain `interpolations` interspersed with the text like `this`.

    And it ends when the indent of `indent` goes back.
"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("I have a format text block literal.",0)
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("It may optionally contain ",0),
                        Token::TextSegmentInterpolate(
                            vec![Token::Variable("interpolations",0)],
                            0
                        ),
                        Token::TextSegmentRaw(" interspersed with the text like ",0),
                        Token::TextSegmentInterpolate(
                            vec![Token::Variable("this",0)],
                            0
                        ),
                        Token::TextSegmentRaw(".",0),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("And it ends when the indent of ",0),
                        Token::TextSegmentInterpolate(vec![Token::Variable("indent",0)],0),
                        Token::TextSegmentRaw(" goes back.",0)
                    ],
                    0,
                    token::LineEnding::LF
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block() {
    let input    = "'''foo bar `interp` 'baz";
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("foo bar ",0),
                Token::TextSegmentInterpolate(vec![Token::Variable("interp",0)],0),
                Token::TextSegmentRaw(" 'baz",0),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_line_escape_test() {
    let input = "'\\'\\U00131313 = \\u{z2}\\u{AFD3} `a + b` \\U23232323\\uAA\\uAAAA a b c \\xAF'";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentEscape(token::EscapeStyle::Literal,"'",0),
                Token::TextSegmentEscape(token::EscapeStyle::U32,"00131313",0),
                Token::TextSegmentRaw(" = ",0),
                Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\u{z2}",0),
                Token::TextSegmentEscape(token::EscapeStyle::U21,"AFD3",0),
                Token::TextSegmentRaw(" ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Variable("a",0),
                        Token::Operator("+",1),
                        Token::Variable("b",1)
                    ],
                    0
                ),
                Token::TextSegmentRaw(" ",0),
                Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\U23232323",0),
                Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\uAA",0),
                Token::TextSegmentEscape(token::EscapeStyle::U16,"AAAA",0),
                Token::TextSegmentRaw(" a b c ",0),
                Token::TextSegmentEscape(token::EscapeStyle::Byte,"AF",0),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_line_unfinished_escape() {
    let input    = "'\\";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::UnclosedLine,
            vec![Token::TextSegmentEscape(token::EscapeStyle::Invalid,"\\",0)],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_escape_sequences() {
    let input    = "'''\\U00131313 =' \\u{z2}\\u{AFD3} `a + b` \\U23232323\\uAA\\uAAAA a b c \\xAF";
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentEscape(token::EscapeStyle::U32,"00131313",0),
                Token::TextSegmentRaw(" =' ",0),
                Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\u{z2}",0),
                Token::TextSegmentEscape(token::EscapeStyle::U21,"AFD3",0),
                Token::TextSegmentRaw(" ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Variable("a",0),
                        Token::Operator("+",1),
                        Token::Variable("b",1)
                    ],
                    0
                ),
                Token::TextSegmentRaw(" ",0),
                Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\U23232323",0),
                Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\uAA",0),
                Token::TextSegmentEscape(token::EscapeStyle::U16,"AAAA",0),
                Token::TextSegmentRaw(" a b c ",0),
                Token::TextSegmentEscape(token::EscapeStyle::Byte,"AF",0),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_escape_sequences() {
    let input    = make_unix_line_endings(
r#"'''
    \U00131313 =' \u{z2}\u{AFD3} `a + b`\`
    \U23232323\uAA\uAAAA a b c \xAF\n
    \'
"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentEscape(token::EscapeStyle::U32,"00131313",0),
                        Token::TextSegmentRaw(" =' ",0),
                        Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\u{z2}",0),
                        Token::TextSegmentEscape(token::EscapeStyle::U21,"AFD3",0),
                        Token::TextSegmentRaw(" ",0),
                        Token::TextSegmentInterpolate(
                            vec![
                                Token::Variable("a",0),
                                Token::Operator("+",1),
                                Token::Variable("b",1)
                            ],
                            0
                        ),
                        Token::TextSegmentEscape(token::EscapeStyle::Literal,"`",0)
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\U23232323",0),
                        Token::TextSegmentEscape(token::EscapeStyle::InvalidUnicode,"\\uAA",0),
                        Token::TextSegmentEscape(token::EscapeStyle::U16,"AAAA",0),
                        Token::TextSegmentRaw(" a b c ",0),
                        Token::TextSegmentEscape(token::EscapeStyle::Byte,"AF",0),
                        Token::TextSegmentEscape(token::EscapeStyle::Literal,"\n",0),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::TextSegmentEscape(token::EscapeStyle::Literal,"'",0),
                    ],
                    0,
                    token::LineEnding::LF
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_line_unclosed_interpolate() {
    let input = "'Foo bar `baz'";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::UnclosedLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentUnclosedInterpolate(
                    vec![Token::Variable("baz'",0)],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_unclosed_interpolate() {
    let input    = "'''Foo bar ` baz";
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentUnclosedInterpolate(
                    vec![
                        Token::Variable("baz",1),
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_unclosed_interpolate() {
    let input = make_unix_line_endings(
r#"'''
    Here's an interpolated block.
    And here's an `unfinished interpolation
    And `another"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![Token::TextSegmentRaw("Here's an interpolated block.",0)],
                    0,
                    token::LineEnding::LF,
                ),
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("And here's an ",0),
                        Token::TextSegmentUnclosedInterpolate(
                            vec![
                                Token::Variable("unfinished",0),
                                Token::Variable("interpolation",1)
                            ],
                            0
                        ),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("And ",0),
                        Token::TextSegmentUnclosedInterpolate(
                            vec![Token::Variable("another",0)],
                            0
                        ),
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_raw_line() {
    let input    = r#"'Foo bar `"baz"`'"#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::RawLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                ),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_raw_line_unclosed() {
    let input    = r#"'Foo bar `"baz`'"#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::UnclosedLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentUnclosedInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![Token::TextSegmentRaw("baz`'",0)],
                            0
                        )
                    ],
                    0
                ),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_format_line() {
    let input    = "'Foo bar `'baz'`'";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::FormatLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                ),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_format_line_unclosed() {
    let input    = "'Foo bar `'baz`'";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                ),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_format_line_interpolate() {
    let input    = "'Foo bar `'baz``'`'";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                ),
                Token::TextSegmentInterpolate(
                    vec![Token::TextLine(
                        token::TextStyle::UnclosedLine,
                        vec![],
                        0
                    )],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_format_line_unclosed_interpolate() {
    let input    = "'Foo bar `'baz";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::UnclosedLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentUnclosedInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_raw_line_unclosed_interpolate() {
    let input    = r#"'Foo bar `"baz"'"#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::UnclosedLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentUnclosedInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::RawLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        ),
                        Token::TextLine(token::TextStyle::UnclosedLine,vec![],0)
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_format_inline_block() {
    let input    = "'Foo bar `''' a b c d`'";
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Unrecognized("'''",0),
                        Token::Variable("a",1),
                        Token::Variable("b",1),
                        Token::Variable("c",1),
                        Token::Variable("d",1),
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_raw_inline_block() {
    let input    = r#"'Foo bar `""" a b c d`'"#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Unrecognized(r#"""""#,0),
                        Token::Variable("a",1),
                        Token::Variable("b",1),
                        Token::Variable("c",1),
                        Token::Variable("d",1),
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_invalid_format_quote() {
    let input    = r#"'Foo bar `'''''`'"#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(vec![Token::InvalidQuote("'''''",0)],0)
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_invalid_raw_quote() {
    let input    = r#"'Foo bar `"""""`'"#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::FormatLine,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(vec![Token::InvalidQuote(r#"""""""#,0)],0)
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_format_block() {
    let input    = make_unix_line_endings(
r#"'Foo bar `'''
Foo`'
"#);
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::Line(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![
                                Token::TextSegmentRaw("Foo bar ",0),
                                Token::TextSegmentUnclosedInterpolate(
                                    vec![
                                        Token::Unrecognized(r#"'''"#,0)
                                    ],
                                    0
                                ),
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Referent("Foo",0),
                        Token::Unrecognized("`",0),
                        Token::TextLine(token::TextStyle::UnclosedLine, vec![],0)
                    ],
                    0,
                    token::LineEnding::LF
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn nested_raw_block() {
    let input    = make_unix_line_endings(
r#"'Foo bar `"""
Foo`'
"#);
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::Line(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![
                                Token::TextSegmentRaw("Foo bar ",0),
                                Token::TextSegmentUnclosedInterpolate(
                                    vec![
                                        Token::Unrecognized(r#"""""#,0)
                                    ],
                                    0
                                ),
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Referent("Foo",0),
                        Token::Unrecognized("`",0),
                        Token::TextLine(token::TextStyle::UnclosedLine, vec![],0)
                    ],
                    0,
                    token::LineEnding::LF
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_raw_line() {
    let input    = r#"'''Foo bar `"baz"`"#;
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::RawLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_raw_line_unclosed() {
    let input    = r#"'''Foo bar `"baz`"#;
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentUnclosedInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![Token::TextSegmentRaw("baz`",0)],
                            0
                        )
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_format_line() {
    let input    = "'''Foo bar `'baz'`";
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::FormatLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_format_line_unclosed() {
    let input    = "'''Foo bar `'baz`";
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_format_line_interpolate() {
    let input    = "'''Foo bar `'baz``'`";
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                ),
                Token::TextSegmentInterpolate(
                    vec![Token::TextLine(
                        token::TextStyle::UnclosedLine,
                        vec![],
                        0
                    )],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_format_line_unclosed_interpolate() {
    let input    = "'''Foo bar `'baz";
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentUnclosedInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::UnclosedLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        )
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_raw_line_unclosed_interpolate() {
    let input    = r#"'''Foo bar `"baz""#;
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentUnclosedInterpolate(
                    vec![
                        Token::TextLine(
                            token::TextStyle::RawLine,
                            vec![Token::TextSegmentRaw("baz",0)],
                            0
                        ),
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_format_inline_block() {
    let input    = "'''Foo bar `''' a b c d`";
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Unrecognized("'''",0),
                        Token::Variable("a",1),
                        Token::Variable("b",1),
                        Token::Variable("c",1),
                        Token::Variable("d",1),
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_raw_inline_block() {
    let input    = r#"'''Foo bar `""" a b c d`"#;
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(
                    vec![
                        Token::Unrecognized(r#"""""#,0),
                        Token::Variable("a",1),
                        Token::Variable("b",1),
                        Token::Variable("c",1),
                        Token::Variable("d",1),
                    ],
                    0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_invalid_format_quote() {
    let input    = r#"'''Foo bar `'''''`"#;
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(vec![Token::InvalidQuote("'''''",0)],0)
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_invalid_raw_quote() {
    let input    = r#"'''Foo bar `"""""`"#;
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::FormatInlineBlock,
            vec![
                Token::TextSegmentRaw("Foo bar ",0),
                Token::TextSegmentInterpolate(vec![Token::InvalidQuote(r#"""""""#,0)],0)
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_format_block() {
    let input    = make_unix_line_endings(
r#"'''Foo bar `'''
Foo`
"#);
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::Line(
                    vec![
                        Token::TextInlineBlock(
                            token::TextStyle::FormatInlineBlock,
                            vec![
                                Token::TextSegmentRaw("Foo bar ",0),
                                Token::TextSegmentUnclosedInterpolate(
                                    vec![
                                        Token::Unrecognized(r#"'''"#,0)
                                    ],
                                    0
                                ),
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Referent("Foo",0),
                        Token::Unrecognized("`",0),
                    ],
                    0,
                    token::LineEnding::LF
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_inline_block_nested_raw_block() {
    let input    = make_unix_line_endings(
r#"'''Foo bar `"""
Foo`
"#);
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::Line(
                    vec![
                        Token::TextInlineBlock(
                            token::TextStyle::FormatInlineBlock,
                            vec![
                                Token::TextSegmentRaw("Foo bar ",0),
                                Token::TextSegmentUnclosedInterpolate(
                                    vec![
                                        Token::Unrecognized(r#"""""#,0)
                                    ],
                                    0
                                ),
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Referent("Foo",0),
                        Token::Unrecognized("`",0),
                    ],
                    0,
                    token::LineEnding::LF
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_raw_line() {
    let input = make_unix_line_endings(
r#"'''
    Foo bar `"baz"`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentInterpolate(
                            vec![
                                Token::TextLine(
                                    token::TextStyle::RawLine,
                                    vec![Token::TextSegmentRaw("baz",0)],
                                    0
                                )
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_raw_line_unclosed() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `"baz`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentUnclosedInterpolate(
                            vec![
                                Token::TextLine(
                                    token::TextStyle::UnclosedLine,
                                    vec![Token::TextSegmentRaw("baz`",0)],
                                    0
                                )
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_format_line() {
    let input = make_unix_line_endings(
r#"'''
    Foo bar `'baz'`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentInterpolate(
                            vec![
                                Token::TextLine(
                                    token::TextStyle::FormatLine,
                                    vec![Token::TextSegmentRaw("baz",0)],
                                    0
                                )
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_format_line_unclosed() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `'baz`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentInterpolate(
                            vec![
                                Token::TextLine(
                                    token::TextStyle::UnclosedLine,
                                    vec![Token::TextSegmentRaw("baz",0)],
                                    0
                                )
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_format_line_interpolate() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `'baz``'`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentInterpolate(
                            vec![
                                Token::TextLine(
                                    token::TextStyle::UnclosedLine,
                                    vec![Token::TextSegmentRaw("baz",0)],
                                    0
                                )
                            ],
                            0
                        ),
                        Token::TextSegmentInterpolate(
                            vec![Token::TextLine(
                                token::TextStyle::UnclosedLine,
                                vec![],
                                0
                            )],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_format_line_unclosed_interpolate() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `'baz"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentUnclosedInterpolate(
                            vec![
                                Token::TextLine(
                                    token::TextStyle::UnclosedLine,
                                    vec![Token::TextSegmentRaw("baz",0)],
                                    0
                                )
                            ],
                            0
                        ),
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_raw_line_unclosed_interpolate() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `"baz"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentUnclosedInterpolate(
                            vec![
                                Token::TextLine(
                                    token::TextStyle::UnclosedLine,
                                    vec![Token::TextSegmentRaw("baz",0)],
                                    0
                                )
                            ],
                            0
                        ),
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_format_inline_block() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `''' a b c d`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentInterpolate(
                            vec![
                                Token::Unrecognized(r#"'''"#,0),
                                Token::Variable("a",1),
                                Token::Variable("b",1),
                                Token::Variable("c",1),
                                Token::Variable("d",1),
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_raw_inline_block() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `""" a b c d`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentInterpolate(
                            vec![
                                Token::Unrecognized(r#"""""#,0),
                                Token::Variable("a",1),
                                Token::Variable("b",1),
                                Token::Variable("c",1),
                                Token::Variable("d",1),
                            ],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_invalid_format_quote() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `'''''`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentInterpolate(
                            vec![Token::InvalidQuote(r#"'''''"#,0)],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_invalid_raw_quote() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `"""""`"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentInterpolate(
                            vec![Token::InvalidQuote(r#"""""""#,0)],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_format_block() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `'''
    Foo`
"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentUnclosedInterpolate(
                            vec![Token::Unrecognized(r#"'''"#,0)],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo",0),
                        Token::TextSegmentUnclosedInterpolate(vec![],0),
                    ],
                    0,
                    token::LineEnding::LF
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_nested_raw_block() {
    let input    = make_unix_line_endings(
r#"'''
    Foo bar `"""
    Foo`
"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo bar ",0),
                        Token::TextSegmentUnclosedInterpolate(
                            vec![Token::Unrecognized(r#"""""#,0)],
                            0
                        )
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::TextSegmentRaw("Foo",0),
                        Token::TextSegmentUnclosedInterpolate(vec![],0),
                    ],
                    0,
                    token::LineEnding::LF
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn invalid_raw_quote() {
    let input    = r#"""""""#;
    let expected = token::Stream::from(vec![Token::InvalidQuote(r#"""""""#,0)]);
    assert_lexes(input,expected);
}

#[test]
fn single_line_raw_text() {
    let input    = r#""dearest creature in creation, studying english pronunciation""#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::RawLine,
            vec![
                Token::TextSegmentRaw(
                    "dearest creature in creation, studying english pronunciation"
                    ,0
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn block_raw_text() {
    let input = make_unix_line_endings(
r#""""
    I have a raw text block literal.

    `Interpolations` are not anything special in these literals.

    And it ends when the prevailing indentation is reduced.
"#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::RawBlock,
            vec![
                Token::Line(
                    vec![Token::TextSegmentRaw("I have a raw text block literal.",0)],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![Token::TextSegmentRaw(
                        "`Interpolations` are not anything special in these literals.",
                        0
                    )],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![Token::TextSegmentRaw(
                        "And it ends when the prevailing indentation is reduced.",
                        0
                    )],
                    0,
                    token::LineEnding::LF
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn raw_inline_block() {
    let input    = r#""""foo bar `interp` 'baz"#;
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::RawInlineBlock,
            vec![Token::TextSegmentRaw("foo bar `interp` 'baz",0), ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn raw_line_escapes() {
    let input    = r#""I must escape \" in raw lines. Other escapes \n\u{EA}\r don't work.""#;
    let expected = token::Stream::from(vec![
        Token::TextLine(
            token::TextStyle::RawLine,
            vec![
                Token::TextSegmentRaw("I must escape ",0),
                Token::TextSegmentEscape(token::EscapeStyle::Literal,r#"""#,0),
                Token::TextSegmentRaw(r" in raw lines. Other escapes \n\u{EA}\r don't work.",0)
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn raw_inline_block_escapes() {
    let input = r#""""I don't have to escape " here but I can \"."#;
    let expected = token::Stream::from(vec![
        Token::TextInlineBlock(
            token::TextStyle::RawInlineBlock,
            vec![
                Token::TextSegmentRaw(r#"I don't have to escape " here but I can "#,0),
                Token::TextSegmentEscape(token::EscapeStyle::Literal,r#"""#,0),
                Token::TextSegmentRaw(".",0)
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn raw_block_escapes() {
    let input = make_unix_line_endings(
r#""""
    I'm in a raw block now.
    I don't have to escape " but I can \".
    Other escapes \xFF\uAAAAA don't work."#);
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::RawBlock,
            vec![
                Token::Line(
                    vec![Token::TextSegmentRaw("I'm in a raw block now.",0)],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::TextSegmentRaw(r#"I don't have to escape " but I can "#,0),
                        Token::TextSegmentEscape(token::EscapeStyle::Literal,r#"""#,0),
                        Token::TextSegmentRaw(".",0),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![Token::TextSegmentRaw(r#"Other escapes \xFF\uAAAAA don't work."#,0)],
                    0,
                    token::LineEnding::None
                )
            ],
            4,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn raw_block_mixed_line_endings() {
    let input    = "\"\"\"\n  Line one.\r\n  Line two.\n    \r\n  Line three.\n";
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::RawBlock,
            vec![
                Token::Line(vec![Token::TextSegmentRaw("Line one.",0)],0,token::LineEnding::CRLF),
                Token::Line(vec![Token::TextSegmentRaw("Line two.",0)],0,token::LineEnding::LF),
                Token::BlankLine(4,token::LineEnding::CRLF),
                Token::Line(vec![Token::TextSegmentRaw("Line three.",0)],0,token::LineEnding::LF),
            ],
            2,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn format_block_mixed_line_endings() {
    let input    = "'''\n  Line one.\r\n  Line two.\n    \r\n  Line three.\n";
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::Line(vec![Token::TextSegmentRaw("Line one.",0)],0,token::LineEnding::CRLF),
                Token::Line(vec![Token::TextSegmentRaw("Line two.",0)],0,token::LineEnding::LF),
                Token::BlankLine(4,token::LineEnding::CRLF),
                Token::Line(vec![Token::TextSegmentRaw("Line three.",0)],0,token::LineEnding::LF),
            ],
            2,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn text_block_first_line_blank() {
    let input =
r#"'''

  a"#;
    let expected = token::Stream::from(vec![
        Token::TextBlock(
            token::LineEnding::LF,
            token::TextStyle::FormatBlock,
            vec![
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(vec![Token::TextSegmentRaw("a",0)],0,token::LineEnding::None)
            ],
            2,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn top_level_text_block() {
    let input = make_unix_line_endings(
r#"'''
  a

  b
c"#);
    let text_block = Token::TextBlock(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::Line(vec![Token::TextSegmentRaw("a",0)],0,token::LineEnding::LF),
            Token::BlankLine(0,token::LineEnding::LF),
            Token::Line(vec![Token::TextSegmentRaw("b",0)],0,token::LineEnding::LF),
        ],
        2,
        0
    );
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::Line(vec![text_block],0,token::LineEnding::None),
                Token::Line(vec![Token::Variable("c",0)],0,token::LineEnding::None),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}
