#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This file contains tests for lexing comments in the Enso lexer.

mod test_utils;

use lexer_definition::library::*;
use test_utils::*;

use lexer_definition::library::token::Token;



// ================
// === Comments ===
// ================

#[test]
fn disable_eof() {
    let input    = "# Here is a nice long comment string.";
    let expected = token::Stream::from(vec![
        Token::disable_comment(" Here is a nice long comment string.", 0)
    ]);
    assert_lexes(input,expected);
}

#[test]
fn disable_lf() {
    let input    = "# Here is a nice long comment string.\n";
    let expected = token::Stream::from(vec![
        Token::block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::line(
                    vec![Token::disable_comment(" Here is a nice long comment string.", 0)],
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
fn disable_crlf() {
    let input    = "# Here is a nice long comment string.\r\n";
    let expected = token::Stream::from(vec![
        Token::block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::line(
                    vec![Token::disable_comment(" Here is a nice long comment string.", 0)],
                    0,
                    token::LineEnding::CRLF
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn disable_in_line() {
    let input    = "a + b <*> N # Compare the frobnicators.";
    let expected = token::Stream::from(vec![
        Token::variable("a", 0),
        Token::operator("+", 1),
        Token::variable("b", 1),
        Token::operator("<*>", 1),
        Token::referent("N", 1),
        Token::disable_comment(" Compare the frobnicators.", 1),
    ]);
    assert_lexes(input,expected)
}

#[test]
fn disable_in_interpolate() {
    let input    = "'String `1 + 1 # add` stuff.'";
    let expected = token::Stream::from(vec![
        Token::text_line(
            token::TextStyle::FormatLine,
            vec![
                Token::text_segment_raw("String ", 0),
                Token::text_segment_interpolate(
                    vec![
                        Token::number("", "1", 0),
                        Token::operator("+", 1),
                        Token::number("", "1", 1),
                        Token::unrecognized("#", 1),
                        Token::variable("add", 1)
                    ],
                    0
                ),
                Token::text_segment_raw(" stuff.", 0),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn doc_single_line_eof() {
    let input    = "## Foo bar baz";
    let expected = token::Stream::from(vec![
        Token::doc_comment(
            vec![
                Token::line(vec![Token::text_segment_raw("Foo bar baz", 0)], 0, token::LineEnding::None)
            ],
            3,
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn doc_single_line_lf() {
    let input    = "## Foo bar baz\n";
    let expected = token::Stream::from(vec![
        Token::block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::line(
                    vec![
                        Token::doc_comment(
                            vec![
                                Token::line(
                                    vec![Token::text_segment_raw("Foo bar baz", 0)],
                                    0,
                                    token::LineEnding::LF
                                )
                            ],
                            3,
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                ),
                Token::blank_line(0, token::LineEnding::None),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn doc_single_line_crlf() {
    let input    = "## Foo bar baz\r\n";
    let expected = token::Stream::from(vec![
        Token::block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::line(
                    vec![
                        Token::doc_comment(
                            vec![
                                Token::line(
                                    vec![Token::text_segment_raw("Foo bar baz", 0)],
                                    0,
                                    token::LineEnding::CRLF
                                )
                            ],
                            3,
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                ),
                Token::blank_line(0, token::LineEnding::None),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn doc_in_interpolate() {
    let input    = "'String `1 + 1 ## add` stuff.'";
    let expected = token::Stream::from(vec![
        Token::text_line(
            token::TextStyle::FormatLine,
            vec![
                Token::text_segment_raw("String ", 0),
                Token::text_segment_interpolate(
                    vec![
                        Token::number("", "1", 0),
                        Token::operator("+", 1),
                        Token::number("", "1", 1),
                        Token::unrecognized("##", 1),
                        Token::variable("add", 1)
                    ],
                    0
                ),
                Token::text_segment_raw(" stuff.", 0),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn doc_multi_line() {
    let input = make_unix_line_endings(
r#"##  Here is a doc comment.
    It spans multiple lines.
        Some are indented much further.
        And this is okay.

    It keeps going, even with blank lines.
    Until the indentation decreases back.

trailing_blanks_not_part_of_comment"#);
    let doc_comment = Token::doc_comment(
        vec![
            Token::line(
                vec![Token::text_segment_raw("Here is a doc comment.", 0)],
                0,
                token::LineEnding::LF
            ),
            Token::line(
                vec![Token::text_segment_raw("It spans multiple lines.", 0)],
                0,
                token::LineEnding::LF
            ),
            Token::line(
                vec![Token::text_segment_raw("    Some are indented much further.", 0)],
                0,
                token::LineEnding::LF
            ),
            Token::line(
                vec![Token::text_segment_raw("    And this is okay.", 0)],
                0,
                token::LineEnding::LF
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::text_segment_raw("It keeps going, even with blank lines.", 0)],
                0,
                token::LineEnding::LF
            ),
            Token::line(
                vec![Token::text_segment_raw("Until the indentation decreases back.", 0)],
                0,
                token::LineEnding::LF
            ),
        ],
        4,
        0
    );
    let expected = token::Stream::from(vec![
        Token::block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::line(vec![doc_comment], 0, token::LineEnding::None),
                Token::blank_line(0, token::LineEnding::LF),
                Token::line(
                    vec![Token::variable("trailing_blanks_not_part_of_comment", 0)],
                    0,
                    token::LineEnding::None
                )
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn doc_mixed_line_endings() {
    let input    = "## Start a doc comment\n   It has indent 3.\r\n    \n    An indented blank too.";
    let expected = token::Stream::from(vec![
        Token::doc_comment(
            vec![
                Token::line(
                    vec![Token::text_segment_raw("Start a doc comment", 0)],
                    0,
                    token::LineEnding::LF
                ),
                Token::line(
                    vec![Token::text_segment_raw("It has indent 3.", 0)],
                    0,
                    token::LineEnding::CRLF
                ),
                Token::blank_line(4, token::LineEnding::LF),
                Token::line(
                    vec![Token::text_segment_raw(" An indented blank too.", 0)],
                    0,
                    token::LineEnding::None
                )
            ],
            3,
            0
        )
    ]);
    assert_lexes(input,expected);
}
