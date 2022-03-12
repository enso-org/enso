//! This file contains tests for lexing text literals in the Enso lexer.

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



// ============
// === Text ===
// ============

#[test]
fn invalid_interpolate_quote() {
    let input = "`";
    let expected = token::Stream::from(vec![Token::unrecognized("`", 0)]);
    assert_lexes(input, expected);
}

#[test]
fn invalid_format_quote() {
    let input = r#"''''"#;
    let expected = token::Stream::from(vec![Token::invalid_quote(r#"''''"#, 0)]);
    assert_lexes(input, expected);
}

#[test]
fn single_line_format_text() {
    let input = r#"'dearest creature in creation, studying english pronunciation'"#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![Token::text_segment_raw(
            "dearest creature in creation, studying english pronunciation",
            0,
        )],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn single_line_format_with_one_interpolation() {
    let input = "'The result is `result.pretty`!'";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("The result is ", 0),
            Token::text_segment_interpolate(
                vec![
                    Token::variable("result", 0),
                    Token::operator(".", 0),
                    Token::variable("pretty", 0),
                ],
                0,
            ),
            Token::text_segment_raw("!", 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn single_line_format_with_multiple_interpolations() {
    let input = "'My_Type: name=`self.name`, suspended=`self.suspended`'";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("My_Type: name=", 0),
            Token::text_segment_interpolate(
                vec![
                    Token::variable("self", 0),
                    Token::operator(".", 0),
                    Token::variable("name", 0),
                ],
                0,
            ),
            Token::text_segment_raw(", suspended=", 0),
            Token::text_segment_interpolate(
                vec![
                    Token::variable("self", 0),
                    Token::operator(".", 0),
                    Token::variable("suspended", 0),
                ],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn block_format_text() {
    let input = make_unix_line_endings(
        r#"'''
    I have a format text block literal.
    It may optionally contain `interpolations` interspersed with the text like `this`.

    And it ends when the indent of `indent` goes back.
"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::line(
                vec![Token::text_segment_raw("I have a format text block literal.", 0)],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::text_segment_raw("It may optionally contain ", 0),
                    Token::text_segment_interpolate(vec![Token::variable("interpolations", 0)], 0),
                    Token::text_segment_raw(" interspersed with the text like ", 0),
                    Token::text_segment_interpolate(vec![Token::variable("this", 0)], 0),
                    Token::text_segment_raw(".", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![
                    Token::text_segment_raw("And it ends when the indent of ", 0),
                    Token::text_segment_interpolate(vec![Token::variable("indent", 0)], 0),
                    Token::text_segment_raw(" goes back.", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block() {
    let input = "'''foo bar `interp` 'baz";
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("foo bar ", 0),
            Token::text_segment_interpolate(vec![Token::variable("interp", 0)], 0),
            Token::text_segment_raw(" 'baz", 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_line_escape_test() {
    let input = "'\\'\\U00131313 = \\u{z2}\\u{AFD3} `a + b` \\U23232323\\uAA\\uAAAA a b c \\xAF'";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_escape(token::EscapeStyle::Literal, "'", 0),
            Token::text_segment_escape(token::EscapeStyle::U32, "00131313", 0),
            Token::text_segment_raw(" = ", 0),
            Token::text_segment_escape(token::EscapeStyle::InvalidUnicode, "\\u{z2}", 0),
            Token::text_segment_escape(token::EscapeStyle::U21, "AFD3", 0),
            Token::text_segment_raw(" ", 0),
            Token::text_segment_interpolate(
                vec![Token::variable("a", 0), Token::operator("+", 1), Token::variable("b", 1)],
                0,
            ),
            Token::text_segment_raw(" ", 0),
            Token::text_segment_escape(token::EscapeStyle::InvalidUnicode, "\\U23232323", 0),
            Token::text_segment_escape(token::EscapeStyle::InvalidUnicode, "\\uAA", 0),
            Token::text_segment_escape(token::EscapeStyle::U16, "AAAA", 0),
            Token::text_segment_raw(" a b c ", 0),
            Token::text_segment_escape(token::EscapeStyle::Byte, "AF", 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_line_unfinished_escape() {
    let input = "'\\";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::UnclosedLine,
        vec![Token::text_segment_escape(token::EscapeStyle::Invalid, "\\", 0)],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_escape_sequences() {
    let input = "'''\\U00131313 =' \\u{z2}\\u{AFD3} `a + b` \\U23232323\\uAA\\uAAAA a b c \\xAF";
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_escape(token::EscapeStyle::U32, "00131313", 0),
            Token::text_segment_raw(" =' ", 0),
            Token::text_segment_escape(token::EscapeStyle::InvalidUnicode, "\\u{z2}", 0),
            Token::text_segment_escape(token::EscapeStyle::U21, "AFD3", 0),
            Token::text_segment_raw(" ", 0),
            Token::text_segment_interpolate(
                vec![Token::variable("a", 0), Token::operator("+", 1), Token::variable("b", 1)],
                0,
            ),
            Token::text_segment_raw(" ", 0),
            Token::text_segment_escape(token::EscapeStyle::InvalidUnicode, "\\U23232323", 0),
            Token::text_segment_escape(token::EscapeStyle::InvalidUnicode, "\\uAA", 0),
            Token::text_segment_escape(token::EscapeStyle::U16, "AAAA", 0),
            Token::text_segment_raw(" a b c ", 0),
            Token::text_segment_escape(token::EscapeStyle::Byte, "AF", 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_escape_sequences() {
    let input = make_unix_line_endings(
        r#"'''
    \U00131313 =' \u{z2}\u{AFD3} `a + b`\`
    \U23232323\uAA\uAAAA a b c \xAF\n
    \'
"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::line(
                vec![
                    Token::text_segment_escape(token::EscapeStyle::U32, "00131313", 0),
                    Token::text_segment_raw(" =' ", 0),
                    Token::text_segment_escape(token::EscapeStyle::InvalidUnicode, "\\u{z2}", 0),
                    Token::text_segment_escape(token::EscapeStyle::U21, "AFD3", 0),
                    Token::text_segment_raw(" ", 0),
                    Token::text_segment_interpolate(
                        vec![
                            Token::variable("a", 0),
                            Token::operator("+", 1),
                            Token::variable("b", 1),
                        ],
                        0,
                    ),
                    Token::text_segment_escape(token::EscapeStyle::Literal, "`", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::text_segment_escape(
                        token::EscapeStyle::InvalidUnicode,
                        "\\U23232323",
                        0,
                    ),
                    Token::text_segment_escape(token::EscapeStyle::InvalidUnicode, "\\uAA", 0),
                    Token::text_segment_escape(token::EscapeStyle::U16, "AAAA", 0),
                    Token::text_segment_raw(" a b c ", 0),
                    Token::text_segment_escape(token::EscapeStyle::Byte, "AF", 0),
                    Token::text_segment_escape(token::EscapeStyle::Literal, "\n", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![Token::text_segment_escape(token::EscapeStyle::Literal, "'", 0)],
                0,
                token::LineEnding::LF,
            ),
        ],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_line_unclosed_interpolate() {
    let input = "'Foo bar `baz'";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::UnclosedLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_unclosed_interpolate(vec![Token::variable("baz'", 0)], 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_unclosed_interpolate() {
    let input = "'''Foo bar ` baz";
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_unclosed_interpolate(vec![Token::variable("baz", 1)], 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_unclosed_interpolate() {
    let input = make_unix_line_endings(
        r#"'''
    Here's an interpolated block.
    And here's an `unfinished interpolation
    And `another"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::line(
                vec![Token::text_segment_raw("Here's an interpolated block.", 0)],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::text_segment_raw("And here's an ", 0),
                    Token::text_segment_unclosed_interpolate(
                        vec![Token::variable("unfinished", 0), Token::variable("interpolation", 1)],
                        0,
                    ),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::text_segment_raw("And ", 0),
                    Token::text_segment_unclosed_interpolate(
                        vec![Token::variable("another", 0)],
                        0,
                    ),
                ],
                0,
                token::LineEnding::None,
            ),
        ],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_raw_line() {
    let input = r#"'Foo bar `"baz"`'"#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![Token::text_line(
                    token::TextStyle::RawLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_raw_line_unclosed() {
    let input = r#"'Foo bar `"baz`'"#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::UnclosedLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_unclosed_interpolate(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![Token::text_segment_raw("baz`'", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_format_line() {
    let input = "'Foo bar `'baz'`'";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![Token::text_line(
                    token::TextStyle::FormatLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_format_line_unclosed() {
    let input = "'Foo bar `'baz`'";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_format_line_interpolate() {
    let input = "'Foo bar `'baz``'`'";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
            Token::text_segment_interpolate(
                vec![Token::text_line(token::TextStyle::UnclosedLine, vec![], 0)],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_format_line_unclosed_interpolate() {
    let input = "'Foo bar `'baz";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::UnclosedLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_unclosed_interpolate(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_raw_line_unclosed_interpolate() {
    let input = r#"'Foo bar `"baz"'"#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::UnclosedLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_unclosed_interpolate(
                vec![
                    Token::text_line(
                        token::TextStyle::RawLine,
                        vec![Token::text_segment_raw("baz", 0)],
                        0,
                    ),
                    Token::text_line(token::TextStyle::UnclosedLine, vec![], 0),
                ],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_format_inline_block() {
    let input = "'Foo bar `''' a b c d`'";
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![
                    Token::unrecognized("'''", 0),
                    Token::variable("a", 1),
                    Token::variable("b", 1),
                    Token::variable("c", 1),
                    Token::variable("d", 1),
                ],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_raw_inline_block() {
    let input = r#"'Foo bar `""" a b c d`'"#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![
                    Token::unrecognized(r#"""""#, 0),
                    Token::variable("a", 1),
                    Token::variable("b", 1),
                    Token::variable("c", 1),
                    Token::variable("d", 1),
                ],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_invalid_format_quote() {
    let input = r#"'Foo bar `'''''`'"#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(vec![Token::invalid_quote("'''''", 0)], 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_invalid_raw_quote() {
    let input = r#"'Foo bar `"""""`'"#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::FormatLine,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(vec![Token::invalid_quote(r#"""""""#, 0)], 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_format_block() {
    let input = make_unix_line_endings(
        r#"'Foo bar `'''
Foo`'
"#,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![
            Token::line(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![
                        Token::text_segment_raw("Foo bar ", 0),
                        Token::text_segment_unclosed_interpolate(
                            vec![Token::unrecognized(r#"'''"#, 0)],
                            0,
                        ),
                    ],
                    0,
                )],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::referent("Foo", 0),
                    Token::unrecognized("`", 0),
                    Token::text_line(token::TextStyle::UnclosedLine, vec![], 0),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn nested_raw_block() {
    let input = make_unix_line_endings(
        r#"'Foo bar `"""
Foo`'
"#,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![
            Token::line(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![
                        Token::text_segment_raw("Foo bar ", 0),
                        Token::text_segment_unclosed_interpolate(
                            vec![Token::unrecognized(r#"""""#, 0)],
                            0,
                        ),
                    ],
                    0,
                )],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::referent("Foo", 0),
                    Token::unrecognized("`", 0),
                    Token::text_line(token::TextStyle::UnclosedLine, vec![], 0),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_raw_line() {
    let input = r#"'''Foo bar `"baz"`"#;
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![Token::text_line(
                    token::TextStyle::RawLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_raw_line_unclosed() {
    let input = r#"'''Foo bar `"baz`"#;
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_unclosed_interpolate(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![Token::text_segment_raw("baz`", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_format_line() {
    let input = "'''Foo bar `'baz'`";
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![Token::text_line(
                    token::TextStyle::FormatLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_format_line_unclosed() {
    let input = "'''Foo bar `'baz`";
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_format_line_interpolate() {
    let input = "'''Foo bar `'baz``'`";
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
            Token::text_segment_interpolate(
                vec![Token::text_line(token::TextStyle::UnclosedLine, vec![], 0)],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_format_line_unclosed_interpolate() {
    let input = "'''Foo bar `'baz";
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_unclosed_interpolate(
                vec![Token::text_line(
                    token::TextStyle::UnclosedLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_raw_line_unclosed_interpolate() {
    let input = r#"'''Foo bar `"baz""#;
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_unclosed_interpolate(
                vec![Token::text_line(
                    token::TextStyle::RawLine,
                    vec![Token::text_segment_raw("baz", 0)],
                    0,
                )],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_format_inline_block() {
    let input = "'''Foo bar `''' a b c d`";
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![
                    Token::unrecognized("'''", 0),
                    Token::variable("a", 1),
                    Token::variable("b", 1),
                    Token::variable("c", 1),
                    Token::variable("d", 1),
                ],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_raw_inline_block() {
    let input = r#"'''Foo bar `""" a b c d`"#;
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(
                vec![
                    Token::unrecognized(r#"""""#, 0),
                    Token::variable("a", 1),
                    Token::variable("b", 1),
                    Token::variable("c", 1),
                    Token::variable("d", 1),
                ],
                0,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_invalid_format_quote() {
    let input = r#"'''Foo bar `'''''`"#;
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(vec![Token::invalid_quote("'''''", 0)], 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_invalid_raw_quote() {
    let input = r#"'''Foo bar `"""""`"#;
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::FormatInlineBlock,
        vec![
            Token::text_segment_raw("Foo bar ", 0),
            Token::text_segment_interpolate(vec![Token::invalid_quote(r#"""""""#, 0)], 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_format_block() {
    let input = make_unix_line_endings(
        r#"'''Foo bar `'''
Foo`
"#,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![
            Token::line(
                vec![Token::text_inline_block(
                    token::TextStyle::FormatInlineBlock,
                    vec![
                        Token::text_segment_raw("Foo bar ", 0),
                        Token::text_segment_unclosed_interpolate(
                            vec![Token::unrecognized(r#"'''"#, 0)],
                            0,
                        ),
                    ],
                    0,
                )],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![Token::referent("Foo", 0), Token::unrecognized("`", 0)],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_inline_block_nested_raw_block() {
    let input = make_unix_line_endings(
        r#"'''Foo bar `"""
Foo`
"#,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![
            Token::line(
                vec![Token::text_inline_block(
                    token::TextStyle::FormatInlineBlock,
                    vec![
                        Token::text_segment_raw("Foo bar ", 0),
                        Token::text_segment_unclosed_interpolate(
                            vec![Token::unrecognized(r#"""""#, 0)],
                            0,
                        ),
                    ],
                    0,
                )],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![Token::referent("Foo", 0), Token::unrecognized("`", 0)],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_raw_line() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `"baz"`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_interpolate(
                    vec![Token::text_line(
                        token::TextStyle::RawLine,
                        vec![Token::text_segment_raw("baz", 0)],
                        0,
                    )],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_raw_line_unclosed() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `"baz`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_unclosed_interpolate(
                    vec![Token::text_line(
                        token::TextStyle::UnclosedLine,
                        vec![Token::text_segment_raw("baz`", 0)],
                        0,
                    )],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_format_line() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `'baz'`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_interpolate(
                    vec![Token::text_line(
                        token::TextStyle::FormatLine,
                        vec![Token::text_segment_raw("baz", 0)],
                        0,
                    )],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_format_line_unclosed() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `'baz`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_interpolate(
                    vec![Token::text_line(
                        token::TextStyle::UnclosedLine,
                        vec![Token::text_segment_raw("baz", 0)],
                        0,
                    )],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_format_line_interpolate() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `'baz``'`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_interpolate(
                    vec![Token::text_line(
                        token::TextStyle::UnclosedLine,
                        vec![Token::text_segment_raw("baz", 0)],
                        0,
                    )],
                    0,
                ),
                Token::text_segment_interpolate(
                    vec![Token::text_line(token::TextStyle::UnclosedLine, vec![], 0)],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_format_line_unclosed_interpolate() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `'baz"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_unclosed_interpolate(
                    vec![Token::text_line(
                        token::TextStyle::UnclosedLine,
                        vec![Token::text_segment_raw("baz", 0)],
                        0,
                    )],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_raw_line_unclosed_interpolate() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `"baz"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_unclosed_interpolate(
                    vec![Token::text_line(
                        token::TextStyle::UnclosedLine,
                        vec![Token::text_segment_raw("baz", 0)],
                        0,
                    )],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_format_inline_block() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `''' a b c d`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_interpolate(
                    vec![
                        Token::unrecognized(r#"'''"#, 0),
                        Token::variable("a", 1),
                        Token::variable("b", 1),
                        Token::variable("c", 1),
                        Token::variable("d", 1),
                    ],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_raw_inline_block() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `""" a b c d`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_interpolate(
                    vec![
                        Token::unrecognized(r#"""""#, 0),
                        Token::variable("a", 1),
                        Token::variable("b", 1),
                        Token::variable("c", 1),
                        Token::variable("d", 1),
                    ],
                    0,
                ),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_invalid_format_quote() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `'''''`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_interpolate(vec![Token::invalid_quote(r#"'''''"#, 0)], 0),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_invalid_raw_quote() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `"""""`"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![Token::line(
            vec![
                Token::text_segment_raw("Foo bar ", 0),
                Token::text_segment_interpolate(vec![Token::invalid_quote(r#"""""""#, 0)], 0),
            ],
            0,
            token::LineEnding::None,
        )],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_format_block() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `'''
    Foo`
"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::line(
                vec![
                    Token::text_segment_raw("Foo bar ", 0),
                    Token::text_segment_unclosed_interpolate(
                        vec![Token::unrecognized(r#"'''"#, 0)],
                        0,
                    ),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::text_segment_raw("Foo", 0),
                    Token::text_segment_unclosed_interpolate(vec![], 0),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_nested_raw_block() {
    let input = make_unix_line_endings(
        r#"'''
    Foo bar `"""
    Foo`
"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::line(
                vec![
                    Token::text_segment_raw("Foo bar ", 0),
                    Token::text_segment_unclosed_interpolate(
                        vec![Token::unrecognized(r#"""""#, 0)],
                        0,
                    ),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::text_segment_raw("Foo", 0),
                    Token::text_segment_unclosed_interpolate(vec![], 0),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn invalid_raw_quote() {
    let input = r#"""""""#;
    let expected = token::Stream::from(vec![Token::invalid_quote(r#"""""""#, 0)]);
    assert_lexes(input, expected);
}

#[test]
fn single_line_raw_text() {
    let input = r#""dearest creature in creation, studying english pronunciation""#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::RawLine,
        vec![Token::text_segment_raw(
            "dearest creature in creation, studying english pronunciation",
            0,
        )],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn block_raw_text() {
    let input = make_unix_line_endings(
        r#""""
    I have a raw text block literal.

    `Interpolations` are not anything special in these literals.

    And it ends when the prevailing indentation is reduced.
"#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::RawBlock,
        vec![
            Token::line(
                vec![Token::text_segment_raw("I have a raw text block literal.", 0)],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::text_segment_raw(
                    "`Interpolations` are not anything special in these literals.",
                    0,
                )],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::text_segment_raw(
                    "And it ends when the prevailing indentation is reduced.",
                    0,
                )],
                0,
                token::LineEnding::LF,
            ),
        ],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn raw_inline_block() {
    let input = r#""""foo bar `interp` 'baz"#;
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::RawInlineBlock,
        vec![Token::text_segment_raw("foo bar `interp` 'baz", 0)],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn raw_line_escapes() {
    let input = r#""I must escape \" in raw lines. Other escapes \n\u{EA}\r don't work.""#;
    let expected = token::Stream::from(vec![Token::text_line(
        token::TextStyle::RawLine,
        vec![
            Token::text_segment_raw("I must escape ", 0),
            Token::text_segment_escape(token::EscapeStyle::Literal, r#"""#, 0),
            Token::text_segment_raw(r" in raw lines. Other escapes \n\u{EA}\r don't work.", 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn raw_inline_block_escapes() {
    let input = r#""""I don't have to escape " here but I can \"."#;
    let expected = token::Stream::from(vec![Token::text_inline_block(
        token::TextStyle::RawInlineBlock,
        vec![
            Token::text_segment_raw(r#"I don't have to escape " here but I can "#, 0),
            Token::text_segment_escape(token::EscapeStyle::Literal, r#"""#, 0),
            Token::text_segment_raw(".", 0),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn raw_block_escapes() {
    let input = make_unix_line_endings(
        r#""""
    I'm in a raw block now.
    I don't have to escape " but I can \".
    Other escapes \xFF\uAAAAA don't work."#,
    );
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::RawBlock,
        vec![
            Token::line(
                vec![Token::text_segment_raw("I'm in a raw block now.", 0)],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::text_segment_raw(r#"I don't have to escape " but I can "#, 0),
                    Token::text_segment_escape(token::EscapeStyle::Literal, r#"""#, 0),
                    Token::text_segment_raw(".", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![Token::text_segment_raw(r#"Other escapes \xFF\uAAAAA don't work."#, 0)],
                0,
                token::LineEnding::None,
            ),
        ],
        4,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn raw_block_mixed_line_endings() {
    let input = "\"\"\"\n  Line one.\r\n  Line two.\n    \r\n  Line three.\n";
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::RawBlock,
        vec![
            Token::line(vec![Token::text_segment_raw("Line one.", 0)], 0, token::LineEnding::CRLF),
            Token::line(vec![Token::text_segment_raw("Line two.", 0)], 0, token::LineEnding::LF),
            Token::blank_line(4, token::LineEnding::CRLF),
            Token::line(vec![Token::text_segment_raw("Line three.", 0)], 0, token::LineEnding::LF),
        ],
        2,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn format_block_mixed_line_endings() {
    let input = "'''\n  Line one.\r\n  Line two.\n    \r\n  Line three.\n";
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::line(vec![Token::text_segment_raw("Line one.", 0)], 0, token::LineEnding::CRLF),
            Token::line(vec![Token::text_segment_raw("Line two.", 0)], 0, token::LineEnding::LF),
            Token::blank_line(4, token::LineEnding::CRLF),
            Token::line(vec![Token::text_segment_raw("Line three.", 0)], 0, token::LineEnding::LF),
        ],
        2,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn text_block_first_line_blank() {
    let input = r#"'''

  a"#;
    let expected = token::Stream::from(vec![Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(vec![Token::text_segment_raw("a", 0)], 0, token::LineEnding::None),
        ],
        2,
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn top_level_text_block() {
    let input = make_unix_line_endings(
        r#"'''
  a

  b
c"#,
    );
    let text_block = Token::text_block(
        token::LineEnding::LF,
        token::TextStyle::FormatBlock,
        vec![
            Token::line(vec![Token::text_segment_raw("a", 0)], 0, token::LineEnding::LF),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(vec![Token::text_segment_raw("b", 0)], 0, token::LineEnding::LF),
        ],
        2,
        0,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![
            Token::line(vec![text_block], 0, token::LineEnding::None),
            Token::line(vec![Token::variable("c", 0)], 0, token::LineEnding::None),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}
