//! This file contains tests for lexing full-on Enso with the lexer.

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



// ================
// === Combined ===
// ================

#[test]
fn method_definition() {
    let input = make_unix_line_endings(
        r#"##  Traverse the heterogeneous list, applying the provided polymorphic function
    wherever it matches.
@Tail_Call
map : forall ts ts' => (this : H_List ts) -> (exists a b . a ~> b) -> H_List ts'
map this fn -> case this.types of
    Cons x xs ->
        x' = fn x
        x.Cons (map xs)
    x -> fn x
"#,
    );
    let doc_comment = Token::line(
        vec![Token::doc_comment(
            vec![
                Token::line(
                    vec![Token::text_segment_raw(
                        "Traverse the heterogeneous list, applying the provided polymorphic \
                                function",
                        0,
                    )],
                    0,
                    token::LineEnding::LF,
                ),
                Token::line(
                    vec![Token::text_segment_raw("wherever it matches.", 0)],
                    0,
                    token::LineEnding::LF,
                ),
            ],
            4,
            0,
        )],
        0,
        token::LineEnding::None,
    );
    let annotation = Token::line(vec![Token::annotation("Tail_Call", 0)], 0, token::LineEnding::LF);
    let signature = Token::line(
        vec![
            Token::variable("map", 0),
            Token::operator(":", 1),
            Token::variable("forall", 1),
            Token::variable("ts", 1),
            Token::variable("ts'", 1),
            Token::operator("=>", 1),
            Token::operator("(", 1),
            Token::variable("this", 0),
            Token::operator(":", 1),
            Token::referent("H_List", 1),
            Token::variable("ts", 1),
            Token::operator(")", 0),
            Token::operator("->", 1),
            Token::operator("(", 1),
            Token::variable("exists", 0),
            Token::variable("a", 1),
            Token::variable("b", 1),
            Token::operator(".", 1),
            Token::variable("a", 1),
            Token::operator("~>", 1),
            Token::variable("b", 1),
            Token::operator(")", 0),
            Token::operator("->", 1),
            Token::referent("H_List", 1),
            Token::variable("ts'", 1),
        ],
        0,
        token::LineEnding::LF,
    );
    let cons_branch_body = Token::block(
        token::BlockType::Discontinuous,
        8,
        vec![
            Token::line(
                vec![
                    Token::variable("x'", 0),
                    Token::operator("=", 1),
                    Token::variable("fn", 1),
                    Token::variable("x", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::variable("x", 0),
                    Token::operator(".", 0),
                    Token::referent("Cons", 0),
                    Token::operator("(", 1),
                    Token::variable("map", 0),
                    Token::variable("xs", 1),
                    Token::operator(")", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    );
    let case_body = Token::block(
        token::BlockType::Continuous,
        4,
        vec![
            Token::line(
                vec![
                    Token::referent("Cons", 0),
                    Token::variable("x", 1),
                    Token::variable("xs", 1),
                    Token::operator("->", 1),
                    cons_branch_body,
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::variable("x", 0),
                    Token::operator("->", 1),
                    Token::variable("fn", 1),
                    Token::variable("x", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    );
    let function = Token::line(
        vec![
            Token::variable("map", 0),
            Token::variable("this", 1),
            Token::variable("fn", 1),
            Token::operator("->", 1),
            Token::variable("case", 1),
            Token::variable("this", 1),
            Token::operator(".", 0),
            Token::variable("types", 0),
            Token::variable("of", 1),
            case_body,
        ],
        0,
        token::LineEnding::LF,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![doc_comment, annotation, signature, function],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn complex_type() {
    let input = make_unix_line_endings(
        r#"
type Maybe a
    type Just item:a
    Nothing

    is_just = case this of
        Just _  -> True
        Nothing -> False
"#,
    );
    let case_block = Token::block(
        token::BlockType::Continuous,
        8,
        vec![
            Token::line(
                vec![
                    Token::referent("Just", 0),
                    Token::blank(1),
                    Token::operator("->", 2),
                    Token::referent("True", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::referent("Nothing", 0),
                    Token::operator("->", 1),
                    Token::referent("False", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    );
    let type_body = Token::block(
        token::BlockType::Continuous,
        4,
        vec![
            Token::line(
                vec![
                    Token::variable("type", 0),
                    Token::referent("Just", 1),
                    Token::variable("item", 1),
                    Token::operator(":", 0),
                    Token::variable("a", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(vec![Token::referent("Nothing", 0)], 0, token::LineEnding::LF),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![
                    Token::variable("is_just", 0),
                    Token::operator("=", 1),
                    Token::variable("case", 1),
                    Token::variable("this", 1),
                    Token::variable("of", 1),
                    case_block,
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    );
    let complex_type = Token::line(
        vec![
            Token::variable("type", 0),
            Token::referent("Maybe", 1),
            Token::variable("a", 1),
            type_body,
        ],
        0,
        token::LineEnding::LF,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![Token::blank_line(0, token::LineEnding::LF), complex_type],
        0,
    )]);
    assert_lexes(input, expected);
}

#[test]
fn imports_exports() {
    let input = make_unix_line_endings(
        r#"import Base.List
import Base.Number.Extensions
from Standard.Builtins import Unit, Number, Integer, Any, True, False

from Standard.Builtins export all

from Base.List export Nil, Cons
from Base.Number.Extensions export all hiding Math

polyglot java import com.ibm.icu.text.BreakIterator
polyglot java import org.enso.base.Text_Utils
"#,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![
            Token::line(
                vec![
                    Token::variable("import", 0),
                    Token::referent("Base", 1),
                    Token::operator(".", 0),
                    Token::referent("List", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::variable("import", 0),
                    Token::referent("Base", 1),
                    Token::operator(".", 0),
                    Token::referent("Number", 0),
                    Token::operator(".", 0),
                    Token::referent("Extensions", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::variable("from", 0),
                    Token::referent("Standard", 1),
                    Token::operator(".", 0),
                    Token::referent("Builtins", 0),
                    Token::variable("import", 1),
                    Token::referent("Unit", 1),
                    Token::operator(",", 0),
                    Token::referent("Number", 1),
                    Token::operator(",", 0),
                    Token::referent("Integer", 1),
                    Token::operator(",", 0),
                    Token::referent("Any", 1),
                    Token::operator(",", 0),
                    Token::referent("True", 1),
                    Token::operator(",", 0),
                    Token::referent("False", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![
                    Token::variable("from", 0),
                    Token::referent("Standard", 1),
                    Token::operator(".", 0),
                    Token::referent("Builtins", 0),
                    Token::variable("export", 1),
                    Token::variable("all", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![
                    Token::variable("from", 0),
                    Token::referent("Base", 1),
                    Token::operator(".", 0),
                    Token::referent("List", 0),
                    Token::variable("export", 1),
                    Token::referent("Nil", 1),
                    Token::operator(",", 0),
                    Token::referent("Cons", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::variable("from", 0),
                    Token::referent("Base", 1),
                    Token::operator(".", 0),
                    Token::referent("Number", 0),
                    Token::operator(".", 0),
                    Token::referent("Extensions", 0),
                    Token::variable("export", 1),
                    Token::variable("all", 1),
                    Token::variable("hiding", 1),
                    Token::referent("Math", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![
                    Token::variable("polyglot", 0),
                    Token::variable("java", 1),
                    Token::variable("import", 1),
                    Token::variable("com", 1),
                    Token::operator(".", 0),
                    Token::variable("ibm", 0),
                    Token::operator(".", 0),
                    Token::variable("icu", 0),
                    Token::operator(".", 0),
                    Token::variable("text", 0),
                    Token::operator(".", 0),
                    Token::external("BreakIterator", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::line(
                vec![
                    Token::variable("polyglot", 0),
                    Token::variable("java", 1),
                    Token::variable("import", 1),
                    Token::variable("org", 1),
                    Token::operator(".", 0),
                    Token::variable("enso", 0),
                    Token::operator(".", 0),
                    Token::variable("base", 0),
                    Token::operator(".", 0),
                    Token::referent("Text_Utils", 0),
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
fn some_stdlib() {
    let input = make_unix_line_endings(
        r#"from Standard.Base import all

## The top-level entry point for a test suite.
type Suite specs

## PRIVATE
type Spec name behaviors

## PRIVATE
type Behavior name result

## PRIVATE
Behavior.is_fail = this.result.is_fail

## PRIVATE
Spec.is_fail = this.behaviors.any is_fail

## PRIVATE
Suite.is_fail = this.specs.any is_fail
"#,
    );
    let expected = token::Stream::from(vec![Token::block(
        token::BlockType::Continuous,
        0,
        vec![
            Token::line(
                vec![
                    Token::variable("from", 0),
                    Token::referent("Standard", 1),
                    Token::operator(".", 0),
                    Token::referent("Base", 0),
                    Token::variable("import", 1),
                    Token::variable("all", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::doc_comment(
                    vec![Token::line(
                        vec![Token::text_segment_raw(
                            "The top-level entry point for a test suite.",
                            0,
                        )],
                        0,
                        token::LineEnding::LF,
                    )],
                    3,
                    0,
                )],
                0,
                token::LineEnding::None,
            ),
            Token::line(
                vec![
                    Token::variable("type", 0),
                    Token::referent("Suite", 1),
                    Token::variable("specs", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::doc_comment(
                    vec![Token::line(
                        vec![Token::text_segment_raw("PRIVATE", 0)],
                        0,
                        token::LineEnding::LF,
                    )],
                    3,
                    0,
                )],
                0,
                token::LineEnding::None,
            ),
            Token::line(
                vec![
                    Token::variable("type", 0),
                    Token::referent("Spec", 1),
                    Token::variable("name", 1),
                    Token::variable("behaviors", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::doc_comment(
                    vec![Token::line(
                        vec![Token::text_segment_raw("PRIVATE", 0)],
                        0,
                        token::LineEnding::LF,
                    )],
                    3,
                    0,
                )],
                0,
                token::LineEnding::None,
            ),
            Token::line(
                vec![
                    Token::variable("type", 0),
                    Token::referent("Behavior", 1),
                    Token::variable("name", 1),
                    Token::variable("result", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::doc_comment(
                    vec![Token::line(
                        vec![Token::text_segment_raw("PRIVATE", 0)],
                        0,
                        token::LineEnding::LF,
                    )],
                    3,
                    0,
                )],
                0,
                token::LineEnding::None,
            ),
            Token::line(
                vec![
                    Token::referent("Behavior", 0),
                    Token::operator(".", 0),
                    Token::variable("is_fail", 0),
                    Token::operator("=", 1),
                    Token::variable("this", 1),
                    Token::operator(".", 0),
                    Token::variable("result", 0),
                    Token::operator(".", 0),
                    Token::variable("is_fail", 0),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::doc_comment(
                    vec![Token::line(
                        vec![Token::text_segment_raw("PRIVATE", 0)],
                        0,
                        token::LineEnding::LF,
                    )],
                    3,
                    0,
                )],
                0,
                token::LineEnding::None,
            ),
            Token::line(
                vec![
                    Token::referent("Spec", 0),
                    Token::operator(".", 0),
                    Token::variable("is_fail", 0),
                    Token::operator("=", 1),
                    Token::variable("this", 1),
                    Token::operator(".", 0),
                    Token::variable("behaviors", 0),
                    Token::operator(".", 0),
                    Token::variable("any", 0),
                    Token::variable("is_fail", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
            Token::blank_line(0, token::LineEnding::LF),
            Token::line(
                vec![Token::doc_comment(
                    vec![Token::line(
                        vec![Token::text_segment_raw("PRIVATE", 0)],
                        0,
                        token::LineEnding::LF,
                    )],
                    3,
                    0,
                )],
                0,
                token::LineEnding::None,
            ),
            Token::line(
                vec![
                    Token::referent("Suite", 0),
                    Token::operator(".", 0),
                    Token::variable("is_fail", 0),
                    Token::operator("=", 1),
                    Token::variable("this", 1),
                    Token::operator(".", 0),
                    Token::variable("specs", 0),
                    Token::operator(".", 0),
                    Token::variable("any", 0),
                    Token::variable("is_fail", 1),
                ],
                0,
                token::LineEnding::LF,
            ),
        ],
        0,
    )]);
    assert_lexes(input, expected);
}
