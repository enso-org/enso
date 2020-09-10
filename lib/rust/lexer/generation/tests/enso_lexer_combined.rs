#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This file contains tests for lexing full-on Enso with the lexer.

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
"#);
    let doc_comment = Token::Line(
        vec![
            Token::DocComment(
                vec![
                    Token::Line(
                        vec![
                            Token::TextSegmentRaw(
                                "Traverse the heterogeneous list, applying the provided polymorphic \
                                function",
                                0
                            )
                        ],
                        0,
                        token::LineEnding::LF,
                    ),
                    Token::Line(
                        vec![Token::TextSegmentRaw("wherever it matches.",0)],
                        0,
                        token::LineEnding::LF
                    )
                ],
                4,
                0
            ),
        ],
        0,
        token::LineEnding::None,
    );
    let annotation = Token::Line(
        vec![Token::Annotation("Tail_Call",0)],
        0,
        token::LineEnding::LF,
    );
    let signature = Token::Line(
        vec![
            Token::Variable("map",0),
            Token::Operator(":",1),
            Token::Variable("forall",1),
            Token::Variable("ts",1),
            Token::Variable("ts'",1),
            Token::Operator("=>",1),
            Token::Operator("(",1),
            Token::Variable("this",0),
            Token::Operator(":",1),
            Token::Referent("H_List",1),
            Token::Variable("ts",1),
            Token::Operator(")",0),
            Token::Operator("->",1),
            Token::Operator("(",1),
            Token::Variable("exists",0),
            Token::Variable("a",1),
            Token::Variable("b",1),
            Token::Operator(".",1),
            Token::Variable("a",1),
            Token::Operator("~>",1),
            Token::Variable("b",1),
            Token::Operator(")",0),
            Token::Operator("->",1),
            Token::Referent("H_List",1),
            Token::Variable("ts'",1),
        ],
        0,
        token::LineEnding::LF
    );
    let cons_branch_body = Token::Block(
        token::BlockType::Discontinuous,
        8,
        vec![
            Token::Line(
                vec![
                    Token::Variable("x'",0),
                    Token::Operator("=",1),
                    Token::Variable("fn",1),
                    Token::Variable("x",1),
                ],
                0,
                token::LineEnding::LF
            ),
            Token::Line(
                vec![
                    Token::Variable("x",0),
                    Token::Operator(".",0),
                    Token::Referent("Cons",0),
                    Token::Operator("(",1),
                    Token::Variable("map",0),
                    Token::Variable("xs",1),
                    Token::Operator(")",0),
                ],
                0,
                token::LineEnding::LF
            ),
        ],
        0
    );
    let case_body = Token::Block(
        token::BlockType::Continuous,
        4,
        vec![
            Token::Line(
                vec![
                    Token::Referent("Cons",0),
                    Token::Variable("x",1),
                    Token::Variable("xs",1),
                    Token::Operator("->",1),
                    cons_branch_body,
                ],
                0,
                token::LineEnding::LF
            ),
            Token::Line(
                vec![
                    Token::Variable("x",0),
                    Token::Operator("->",1),
                    Token::Variable("fn",1),
                    Token::Variable("x",1)
                ],
                0,
                token::LineEnding::LF,
            )
        ],
        0
    );
    let function = Token::Line(
        vec![
            Token::Variable("map",0),
            Token::Variable("this",1),
            Token::Variable("fn",1),
            Token::Operator("->",1),
            Token::Variable("case",1),
            Token::Variable("this",1),
            Token::Operator(".",0),
            Token::Variable("types",0),
            Token::Variable("of",1),
            case_body,
        ],
        0,
        token::LineEnding::LF
    );
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![doc_comment,annotation,signature,function],
            0
        )
    ]);
    assert_lexes(input,expected);
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
"#);
    let case_block = Token::Block(
        token::BlockType::Continuous,
        8,
        vec![
            Token::Line(
                vec![
                    Token::Referent("Just",0),
                    Token::Blank(1),
                    Token::Operator("->",2),
                    Token::Referent("True",1),
                ],
                0,
                token::LineEnding::LF
            ),
            Token::Line(
                vec![
                    Token::Referent("Nothing",0),
                    Token::Operator("->",1),
                    Token::Referent("False",1)
                ],
                0,
                token::LineEnding::LF
            ),
        ],
        0
    );
    let type_body = Token::Block(
        token::BlockType::Continuous,
        4,
        vec![
            Token::Line(
                vec![
                    Token::Variable("type",0),
                    Token::Referent("Just",1),
                    Token::Variable("item",1),
                    Token::Operator(":",0),
                    Token::Variable("a",0),
                ],
                0,
                token::LineEnding::LF
            ),
            Token::Line(vec![Token::Referent("Nothing",0)],0,token::LineEnding::LF),
            Token::BlankLine(0,token::LineEnding::LF),
            Token::Line(
                vec![
                    Token::Variable("is_just",0),
                    Token::Operator("=",1),
                    Token::Variable("case",1),
                    Token::Variable("this",1),
                    Token::Variable("of",1),
                    case_block,
                ],
                0,
                token::LineEnding::LF
            )
        ],
        0
    );
    let complex_type = Token::Line(
        vec![
            Token::Variable("type",0),
            Token::Referent("Maybe",1),
            Token::Variable("a",1),
            type_body,
        ],
        0,
        token::LineEnding::LF
    );
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::BlankLine(0,token::LineEnding::LF),
                complex_type
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn imports_exports() {
    let input = make_unix_line_endings(
r#"import Base.List
import Base.Number.Extensions
from Builtins import Unit, Number, Integer, Any, True, False

from Builtins export all

from Base.List export Nil, Cons
from Base.Number.Extensions export all hiding Math

polyglot java import com.ibm.icu.text.BreakIterator
polyglot java import org.enso.base.Text_Utils
"#);
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::Line(
                    vec![
                        Token::Variable("import",0),
                        Token::Referent("Base",1),
                        Token::Operator(".",0),
                        Token::Referent("List",0),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Variable("import",0),
                        Token::Referent("Base",1),
                        Token::Operator(".",0),
                        Token::Referent("Number",0),
                        Token::Operator(".",0),
                        Token::Referent("Extensions",0),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Variable("from",0),
                        Token::Referent("Builtins",1),
                        Token::Variable("import",1),
                        Token::Referent("Unit",1),
                        Token::Operator(",",0),
                        Token::Referent("Number",1),
                        Token::Operator(",",0),
                        Token::Referent("Integer",1),
                        Token::Operator(",",0),
                        Token::Referent("Any",1),
                        Token::Operator(",",0),
                        Token::Referent("True",1),
                        Token::Operator(",",0),
                        Token::Referent("False",1),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::Variable("from",0),
                        Token::Referent("Builtins",1),
                        Token::Variable("export",1),
                        Token::Variable("all",1),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::Variable("from",0),
                        Token::Referent("Base",1),
                        Token::Operator(".",0),
                        Token::Referent("List",0),
                        Token::Variable("export",1),
                        Token::Referent("Nil",1),
                        Token::Operator(",",0),
                        Token::Referent("Cons",1),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Variable("from",0),
                        Token::Referent("Base",1),
                        Token::Operator(".",0),
                        Token::Referent("Number",0),
                        Token::Operator(".",0),
                        Token::Referent("Extensions",0),
                        Token::Variable("export",1),
                        Token::Variable("all",1),
                        Token::Variable("hiding",1),
                        Token::Referent("Math",1),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::Variable("polyglot",0),
                        Token::Variable("java",1),
                        Token::Variable("import",1),
                        Token::Variable("com",1),
                        Token::Operator(".",0),
                        Token::Variable("ibm",0),
                        Token::Operator(".",0),
                        Token::Variable("icu",0),
                        Token::Operator(".",0),
                        Token::Variable("text",0),
                        Token::Operator(".",0),
                        Token::External("BreakIterator",0),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::Line(
                    vec![
                        Token::Variable("polyglot",0),
                        Token::Variable("java",1),
                        Token::Variable("import",1),
                        Token::Variable("org",1),
                        Token::Operator(".",0),
                        Token::Variable("enso",0),
                        Token::Operator(".",0),
                        Token::Variable("base",0),
                        Token::Operator(".",0),
                        Token::Referent("Text_Utils",0),
                    ],
                    0,
                    token::LineEnding::LF
                ),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}

#[test]
fn some_stdlib() {
    let input = make_unix_line_endings(
r#"from Base import all

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
"#);
    let expected = token::Stream::from(vec![
        Token::Block(
            token::BlockType::Continuous,
            0,
            vec![
                Token::Line(
                    vec![
                        Token::Variable("from",0),
                        Token::Referent("Base",1),
                        Token::Variable("import",1),
                        Token::Variable("all",1),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::DocComment(
                            vec![
                                Token::Line(
                                    vec![
                                        Token::TextSegmentRaw(
                                            "The top-level entry point for a test suite.",
                                            0
                                        ),
                                    ],
                                    0,
                                    token::LineEnding::LF,
                                )
                            ],
                            3,
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                ),
                Token::Line(
                    vec![
                        Token::Variable("type",0),
                        Token::Referent("Suite",1),
                        Token::Variable("specs",1),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::DocComment(
                            vec![
                                Token::Line(
                                    vec![Token::TextSegmentRaw("PRIVATE",0),],
                                    0,
                                    token::LineEnding::LF,
                                )
                            ],
                            3,
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                ),
                Token::Line(
                    vec![
                        Token::Variable("type",0),
                        Token::Referent("Spec",1),
                        Token::Variable("name",1),
                        Token::Variable("behaviors",1)
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::DocComment(
                            vec![
                                Token::Line(
                                    vec![Token::TextSegmentRaw("PRIVATE",0),],
                                    0,
                                    token::LineEnding::LF,
                                )
                            ],
                            3,
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                ),
                Token::Line(
                    vec![
                        Token::Variable("type",0),
                        Token::Referent("Behavior",1),
                        Token::Variable("name",1),
                        Token::Variable("result",1)
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::DocComment(
                            vec![
                                Token::Line(
                                    vec![Token::TextSegmentRaw("PRIVATE",0),],
                                    0,
                                    token::LineEnding::LF,
                                )
                            ],
                            3,
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                ),
                Token::Line(
                    vec![
                        Token::Referent("Behavior",0),
                        Token::Operator(".",0),
                        Token::Variable("is_fail",0),
                        Token::Operator("=",1),
                        Token::Variable("this",1),
                        Token::Operator(".",0),
                        Token::Variable("result",0),
                        Token::Operator(".",0),
                        Token::Variable("is_fail",0),
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::DocComment(
                            vec![
                                Token::Line(
                                    vec![Token::TextSegmentRaw("PRIVATE",0),],
                                    0,
                                    token::LineEnding::LF,
                                )
                            ],
                            3,
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                ),
                Token::Line(
                    vec![
                        Token::Referent("Spec",0),
                        Token::Operator(".",0),
                        Token::Variable("is_fail",0),
                        Token::Operator("=",1),
                        Token::Variable("this",1),
                        Token::Operator(".",0),
                        Token::Variable("behaviors",0),
                        Token::Operator(".",0),
                        Token::Variable("any",0),
                        Token::Variable("is_fail",1)
                    ],
                    0,
                    token::LineEnding::LF
                ),
                Token::BlankLine(0,token::LineEnding::LF),
                Token::Line(
                    vec![
                        Token::DocComment(
                            vec![
                                Token::Line(
                                    vec![Token::TextSegmentRaw("PRIVATE",0),],
                                    0,
                                    token::LineEnding::LF,
                                )
                            ],
                            3,
                            0
                        )
                    ],
                    0,
                    token::LineEnding::None
                ),
                Token::Line(
                    vec![
                        Token::Referent("Suite",0),
                        Token::Operator(".",0),
                        Token::Variable("is_fail",0),
                        Token::Operator("=",1),
                        Token::Variable("this",1),
                        Token::Operator(".",0),
                        Token::Variable("specs",0),
                        Token::Operator(".",0),
                        Token::Variable("any",0),
                        Token::Variable("is_fail",1)
                    ],
                    0,
                    token::LineEnding::LF
                ),
            ],
            0
        )
    ]);
    assert_lexes(input,expected);
}
