//! The Enso parser. Parsing is a multi-stage process:
//!
//! # Lexing.
//! First, the source code is feed to [`lexer::Lexer`], which consumes it and outputs a stream of
//! [`Token`]. Tokens are chunks of the input with a generic description attached, like "operator",
//! or "identifier".
//!
//! # Building macro registry.
//! Macros in Enso are a very powerful mechanism and are used to transform group of tokens into
//! almost any statement. First, macros need to be discovered and registered. Currently, there is no
//! real macro discovery process, as there is no support for user-defined macros. Instead, there is
//! a set of hardcoded macros defined in the compiler.
//!
//! Each macro defines one or more segments. Every segment starts with a predefined token and can
//! contain any number of other tokens. For example, the macro `if ... then ... else ...` contains
//! three segments. Macros can also accept prefix tokens, a set of tokens on the left of the first
//! segment. A good example is the lambda macro `... -> ...`.
//!
//! In this step, a [`MacroMatchTree`] is built. Basically, it is a map from the possible next
//! segment name to information of what other segments are required and what is the macro definition
//! in case these segments were found. For example, let's consider two macros: `if ... then ...`,
//! and `if ... then ... else ...`. In such a case, the macro registry will contain only one entry,
//! "if", and two sets of possible resolution paths: ["then"], and ["then", "else"], each associated
//! with the corresponding macro definition.
//!
//! # Splitting the token stream by the macro segments.
//! The input token stream is being iterated and is being split based on the segments of the
//! registered macros. For example, for the input `if a b then c d else e f`, the token stream will
//! be split into three segments, `a b`, `c d`, and `e f`, which will be associated with the
//! `if ... then ... else ...` macro definition.
//!
//! The splitting process is hierarchical. It means that a new macro can start being resolved during
//! resolution of a parent macro. For example, `if if a then b then c else d` is a correct
//! expression. After finding the first `if` token, the token stream will be split. The next `if`
//! token starts a new token stream splitting. The first `then` token belongs to the nested macro,
//! however, as soon as the resolver sees the second `then` token, it will consider the nested macro
//! to be finished, and will come back to parent macro resolution.
//!
//! # Resolving right-hand-side patterns of macro segments.
//! In the next steps, each macro is being analyzed, started from the most nested ones. For each
//! macro, the [`Pattern`] of last segment is being run to check which tokens belong to that macro,
//! and which tokens should be transferred to parent macro definition. For example, consider the
//! following code `process (read file) content-> print content`. The `(...)` is a macro with two
//! sections `(` and `)`. Let's mark the token splitting with `[` and `]` characters. The previous
//! macro resolution steps would output such split of the token stream:
//! `process [(read file][) content[-> print content]]`. In this step, the most inner macro will be
//! analyzed first. The pattern of the last segment of the inner macro (`->`) defines that it
//! consumes all tokens, so all the tokens `print content` are left as they are. Now, the resolution
//! moves to the parent macro. Its last segment starts with the `)` token, which pattern defines
//! that it does not consume any tokens, so all of its current tokens (`content[-> print content]]`)
//! are popped to a parent definition, forming `process [(read file][)] content[-> print content]`.
//!
//! Please note, that root of the expression is considered a special macro as well. It is done for
//! the algorithm unification purposes.
//!
//! # Resolving left-hand-side patterns of macro segments.
//! In this step, each macro is being analyzed, started from the most nested ones. For each macro,
//! the [`Pattern`] of the macro prefix is being run to check which tokens belong to the prefix of
//! the macro (in case the macro defines the prefix). In the example above, the macro `->` defines
//! complex prefix rules: if the token on the left of the arrow used no space, then only a single
//! token will be consumed. As a result of this step, the following token split will occur:
//! `[process [(read file][)] [content-> print content]`, which is exactly what we wanted.
//!
//! # Resolving patterns of macro segments.
//! In this step, all macro segment patterns are being resolved and errors are reported in case it
//! was not possible. If tokens in a segment match the segment pattern, they are sent to the
//! operator precedence resolver for final transformation.
//!
//! # Operator precedence resolution.
//! Each token stream sent to the operator resolver is processed by a modified Shunting Yard
//! algorithm, which handles such situations as multiple operators placed next to each other,
//! multiple identifiers placed next to each other, and also takes spacing into consideration in
//! order to implement spacing-aware precedence rules. After all segments are resolved, the macro
//! is being treated as a single token in one of the segments of the parent macro, and is being
//! processed by the operator precedence resolver as well. In the end, a single [`syntax::Tree`] is
//! produced, containing the parsed expression.

#![recursion_limit = "256"]
// === Features ===
#![allow(incomplete_features)]
#![feature(allocator_api)]
#![feature(test)]
#![feature(specialization)]
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use crate::prelude::*;
use std::collections::VecDeque;

use crate::source::VisibleOffset;

use crate::macros::pattern::Match;
use crate::macros::pattern::MatchResult;
use crate::macros::pattern::MatchedSegment;
use enso_data_structures::im_list;
use enso_data_structures::im_list::List;
use lexer::Lexer;
use macros::pattern::Pattern;
use syntax::token;
use syntax::token::Token;


// ==============
// === Export ===
// ==============

pub mod lexer;
pub mod macros;
pub mod source;
pub mod syntax;



/// Popular utilities, imported by most modules of this crate.
pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_types::traits::*;
    pub use enso_types::unit2::Bytes;
}



fn matched_segments_into_multi_segment_app<'s>(
    matched_segments: NonEmptyVec<MatchedSegment<'s>>,
) -> syntax::Tree<'s> {
    let segments = matched_segments.mapped(|segment| {
        let header = segment.header;
        let tokens = segment.result.tokens();
        let body = (!tokens.is_empty())
            .as_some_from(|| macros::resolver::resolve_operator_precedence(tokens));
        syntax::tree::MultiSegmentAppSegment { header, body }
    });
    syntax::Tree::multi_segment_app(segments)
}



// =========================
// === Macro Definitions ===
// =========================

fn macro_if_then_else<'s>() -> macros::Definition<'s> {
    macro_definition! {
        ("if", Pattern::Everything, "then", Pattern::Everything, "else", Pattern::Everything)
        matched_segments_into_multi_segment_app
    }
}

fn macro_if_then<'s>() -> macros::Definition<'s> {
    macro_definition! {
        ("if", Pattern::Everything, "then", Pattern::Everything)
        matched_segments_into_multi_segment_app
    }
}

fn macro_group<'s>() -> macros::Definition<'s> {
    macro_definition! {
        ("(", Pattern::Everything, ")", Pattern::Nothing)
        matched_segments_into_multi_segment_app
    }
}

fn macro_type_def<'s>() -> macros::Definition<'s> {
    use macros::pattern::*;
    let pattern = Identifier / "name" % "type name"
        >> (Identifier / "param").many() % "type parameters"
        >> Block((Identifier / "constructor").many() % "type constructors" >> Everything)
            % "type definition body";
    // let pattern2 = Everything;
    macro_definition! {
        ("type", pattern)
        ttt
    }
}

fn ttt<'s>(matched_segments: NonEmptyVec<MatchedSegment<'s>>) -> syntax::Tree<'s> {
    let header = matched_segments.first().header.clone();
    println!(">>>");
    println!("{:#?}", matched_segments);
    println!(">>>");
    println!("{:#?}", matched_segments.mapped(|t| t.result.into_match_tree()));
    syntax::Tree::type_def(header)
}

fn builtin_macros() -> macros::resolver::MacroMatchTree<'static> {
    let mut macro_map = macros::resolver::MacroMatchTree::default();
    // macro_map.register(macro_if_then());
    // macro_map.register(macro_if_then_else());
    // macro_map.register(macro_group());
    // macro_map.register(macro_lambda());
    macro_map.register(macro_type_def());
    macro_map
}



// ============
// === Main ===
// ============

pub fn build_block_hierarchy<'s>(tokens: Vec<Token<'s>>) -> Vec<syntax::Item<'s>> {
    let mut stack = vec![];
    let mut out: Vec<syntax::Item<'s>> = vec![];
    for token in tokens {
        match token.variant {
            token::Variant::BlockStart(_) => stack.push(mem::take(&mut out)),
            token::Variant::BlockEnd(_) => {
                let new_out = stack.pop().unwrap();
                let block = mem::replace(&mut out, new_out);
                out.push(syntax::Item::Block(block));
            }
            _ => out.push(token.into()),
        }
    }
    if !stack.is_empty() {
        panic!("Internal error. Block start token not paired with block end token.");
    }
    out
}

pub fn lex<'a>(code: &'a str) -> Vec<syntax::Item<'a>> {
    let mut lexer = Lexer::new(code);
    lexer.run();
    build_block_hierarchy(lexer.output)
}

pub fn parse<'a>(code: &'a str) -> syntax::Tree<'a> {
    let mut tokens = lex(code);
    event!(TRACE, "Tokens:\n{:#?}", tokens);
    let root_macro_map = builtin_macros();
    event!(TRACE, "Registered macros:\n{:#?}", root_macro_map);
    let mut tokens = tokens.into_iter().peekable();
    let mut statements = vec![];
    while tokens.peek().is_some() {
        let resolver = macros::resolver::Resolver::new_root();
        let (tree, new_tokens) = resolver.run(&root_macro_map, tokens);
        statements.push(tree);
        tokens = new_tokens;
    }
    syntax::Tree::module(statements)
}

use enso_parser_syntax_tree_builder::ast_builder;

fn main() {
    init_tracing(TRACE);
    // let str = "if a then b else c";
    // let str = "if if * a + b * then y then b";
    // let str = "* a + b *";
    // let str = "* a + * b";
    // let str = "(a) (b) c";
    // let str = "if (a) then b";
    // let str = "foo a-> b";
    // let str = "a+b * c";
    // let str = "foo if a then b";
    // let str = "foo *(a)";
    // let ast = parse("foo if a then b else c");
    // let ast = parse("type Option a b c\n    None");
    let ast = parse("type Option a b c\ntype Option a b c");
    // let ast = parse("if if a then b then c else d");
    // let ast2 = ast_builder! {{if} a {then} b};
    // MultiSegmentApp<'s>(prefix: Option<Tree<'s>>, segments:
    // NonEmptyVec<MultiSegmentAppSegment<'s>>)

    println!("\n\n==================\n\n");

    // let ast2 = Token("", "foo", syntax::token::Variant::new_ident_unchecked("foo"));
    println!("{:#?}", ast);
    // println!("\n\n{}", ast.code());
    // println!("\n\n{:?}", ast2);

    // let tokens = lex("type Bool\n    True\n    False");
    // println!("{:#?}", tokens);

    // lexer::main();
}



// =============
// === Tests ===
// =============

// #[cfg(test)]
// mod test {
//     use super::*;
//
//     pub fn ident(repr: &str) -> syntax::Tree {
//         match token::Variant::to_ident_unchecked(repr) {
//             token::Variant::Ident(ident) => span::With::new_no_left_offset_no_start(
//                 Bytes::from(repr.len()),
//                 syntax::tree::Type::from(syntax::tree::Ident(ident)),
//             ),
//             _ => panic!(),
//         }
//     }
//
//     pub fn app_segment(
//         header: Token,
//         body: Option<syntax::Tree>,
//     ) -> syntax::tree::MultiSegmentAppSegment {
//         syntax::tree::MultiSegmentAppSegment { header, body }
//     }
// }
//
//
//
#[cfg(test)]
mod tests {
    use super::*;
    use enso_parser_syntax_tree_builder::ast_builder;

    //     fn one_shot(input: &str) -> syntax::Tree {
    //         let mut lexer = Lexer::new(input);
    //         lexer.run();
    //         let root_macro_map = builtin_macros();
    //         let resolver = Resolver::new_root();
    //         let ast = resolver.run(
    //             &lexer,
    //             &root_macro_map,
    //             lexer.output.borrow_vec().iter().map(|t| (*t).into()).collect_vec(),
    //         );
    //         ast
    //     }
    //
    macro_rules! test_parse {
            ($input:tt = {$($def:tt)*}) => {
                assert_eq!(
                    parse($input),
                    ast_builder! { $($def)* }
                )
            };
        }

    #[test]
    fn test_expressions() {
        test_parse! {"a" = {a}};
        test_parse! {"a b" = {a b}};
        test_parse! {"a b c" = {[a b] c}};
        // test_parse!("if a then b" = { {if} a {then} b });
        // test_parse!("if a then b else c" = { {if} a {then} b {else} c });
        // test_parse!("if a b then c d else e f" = { {if} a b {then} c d {else} e f });
    }
}



#[cfg(test)]
mod benches {
    use super::*;
    extern crate test;
    use test::Bencher;

    #[bench]
    fn bench_str_iter_and_compare(b: &mut Bencher) {
        let reps = 1_000;
        let str = "type Option a b c\n".repeat(reps);

        b.iter(move || {
            let ast = parse(&str);
        });
    }
}
