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
#![feature(exact_size_is_empty)]
#![feature(test)]
#![feature(specialization)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(box_patterns)]
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


// ==============
// === Export ===
// ==============

pub mod lexer;
pub mod macros;
pub mod serialization;
pub mod source;
pub mod syntax;



/// Popular utilities, imported by most modules of this crate.
pub mod prelude {
    pub use enso_prelude::serde_reexports::*;
    pub use enso_prelude::*;
    pub use enso_reflect as reflect;
    pub use enso_reflect::Reflect;
    pub use enso_types::traits::*;
    pub use enso_types::unit2::Bytes;
}



// ==============
// === Parser ===
// ==============

/// Enso parser. See the module documentation to learn more about how it works.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Parser {
    pub macros: macros::resolver::SegmentMap<'static>,
}

impl Parser {
    /// Constructor.
    pub fn new() -> Self {
        let macros = macros::built_in::all();
        Self { macros }
    }

    /// Main entry point.
    pub fn run<'s>(&self, code: &'s str) -> syntax::Tree<'s> {
        let tokens = lexer::run(code);
        let resolver = macros::resolver::Resolver::new_root();
        resolver.run(&self.macros, tokens)
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}


// == Parsing helpers ==

/// Reinterpret an expression in a statement context (i.e. as a top level member of a block).
///
/// In statement context, an expression that has an assignment operator at its top level is
/// interpreted as a variable assignment or method definition.
fn expression_to_statement(mut tree: syntax::Tree<'_>) -> syntax::Tree<'_> {
    use syntax::tree::*;
    let tree_ = match &mut *tree.variant {
        Variant::OprSectionBoundary(OprSectionBoundary { ast }) => ast,
        _ => &mut tree,
    };
    let opr_app = match &mut *tree_.variant {
        Variant::OprApp(opr_app) => opr_app,
        _ => return tree,
    };
    if let OprApp { lhs: Some(lhs), opr: Ok(opr), rhs } = opr_app && opr.code == "=" {
        let mut args = vec![];
        let mut lhs = lhs;
        while let Tree { variant: box Variant::App(App { func, arg }), .. } = lhs {
            lhs = func;
            args.push(arg.clone());
        }
        args.reverse();
        if args.is_empty() && let Some(rhs) = rhs && !is_body_block(rhs) {
            // If the LHS has no arguments, and there is a RHS, and the RHS is not a body block,
            // this is a variable assignment.
            return Tree::assignment(mem::take(lhs), mem::take(opr), mem::take(rhs))
        }
        if let Variant::Ident(Ident { token }) = &mut *lhs.variant {
            // If this is not a variable assignment, and the leftmost leaf of the `App` tree is
            // an identifier, this is a function definition.
            return Tree::function(mem::take(token), args, mem::take(opr), mem::take(rhs))
        }
    }
    tree
}

/// Return whether the expression is a body block.
fn is_body_block(expression: &syntax::tree::Tree<'_>) -> bool {
    matches!(&*expression.variant, syntax::tree::Variant::BodyBlock { .. })
}



// ==================
// === Benchmarks ===
// ==================

#[cfg(test)]
mod benches {
    use super::*;
    extern crate test;
    use test::Bencher;

    #[bench]
    fn bench_parsing_type_defs(bencher: &mut Bencher) {
        let reps = 1_000;
        let str = "type Option a b c\n".repeat(reps);
        let parser = Parser::new();
        bencher.iter(move || {
            parser.run(&str);
        });
    }

    #[bench]
    fn bench_blocks(bencher: &mut Bencher) {
        use rand::prelude::*;
        use rand_chacha::ChaCha8Rng;
        let lines = 10_000;
        let mut str = String::new();
        let mut rng = ChaCha8Rng::seed_from_u64(0);
        let mut indent = 0u32;
        for _ in 0..lines {
            // Indent:
            // 1/8 chance of increasing.
            // 1/8 chance of decreasing.
            // 3/4 chance of leaving unchanged.
            match rng.gen_range(0..8) {
                0u32 => indent = indent.saturating_sub(1),
                1 => indent += 1,
                _ => (),
            }
            for _ in 0..indent {
                str.push(' ');
            }
            // 1/4 chance of operator-block line syntax.
            if rng.gen_range(0..4) == 0u32 {
                str.push_str("* ");
            }
            str.push('x');
            // Equal chance of the next line being interpreted as a body block or argument block
            // line, if it is indented and doesn't match the operator-block syntax.
            // The `=` operator is chosen to exercise the expression-to-statement conversion path.
            if rng.gen() {
                str.push_str(" =");
            }
            str.push('\n');
        }
        let parser = Parser::new();
        bencher.iter(move || {
            parser.run(&str);
        });
    }
}
