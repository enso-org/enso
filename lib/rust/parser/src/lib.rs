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

// === Features ===
#![feature(test)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]

use crate::prelude::*;

use crate::lexer::Lexer;
use crate::macros::resolver::RootContext;
use crate::source::Code;
use crate::syntax::token;
use crate::syntax::tree::SyntaxError;
use crate::syntax::Finish;

mod im_list;

// ==============
// === Export ===
// ==============

pub mod format;
pub mod lexer;
pub mod macros;
pub mod metadata;
pub mod serialization;
pub mod source;
pub mod syntax;



/// Popular utilities, imported by most modules of this crate.
pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_reflect as reflect;
    pub use enso_reflect::Reflect;
    pub(crate) use paste::paste;

    /// Return type for functions that will only fail in case of a bug in the implementation.
    #[derive(Debug, Default)]
    pub struct ParseResult<T> {
        /// The result of the operation. If `internal_error` is set, this is a best-effort value
        /// that cannot be assumed to be accurate; otherwise, it should be correct.
        pub value:          T,
        /// Internal error encountered while computing this result.
        pub internal_error: Option<String>,
    }

    impl<T> ParseResult<T> {
        /// Return a new [`ParseResult`] whose value is the result of applying the given function to
        /// the input's value, and whose `internal_error` field is the same as the input.
        pub fn map<U, F>(self, f: F) -> ParseResult<U>
        where F: FnOnce(T) -> U {
            let ParseResult { value, internal_error } = self;
            let value = f(value);
            ParseResult { value, internal_error }
        }

        /// Panic if the result contains an internal error; otherwise, return the contained value.
        pub fn unwrap(self) -> T {
            assert_eq!(self.internal_error, None);
            self.value
        }
    }
}


// ==============
// === Parser ===
// ==============

/// Enso parser. See the module documentation to learn more about how it works.
#[derive(Debug)]
pub struct Parser {
    macros: macros::resolver::MacroMap,
}

impl Parser {
    /// Constructor.
    pub fn new() -> Self {
        let macros = macros::built_in::all();
        Self { macros }
    }

    /// Main entry point. Interprets the input as a module, and returns the resulting [`BodyBlock`].
    pub fn parse_module<'s>(&self, code: &'s str) -> syntax::Tree<'s> {
        self.run(code, RootContext::Module)
    }

    /// Parses the input as a block.
    pub fn parse_block<'s>(&self, code: &'s str) -> syntax::Tree<'s> {
        self.run(code, RootContext::Block)
    }

    fn run<'s>(&self, code: &'s str, root_context: RootContext) -> syntax::Tree<'s> {
        let resolver = macros::resolver::Resolver::new(&self.macros, root_context);
        let ParseResult { value, internal_error } = Lexer::new(code, resolver).finish();
        if let Some(error) = internal_error {
            return value.with_error(format!("Internal error: {error}"));
        }
        value
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}


// == Parsing helpers ==

fn is_qualified_name(tree: &syntax::Tree) -> bool {
    use syntax::tree::*;
    match &tree.variant {
        Variant::Ident(_) => true,
        Variant::OprApp(app) => match &**app {
            OprApp { lhs: Some(lhs), opr: Ok(opr), rhs: Some(rhs) }
                if matches!(rhs.variant, Variant::Ident(_)) && opr.code.repr.0 == "." =>
                is_qualified_name(lhs),
            _ => false,
        },
        _ => false,
    }
}

fn expect_qualified_name(tree: syntax::Tree) -> syntax::Tree {
    if is_qualified_name(&tree) {
        tree
    } else {
        tree.with_error(SyntaxError::ExpectedQualifiedName)
    }
}

fn empty_tree(location: Code) -> syntax::Tree {
    syntax::Tree::ident(token::ident(location.clone(), location, false, 0, false, false, false))
}

fn expression_to_pattern(mut input: syntax::Tree<'_>) -> syntax::Tree<'_> {
    use syntax::tree::*;
    if let Variant::Wildcard(wildcard) = &mut input.variant {
        wildcard.de_bruijn_index = None;
        return input;
    }
    let mut error = None;
    match input.variant {
        // === Recursions ===
        Variant::Group(ref mut group) =>
            if let Group { body: Some(ref mut body), .. } = &mut **group {
                transform_tree(body, expression_to_pattern)
            },
        Variant::App(ref mut app) => match &mut **app {
            // === Special-case error ===
            App { func: Tree { variant: Variant::Ident(ref ident), .. }, .. }
                if !ident.token.is_type =>
                error = Some(SyntaxError::PatternUnexpectedExpression),
            App { ref mut func, ref mut arg } => {
                transform_tree(func, expression_to_pattern);
                transform_tree(arg, expression_to_pattern);
            }
        },
        Variant::TypeAnnotated(ref mut inner) =>
            transform_tree(&mut inner.expression, expression_to_pattern),
        Variant::OprApp(ref opr_app)
            if opr_app.opr.as_ref().ok().map_or(false, |o| o.code == ".") =>
            if !is_qualified_name(&input) {
                error = Some(SyntaxError::PatternUnexpectedDot);
            },

        // === Transformations ===
        Variant::TemplateFunction(func) => {
            let mut out = expression_to_pattern(func.ast);
            out.span.left_offset += input.span.left_offset;
            return out;
        }

        // === Unconditional and fallthrough errors ===
        Variant::AutoscopedIdentifier(_) => error = Some(SyntaxError::PatternUnexpectedExpression),
        Variant::OprApp(_) => error = Some(SyntaxError::PatternUnexpectedExpression),

        // === Unhandled ===
        _ => {}
    };
    maybe_with_error(input, error)
}

thread_local! {
    static DEFAULT_TREE: RefCell<Option<syntax::Tree<'static>>> = default();
}

fn transform_tree(tree: &mut syntax::Tree, f: impl FnOnce(syntax::Tree) -> syntax::Tree) {
    let default: syntax::Tree<'static> =
        DEFAULT_TREE.with(|default| default.borrow_mut().take()).unwrap_or_default();
    let original = mem::replace(tree, default);
    let transformed = f(original);
    let default_returned = mem::replace(tree, transformed);
    // This lifetime cast is sound because this is the same value as `default` above; its lifetime
    // was narrowed by the type system when it was stored in the `tree` reference.
    #[allow(unsafe_code)]
    let default_returned =
        unsafe { mem::transmute::<syntax::Tree<'_>, syntax::Tree<'static>>(default_returned) };
    DEFAULT_TREE.with(|default| *default.borrow_mut() = Some(default_returned));
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
            parser.parse_module(&str);
        });
    }

    #[bench]
    #[cfg(not(target_arch = "wasm32"))]
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
        bencher.bytes = str.len() as u64;
        bencher.iter(move || {
            parser.parse_module(&str);
        });
    }

    #[bench]
    #[cfg(not(target_arch = "wasm32"))]
    fn bench_expressions(bencher: &mut Bencher) {
        use rand::prelude::*;
        use rand_chacha::ChaCha8Rng;
        let lines = 100;
        let avg_group_len = 20;
        let avg_groups_per_line = 20;
        let mut str = String::new();
        let mut rng = ChaCha8Rng::seed_from_u64(0);
        let normal = rand_distr::StandardNormal;
        for _ in 0..lines {
            let operators = ['=', '+', '-', '*', ':'];
            let groups: f64 = normal.sample(&mut rng);
            let groups = (groups * avg_groups_per_line as f64) as usize;
            for _ in 0..groups {
                let len: f64 = normal.sample(&mut rng);
                let len = (len * avg_group_len as f64) as usize;
                str.push('x');
                for _ in 0..len {
                    let i = rng.gen_range(0..operators.len());
                    str.push(operators[i]);
                    str.push('x');
                }
                str.push(' ');
            }
            str.push('\n');
        }
        let parser = Parser::new();
        bencher.bytes = str.len() as u64;
        bencher.iter(move || {
            parser.parse_module(&str);
        });
    }
}
