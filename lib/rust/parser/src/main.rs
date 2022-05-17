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

use crate::source::VisibleOffset;

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



// =================================
// === SyntaxItemOrMacroResolver ===
// =================================

/// One of [`syntax::Item`] or [`MacroResolver`].
#[derive(Debug)]
#[allow(missing_docs)]
pub enum SyntaxItemOrMacroResolver<'s> {
    SyntaxItem(syntax::Item<'s>),
    MacroResolver(MacroResolver<'s>),
}

impl<'s> From<syntax::Item<'s>> for SyntaxItemOrMacroResolver<'s> {
    fn from(t: syntax::Item<'s>) -> Self {
        Self::SyntaxItem(t)
    }
}

impl<'s> From<MacroResolver<'s>> for SyntaxItemOrMacroResolver<'s> {
    fn from(t: MacroResolver<'s>) -> Self {
        Self::MacroResolver(t)
    }
}

impl<'s> TryAsRef<syntax::Item<'s>> for SyntaxItemOrMacroResolver<'s> {
    fn try_as_ref(&self) -> Option<&syntax::Item<'s>> {
        match self {
            Self::SyntaxItem(t) => Some(t),
            _ => None,
        }
    }
}


// ======================
// === MacroMatchTree ===
// ======================

/// A tree-like structure encoding potential macro matches. The keys are representations of tokens
/// that can be matched. For example, the key could be "if" or "->". Each key is associated with one
/// or more [`PartiallyMatchedMacro`], which stories a list of required segments and a macro
/// definition in case all the segments were matched. For example, for the "if" key, there can be
/// two required segment lists, one for "then" and "else" segments, and one for the "then" segment
/// only.
#[derive(Default, Debug, Deref, DerefMut)]
pub struct MacroMatchTree<'s> {
    map: HashMap<&'s str, NonEmptyVec<PartiallyMatchedMacro<'s>>>,
}

/// Partially matched macro info. See docs of [`MacroMatchTree`] to learn more.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct PartiallyMatchedMacro<'s> {
    pub required_segments: List<macros::SegmentDefinition<'s>>,
    pub definition:        Rc<macros::Definition<'s>>,
}

impl<'a> MacroMatchTree<'a> {
    /// Register a new macro definition in this macro tree.
    pub fn register(&mut self, definition: macros::Definition<'a>) {
        let header = definition.segments.head.header;
        let entry = PartiallyMatchedMacro {
            required_segments: definition.segments.tail.clone(),
            definition:        Rc::new(definition),
        };
        if let Some(node) = self.get_mut(header) {
            node.push(entry);
        } else {
            self.insert(header, NonEmptyVec::singleton(entry));
        }
    }
}



// =====================
// === MacroResolver ===
// =====================

/// Enso macro resolver. See the docs of the main module to learn more about the macro resolution
/// steps.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct MacroResolver<'s> {
    pub current_segment:        MatchedSegment<'s>,
    pub resolved_segments:      Vec<MatchedSegment<'s>>,
    pub possible_next_segments: MacroMatchTree<'s>,
    pub matched_macro_def:      Option<Rc<macros::Definition<'s>>>,
}

impl<'a> MacroResolver<'a> {
    /// A new macro resolver with a special "root" segment definition. The "root" segment does not
    /// exist in the source code, it is simply the whole expression being parsed. It is treated
    /// as a macro in order to unify the algorithms.
    pub fn new_root() -> Self {
        let current_segment =
            MatchedSegment { header: Token("", "", token::Variant::newline()), body: default() };
        let resolved_segments = default();
        let possible_next_segments = default();
        let matched_macro_def = Some(Rc::new(macros::Definition {
            rev_prefix_pattern: None,
            segments:           im_list::NonEmpty::singleton(macros::SegmentDefinition {
                header:  "__ROOT__",
                pattern: Pattern::Everything,
            }),
            body:               Rc::new(|_, v| {
                if v.len() != 1 {
                    panic!()
                }
                let t = v.into_vec().pop().unwrap().1;
                resolve_operator_precedence(t)
            }),
        }));
        Self { current_segment, resolved_segments, possible_next_segments, matched_macro_def }
    }
}

/// A matched macro segment. Partial macro resolution product.
#[derive(Debug)]
pub struct MatchedSegment<'s> {
    header: Token<'s>,
    body:   Vec<SyntaxItemOrMacroResolver<'s>>,
}

impl<'s> MatchedSegment<'s> {
    /// Constructor.
    pub fn new(header: Token<'s>) -> Self {
        let body = default();
        Self { header, body }
    }
}


/// Main macro resolver capable of resolving nested macro usages. See the docs of the main module to
/// learn more about the macro resolution steps.
#[derive(Debug)]
pub struct Resolver<'s> {
    current_macro: MacroResolver<'s>,
    macro_stack:   Vec<MacroResolver<'s>>,
}

/// Result of the macro resolution step.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ResolverStep {
    NormalToken,
    NewSegmentStarted,
    MacroStackPop,
}

impl<'s> Resolver<'s> {
    fn new_root() -> Self {
        let current_macro = MacroResolver::new_root();
        let macro_stack = default();
        Self { current_macro, macro_stack }
    }

    fn run(
        mut self,
        lexer: &Lexer<'s>,
        root_macro_map: &MacroMatchTree<'s>,
        tokens: Vec<syntax::Item<'s>>,
    ) -> syntax::Tree<'s> {
        let mut stream = tokens.into_iter();
        let mut opt_token: Option<syntax::Item<'s>>;
        macro_rules! next_token {
            () => {{
                opt_token = stream.next();
                if let Some(token) = opt_token.as_ref() {
                    event!(TRACE, "New token {:#?}", token);
                }
            }};
        }
        macro_rules! trace_state {
            () => {
                event!(TRACE, "Current macro:\n{:#?}", self.current_macro);
                event!(TRACE, "Parent macros:\n{:#?}", self.macro_stack);
            };
        }
        next_token!();
        while let Some(token) = opt_token {
            let step_result = match &token {
                // FIXME: clone?
                syntax::Item::Token(token) => self.process_token(root_macro_map, token.clone()),
                _ => ResolverStep::NormalToken,
            };
            match step_result {
                ResolverStep::MacroStackPop => {
                    trace_state!();
                    opt_token = Some(token)
                }
                ResolverStep::NewSegmentStarted => {
                    trace_state!();
                    next_token!()
                }
                ResolverStep::NormalToken => {
                    self.current_macro.current_segment.body.push(token.into());
                    trace_state!();
                    next_token!();
                }
            }
        }

        while let Some(parent_macro) = self.macro_stack.pop() {
            self.replace_current_with_parent_macro(parent_macro);
        }

        trace_state!();

        Self::resolve(lexer, self.current_macro, None)
    }

    fn replace_current_with_parent_macro(&mut self, mut parent_macro: MacroResolver<'s>) {
        mem::swap(&mut parent_macro, &mut self.current_macro);
        let mut child_macro = parent_macro;
        if let Some(def) = &child_macro.matched_macro_def {
            let pattern = &def.segments.last().pattern;
            let child_tokens = mem::take(&mut child_macro.current_segment.body);
            // FIXME: the first [`false`] below is invalid.
            let match_result = pattern.resolve(child_tokens, false, false).unwrap();
            let mut new_child_tokens = match_result.matched;
            let new_parent_tokens = match_result.rest;
            mem::swap(&mut child_macro.current_segment.body, &mut new_child_tokens);
            self.current_macro.current_segment.body.push(child_macro.into());
            self.current_macro.current_segment.body.extend(new_parent_tokens);
        } else {
            panic!()
        }
    }

    fn resolve(
        lexer: &Lexer<'s>,
        m: MacroResolver<'s>,
        prefix_tokens: Option<Vec<syntax::Item<'s>>>,
    ) -> syntax::Tree<'s> {
        let segments = NonEmptyVec::new_with_last(m.resolved_segments, m.current_segment);
        let sss: NonEmptyVec<(Token, Vec<syntax::Item<'s>>)> = segments.mapped(|segment| {
            let mut ss: Vec<syntax::Item<'s>> = vec![];
            for item in segment.body {
                let resolved_token = match item {
                        SyntaxItemOrMacroResolver::MacroResolver(m2) => {
                            if let Some(macro_def) = &m2.matched_macro_def
                            && let Some(pfx_pattern) = &macro_def.rev_prefix_pattern {
                                ss.reverse();
                                let spacing = m2.current_segment.header.left_offset.visible > VisibleOffset(0);
                                let mut match_result = pfx_pattern.resolve(ss,spacing,true).unwrap();
                                match_result.matched.reverse();
                                ss = match_result.rest;
                                ss.reverse();
                                Self::resolve(lexer, m2, Some(match_result.matched)).into()
                            } else {
                                Self::resolve(lexer, m2, None).into()
                            }
                        },
                        SyntaxItemOrMacroResolver::SyntaxItem(t) => t,
                    };
                ss.push(resolved_token);
            }
            (segment.header, ss)
        });

        if let Some(macro_def) = m.matched_macro_def {
            (macro_def.body)(prefix_tokens, sss)
        } else {
            todo!("Handling non-fully-resolved macros")
        }
    }

    fn pop_macro_stack_if_reserved(&mut self, repr: &str) -> Option<MacroResolver<'s>> {
        let reserved = self.macro_stack.iter().any(|p| p.possible_next_segments.contains_key(repr));
        if reserved {
            self.macro_stack.pop()
        } else {
            None
        }
    }

    fn process_token(
        &mut self,
        root_macro_map: &MacroMatchTree<'s>,
        token: Token<'s>,
    ) -> ResolverStep {
        let repr = &**token.code;
        if let Some(subsegments) = self.current_macro.possible_next_segments.get(repr) {
            event!(TRACE, "Entering next segment of the current macro.");
            let mut new_match_tree =
                Self::enter(&mut self.current_macro.matched_macro_def, subsegments);
            let mut current_segment = MatchedSegment::new(token);
            mem::swap(&mut new_match_tree, &mut self.current_macro.possible_next_segments);
            mem::swap(&mut self.current_macro.current_segment, &mut current_segment);
            self.current_macro.resolved_segments.push(current_segment);
            ResolverStep::NewSegmentStarted
        } else if let Some(parent_macro) = self.pop_macro_stack_if_reserved(repr) {
            event!(TRACE, "Next token reserved by parent macro. Resolving current macro.");
            self.replace_current_with_parent_macro(parent_macro);
            ResolverStep::MacroStackPop
        } else if let Some(segments) = root_macro_map.get(repr) {
            event!(TRACE, "Starting a new nested macro resolution.");
            let mut matched_macro_def = default();
            let mut current_macro = MacroResolver {
                current_segment: MatchedSegment { header: token, body: default() },
                resolved_segments: default(),
                possible_next_segments: Self::enter(&mut matched_macro_def, segments),
                matched_macro_def,
            };
            mem::swap(&mut self.current_macro, &mut current_macro);
            self.macro_stack.push(current_macro);
            ResolverStep::NewSegmentStarted
        } else {
            event!(TRACE, "Consuming token as current segment body.");
            ResolverStep::NormalToken
        }
    }

    fn enter(
        matched_macro_def: &mut Option<Rc<macros::Definition<'s>>>,
        path: &[PartiallyMatchedMacro<'s>],
    ) -> MacroMatchTree<'s> {
        *matched_macro_def = None;
        let mut new_section_tree = MacroMatchTree::default();
        for v in path {
            if let Some(first) = v.required_segments.head() {
                let tail = v.required_segments.tail().cloned().unwrap_or_default();
                let definition = v.definition.clone_ref();
                let x = PartiallyMatchedMacro { required_segments: tail, definition };
                if let Some(node) = new_section_tree.get_mut(&first.header) {
                    node.push(x);
                } else {
                    new_section_tree.insert(first.header, NonEmptyVec::singleton(x));
                }
            } else {
                if matched_macro_def.is_some() {
                    event!(ERROR, "Internal error. Duplicate macro definition.");
                }
                *matched_macro_def = Some(v.definition.clone_ref());
            }
        }
        new_section_tree
    }
}


// FIXME: hardcoded values + not finished implementation.
fn precedence_of(operator: &str) -> usize {
    match operator {
        "+" => 3,
        "-" => 3,
        "*" => 7,
        _ => panic!("Operator not supported: {}", operator),
    }
}
//
#[derive(Clone, Copy, Debug, Deref, DerefMut)]
struct WithPrecedence<T> {
    #[deref]
    #[deref_mut]
    elem:       T,
    precedence: usize,
}

impl<T> WithPrecedence<T> {
    pub fn new(precedence: usize, elem: T) -> Self {
        Self { elem, precedence }
    }
}


fn annotate_tokens_that_need_spacing(items: Vec<syntax::Item>) -> Vec<syntax::Item> {
    items
        .into_iter()
        .map(|item| match item {
            syntax::Item::Token(_) => item,
            syntax::Item::Tree(ast) =>
                match &*ast.variant {
                    syntax::tree::Variant::MultiSegmentApp(data) => {
                        if data.segments.first().header.variant.marker()
                            != token::variant::VariantMarker::Symbol
                        {
                            syntax::Item::Tree(ast.with_error(
                                "This expression cannot be used in a non-spaced equation.",
                            ))
                        } else {
                            syntax::Item::Tree(ast)
                        }
                    }
                    _ => syntax::Item::Tree(ast),
                },
        })
        .collect()
}

fn resolve_operator_precedence<'s>(items: Vec<syntax::Item<'s>>) -> syntax::Tree<'s> {
    type Tokens<'s> = Vec<syntax::Item<'s>>;
    let mut flattened: Tokens<'s> = default();
    let mut no_space_group: Tokens<'s> = default();
    let processs_no_space_group = |flattened: &mut Tokens<'s>, no_space_group: &mut Tokens<'s>| {
        let tokens = mem::take(no_space_group);
        if tokens.len() == 1 {
            flattened.extend(tokens);
        } else {
            let tokens = annotate_tokens_that_need_spacing(tokens);
            let ast = resolve_operator_precedence_internal(tokens);
            flattened.push(ast.into());
        }
    };
    for item in items {
        if item.span().left_offset.visible.width_in_spaces == 0 || no_space_group.is_empty() {
            no_space_group.push(item)
        } else if !no_space_group.is_empty() {
            processs_no_space_group(&mut flattened, &mut no_space_group);
            no_space_group.push(item);
        } else {
            // FIXME: this is unreachable.
            flattened.push(item);
        }
    }
    if !no_space_group.is_empty() {
        processs_no_space_group(&mut flattened, &mut no_space_group);
    }
    resolve_operator_precedence_internal(flattened)
}

fn resolve_operator_precedence_internal(items: Vec<syntax::Item<'_>>) -> syntax::Tree<'_> {
    // Reverse-polish notation encoding.
    let mut output: Vec<syntax::Item> = default();
    let mut operator_stack: Vec<WithPrecedence<syntax::tree::OperatorOrError>> = default();
    let mut last_token_was_ast = false;
    let mut last_token_was_opr = false;
    for item in items {
        let i2 = item.clone(); // FIXME
        if let syntax::Item::Token(token) = i2 && let token::Variant::Operator(opr) = token.variant {
            // Item is an operator.
            let last_token_was_opr_copy = last_token_was_opr;
            last_token_was_ast = false;
            last_token_was_opr = true;

            let prec = precedence_of(&token.code);
            let opr = Token(token.left_offset, token.code, opr);
            // let opr = item.span().with(opr);

            if last_token_was_opr_copy && let Some(prev_opr) = operator_stack.last_mut() {
                // Error. Multiple operators next to each other.
                match &mut prev_opr.elem {
                    Err(err) => err.operators.push(opr),
                    Ok(prev) => {
                        let operators = NonEmptyVec::new(prev.clone(),vec![opr]); // FIXME: clone?
                        prev_opr.elem = Err(syntax::tree::MultipleOperatorError{operators});
                    }
                }
            } else {
                while let Some(prev_opr) = operator_stack.last()
                   && prev_opr.precedence >= prec
                   && let Some(prev_opr) = operator_stack.pop()
                   && let Some(rhs) = output.pop()
                {
                    // Prev operator in the [`operator_stack`] has a higher precedence.
                    let lhs = output.pop().map(token_to_ast);
                    let ast = syntax::Tree::opr_app(lhs, prev_opr.elem, Some(token_to_ast(rhs)));
                    output.push(ast.into());
                }
                operator_stack.push(WithPrecedence::new(prec, Ok(opr)));
            }
        } else if last_token_was_ast && let Some(lhs) = output.pop() {
            // Multiple non-operators next to each other.
            let lhs = token_to_ast(lhs);
            let rhs = token_to_ast(item);
            let ast = syntax::Tree::app(lhs, rhs);
            output.push(ast.into());
        } else {
            // Non-operator that follows previously consumed operator.
            last_token_was_ast = true;
            last_token_was_opr = false;
            output.push(item);
        }
    }
    let mut opt_rhs = last_token_was_ast.and_option_from(|| output.pop().map(token_to_ast));
    while let Some(opr) = operator_stack.pop() {
        let opt_lhs = output.pop().map(token_to_ast);
        opt_rhs = Some(syntax::Tree::opr_app(opt_lhs, opr.elem, opt_rhs));
    }
    if !output.is_empty() {
        panic!(
            "Internal error. Not all tokens were consumed while constructing the
expression."
        );
    }
    syntax::Tree::opr_section_boundary(opt_rhs.unwrap()) // fixme
}

fn token_to_ast(elem: syntax::Item) -> syntax::Tree {
    match elem {
        syntax::Item::Token(token) => match token.variant {
            token::Variant::Ident(ident) => {
                let ii2 = token.with_variant(ident);
                syntax::tree::Tree::ident(ii2)
            }
            _ => panic!(),
        },
        syntax::Item::Tree(ast) => ast,
    }
}

fn matched_segments_into_multi_segment_app<'s>(
    prefix_tokens: Option<Vec<syntax::Item<'s>>>,
    matched_segments: NonEmptyVec<(Token<'s>, Vec<syntax::Item<'s>>)>,
) -> syntax::Tree<'s> {
    // FIXME: remove into_vec and use NonEmptyVec::mapped
    let segments = matched_segments
        .into_vec()
        .into_iter()
        .map(|segment| {
            let header = segment.0;
            let body =
                (!segment.1.is_empty()).as_some_from(|| resolve_operator_precedence(segment.1));
            syntax::tree::MultiSegmentAppSegment { header, body }
        })
        .collect_vec();
    if let Ok(segments) = NonEmptyVec::try_from(segments) {
        let prefix = prefix_tokens.map(resolve_operator_precedence);
        syntax::Tree::multi_segment_app(prefix, segments)
    } else {
        panic!()
    }
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

fn macro_lambda<'s>() -> macros::Definition<'s> {
    let prefix = Pattern::Or(
        Box::new(Pattern::Item(macros::pattern::Item { has_rhs_spacing: Some(false) })),
        Box::new(Pattern::Everything),
    );
    macro_definition! {
        (prefix, "->", Pattern::Everything)
        matched_segments_into_multi_segment_app
    }
}

fn builtin_macros() -> MacroMatchTree<'static> {
    let mut macro_map = MacroMatchTree::default();
    macro_map.register(macro_if_then());
    macro_map.register(macro_if_then_else());
    macro_map.register(macro_group());
    macro_map.register(macro_lambda());
    macro_map
}



// ============
// === Main ===
// ============

// fn main() {
//     lexer::lexer_main();
// }

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
    let str = "foo if a then b else c";
    let mut lexer = Lexer::new(str);
    lexer.run();

    let root_macro_map = builtin_macros();

    event!(TRACE, "Registered macros:\n{:#?}", root_macro_map);

    let resolver = Resolver::new_root();
    let ast = resolver.run(
        &lexer,
        &root_macro_map,
        lexer.output.iter().map(|t| t.clone().into()).collect_vec(),
    );
    println!("{:#?}", ast);
    println!("\n\n{}", ast.code());

    println!("\n\n==================\n\n");

    lexer::main();
}
//
//
//
// // =============
// // === Tests ===
// // =============
//
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
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use enso_parser_syntax_tree_builder::ast_builder;
//
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
//     macro_rules! test_parse {
//         ($input:tt = {$($def:tt)*}) => {
//             assert_eq!(
//                 one_shot($input).with_removed_span_info(),
//                 ast_builder! { $($def)* }.with_removed_span_info()
//             )
//         };
//     }
//
//     #[test]
//     fn test_expressions() {
//         test_parse!("if a then b" = { {if} a {then} b });
//         test_parse!("if a then b else c" = { {if} a {then} b {else} c });
//         test_parse!("if a b then c d else e f" = { {if} a b {then} c d {else} e f });
//     }
// }
