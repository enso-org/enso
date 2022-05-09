//! Enso parser.
//! TODO: Process description

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

use enso_data_structures::im_list;
use enso_data_structures::im_list::List;
use macros::pattern::Pattern;
use source::span;
use syntax::token::Token;


// ==============
// === Export ===
// ==============

pub mod lexer;
pub mod macros;
pub mod source;
pub mod syntax;

use lexer::Lexer;
use syntax::token;


/// Popular utilities, imported by most modules of this crate.
pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_types::traits::*;
    pub use enso_types::Bytes;
}



#[derive(Debug)]
pub enum TokenOrAstOrMacroResolver<'a> {
    SyntaxItem(syntax::Item),
    MacroResolver(MacroResolver<'a>),
}

impl<'a> From<syntax::Item> for TokenOrAstOrMacroResolver<'a> {
    fn from(t: syntax::Item) -> Self {
        Self::SyntaxItem(t)
    }
}

impl<'a> From<MacroResolver<'a>> for TokenOrAstOrMacroResolver<'a> {
    fn from(t: MacroResolver<'a>) -> Self {
        Self::MacroResolver(t)
    }
}

impl<'a> TryAsRef<syntax::Item> for TokenOrAstOrMacroResolver<'a> {
    fn try_as_ref(&self) -> Option<&syntax::Item> {
        match self {
            Self::SyntaxItem(t) => Some(t),
            _ => None,
        }
    }
}

impl<'a> source::HasRepr<'a> for source::With<'a, &syntax::Item> {
    fn repr(&self) -> &'a str {
        match self.data {
            syntax::Item::Token(t) => self.with_data(t).repr(),
            syntax::Item::Tree(t) => self.with_data(t).repr(),
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
pub struct MacroMatchTree<'a> {
    map: HashMap<&'a str, NonEmptyVec<PartiallyMatchedMacro<'a>>>,
}

/// Partially matched macro info. See docs of [`MacroMatchTree`] to learn more.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct PartiallyMatchedMacro<'a> {
    pub required_segments: List<macros::SegmentDefinition<'a>>,
    pub definition:        Rc<macros::Definition<'a>>,
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

#[derive(Debug)]
pub struct MacroResolver<'a> {
    current_segment:        MatchedSegment<'a>,
    resolved_segments:      Vec<MatchedSegment<'a>>,
    possible_next_segments: MacroMatchTree<'a>,
    matched_macro_def:      Option<Rc<macros::Definition<'a>>>,
}

impl<'a> MacroResolver<'a> {
    pub fn new_root() -> Self {
        let current_segment = MatchedSegment {
            header: span::With::new_no_left_offset_no_len(Bytes::from(0), token::Type::newline()),
            body:   default(),
        };
        let resolved_segments = default();
        let possible_next_segments = default();
        let matched_macro_def = Some(Rc::new(macros::Definition {
            rev_prefix_pattern: None,
            segments:           im_list::NonEmpty::singleton(macros::SegmentDefinition {
                header:  "__ROOT__",
                pattern: Pattern::Everything,
            }),
            body:               Rc::new(|lexer, _, v| {
                if v.len() != 1 {
                    panic!()
                }
                let t = v.into_vec().pop().unwrap().1;
                resolve_operator_precedence(lexer, t)
            }),
        }));
        Self { current_segment, resolved_segments, possible_next_segments, matched_macro_def }
    }
}

#[derive(Debug)]
pub struct MatchedSegment<'a> {
    header: Token,
    body:   Vec<TokenOrAstOrMacroResolver<'a>>,
}

impl<'a> MatchedSegment<'a> {
    pub fn new(header: Token) -> Self {
        let body = default();
        Self { header, body }
    }
}


#[derive(Debug)]
pub struct Resolver<'a> {
    current_macro: MacroResolver<'a>,
    macro_stack:   Vec<MacroResolver<'a>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ResolverStep {
    NormalToken,
    NewSegmentStarted,
    MacroStackPop,
}

impl<'a> Resolver<'a> {
    pub fn new_root() -> Self {
        let current_macro = MacroResolver::new_root();
        let macro_stack = default();
        Self { current_macro, macro_stack }
    }

    pub fn run<'b>(
        mut self,
        lexer: &Lexer<'b>,
        root_macro_map: &MacroMatchTree<'a>,
        tokens: Vec<syntax::Item>,
    ) -> syntax::Tree {
        let mut stream = tokens.into_iter();
        let mut opt_token: Option<syntax::Item>;
        macro_rules! next_token {
            () => {{
                opt_token = stream.next();
                if let Some(token) = opt_token.as_ref() {
                    let repr = lexer.repr(token);
                    event!(TRACE, "New token '{}' = {:#?}", repr, token);
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
            let step_result = match token {
                syntax::Item::Token(token) => self.process_token(lexer, root_macro_map, token),
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

    fn replace_current_with_parent_macro(&mut self, mut parent_macro: MacroResolver<'a>) {
        mem::swap(&mut parent_macro, &mut self.current_macro);
        let mut child_macro = parent_macro;
        if let Some(def) = &child_macro.matched_macro_def {
            let pattern = &def.segments.last().pattern;
            let child_tokens = mem::take(&mut child_macro.current_segment.body);
            // FIXME: the first [`false`] below is invalid.
            let match_result = pattern.resolve(child_tokens, false, false).unwrap();
            let mut new_child_tokens = match_result.matched;
            let new_parent_tokens = match_result.not_matched;
            mem::swap(&mut child_macro.current_segment.body, &mut new_child_tokens);
            self.current_macro.current_segment.body.push(child_macro.into());
            self.current_macro.current_segment.body.extend(new_parent_tokens);
        } else {
            panic!()
        }
    }

    fn resolve<'b>(
        lexer: &Lexer<'b>,
        m: MacroResolver<'a>,
        prefix_tokens: Option<Vec<syntax::Item>>,
    ) -> syntax::Tree {
        let segments = NonEmptyVec::new_with_last(m.resolved_segments, m.current_segment);
        let sss: NonEmptyVec<(Token, Vec<syntax::Item>)> = segments.mapped(|segment| {
            let mut ss: Vec<syntax::Item> = vec![];
            for item in segment.body {
                let resolved_token = match item {
                    TokenOrAstOrMacroResolver::MacroResolver(m2) => {
                        if let Some(macro_def) = &m2.matched_macro_def
                            && let Some(pfx_pattern) = &macro_def.rev_prefix_pattern {
                            ss.reverse();
                            let spacing = m2.current_segment.header.span.left_offset > Bytes::from(0);
                            let mut match_result = pfx_pattern.resolve(ss,spacing,true).unwrap();
                            match_result.matched.reverse();
                            ss = match_result.not_matched;
                            ss.reverse();
                            Self::resolve(lexer, m2, Some(match_result.matched)).into()
                        } else {
                            Self::resolve(lexer, m2, None).into()
                        }
                    },
                    TokenOrAstOrMacroResolver::SyntaxItem(t) => t,
                };
                ss.push(resolved_token);
            }
            (segment.header, ss)
        });

        if let Some(macro_def) = m.matched_macro_def {
            (macro_def.body)(lexer, prefix_tokens, sss)
        } else {
            todo!("Handling non-fully-resolved macros")
        }
    }

    pub fn pop_macro_stack_if_reserved(&mut self, repr: &str) -> Option<MacroResolver<'a>> {
        let reserved = self.macro_stack.iter().any(|p| p.possible_next_segments.contains_key(repr));
        if reserved {
            self.macro_stack.pop()
        } else {
            None
        }
    }

    pub fn process_token<'b>(
        &mut self,
        lexer: &Lexer<'b>,
        root_macro_map: &MacroMatchTree<'a>,
        token: Token,
    ) -> ResolverStep {
        let repr = lexer.repr(token);
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
        matched_macro_def: &mut Option<Rc<macros::Definition<'a>>>,
        path: &[PartiallyMatchedMacro<'a>],
    ) -> MacroMatchTree<'a> {
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

// TODO: cargo build --timings


pub fn precedence_of(operator: &str) -> usize {
    match operator {
        "+" => 3,
        "-" => 3,
        "*" => 7,
        _ => panic!("Operator not supported: {}", operator),
    }
}

#[derive(Clone, Copy, Debug, Deref, DerefMut)]
pub struct WithPrecedence<T> {
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
            syntax::Item::Tree(ast) => match &ast.elem {
                syntax::tree::Type::MultiSegmentApp(data) => {
                    if data.segments.first().header.elem.variant() != token::TypeVariant::Symbol {
                        syntax::Item::Tree(
                            ast.with_error(
                                "This expression cannot be used in a non-spaced equation.",
                            ),
                        )
                    } else {
                        syntax::Item::Tree(ast)
                    }
                }
                _ => syntax::Item::Tree(ast),
            },
        })
        .collect()
}

pub fn resolve_operator_precedence(lexer: &Lexer, items: Vec<syntax::Item>) -> syntax::Tree {
    type Tokens = Vec<syntax::Item>;
    let mut flattened: Tokens = default();
    let mut no_space_group: Tokens = default();
    let processs_no_space_group = |flattened: &mut Tokens, no_space_group: &mut Tokens| {
        let tokens = mem::take(no_space_group);
        if tokens.len() == 1 {
            flattened.extend(tokens);
        } else {
            let tokens = annotate_tokens_that_need_spacing(tokens);
            let ast = resolve_operator_precedence_internal(lexer, tokens);
            flattened.push(ast.into());
        }
    };
    for item in items {
        if item.span().left_visible_offset.number == 0 || no_space_group.is_empty() {
            no_space_group.push(item)
        } else if !no_space_group.is_empty() {
            processs_no_space_group(&mut flattened, &mut no_space_group);
            no_space_group.push(item);
        } else {
            flattened.push(item);
        }
    }
    if !no_space_group.is_empty() {
        processs_no_space_group(&mut flattened, &mut no_space_group);
    }
    resolve_operator_precedence_internal(lexer, flattened)
}

fn resolve_operator_precedence_internal(lexer: &Lexer, items: Vec<syntax::Item>) -> syntax::Tree {
    // Reverse-polish notation encoding.
    let mut output: Vec<syntax::Item> = default();
    let mut operator_stack: Vec<WithPrecedence<syntax::tree::OperatorOrError>> = default();
    let mut last_token_was_ast = false;
    let mut last_token_was_opr = false;
    for item in items {
        if let syntax::Item::Token(token) = item && let token::Type::Operator(opr) = token.elem {
            // Item is an operator.
            let last_token_was_opr_copy = last_token_was_opr;
            last_token_was_ast = false;
            last_token_was_opr = true;

            let prec = precedence_of(lexer.repr(&item));
            let opr = item.span().with(opr);

            if last_token_was_opr_copy && let Some(prev_opr) = operator_stack.last_mut() {
                // Error. Multiple operators next to each other.
                match &mut prev_opr.elem {
                    Err(err) => err.operators.push(opr),
                    Ok(prev) => {
                        let operators = NonEmptyVec::new(*prev,vec![opr]);
                        prev_opr.elem = Err(syntax::tree::MultipleOperatorError::new(operators));
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
        let opt_lhs = output.pop().map(|t| token_to_ast(t));
        opt_rhs = Some(syntax::Tree::opr_app(opt_lhs, opr.elem, opt_rhs));
    }
    if !output.is_empty() {
        panic!("Internal error. Not all tokens were consumed while constructing the expression.");
    }
    syntax::Tree::opr_section_boundary(opt_rhs.unwrap()) // fixme
}



impl<'s, 't, T> Debug for source::With<'s, &'t Box<T>>
where source::With<'s, &'t T>: Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t: &T = &**self.data;
        Debug::fmt(&self.with_data(t), f)
    }
}



impl<'s, 't, T> Debug for source::With<'s, &'t Option<T>>
where source::With<'s, &'t T>: Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.data.as_ref().map(|t| self.with_data(t)), f)
    }
}

impl<'s, 't, T, E> Debug for source::With<'s, &'t Result<T, E>>
where
    source::With<'s, &'t T>: Debug,
    source::With<'s, &'t E>: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.data.as_ref().map(|t| self.with_data(t)).map_err(|t| self.with_data(t)), f)
    }
}


fn token_to_ast(elem: syntax::Item) -> syntax::Tree {
    match elem {
        syntax::Item::Token(token) => match token.elem {
            token::Type::Ident(ident) =>
                elem.span().with(syntax::tree::Type::from(syntax::tree::Ident(ident))),
            _ => panic!(),
        },
        syntax::Item::Tree(ast) => ast,
    }
}

fn matched_segments_into_multi_segment_app<'s>(
    lexer: &Lexer<'s>,
    prefix_tokens: Option<Vec<syntax::Item>>,
    matched_segments: NonEmptyVec<(Token, Vec<syntax::Item>)>,
) -> syntax::Tree {
    // FIXME: remove into_vec
    let segments = matched_segments
        .into_vec()
        .into_iter()
        .map(|segment| {
            let header = segment.0;
            let body = (segment.1.len() > 0)
                .as_some_from(|| resolve_operator_precedence(lexer, segment.1));
            syntax::tree::MultiSegmentAppSegment { header, body }
        })
        .collect_vec();
    if let Ok(segments) = NonEmptyVec::try_from(segments) {
        let prefix = prefix_tokens.map(|t| resolve_operator_precedence(lexer, t));
        syntax::Tree::multi_segment_app(prefix, segments)
    } else {
        panic!()
    }
}



// =========================
// === Macro Definitions ===
// =========================

fn macro_if_then_else<'a>() -> macros::Definition<'a> {
    macro_definition! {
        ("if", Pattern::Everything, "then", Pattern::Everything, "else", Pattern::Everything)
        matched_segments_into_multi_segment_app
    }
}

fn macro_if_then<'a>() -> macros::Definition<'a> {
    macro_definition! {
        ("if", Pattern::Everything, "then", Pattern::Everything)
        matched_segments_into_multi_segment_app
    }
}

fn macro_group<'a>() -> macros::Definition<'a> {
    macro_definition! {
        ("(", Pattern::Everything, ")", Pattern::Nothing)
        matched_segments_into_multi_segment_app
    }
}

fn macro_lambda<'a>() -> macros::Definition<'a> {
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

#[cfg(test)]
mod test {
    use super::*;

    pub fn ident(repr: &str) -> syntax::Tree {
        match token::Type::to_ident_unchecked(repr) {
            token::Type::Ident(ident) => span::With::new_no_left_offset_no_start(
                Bytes::from(repr.len()),
                syntax::tree::Type::from(syntax::tree::Ident(ident)),
            ),
            _ => panic!(),
        }
    }

    pub fn app_segment(
        header: Token,
        body: Option<syntax::Tree>,
    ) -> syntax::tree::MultiSegmentAppSegment {
        syntax::tree::MultiSegmentAppSegment { header, body }
    }
}



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
        lexer.output.borrow_vec().iter().map(|t| (*t).into()).collect_vec(),
    );
    println!("{:#?}", source::With::new(str, &ast));

    // let seg1 =
    //     test::app_segment(Token::symbol("("),
    // Some(syntax::Tree::opr_section_boundary(test::ident("a")))); let seg2 =
    // test::app_segment(Token::symbol(")"), None);
    //
    // let ast2 = syntax::Tree::opr_section_boundary(location::With::new_with_len(
    //     Bytes::from(0),
    //     syntax::tree::Data::MultiSegmentApp(MultiSegmentApp {
    //         prefix:   None,
    //         segments: NonEmptyVec::try_from(vec![seg1, seg2]).unwrap(),
    //     }),
    // ));
    //
    // println!("{:#?}", &ast2);

    println!("\n\n==================\n\n");

    // let mut ast2 = ast_builder! { {if} a {then} b {else} c};
    // println!("{:#?}", ast2);
    // ast.remove_span_info();
    // ast2.remove_span_info();
    // println!("{:#?}", ast);
    // println!("{:#?}", ast2);
    // println!("{:#?}", ast == ast2);

    // let a = 5;
    // let b = 5;
    // let c = a + / b;
}


#[cfg(test)]
mod tests {
    use super::*;
    use enso_parser_syntax_tree_builder::ast_builder;

    fn one_shot(input: &str) -> syntax::Tree {
        let mut lexer = Lexer::new(input);
        lexer.run();
        let root_macro_map = builtin_macros();
        let resolver = Resolver::new_root();
        let ast = resolver.run(
            &lexer,
            &root_macro_map,
            lexer.output.borrow_vec().iter().map(|t| (*t).into()).collect_vec(),
        );
        ast
    }

    macro_rules! test_parse {
        ($input:tt = {$($def:tt)*}) => {
            assert_eq!(
                one_shot($input).with_removed_span_info(),
                ast_builder! { $($def)* }.with_removed_span_info()
            )
        };
    }

    #[test]
    fn test_expressions() {
        test_parse!("if a then b" = { {if} a {then} b });
        test_parse!("if a then b else c" = { {if} a {then} b {else} c });
        test_parse!("if a b then c d else e f" = { {if} a b {then} c d {else} e f });
        // test_parse!("foo (a)" = { foo [{"("} a {")"}]});
    }
}


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
// let str = "foo (a)";

// app(["(","a","]"])

/// Requirements:
///
/// - Parentheses have priority over space-aware precedence, e.g. `test (a + b)` is OK because `(a`
///   is not interpreted as expression.
///
/// - Parentheses allow operators glued to them, e.g. `test *(a + b)` is OK, but `test *if a then b`
///   is not.
///
/// - Lambdas have different precedence on left and right side, e.g. `test <| a -> (+1) <| a` should
///   be interpreted as `test (a -> ((+1) a))`.
///
/// - Lambdas consume everything on the right-hand side even if left argument does not use space.
///   `test a-> a + 1` is the same as `test (a -> a + 1)`.
///
/// - Macro sections have higher precedence then everything, including lambdas. `if a then x -> x +
///   1 else x -> x - 1` should be OK.
fn _tmp() {}

// (.foo a)
// (* foo + bar)
// a + * b


// a) {(a b) c x-> y}
// b) {(a b) c x-> y -)}

// Idea:
// 1. Match macros as now.
// 2. After closing a macro, do not build AST, build hierarchical macro resolution.
//    Example a) would become [{[(a b][) c x[-> y]]][}] . Let's skip curly braces:
//                              [(a b][) c x[-> y]]
// 3. Then when closing macros (after parsing '}'), macros free right hand side:
//                             [(a b][)] c x[-> y]
// 4. Then macros are resolved left-to-right, including left-hand-sides.
//                             [(a b)] c [x-> y]


// TODO: what this should return?
// if a then x -> x + 1 else c
// a) (if a then x) -> x + 1 else c
// b) if a then (x -> x + 1) else c
// c) if a then+ y else z
// d) if 1 + 2 > a then b else c

// TODO: HOWEVER this should result in an error:
// a +if x then y else z
// but this should be fine:
// a +(x y z)

// TODO: AND this has to work:
// foo x-> bar

// So we should create passes:
// 1. parentheses
// 2. lambdas
// 3. space-aware precedence
// 4. All other macros

// NO! Then `if a then x -> x + 1 else c` would not work – points 2 and 4 have to be 1 pass.
//     If we want macros to be able to invalidate space aware precedence (like `foo x-> a + b`),
//     then point 3 has to be deleted.
//
//     But how to handle `if a then+ y else z` or `foo +if a then b else c`? Probably by using some
//     additional checks.

// Do we even need parentheses as a separate pass now? Somehow this has to work `+(x y z)` and
// this not `+if x then y else z`. Maybe additional checks for the macros again? This will make
// macros even more powerful. But we don't need to expose all the power to users.



// TODO: NOTE
// probably we want to keep the struct AstOrTokenOrMatchedMacro = AstOrToken | MacroSomething
// because we will be very fast going back to AstOrToken
