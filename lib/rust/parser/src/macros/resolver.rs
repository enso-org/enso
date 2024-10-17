//! Macro resolver implementation. Refer to the docs of the main parser module to learn more.
//!
//! # Blocks
//!
//! Macro resolution is informed by block structure.
//!
//! Macros can explicitly manipulate blocks: A macro can use [`pattern`]s to match depending on the
//! contents of a child block, and a macro can create any arbitrary block structure in its output.
//!
//! However, there is one rule that makes block structure more primitive than macros: Each of a
//! macro's segments must begin in the top level of the same block.
//!
//! For some invalid inputs, this rule affects how errors are reported. For example:
//! ```Enso
//! if foo
//!     then bar
//! ```
//! This will be parsed as an `if` macro whose condition is an argument block application applying
//! `foo` to `then bar`; the reported error will be an incomplete application of the `if` macro.
//!
//! This is implemented by starting a new macro resolution [`Block`] at the beginning of every
//! block; the new scope is initialized with only the root macro. Within a scope the state of all
//! macros defined in parent scopes will never be advanced.

use crate::prelude::*;

use crate::im_list::List;
use crate::macros;
use crate::macros::pattern;
use crate::source::Code;
use crate::syntax;
use crate::syntax::token;
use crate::syntax::token::Token;
use crate::syntax::BlockHierarchyConsumer;
use crate::syntax::Finish;
use crate::syntax::GroupHierarchyConsumer;
use crate::syntax::Item;
use crate::syntax::NewlineConsumer;
use crate::syntax::TokenConsumer;

use std::collections::HashMap;
use std::collections::VecDeque;



// ================
// === MacroMap ===
// ================

/// Represents the sets of macros defined in different contexts.
#[derive(Default, Debug)]
pub struct MacroMap {
    /// Macros that can occur anywhere in an expression.
    pub expression: SegmentMap<'static>,
    /// Macros that can only occur in statement context.
    pub statement:  SegmentMap<'static>,
}

impl MacroMap {
    /// Return the macro matching the given token in the given context, if any.
    fn get(&self, key: &str, context: Context) -> Option<&NonEmptyVec<SegmentEntry<'static>>> {
        let statement_result = || self.statement.get(key);
        let expression_result = || self.expression.get(key);
        (context == Context::Statement).then(statement_result).flatten().or_else(expression_result)
    }
}


// === Context ===

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Context {
    Expression,
    Statement,
}


// ==================
// === SegmentMap ===
// ==================

/// A tree-like structure encoding potential macro matches. The keys are code representations of
/// [`macros::SegmentDefinition`] headers (first tokens of sections). Each key is associated with
/// one or more [`SegmentEntry`], which stories a list of required subsequent segments
/// and a macro definition that should be used when all the segments will be matched. For example,
/// after matching the "if" keyword, this struct will contain one entry "then" with two values, one
/// for the required "else" section, and one without a required section (for the "if ... then ..."
/// case).
#[derive(Default, Debug, Deref, DerefMut)]
pub struct SegmentMap<'s> {
    map: HashMap<&'s str, NonEmptyVec<SegmentEntry<'s>>>,
}

/// Partially matched macro info. See docs of [`SegmentMap`] to learn more.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct SegmentEntry<'s> {
    /// All the segment headers that are required for the macro definition to be used.
    pub required_segments: List<macros::SegmentDefinition<'s>>,
    /// Definition of the macro that should be used when all the required segments will be matched.
    /// It contains [`Pattern`] definition for every segment that will be used after all the
    /// segment tokens are discovered.
    pub definition:        Rc<macros::Definition<'s>>,
}


impl<'a> SegmentMap<'a> {
    /// Register a new macro definition in this macro tree.
    pub fn register(&mut self, definition: macros::Definition<'a>) {
        let header = definition.segments.head.header;
        let entry = SegmentEntry {
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


// ================
// === Resolver ===
// ================

/// Macro resolver capable of resolving nested macro usages. See the docs of the main parser module
/// to learn more about the macro resolution steps.
#[derive(Debug)]
struct ResolverState<'s> {
    blocks:       Vec<Block>,
    /// The lines of all currently-open blocks. This is partitioned by `blocks`.
    lines:        Vec<syntax::item::Line<'s>>,
    groups:       Vec<OpenGroup<'s>>,
    /// All currently-open macros. These are partitioned into scopes by `blocks`.
    macros:       Vec<PartiallyMatchedMacro<'s>>,
    /// Segments of all currently-open macros. These are partitioned by `macros`.
    segments:     Vec<MatchedSegment<'s>>,
    /// Items of all segments of all currently-open macros. These are partitioned by `segments`.
    items:        Vec<Item<'s>>,
    context:      Context,
    root_context: RootContext,
    precedence:   syntax::operator::Precedence<'s>,
}


// === Public API ===

impl<'s> ResolverState<'s> {
    /// Create a new resolver.
    fn new(root_context: RootContext, context: Context) -> Self {
        Self {
            context,
            root_context,
            precedence: syntax::operator::Precedence::new(),
            blocks: default(),
            lines: vec![initial_line()],
            groups: default(),
            macros: default(),
            segments: default(),
            items: default(),
        }
    }
}

fn initial_line<'s>() -> syntax::item::Line<'s> {
    syntax::item::Line {
        newline: token::newline(Code::empty(default()), Code::empty(default())),
        items:   default(),
    }
}

impl<'s> Finish for ResolverState<'s> {
    type Result = syntax::Tree<'s>;

    fn finish(&mut self) -> Self::Result {
        self.finish_current_line();
        let tree = match self.root_context {
            RootContext::Module =>
                syntax::tree::block::parse_module(&mut self.lines, &mut self.precedence),
            RootContext::Block =>
                syntax::tree::block::parse_block(&mut self.lines, &mut self.precedence),
        };
        debug_assert!(self.blocks.is_empty());
        debug_assert!(self.lines.is_empty());
        debug_assert!(self.groups.is_empty());
        debug_assert!(self.macros.is_empty());
        debug_assert!(self.segments.is_empty());
        debug_assert!(self.items.is_empty());
        self.context = Context::Statement;
        self.lines.push(initial_line());
        tree
    }
}

/// Specifies how statements of the input should be interpreted.
#[derive(Debug, Copy, Clone)]
pub enum RootContext {
    /// Interpret the input as a sequence of module-level statements.
    Module,
    /// Interpret the input as a sequence of statements inside a body block.
    Block,
}

/// Resolves macros.
#[derive(Debug)]
pub struct Resolver<'s, 'macros> {
    resolver:       ResolverState<'s>,
    root_macro_map: &'macros MacroMap,
}

impl<'s, 'macros> Resolver<'s, 'macros> {
    /// Creates a macro resolver to use with the given macro map.
    pub fn new(root_macro_map: &'macros MacroMap, root_context: RootContext) -> Self {
        Self { resolver: ResolverState::new(root_context, Context::Statement), root_macro_map }
    }
}

impl<'s, 'macros> TokenConsumer<'s> for Resolver<'s, 'macros> {
    fn push_token(&mut self, token: Token<'s>) {
        self.resolver.push(self.root_macro_map, token);
    }
}

impl<'s, 'macros> NewlineConsumer<'s> for Resolver<'s, 'macros> {
    fn push_newline(&mut self, newline: token::Newline<'s>) {
        self.resolver.push_newline(newline);
    }
}

impl<'s, 'macros> BlockHierarchyConsumer for Resolver<'s, 'macros> {
    fn start_block(&mut self) {
        self.resolver.start_block()
    }

    fn end_block(&mut self) {
        self.resolver.end_block()
    }
}

impl<'s, 'macros> GroupHierarchyConsumer<'s> for Resolver<'s, 'macros> {
    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        self.resolver.start_group(open);
    }

    fn end_group(&mut self, close: token::CloseSymbol<'s>) {
        self.resolver.close_group(close);
    }
}

impl<'s, 'macros> Finish for Resolver<'s, 'macros> {
    type Result = syntax::Tree<'s>;

    fn finish(&mut self) -> Self::Result {
        self.resolver.finish()
    }
}


// === Implementation ===

/// Result of the macro resolution step.
#[derive(Clone, Debug)]
enum Step<'s> {
    StartSegment(Token<'s>),
    NormalToken(Item<'s>),
    MacroStackPop(Item<'s>),
}

/// Information about macro resolution state that is stored while processing a deeper indentation
/// level.
///
/// See the module docs ([`self`]) for about the interaction between blocks and macros.
#[derive(Debug)]
struct Block {
    /// Index in `macro_stack` after the last element in the enclosing scope.
    macros_start:  usize,
    /// Index in `open_blocks` after the last element in the enclosing scope.
    outputs_start: usize,
    /// Index in `items` after the last element in the enclosing scope.
    items:         usize,
}

#[derive(Debug)]
struct OpenGroup<'s> {
    open:         token::OpenSymbol<'s>,
    /// Index in `macro_stack` after the last element in the enclosing scope.
    macros_start: usize,
    /// Index in `items` after the last element in the enclosing scope.
    items:        usize,
}

impl<'s> ResolverState<'s> {
    /// Returns the index of the first element in `self.macro_stack` that is active in the current
    /// scope. Any macros before that index are active in some block that contains the current
    /// block, so they will not match tokens within this block.
    fn macro_scope_start(&self) -> usize {
        self.groups
            .last()
            .map(|scope| scope.macros_start)
            .or_else(|| self.blocks.last().map(|scope| scope.macros_start))
            .unwrap_or_default()
    }

    fn items_start(&self) -> usize {
        self.groups
            .last()
            .map(|scope| scope.items)
            .or_else(|| self.blocks.last().map(|scope| scope.items))
            .unwrap_or_default()
    }

    /// Pop the macro stack if the current token is reserved. For example, when matching the
    /// `if a if b then c then d` expression, the token `then` after the token `c` will be
    /// considered reserved and the macro resolution of `if b then c` will be popped from the stack.
    fn pop_macro_stack_if_reserved(&mut self, repr: &str) -> Option<PartiallyMatchedMacro<'s>> {
        let macros = &self.macros[self.macro_scope_start()..];
        let reserved = macros.iter().any(|p| p.possible_next_segments.contains_key(repr));
        reserved.and_option_from(|| self.macros.pop())
    }

    fn start_block(&mut self) {
        while let Some(group) = self.groups.pop() {
            self.end_group(group, None);
        }
        let macros_start = self.macros.len();
        let outputs_start = self.lines.len();
        let items = self.items.len();
        self.blocks.push(Block { macros_start, outputs_start, items });
        self.context = Context::Statement;
    }

    fn end_block(&mut self) {
        self.finish_current_line();
        if let Some(Block { macros_start, outputs_start, items }) = self.blocks.pop() {
            debug_assert_eq!(macros_start, self.macros.len());
            debug_assert_eq!(items, self.items.len());
            let block = self.lines.drain(outputs_start..).collect();
            self.items.push(Item::Block(block));
        }
    }

    fn start_group(&mut self, open: token::OpenSymbol<'s>) {
        let macros_start = self.macros.len();
        let items = self.items.len();
        self.groups.push(OpenGroup { open, macros_start, items });
        self.context = Context::Expression;
    }

    fn close_group(&mut self, close: token::CloseSymbol<'s>) {
        match self.groups.pop() {
            Some(group) => self.end_group(group, close.into()),
            None => self.items.push(Item::Token(close.into())),
        }
    }

    fn end_group(&mut self, group: OpenGroup<'s>, close: Option<token::CloseSymbol<'s>>) {
        let OpenGroup { open, macros_start, items } = group;
        while self.macros.len() > macros_start {
            let mac = self.macros.pop().unwrap();
            self.resolve(mac);
        }
        let body = self.items.drain(items..).collect();
        self.items.push(syntax::item::Group { open, body, close }.into());
    }

    fn push_newline(&mut self, newline: token::Newline<'s>) {
        self.finish_current_line();
        self.lines.push(syntax::item::Line { newline, items: default() });
        self.context = Context::Statement;
    }

    /// Append a token to the state.
    fn push(&mut self, root_macro_map: &MacroMap, mut token: Token<'s>) {
        debug_assert!(!matches!(token.variant, token::Variant::Newline(_)));
        loop {
            token = match self.process_token(root_macro_map, token, self.context) {
                Step::MacroStackPop(Item::Token(t)) => t,
                Step::MacroStackPop(item) => {
                    self.items.push(item);
                    break;
                }
                Step::StartSegment(header) => {
                    let items_start = self.items.len();
                    self.segments.push(MatchedSegment { header, items_start });
                    self.context = Context::Expression;
                    break;
                }
                Step::NormalToken(item) => {
                    self.items.push(item);
                    self.context = Context::Expression;
                    break;
                }
            }
        }
    }

    fn finish_current_line(&mut self) {
        while let Some(group) = self.groups.pop() {
            self.end_group(group, None);
        }
        let macros_start = self.macro_scope_start();
        let items_start = self.items_start();
        while self.macros.len() > macros_start {
            let mac = self.macros.pop().unwrap();
            self.resolve(mac);
        }
        self.lines.last_mut().unwrap().items.extend(self.items.drain(items_start..));
    }

    fn process_token(
        &mut self,
        root_macro_map: &MacroMap,
        token: Token<'s>,
        context: Context,
    ) -> Step<'s> {
        let repr = &**token.code;
        if !token.variant.can_start_macro_segment() {
            return Step::NormalToken(token.into());
        }
        if self.macros.len() > self.macro_scope_start() {
            let current_macro = self.macros.last_mut().unwrap();
            if let Some(subsegments) = current_macro.possible_next_segments.get(repr) {
                let mut new_match_tree =
                    Self::move_to_next_segment(&mut current_macro.matched_macro_def, subsegments);
                mem::swap(&mut new_match_tree, &mut current_macro.possible_next_segments);
                return Step::StartSegment(token);
            } else if let Some(popped) = self.pop_macro_stack_if_reserved(repr) {
                self.resolve(popped);
                return Step::MacroStackPop(token.into());
            }
        }
        if let Some(segments) = root_macro_map.get(repr, context) {
            let mut matched_macro_def = default();
            let segments_start = self.segments.len();
            let new_macro = PartiallyMatchedMacro {
                segments_start,
                possible_next_segments: Self::move_to_next_segment(
                    &mut matched_macro_def,
                    segments,
                ),
                matched_macro_def,
            };
            self.macros.push(new_macro);
            Step::StartSegment(token)
        } else {
            Step::NormalToken(token.into())
        }
    }

    /// Resolve the [`PartiallyMatchedMacro`]. Appends the resulting AST and any trailing unused
    /// tokens to the given collection. For example, the resolution of the `(a)` macro in the
    /// `(a) x (b)` expression will produce the `(a)` AST and the `x` and `(b)` items (already
    /// resolved).
    fn resolve(&mut self, m: PartiallyMatchedMacro<'s>) {
        let PartiallyMatchedMacro { matched_macro_def, segments_start, .. } = m;
        if let Some(macro_def) = matched_macro_def {
            self.resolve_match(&macro_def, segments_start)
        } else {
            self.resolve_failed_match(segments_start)
        };
    }

    fn resolve_match(&mut self, macro_def: &macros::Definition, segments_start: usize) {
        let mut def_segments = macro_def.segments.to_vec().into_iter().rev();
        let segments = self.segments.drain(segments_start..).rev();
        let segments: NonEmptyVec<_> = segments.collect::<Vec<_>>().try_into().unwrap();
        let mut pattern_matched_segments = segments.mapped(|segment| {
            let count_must_match =
                "Internal error. Macro definition and match segments count mismatch.";
            let def = def_segments.next().expect(count_must_match);
            let items = self.items.drain(segment.items_start..).collect();
            (segment.header, def.pattern.resolve(items))
        });
        pattern_matched_segments.reverse();
        let unused_items_of_last_segment = match &mut pattern_matched_segments.last_mut().1 {
            Err(rest) => mem::take(rest),
            Ok(segment) => mem::take(&mut segment.rest),
        };
        let all_tokens_consumed =
            pattern_matched_segments.iter().all(|(_, match_result)| match match_result {
                Ok(result) => result.rest.is_empty(),
                Err(_) => false,
            });
        let out = if all_tokens_consumed {
            let unwrap_match = |(header, match_result)| {
                let match_result: Result<pattern::MatchResult, VecDeque<Item>> = match_result;
                pattern::MatchedSegment::new(header, match_result.unwrap().matched)
            };
            let parser = &mut self.precedence;
            (macro_def.body)(pattern_matched_segments.mapped(unwrap_match), parser)
        } else {
            // The input matched a macro invocation pattern, except extra tokens were found in
            // some segment. Since the start and end of a pattern were found, we know (probably)
            // what tokens were intended to be used as the macro invocation; however we cannot
            // pass these tokens to the macro body function, because it expects a correct match
            // of its pattern. Use a [`MultiSegmentApp`] to represent all the tokens, wrapping
            // only the excess tokens in [`Invalid`] nodes, so that the error can be reported
            // precisely.
            let segments = pattern_matched_segments.mapped(|(header, match_result)| {
                let mut tokens = Vec::new();
                let excess = match match_result {
                    Ok(pattern::MatchResult { matched, rest }) => {
                        matched.get_tokens(&mut tokens);
                        rest
                    }
                    Err(tokens) => tokens,
                };
                if let Some(excess) = self.precedence.resolve(&mut excess.into()) {
                    let excess = excess.with_error("Unexpected tokens in macro invocation.");
                    tokens.push(excess.into());
                }
                let body = self.precedence.resolve(&mut tokens);
                syntax::tree::MultiSegmentAppSegment { header, body }
            });
            syntax::Tree::multi_segment_app(segments)
        };
        self.items.push(out.into());
        self.items.extend(unused_items_of_last_segment);
    }

    fn resolve_failed_match(&mut self, segments_start: usize) {
        // The input matched the first token of a macro, but didn't match any complete pattern
        // for that macro. Because we don't know what tokens were intended to be consumed by the
        // macro, consume only the first token (wrapping it in an [`Invalid`] node).
        let mut items = Vec::with_capacity(self.segments.len() - segments_start);
        while self.segments.len() > segments_start + 1 {
            let segment = self.segments.pop().unwrap();
            items.extend(self.items.drain(segment.items_start..).rev());
            items.push(segment.header.into());
        }
        let segment0 = self.segments.pop().unwrap();
        let header0 = syntax::tree::to_ast(segment0.header).with_error("Invalid macro invocation.");
        items.extend(self.items.drain(segment0.items_start..).rev());
        self.items.push(header0.into());
        self.items.extend(items.into_iter().rev());
    }

    /// Move the resolution to the next segment. Takes possible next segments and merges them in a
    /// new [`SegmentMap`]. If after moving to the next segment there is a macro definition that is
    /// fully matched, its definition will be recorded.
    fn move_to_next_segment(
        matched_macro_def: &mut Option<Rc<macros::Definition<'s>>>,
        possible_segments: &[SegmentEntry<'s>],
    ) -> SegmentMap<'s> {
        *matched_macro_def = None;
        let mut new_section_tree = SegmentMap::default();
        for segment_entry in possible_segments {
            if let Some(first) = segment_entry.required_segments.head() {
                let tail = segment_entry.required_segments.tail().cloned().unwrap_or_default();
                let definition = segment_entry.definition.clone();
                let entry = SegmentEntry { required_segments: tail, definition };
                if let Some(node) = new_section_tree.get_mut(&first.header) {
                    node.push(entry);
                } else {
                    new_section_tree.insert(first.header, NonEmptyVec::singleton(entry));
                }
            } else {
                *matched_macro_def = Some(segment_entry.definition.clone());
            }
        }
        new_section_tree
    }
}


// =============================
// === PartiallyMatchedMacro ===
// =============================

/// Partially matched macro. It contains the current section being matched, all the sections matched
/// so far, and the macro definition in case the macro was fully matched. Please note that the
/// definition can change during macro resolution. For example, after finding both "if" and "then"
/// sections, the definition of the "if ... then ..." macro will be used. However, after finding the
/// "else" token, the definition will be replaced with the "if ... then ... else ..." macro one.
#[derive(Debug)]
struct PartiallyMatchedMacro<'s> {
    possible_next_segments: SegmentMap<'s>,
    matched_macro_def:      Option<Rc<macros::Definition<'s>>>,
    /// Height in `segments` where this macro's resolved segments begin.
    segments_start:         usize,
}


// ======================
// === MatchedSegment ===
// ======================

#[derive(Debug)]
struct MatchedSegment<'s> {
    header:      Token<'s>,
    items_start: usize,
}
