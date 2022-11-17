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
//! This is implemented by starting a new macro resolution [`Scope`] at the beginning of every
//! block; the new scope is initialized with only the root macro. Within a scope the state of all
//! macros defined in parent scopes will never be advanced.

use crate::prelude::*;

use crate::macros;
use crate::macros::pattern;
use crate::syntax;
use crate::syntax::token;
use crate::syntax::token::Token;

use enso_data_structures::im_list;
use enso_data_structures::im_list::List;
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



// =============================
// === PartiallyMatchedMacro ===
// =============================

/// Partially matched macro. It contains the current section being matched, all the sections matched
/// so far, and the macro definition in case the macro was fully matched. Please note that the
/// definition can change during macro resolution. For example, after finding both "if" and "then"
/// sections, the definition of the "if ... then ..." macro will be used. However, after finding the
/// "else" token, the definition will be replaced with the "if ... then ... else ..." macro one.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct PartiallyMatchedMacro<'s> {
    pub current_segment:        MatchedSegment<'s>,
    pub resolved_segments:      Vec<MatchedSegment<'s>>,
    pub possible_next_segments: SegmentMap<'s>,
    pub matched_macro_def:      Option<Rc<macros::Definition<'s>>>,
}

impl<'a> PartiallyMatchedMacro<'a> {
    /// A new macro resolver with a special "root" segment definition. The "root" segment does not
    /// exist in the source code, it is simply the whole expression being parsed. It is treated
    /// as a macro in order to unify the algorithms.
    pub fn new_root() -> Self {
        let current_segment = MatchedSegment::new(Token("", "", token::Variant::newline()));
        let resolved_segments = default();
        let possible_next_segments = default();
        let matched_macro_def = Some(Rc::new(macros::Definition {
            segments: im_list::NonEmpty::singleton(macros::SegmentDefinition {
                header:  "__ROOT__",
                pattern: pattern::everything(),
            }),
            body:     Rc::new(|v| {
                // Taking the first segment, hardcoded above.
                let body = v.pop().0.result;
                syntax::operator::resolve_operator_precedence_if_non_empty(body.tokens()).unwrap()
            }),
        }));
        Self { current_segment, resolved_segments, possible_next_segments, matched_macro_def }
    }

    /// Append an item or partially-matched macro to the current segment.
    fn push(&mut self, item: impl Into<ItemOrPartiallyMatchedMacro<'a>>) {
        self.current_segment.body.push(item.into());
    }
}



// ======================
// === MatchedSegment ===
// ======================

/// A macro segment which header was matched. Its body contains a list of tokens and nested macros
/// that were found. Please note that the body tokens are not matched against the pattern yet.
/// Because of that, the macro nesting is incorrect for patterns that do not consume all tokens till
/// the end of the stream. For example, the expression `(a) (b)` will be matched in such a way, that
/// the macro `(b)` will be part of the body of the `)` segment of the `(a)` macro. This will be
/// restructured in the patter matching phase. See the parser module docs to learn more about this
/// process.
#[derive(Debug)]
pub struct MatchedSegment<'s> {
    header: Token<'s>,
    body:   Vec<ItemOrPartiallyMatchedMacro<'s>>,
}

impl<'s> MatchedSegment<'s> {
    /// Constructor.
    pub fn new(header: Token<'s>) -> Self {
        let body = default();
        Self { header, body }
    }
}



// ===================================
// === ItemOrPartiallyMatchedMacro ===
// ===================================

/// One of [`syntax::Item`] or [`PartiallyMatchedMacro`]. Used during macro resolution when some
/// items are already resolved as macros, and some are not yet. For example, after matching the
/// expression `(a) x (b)`, the `x` token and the `(b)` macro will be items of the body of the last
/// segment of the `(a)` macro.
#[derive(Debug, From)]
#[allow(missing_docs)]
enum ItemOrPartiallyMatchedMacro<'s> {
    SyntaxItem(syntax::Item<'s>),
    PartiallyMatchedMacro(PartiallyMatchedMacro<'s>),
}

impl<'s> TryAsRef<syntax::Item<'s>> for ItemOrPartiallyMatchedMacro<'s> {
    fn try_as_ref(&self) -> Option<&syntax::Item<'s>> {
        match self {
            Self::SyntaxItem(t) => Some(t),
            _ => None,
        }
    }
}

impl<'s> TryAsRef<PartiallyMatchedMacro<'s>> for ItemOrPartiallyMatchedMacro<'s> {
    fn try_as_ref(&self) -> Option<&PartiallyMatchedMacro<'s>> {
        match self {
            Self::PartiallyMatchedMacro(t) => Some(t),
            _ => None,
        }
    }
}



// ================
// === Resolver ===
// ================

/// Macro resolver capable of resolving nested macro usages. See the docs of the main parser module
/// to learn more about the macro resolution steps.
#[derive(Debug)]
pub struct Resolver<'s> {
    macro_stack: Vec<PartiallyMatchedMacro<'s>>,
    scopes:      Vec<Scope>,
    open_blocks: Vec<syntax::item::Line<'s>>,
    context:     Context,
}

/// Result of the macro resolution step.
#[derive(Clone, Debug)]
enum Step<'s> {
    NewSegmentStarted,
    NormalToken(syntax::Item<'s>),
    MacroStackPop(syntax::Item<'s>),
}

/// Information about macro resolution state that is stored while processing a deeper indentation
/// level.
///
/// See the module docs ([`self`]) for about the interaction between blocks and macros.
#[derive(Debug)]
struct Scope {
    macros_start:  usize,
    outputs_start: usize,
}

impl<'s> Resolver<'s> {
    /// New resolver with a special "root" segment definition allowing parsing arbitrary
    /// expressions.
    pub fn new_root() -> Self {
        let macro_stack = default();
        let scopes = default();
        let newline = token::newline("", "");
        let open_blocks = vec![syntax::item::Line { newline, items: default() }];
        let context = Context::Statement;
        Self { macro_stack, scopes, open_blocks, context }
    }

    /// Returns the index of the first element in `self.macro_stack` that is active in the current
    /// scope. Any macros before that index are active in some block that contains the current
    /// block, so they will not match tokens within this block.
    fn macro_scope_start(&self) -> usize {
        self.scopes.last().map(|scope| scope.macros_start).unwrap_or_default()
    }

    /// Pop the macro stack if the current token is reserved. For example, when matching the
    /// `if a if b then c then d` expression, the token `then` after the token `c` will be
    /// considered reserved and the macro resolution of `if b then c` will be popped from the stack.
    fn pop_macro_stack_if_reserved(&mut self, repr: &str) -> Option<PartiallyMatchedMacro<'s>> {
        let macros = &self.macro_stack[self.macro_scope_start()..];
        let reserved = macros.iter().any(|p| p.possible_next_segments.contains_key(repr));
        reserved.and_option_from(|| self.macro_stack.pop())
    }

    /// Run the resolver. Returns the resolved AST.
    pub fn run(mut self, root_macro_map: &MacroMap, tokens: Vec<Token<'s>>) -> syntax::Tree<'s> {
        tokens.into_iter().for_each(|t| self.push(root_macro_map, t));
        self.finish_current_line();
        let mut precedence = syntax::operator::Precedence::new();
        let lines = self.open_blocks.into_iter().map(|syntax::item::Line { newline, items }| {
            syntax::tree::block::Line { newline, expression: precedence.resolve(items) }
        });
        syntax::tree::block::body_from_lines(lines)
    }

    /// Append a token to the state.
    fn push(&mut self, root_macro_map: &MacroMap, token: Token<'s>) {
        match token.variant {
            token::Variant::Newline(_) => {
                self.finish_current_line();
                let newline = token::newline(token.left_offset, token.code);
                self.open_blocks.push(syntax::item::Line { newline, items: default() });
                self.context = Context::Statement;
            }
            token::Variant::BlockStart(_) => {
                let macros_start = self.macro_stack.len();
                let outputs_start = self.open_blocks.len();
                let scope = Scope { macros_start, outputs_start };
                self.scopes.push(scope);
                self.context = Context::Statement;
            }
            token::Variant::BlockEnd(_) => {
                self.finish_current_line();
                if let Some(Scope { macros_start, outputs_start }) = self.scopes.pop() {
                    debug_assert_eq!(macros_start, self.macro_stack.len());
                    let block = self.open_blocks.drain(outputs_start..).collect();
                    self.push_item(syntax::Item::Block(block));
                }
            }
            _ => {
                let mut token = token;
                loop {
                    token = match self.process_token(root_macro_map, token, self.context) {
                        Step::MacroStackPop(syntax::Item::Token(t)) => t,
                        Step::MacroStackPop(item) => {
                            self.push_item(item);
                            break;
                        }
                        Step::NewSegmentStarted => {
                            self.context = Context::Expression;
                            break;
                        }
                        Step::NormalToken(item) => {
                            self.push_item(item);
                            self.context = Context::Expression;
                            break;
                        }
                    }
                }
            }
        }
    }

    fn finish_current_line(&mut self) {
        let mut macros = self.macro_stack.drain(self.macro_scope_start()..).rev();
        if let Some(mut mac) = macros.next() {
            for mut mac_ in macros {
                mac_.push(mac);
                mac = mac_;
            }
            let (resolved, extra) = Self::resolve(mac);
            self.open_blocks.last_mut().unwrap().items.push(resolved.into());
            self.open_blocks.last_mut().unwrap().items.extend(extra);
        }
    }

    fn push_item(&mut self, item: syntax::Item<'s>) {
        let i = self.macro_scope_start();
        if let Some(mac) = self.macro_stack[i..].last_mut() {
            mac.push(item);
        } else {
            self.open_blocks.last_mut().unwrap().items.push(item);
        }
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
        if self.macro_stack.len() > self.macro_scope_start() {
            let current_macro = self.macro_stack.last_mut().unwrap();
            if let Some(subsegments) = current_macro.possible_next_segments.get(repr) {
                trace!("Entering next segment of the current macro.");
                let mut new_match_tree =
                    Self::move_to_next_segment(&mut current_macro.matched_macro_def, subsegments);
                let mut current_segment = MatchedSegment::new(token);
                mem::swap(&mut new_match_tree, &mut current_macro.possible_next_segments);
                mem::swap(&mut current_macro.current_segment, &mut current_segment);
                current_macro.resolved_segments.push(current_segment);
                return Step::NewSegmentStarted;
            } else if let Some(popped) = self.pop_macro_stack_if_reserved(repr) {
                trace!("Next token reserved by parent macro. Resolving current macro.");
                let current_macro = self.macro_stack.last_mut().unwrap();
                current_macro.push(popped);
                return Step::MacroStackPop(token.into());
            }
        }
        if let Some(segments) = root_macro_map.get(repr, context) {
            trace!("Starting a new nested macro resolution.");
            let mut matched_macro_def = default();
            let new_macro = PartiallyMatchedMacro {
                current_segment: MatchedSegment { header: token, body: default() },
                resolved_segments: default(),
                possible_next_segments: Self::move_to_next_segment(
                    &mut matched_macro_def,
                    segments,
                ),
                matched_macro_def,
            };
            self.macro_stack.push(new_macro);
            Step::NewSegmentStarted
        } else {
            trace!("Consuming token as current segment body.");
            Step::NormalToken(token.into())
        }
    }

    /// Resolve the [`PartiallyMatchedMacro`]. Returns the AST and the non-used tokens. For example,
    /// the resolution of the `(a)` macro in the `(a) x (b)` expression will return the `(a)` AST
    /// and the `x` and `(b)` items (already resolved).
    fn resolve(m: PartiallyMatchedMacro<'s>) -> (syntax::Tree<'s>, VecDeque<syntax::Item<'s>>) {
        let segments = NonEmptyVec::<MatchedSegment, usize>::new_with_last(
            m.resolved_segments,
            m.current_segment,
        );
        let resolved_segments = segments.mapped(|segment| {
            let mut items: VecDeque<syntax::Item<'s>> = default();
            for item in segment.body {
                match item {
                    ItemOrPartiallyMatchedMacro::SyntaxItem(t) => items.push_back(t),
                    ItemOrPartiallyMatchedMacro::PartiallyMatchedMacro(unresolved_macro) => {
                        let (resolved_macro, unused_items) = Self::resolve(unresolved_macro);
                        items.push_back(resolved_macro.into());
                        items.extend(unused_items);
                    }
                }
            }
            (segment.header, items)
        });

        if let Some(macro_def) = m.matched_macro_def {
            let mut def_segments = macro_def.segments.to_vec().into_iter();
            let mut pattern_matched_segments = resolved_segments.mapped(|(header, items)| {
                let err = "Internal error. Macro definition and match segments count mismatch.";
                let def = def_segments.next().unwrap_or_else(|| panic!("{}", err));
                (header, def.pattern.resolve(items))
            });

            // Moving not pattern-matched tokens of the last segment to parent.
            let mut not_used_items_of_last_segment = VecDeque::new();
            match &mut pattern_matched_segments.last_mut().1 {
                Err(rest) => mem::swap(&mut not_used_items_of_last_segment, rest),
                Ok(segment) => mem::swap(&mut not_used_items_of_last_segment, &mut segment.rest),
            }

            let pattern_matched_segments =
                pattern_matched_segments.mapped(|(header, match_result)| match match_result {
                    Ok(result) => {
                        if !result.rest.is_empty() {
                            todo!("Mark unmatched tokens as unexpected.");
                        }
                        pattern::MatchedSegment::new(header, result.matched)
                    }
                    Err(_unmatched_items) => todo!("Mark unmatched tokens as unexpected."),
                });

            let out = (macro_def.body)(pattern_matched_segments);
            (out, not_used_items_of_last_segment)
        } else {
            let mut segments = resolved_segments.into_iter();
            let (header0, mut segment) = segments.next().unwrap();
            let mut items = VecDeque::new();
            items.append(&mut segment);
            for (header, mut segment) in segments {
                items.push_back(syntax::Item::Token(header));
                items.append(&mut segment);
            }
            let mut body = String::new();
            for item in &items {
                match item {
                    syntax::Item::Token(token) => body.push_str(&token.code.repr),
                    syntax::Item::Block(_block) => body.push_str("<block>"),
                    syntax::Item::Tree(tree) => body.push_str(&tree.code()),
                }
            }
            let header0 = syntax::Tree::from(header0).with_error("Invalid macro invocation.");
            (header0, items)
        }
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
                let definition = segment_entry.definition.clone_ref();
                let entry = SegmentEntry { required_segments: tail, definition };
                if let Some(node) = new_section_tree.get_mut(&first.header) {
                    node.push(entry);
                } else {
                    new_section_tree.insert(first.header, NonEmptyVec::singleton(entry));
                }
            } else {
                *matched_macro_def = Some(segment_entry.definition.clone_ref());
            }
        }
        new_section_tree
    }
}
