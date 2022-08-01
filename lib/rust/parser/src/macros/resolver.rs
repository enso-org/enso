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
    current_macro:       PartiallyMatchedMacro<'s>,
    macro_stack:         Vec<PartiallyMatchedMacro<'s>>,
    scopes:              Vec<Scope<'s>>,
    lines:               Vec<syntax::tree::block::Line<'s>>,
    newline:             Option<token::Newline<'s>>,
    line_contains_items: bool,
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
struct Scope<'s> {
    parent_tokens: std::vec::IntoIter<syntax::Item<'s>>,
    macros_start:  usize,
    outputs_start: usize,
    prev_newline:  Option<token::Newline<'s>>,
    prev_macro:    PartiallyMatchedMacro<'s>,
}

impl<'s> Resolver<'s> {
    /// New resolver with a special "root" segment definition allowing parsing arbitrary
    /// expressions.
    pub fn new_root() -> Self {
        let current_macro = PartiallyMatchedMacro::new_root();
        let macro_stack = default();
        let scopes = default();
        let lines = default();
        let newline = Some(token::newline("", ""));
        let line_contains_items = default();
        Self { current_macro, macro_stack, scopes, lines, newline, line_contains_items }
    }

    fn replace_current_with_parent_macro(&mut self, parent_macro: PartiallyMatchedMacro<'s>) {
        let child_macro = mem::replace(&mut self.current_macro, parent_macro);
        self.current_macro.push(child_macro);
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
    pub fn run(
        mut self,
        root_macro_map: &SegmentMap<'s>,
        tokens: Vec<syntax::Item<'s>>,
    ) -> syntax::Tree<'s> {
        let mut tokens = tokens.into_iter();
        event!(TRACE, "Running macro resolver. Registered macros:\n{:#?}", root_macro_map);
        let mut opt_item: Option<syntax::Item<'s>>;
        macro_rules! next_token {
            () => {{
                opt_item = tokens.next();
                event!(TRACE, "Next token {:#?}", &opt_item);
            }};
        }
        macro_rules! trace_state {
            () => {
                event!(TRACE, "Current macro:\n{:#?}", self.current_macro);
                event!(TRACE, "Parent macros:\n{:#?}", self.macro_stack);
            };
        }
        next_token!();
        loop {
            while opt_item.is_none() {
                if let Some(newline) = self.newline.take() {
                    let expression = self.line_contains_items.as_some_from(|| self.unwind_stack());
                    self.lines.push(syntax::tree::block::Line { newline, expression });
                }
                if let Some(parent_tokens) = self.exit_current_scope() {
                    tokens = parent_tokens;
                    next_token!();
                    continue;
                }
                break;
            }
            let token = match opt_item {
                Some(token) => token,
                None => break,
            };
            if let syntax::Item::Token(Token {
                variant: token::Variant::Newline(_),
                left_offset,
                code,
            }) = token
            {
                let new_newline = token::newline(left_offset, code);
                let newline = mem::replace(&mut self.newline, Some(new_newline));
                if let Some(newline) = newline {
                    let expression = self.line_contains_items.as_some_from(|| self.unwind_stack());
                    self.lines.push(syntax::tree::block::Line { newline, expression });
                }
                next_token!();
                self.line_contains_items = false;
                continue;
            }
            self.line_contains_items = true;
            let step_result = match token {
                syntax::Item::Token(token) => self.process_token(root_macro_map, token),
                syntax::Item::Block(tokens_) => {
                    let parent_tokens = mem::replace(&mut tokens, tokens_.into_iter());
                    let new_root = PartiallyMatchedMacro::new_root();
                    let prev_macro = mem::replace(&mut self.current_macro, new_root);
                    let macros_start = self.macro_stack.len();
                    let outputs_start = self.lines.len();
                    let prev_newline = self.newline.take();
                    let scope = Scope {
                        parent_tokens,
                        macros_start,
                        outputs_start,
                        prev_newline,
                        prev_macro,
                    };
                    self.scopes.push(scope);
                    next_token!();
                    self.line_contains_items = false;
                    continue;
                }
                _ => Step::NormalToken(token),
            };
            match step_result {
                Step::MacroStackPop(item) => {
                    trace_state!();
                    opt_item = Some(item)
                }
                Step::NewSegmentStarted => {
                    trace_state!();
                    next_token!()
                }
                Step::NormalToken(item) => {
                    self.current_macro.push(item);
                    trace_state!();
                    next_token!();
                }
            }
        }
        syntax::tree::block::body_from_lines(self.lines)
    }

    /// Finish processing the current block and close its macro scope, unless this is the top-level
    /// block, which is indicated by returning `None`.
    ///
    /// This builds a [`syntax::Item::Block`] from the outputs of the current scope, restores the
    /// state to resume processing the parent scope, and submits the built block as a token to the
    /// newly-current macro (which would have been the macro active when the block began).
    ///
    /// Returns the remaining tokens of the parent block.
    fn exit_current_scope(&mut self) -> Option<std::vec::IntoIter<syntax::Item<'s>>> {
        let scope = self.scopes.pop()?;
        let Scope { parent_tokens, macros_start, outputs_start, prev_newline, prev_macro } = scope;
        debug_assert_eq!(macros_start, self.macro_stack.len());
        self.current_macro = prev_macro;
        let lines = self.lines.drain(outputs_start..);
        let mut out = Vec::with_capacity(lines.len() * 2);
        for line in lines {
            let syntax::tree::block::Line { newline, expression } = line;
            let newline = syntax::Token::from(newline);
            let newline = syntax::Item::from(newline);
            out.push(newline);
            if let Some(expression) = expression {
                let expression = syntax::Item::from(expression);
                out.push(expression);
            }
        }
        let block = syntax::Item::Block(out);
        self.current_macro.push(block);
        self.line_contains_items = true;
        self.newline = prev_newline;
        Some(parent_tokens)
    }

    fn unwind_stack(&mut self) -> syntax::Tree<'s> {
        macro_rules! trace_state {
            () => {
                event!(TRACE, "Current macro:\n{:#?}", self.current_macro);
                event!(TRACE, "Parent macros:\n{:#?}", self.macro_stack);
            };
        }
        event!(TRACE, "Finishing resolution. Popping the macro stack.");
        let macros = self.macro_stack.drain(self.macro_scope_start()..).rev();
        for parent_macro in macros {
            let child_macro = mem::replace(&mut self.current_macro, parent_macro);
            self.current_macro.push(child_macro);
        }
        trace_state!();
        let macro_ = mem::replace(&mut self.current_macro, PartiallyMatchedMacro::new_root());
        let (tree, rest) = Self::resolve(macro_);
        if !rest.is_empty() {
            panic!(
                "Internal error. Not all tokens were consumed by the macro resolver:\n{:#?}",
                rest
            );
        }
        tree
    }

    fn process_token(&mut self, root_macro_map: &SegmentMap<'s>, token: Token<'s>) -> Step<'s> {
        let repr = &**token.code;
        if let Some(subsegments) = self.current_macro.possible_next_segments.get(repr) {
            event!(TRACE, "Entering next segment of the current macro.");
            let mut new_match_tree =
                Self::move_to_next_segment(&mut self.current_macro.matched_macro_def, subsegments);
            let mut current_segment = MatchedSegment::new(token);
            mem::swap(&mut new_match_tree, &mut self.current_macro.possible_next_segments);
            mem::swap(&mut self.current_macro.current_segment, &mut current_segment);
            self.current_macro.resolved_segments.push(current_segment);
            Step::NewSegmentStarted
        } else if let Some(parent_macro) = self.pop_macro_stack_if_reserved(repr) {
            event!(TRACE, "Next token reserved by parent macro. Resolving current macro.");
            self.replace_current_with_parent_macro(parent_macro);
            Step::MacroStackPop(token.into())
        } else if let Some(segments) = root_macro_map.get(repr) {
            event!(TRACE, "Starting a new nested macro resolution.");
            let mut matched_macro_def = default();
            let mut current_macro = PartiallyMatchedMacro {
                current_segment: MatchedSegment { header: token, body: default() },
                resolved_segments: default(),
                possible_next_segments: Self::move_to_next_segment(
                    &mut matched_macro_def,
                    segments,
                ),
                matched_macro_def,
            };
            mem::swap(&mut self.current_macro, &mut current_macro);
            self.macro_stack.push(current_macro);
            Step::NewSegmentStarted
        } else {
            event!(TRACE, "Consuming token as current segment body.");
            Step::NormalToken(token.into())
        }
    }

    /// Resolve the [`PartiallyMatchedMacro`]. Returns the AST and the non-used tokens. For example,
    /// the resolution of the `(a)` macro in the `(a) x (b)` expression will return the `(a)` AST
    /// and the `x` and `(b)` items (already resolved).
    fn resolve(m: PartiallyMatchedMacro<'s>) -> (syntax::Tree<'s>, VecDeque<syntax::Item<'s>>) {
        let segments = NonEmptyVec::new_with_last(m.resolved_segments, m.current_segment);
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
            let message = format!("Macro was not matched with any known macro definition.\nResolved segments: {resolved_segments:?}");
            let error = syntax::tree::Error::new(message);
            (syntax::tree::Tree::invalid(error, default()), default())
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
