use crate::macros;
use crate::macros::pattern;
use crate::prelude::*;
use crate::syntax;
use crate::syntax::token;
use crate::syntax::token::Token;
use crate::Pattern;
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
                resolve_operator_precedence(body.tokens())
            }),
        }));
        Self { current_segment, resolved_segments, possible_next_segments, matched_macro_def }
    }
}



// ======================
// === MatchedSegment ===
// ======================

/// A macro segment which header was matched. It's body contains a list of tokens and nested macros
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
    current_macro: PartiallyMatchedMacro<'s>,
    macro_stack:   Vec<PartiallyMatchedMacro<'s>>,
}

/// Result of the macro resolution step.
#[derive(Clone, Debug)]
enum Step<'s> {
    NewSegmentStarted,
    NormalToken(syntax::Item<'s>),
    MacroStackPop(syntax::Item<'s>),
}

impl<'s> Resolver<'s> {
    /// New resolver with a special "root" segment definition allowing parsing arbitrary
    /// expressions.
    pub fn new_root() -> Self {
        let current_macro = PartiallyMatchedMacro::new_root();
        let macro_stack = default();
        Self { current_macro, macro_stack }
    }

    fn replace_current_with_parent_macro(&mut self, mut parent_macro: PartiallyMatchedMacro<'s>) {
        mem::swap(&mut parent_macro, &mut self.current_macro);
        let mut child_macro = parent_macro;
        self.current_macro.current_segment.body.push(child_macro.into());
    }

    /// Pop the macro stack if the current token is reserved. For example, when matching the
    /// `if a if b then c then d` expression, the token `then` after the token `c` will be
    /// considered reserved and the macro resolution of `if b then c` will be popped from the stack.
    fn pop_macro_stack_if_reserved(&mut self, repr: &str) -> Option<PartiallyMatchedMacro<'s>> {
        let reserved = self.macro_stack.iter().any(|p| p.possible_next_segments.contains_key(repr));
        reserved.and_option_from(|| self.macro_stack.pop())
    }

    /// Run the resolver. Returns the resolved AST.
    pub fn run(
        mut self,
        root_macro_map: &SegmentMap<'s>,
        tokens: &mut iter::Peekable<std::vec::IntoIter<syntax::Item<'s>>>,
    ) -> syntax::Tree<'s> {
        event!(TRACE, "Running macro resolver. Registered macros:\n{:#?}", root_macro_map);
        let mut opt_item: Option<syntax::Item<'s>>;
        macro_rules! next_token {
            () => {{
                opt_item = tokens.next();
                if let Some(token) = opt_item.as_ref() {
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
        while let Some(token) = opt_item && !token.is_newline() {
            let step_result = match token {
                syntax::Item::Token(token) => self.process_token(root_macro_map, token),
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
                    self.current_macro.current_segment.body.push(item.into());
                    trace_state!();
                    next_token!();
                }
            }
        }

        event!(TRACE, "Finishing resolution. Popping the macro stack.");
        while let Some(parent_macro) = self.macro_stack.pop() {
            self.replace_current_with_parent_macro(parent_macro);
        }

        trace_state!();
        let (tree, rest) = Self::resolve(self.current_macro);
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
            todo!("Macro was not matched with any known macro definition. This should return an AST node indicating invalid match.")
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
            syntax::Item::Block(_) => item,
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

#[inline(always)]
pub fn resolve_operator_precedence<'s>(items: Vec<syntax::Item<'s>>) -> syntax::Tree<'s> {
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
        if item.left_visible_offset().width_in_spaces == 0 || no_space_group.is_empty() {
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
    let mut was_section_used = false;
    let mut output: Vec<syntax::Item> = default();
    let mut operator_stack: Vec<WithPrecedence<syntax::tree::OperatorOrError>> = default();
    let mut last_token_was_ast = false;
    let mut last_token_was_opr = false;
    for item in items {
        let i2 = item.clone(); // FIXME: Why is this even needed? Rust bug?
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
                        let operators = NonEmptyVec::new(prev.clone(),vec![opr]);
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
                    if lhs.is_none() { was_section_used = true; }
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
        if opt_lhs.is_none() || opt_rhs.is_none() {
            was_section_used = true;
        }
        opt_rhs = Some(syntax::Tree::opr_app(opt_lhs, opr.elem, opt_rhs));
    }
    if !output.is_empty() {
        panic!("Internal error. Not all tokens were consumed while constructing the expression.");
    }

    // FIXME
    let out = opt_rhs.unwrap();
    if was_section_used {
        syntax::Tree::opr_section_boundary(out)
    } else {
        out
    }
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
        syntax::Item::Block(_) => panic!(),
    }
}
