use crate::macros;
use crate::macros::pattern::MatchedSegment;
use crate::prelude::*;
use crate::syntax;
use crate::syntax::token;
use crate::syntax::token::Token;
use crate::Pattern;
use enso_data_structures::im_list;
use enso_data_structures::im_list::List;
use std::collections::VecDeque;



// =========================
// === FrameOrSyntaxItem ===
// =========================

/// One of [`syntax::Item`] or [`Frame`].
#[derive(Debug, From)]
#[allow(missing_docs)]
enum FrameOrSyntaxItem<'s> {
    SyntaxItem(syntax::Item<'s>),
    Frame(Frame<'s>),
}

impl<'s> TryAsRef<syntax::Item<'s>> for FrameOrSyntaxItem<'s> {
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



// ===============
// === Segment ===
// ===============

/// A matched macro segment. Partial macro resolution product.
#[derive(Debug)]
pub struct Segment<'s> {
    header: Token<'s>,
    body:   Vec<FrameOrSyntaxItem<'s>>,
}

impl<'s> Segment<'s> {
    /// Constructor.
    pub fn new(header: Token<'s>) -> Self {
        let body = default();
        Self { header, body }
    }
}



// =============
// === Frame ===
// =============

/// Enso macro resolver. See the docs of the main module to learn more about the macro resolution
/// steps.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Frame<'s> {
    pub current_segment:        Segment<'s>,
    pub resolved_segments:      Vec<Segment<'s>>,
    pub possible_next_segments: MacroMatchTree<'s>,
    pub matched_macro_def:      Option<Rc<macros::Definition<'s>>>,
}

impl<'a> Frame<'a> {
    /// A new macro resolver with a special "root" segment definition. The "root" segment does not
    /// exist in the source code, it is simply the whole expression being parsed. It is treated
    /// as a macro in order to unify the algorithms.
    pub fn new_root() -> Self {
        let current_segment = Segment::new(Token("", "", token::Variant::newline()));
        let resolved_segments = default();
        let possible_next_segments = default();
        let matched_macro_def = Some(Rc::new(macros::Definition {
            segments: im_list::NonEmpty::singleton(macros::SegmentDefinition {
                header:  "__ROOT__",
                pattern: Pattern::Everything,
            }),
            body:     Rc::new(|v| {
                let t = v.into_vec().pop().unwrap().result;
                resolve_operator_precedence(t.tokens())
            }),
        }));
        Self { current_segment, resolved_segments, possible_next_segments, matched_macro_def }
    }
}



// ================
// === Resolver ===
// ================

/// Main macro resolver capable of resolving nested macro usages. See the docs of the main module to
/// learn more about the macro resolution steps.
#[derive(Debug)]
pub struct Resolver<'s> {
    current_frame: Frame<'s>,
    frame_stack:   Vec<Frame<'s>>,
}

/// Result of the macro resolution step.
#[derive(Clone, Debug)]
enum Step<'s> {
    NewSegmentStarted,
    NormalToken(syntax::Item<'s>),
    MacroStackPop(syntax::Item<'s>),
}

impl<'s> Resolver<'s> {
    pub fn new_root() -> Self {
        let current_frame = Frame::new_root();
        let frame_stack = default();
        Self { current_frame, frame_stack }
    }

    pub fn run(
        mut self,
        root_macro_map: &MacroMatchTree<'s>,
        tokens: iter::Peekable<std::vec::IntoIter<syntax::Item<'s>>>,
    ) -> (syntax::Tree<'s>, iter::Peekable<std::vec::IntoIter<syntax::Item<'s>>>) {
        event!(TRACE, "Running macro resolver. Registered macros:\n{:#?}", root_macro_map);

        let mut tokens = tokens.into_iter();
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
                event!(TRACE, "Current macro:\n{:#?}", self.current_frame);
                event!(TRACE, "Parent macros:\n{:#?}", self.frame_stack);
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
                    self.current_frame.current_segment.body.push(item.into());
                    trace_state!();
                    next_token!();
                }
            }
        }


        event!(TRACE, "Finishing resolution. Popping the macro stack.");

        while let Some(parent_macro) = self.frame_stack.pop() {
            self.replace_current_with_parent_macro(parent_macro);
        }

        trace_state!();

        let (tree, rest) = Self::resolve(self.current_frame);
        if !rest.is_empty() {
            panic!("Internal error.");
        }
        (tree, tokens)
    }

    fn replace_current_with_parent_macro(&mut self, mut parent_macro: Frame<'s>) {
        mem::swap(&mut parent_macro, &mut self.current_frame);
        let mut child_macro = parent_macro;
        self.current_frame.current_segment.body.push(child_macro.into());
    }

    fn resolve(m: Frame<'s>) -> (syntax::Tree<'s>, VecDeque<syntax::Item<'s>>) {
        let segments = NonEmptyVec::new_with_last(m.resolved_segments, m.current_segment);
        let resolved_segments = segments.mapped(|segment| {
            let mut ss: VecDeque<syntax::Item<'s>> = default();
            for item in segment.body {
                match item {
                    FrameOrSyntaxItem::SyntaxItem(t) => ss.push_back(t),
                    FrameOrSyntaxItem::Frame(m2) => {
                        let (tree, rest) = Self::resolve(m2);
                        ss.push_back(tree.into());
                        ss.extend(rest);
                    }
                }
            }
            (segment.header, ss)
        });

        if let Some(macro_def) = m.matched_macro_def {
            let mut def_segments = macro_def.segments.to_vec().into_iter();
            let mut sx = resolved_segments.mapped(|(header, items)| {
                let def = def_segments.next().unwrap_or_else(|| panic!("Internal error."));
                (header, def.pattern.resolve(items))
            });

            // Moving not pattern-matched tokens of the last segment to parent.
            let mut rest = VecDeque::new();
            match &mut sx.last_mut().1 {
                Err(rest2) => mem::swap(&mut rest, rest2),
                Ok(t) => mem::swap(&mut rest, &mut t.rest),
            }
            // TODO: handle unmatched cases, handle .rest
            let sx = sx.mapped(|t| MatchedSegment::new(t.0, t.1.unwrap().matched));

            let out = (macro_def.body)(sx);
            (out, rest)
        } else {
            todo!("Handling non-fully-resolved macros")
        }
    }

    fn pop_frame_stack_if_reserved(&mut self, repr: &str) -> Option<Frame<'s>> {
        let reserved = self.frame_stack.iter().any(|p| p.possible_next_segments.contains_key(repr));
        if reserved {
            self.frame_stack.pop()
        } else {
            None
        }
    }

    fn process_token(&mut self, root_macro_map: &MacroMatchTree<'s>, token: Token<'s>) -> Step<'s> {
        let repr = &**token.code;
        if let Some(subsegments) = self.current_frame.possible_next_segments.get(repr) {
            event!(TRACE, "Entering next segment of the current macro.");
            let mut new_match_tree =
                Self::enter(&mut self.current_frame.matched_macro_def, subsegments);
            let mut current_segment = Segment::new(token);
            mem::swap(&mut new_match_tree, &mut self.current_frame.possible_next_segments);
            mem::swap(&mut self.current_frame.current_segment, &mut current_segment);
            self.current_frame.resolved_segments.push(current_segment);
            Step::NewSegmentStarted
        } else if let Some(parent_macro) = self.pop_frame_stack_if_reserved(repr) {
            event!(TRACE, "Next token reserved by parent macro. Resolving current macro.");
            self.replace_current_with_parent_macro(parent_macro);
            Step::MacroStackPop(token.into())
        } else if let Some(segments) = root_macro_map.get(repr) {
            event!(TRACE, "Starting a new nested macro resolution.");
            let mut matched_macro_def = default();
            let mut current_frame = Frame {
                current_segment: Segment { header: token, body: default() },
                resolved_segments: default(),
                possible_next_segments: Self::enter(&mut matched_macro_def, segments),
                matched_macro_def,
            };
            mem::swap(&mut self.current_frame, &mut current_frame);
            self.frame_stack.push(current_frame);
            Step::NewSegmentStarted
        } else {
            event!(TRACE, "Consuming token as current segment body.");
            Step::NormalToken(token.into())
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
        panic!("Internal error. Not all tokens were consumed while constructing the expression.");
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
        syntax::Item::Block(_) => panic!(),
    }
}
