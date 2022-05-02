#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]
#![feature(generic_associated_types)]
#![recursion_limit = "256"]
#![feature(specialization)]
#![allow(incomplete_features)]
#![feature(let_chains)]

use crate::prelude::*;

pub mod lexer;
pub mod location;
pub mod macros;
pub mod source;

use enso_data_structures::im_list;
use enso_data_structures::im_list::List;
use lexer::Token;
use macros::Pattern;

pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_types::traits::*;
    pub use enso_types::Bytes;
}



// ==================================



use crate::source::DebugLeaf;
use crate::source::HasRepr;
use lexer::Lexer;


#[derive(Clone, Debug)]
pub enum TokenOrAst {
    Token(Token),
    Ast(Ast),
}

impl TokenOrAst {
    fn is_variant(&self, variant: lexer::KindVariant) -> bool {
        match self {
            TokenOrAst::Token(token) => token.is(variant),
            _ => false,
        }
    }

    fn is_operator(&self) -> bool {
        self.is_variant(lexer::KindVariant::Operator)
    }

    fn left_visible_offset(&self) -> usize {
        match self {
            Self::Token(t) => t.left_visible_offset,
            Self::Ast(t) => t.left_visible_offset,
        }
    }

    fn location(&self) -> location::Info {
        match self {
            Self::Token(t) => t.location(),
            Self::Ast(t) => t.location(),
        }
    }

    fn trim_left(&mut self) -> location::Info {
        match self {
            Self::Token(t) => t.trim_left(),
            Self::Ast(t) => t.trim_left(),
        }
    }
}

impl From<Token> for TokenOrAst {
    fn from(t: Token) -> Self {
        TokenOrAst::Token(t)
    }
}

impl From<Ast> for TokenOrAst {
    fn from(t: Ast) -> Self {
        TokenOrAst::Ast(t)
    }
}


#[derive(Debug)]
pub enum TokenOrAstOrMacroResolver<'a> {
    TokenOrAst(TokenOrAst),
    MacroResolver(MacroResolver<'a>),
}

impl<'a> From<TokenOrAst> for TokenOrAstOrMacroResolver<'a> {
    fn from(t: TokenOrAst) -> Self {
        Self::TokenOrAst(t)
    }
}

impl<'a> From<MacroResolver<'a>> for TokenOrAstOrMacroResolver<'a> {
    fn from(t: MacroResolver<'a>) -> Self {
        Self::MacroResolver(t)
    }
}

impl<'a> TryAsRef<TokenOrAst> for TokenOrAstOrMacroResolver<'a> {
    fn try_as_ref(&self) -> Option<&TokenOrAst> {
        match self {
            Self::TokenOrAst(t) => Some(t),
            _ => None,
        }
    }
}

impl<'a> source::HasRepr<'a> for source::With<'a, &TokenOrAst> {
    fn repr(&self) -> &'a str {
        match self.data {
            TokenOrAst::Token(t) => self.with_data(t).repr(),
            TokenOrAst::Ast(t) => self.with_data(t).repr(),
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
        self.entry(header).or_insert(NonEmptyVec::singleton(entry));
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
            header: location::With::new_no_offset_phantom(Bytes::from(0), lexer::Kind::newline()),
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
            body:               Rc::new(|lexer, _, mut v| {
                if v.len() != 1 {
                    panic!()
                }
                let t = v.pop().unwrap().1;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    pub fn run(
        mut self,
        lexer: &Lexer<'a>,
        root_macro_map: &MacroMatchTree<'a>,
        tokens: Vec<TokenOrAst>,
    ) -> Ast {
        let mut stream = tokens.into_iter();
        let mut opt_token: Option<TokenOrAst>;
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
                TokenOrAst::Token(token) => self.process_token(lexer, root_macro_map, token),
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

        println!("===================");
        trace_state!();

        Self::resolve(lexer, self.current_macro, None)
    }

    fn replace_current_with_parent_macro(&mut self, mut parent_macro: MacroResolver<'a>) {
        mem::swap(&mut parent_macro, &mut self.current_macro);
        let mut child_macro = parent_macro;
        if let Some(def) = &child_macro.matched_macro_def {
            let pattern = &def.segments.last().pattern;
            let child_tokens = mem::take(&mut child_macro.current_segment.body);
            let (mut new_child_tokens, new_parent_tokens) = pattern.resolve(child_tokens);
            mem::swap(&mut child_macro.current_segment.body, &mut new_child_tokens);
            self.current_macro.current_segment.body.push(child_macro.into());
            self.current_macro.current_segment.body.extend(new_parent_tokens);
        } else {
            panic!()
        }
    }

    fn resolve(
        lexer: &Lexer<'a>,
        m: MacroResolver<'a>,
        prefix_tokens: Option<Vec<TokenOrAst>>,
    ) -> Ast {
        let mut segments = m.resolved_segments;
        segments.push(m.current_segment);
        let mut sss: Vec<(Token, Vec<TokenOrAst>)> = vec![];
        for segment in segments {
            let mut ss: Vec<TokenOrAst> = vec![];
            for item in segment.body {
                let resolved_token = match item {
                    TokenOrAstOrMacroResolver::MacroResolver(m2) => {
                        if let Some(macro_def) = &m2.matched_macro_def
                        && let Some(pfx_pattern) = &macro_def.rev_prefix_pattern {
                            ss.reverse();
                            let spacing = m2.current_segment.header.left_offset > Bytes::from(0);
                            let (mut matched, unmatched) = pfx_pattern.resolve2(ss,spacing).unwrap();
                            matched.reverse();
                            ss = unmatched;
                            ss.reverse();
                            Self::resolve(lexer, m2, Some(matched)).into()
                        } else {
                            Self::resolve(lexer, m2, None).into()
                        }
                    },
                    TokenOrAstOrMacroResolver::TokenOrAst(t) => t,
                };
                ss.push(resolved_token);
            }
            sss.push((segment.header, ss));
        }

        if let Some(macro_def) = m.matched_macro_def {
            (macro_def.body)(lexer, prefix_tokens, sss)
        } else {
            panic!()
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

    pub fn process_token(
        &mut self,
        lexer: &Lexer<'a>,
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
        } else if let Some(mut parent_macro) = self.pop_macro_stack_if_reserved(repr) {
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
                new_section_tree.entry(&first.header).or_insert(NonEmptyVec::singleton(x));
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


fn annotate_tokens_that_need_spacing(items: Vec<TokenOrAst>) -> Vec<TokenOrAst> {
    items
        .into_iter()
        .map(|item| match item {
            TokenOrAst::Token(_) => item,
            TokenOrAst::Ast(ast) => match &ast.elem {
                AstData::MultiSegmentApp(data) => {
                    if data.segments.first().header.elem.variant() != lexer::KindVariant::Symbol {
                        TokenOrAst::Ast(
                            ast.with_error(
                                "This expression cannot be used in a non-spaced equation.",
                            ),
                        )
                    } else {
                        TokenOrAst::Ast(ast)
                    }
                }
                _ => TokenOrAst::Ast(ast),
            },
        })
        .collect()
}

pub fn resolve_operator_precedence(lexer: &Lexer, items: Vec<TokenOrAst>) -> Ast {
    type Tokens = Vec<TokenOrAst>;
    let mut flattened: Tokens = default();
    let mut no_space_group: Tokens = default();
    let mut processs_no_space_group = |flattened: &mut Tokens, no_space_group: &mut Tokens| {
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
        if item.left_visible_offset() == 0 || no_space_group.is_empty() {
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

fn resolve_operator_precedence_internal(lexer: &Lexer, items: Vec<TokenOrAst>) -> Ast {
    // Reverse-polish notation encoding.
    let mut output: Vec<TokenOrAst> = default();
    let mut operator_stack: Vec<WithPrecedence<OprAppOpr>> = default();
    let mut last_token_was_ast = false;
    let mut last_token_was_opr = false;
    for item in items {
        if let TokenOrAst::Token(token) = item && let lexer::Kind::Operator(opr) = token.elem {
            // Item is an operator.
            let last_token_was_opr_copy = last_token_was_opr;
            last_token_was_ast = false;
            last_token_was_opr = true;

            let prec = precedence_of(lexer.repr(&item));
            let opr = item.location().with_elem(opr);

            if last_token_was_opr_copy && let Some(prev_opr) = operator_stack.last_mut() {
                // Error. Multiple operators next to each other.
                let prev_opr_prec = prev_opr.precedence;
                match &mut prev_opr.elem {
                    Err(err) => err.oprs.push(opr),
                    Ok(prev) => prev_opr.elem = Err(MultipleOperatorError::new(vec![*prev, opr]))
                }
            } else {
                while let Some(prev_opr) = operator_stack.last()
                   && prev_opr.precedence >= prec
                   && let Some(prev_opr) = operator_stack.pop()
                   && let Some(rhs) = output.pop()
                {
                    // Prev operator in the [`operator_stack`] has a higher precedence.
                    let lhs = output.pop().map(token_to_ast);
                    let ast = Ast::opr_app(lhs, prev_opr.elem, Some(token_to_ast(rhs)));
                    output.push(ast.into());
                }
                operator_stack.push(WithPrecedence::new(prec, Ok(opr)));
            }
        } else if last_token_was_ast && let Some(lhs) = output.pop() {
            // Multiple non-operators next to each other.
            let lhs = token_to_ast(lhs);
            let rhs = token_to_ast(item);
            let ast = Ast::app(lhs, rhs);
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
        opt_rhs = Some(Ast::opr_app(opt_lhs, opr.elem, opt_rhs));
    }
    if !output.is_empty() {
        panic!("Internal error. Not all tokens were consumed while constructing the expression.");
    }
    Ast::opr_section_boundary(opt_rhs.unwrap_or_else(|| Ast::fixme()))
}


pub struct ChildrenVisitor<T> {
    sub_visitor: T,
}

impl<T> Visitor for ChildrenVisitor<T> {
    type SubVisitor = T;
    fn sub_visitor(&mut self) -> &mut Self::SubVisitor {
        &mut self.sub_visitor
    }
}

pub trait Visitor {
    type SubVisitor;
    fn sub_visitor(&mut self) -> &mut Self::SubVisitor;
}

pub trait Visit<T: ?Sized> {
    fn visit(&mut self, elem: &T);
    // fn visit_mut(&mut self, elem:&mut T);
}

impl<T: Visit<S>, S> Visit<Option<S>> for T {
    default fn visit(&mut self, t: &Option<S>) {
        if let Some(elem) = t {
            self.visit(elem)
        }
    }
}

impl<T: Visit<S>, E, S> Visit<Result<S, E>> for T {
    default fn visit(&mut self, t: &Result<S, E>) {
        if let Ok(elem) = t {
            self.visit(elem)
        }
    }
}

impl<T> Visit<str> for T {
    default fn visit(&mut self, t: &str) {}
}

impl<T> Visit<&str> for T {
    default fn visit(&mut self, t: &&str) {}
}

impl<T: Visit<S>, S> Visit<Box<S>> for T {
    default fn visit(&mut self, t: &Box<S>) {
        self.visit(&*t)
    }
}

impl<T: Visit<S>, S> Visit<NonEmptyVec<S>> for T {
    default fn visit(&mut self, t: &NonEmptyVec<S>) {
        t.iter().map(|s| self.visit(s));
    }
}

impl<T: Visit<S>, S> Visit<location::With<S>> for T {
    default fn visit(&mut self, t: &location::With<S>) {
        self.visit(&t.elem)
    }
}

impl<T> Visit<lexer::Ident> for T {
    default fn visit(&mut self, token: &lexer::Ident) {}
}

impl<T> Visit<lexer::Operator> for T {
    default fn visit(&mut self, token: &lexer::Operator) {}
}

// Fixme
impl<T> Visit<lexer::Kind> for T {
    default fn visit(&mut self, t: &lexer::Kind) {}
}

pub struct TestVisitor {}
impl Visitor for TestVisitor {
    type SubVisitor = Self;

    fn sub_visitor(&mut self) -> &mut Self::SubVisitor {
        self
    }
}


pub type Ast = location::With<AstData>;

#[derive(Clone, Copy, Debug, Visitor)]
pub struct Error {
    message: &'static str,
}

impl Error {
    pub fn new(message: &'static str) -> Self {
        Self { message }
    }
}

#[derive(Clone, Debug, Visitor)]
pub enum AstData {
    WithError(Error, Box<Ast>),
    Ident(lexer::Ident),
    MultiSegmentApp(MultiSegmentApp),
    OprSectionBoundary(Box<Ast>),
    OprApp(Box<OprApp>),
    App(Box<App>),
    Fixme,
}


impl Ast {
    fn fixme() -> Ast {
        location::With::new_no_offset_phantom(Bytes::from(0), AstData::Fixme)
    }

    fn with_error(self, message: &'static str) -> Self {
        let error = Error::new(message);
        let location = self.location();
        let data = AstData::WithError(error, Box::new(self));
        location.with_elem(data)
    }

    fn opr_section_boundary(section: Ast) -> Ast {
        let (left_offset_token, section) = section.split_at_start();
        let total = left_offset_token.extended_to(&section);
        let ast_data = AstData::OprSectionBoundary(Box::new(section));
        total.with_elem(ast_data)
    }

    fn app(func: Ast, arg: Ast) -> Ast {
        let (left_offset_token, func) = func.split_at_start();
        let total = left_offset_token.extended_to(&arg);
        let ast_data = AstData::App(Box::new(App { func, arg }));
        total.with_elem(ast_data)
    }

    fn opr_app(
        mut lhs: Option<Ast>,
        mut opr: Result<location::With<lexer::Operator>, MultipleOperatorError>,
        rhs: Option<Ast>,
    ) -> Ast {
        let left_offset_token = if let Some(lhs_val) = lhs {
            let (left_offset_token, new_lhs) = lhs_val.split_at_start();
            lhs = Some(new_lhs);
            left_offset_token
        } else {
            match &mut opr {
                Ok(xopr) => {
                    let (left_offset_token, new_opr) = xopr.split_at_start();
                    *xopr = new_opr;
                    left_offset_token
                }
                Err(e) => {
                    let first = e.oprs.first_mut().unwrap(); // fixme
                    let (left_offset_token, new_opr) = first.split_at_start();
                    *first = new_opr;
                    left_offset_token
                }
            }
        };
        let total = if let Some(ref rhs) = rhs {
            left_offset_token.extended_to(&rhs)
        } else {
            match &opr {
                Ok(xopr) => left_offset_token.extended_to(&xopr),
                Err(e) => left_offset_token.extended_to(e.oprs.last().unwrap()), // fixme
            }
        };
        let ast_data = AstData::OprApp(Box::new(OprApp { lhs, opr, rhs }));
        total.with_elem(ast_data)
    }
}



#[derive(Clone, Debug, Visitor)]
pub struct App {
    func: Ast,
    arg:  Ast,
}

pub type OprAppOpr = Result<location::With<lexer::Operator>, MultipleOperatorError>;

#[derive(Clone, Debug, Visitor)]
pub struct OprApp {
    lhs: Option<Ast>,
    opr: OprAppOpr,
    rhs: Option<Ast>,
}

#[derive(Clone, Debug)]
pub struct MultipleOperatorError {
    oprs: Vec<location::With<lexer::Operator>>,
}

impl MultipleOperatorError {
    pub fn new(oprs: Vec<location::With<lexer::Operator>>) -> Self {
        Self { oprs }
    }
}

#[derive(Clone, Debug, Visitor)]
pub struct MultiSegmentApp {
    prefix:   Option<Box<Ast>>,
    segments: NonEmptyVec<MultiSegmentAppSegment>,
}

#[derive(Clone, Debug, Visitor)]
pub struct MultiSegmentAppSegment {
    header: Token,
    body:   Option<Ast>,
}

impl<'s, 't, T> Debug for source::With<'s, &'t Box<T>>
where source::With<'s, &'t T>: Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t: &T = &**self.data;
        Debug::fmt(&self.with_data(t), f)
    }
}

impl<'s> Debug for source::With<'s, &AstData> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data {
            AstData::WithError(err, ast) =>
                f.debug_tuple("Fixme").field(&err).field(&self.trans(|_| ast)).finish(),
            AstData::Fixme => f.debug_tuple("Fixme").finish(),
            AstData::Ident(t) => f.debug_tuple("Ident").field(&self.trans(|_| t)).finish(),
            AstData::MultiSegmentApp(t) =>
                f.debug_tuple("MultiSegmentApp").field(&self.trans(|_| t)).finish(),
            AstData::OprSectionBoundary(t) =>
                f.debug_tuple("OprSectionBoundary").field(&self.trans(|_| t)).finish(),
            AstData::OprApp(t) => f.debug_tuple("OprApp").field(&self.trans(|_| t)).finish(),
            AstData::App(t) => f.debug_tuple("App").field(&self.trans(|_| t)).finish(),
        }
    }
}

impl<'s> Debug for source::With<'s, &App> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entry(&self.with_data(&self.func)).entry(&self.with_data(&self.arg)).finish()
    }
}

impl<'s, 't, T> Debug for source::With<'s, &'t Option<T>>
where source::With<'s, &'t T>: Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.data.as_ref().map(|t| self.with_data(t)), f)
    }
}

impl<'s, 't, T, E> Debug for source::With<'s, &'t Result<T, E>>
where
    source::With<'s, &'t T>: Debug,
    source::With<'s, &'t E>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.data.as_ref().map(|t| self.with_data(t)).map_err(|t| self.with_data(t)), f)
    }
}

impl<'s> Debug for source::With<'s, &OprApp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entry(&self.with_data(&self.lhs))
            .entry(&self.with_data(&self.opr))
            .entry(&self.with_data(&self.rhs))
            .finish()
    }
}

impl<'s> Debug for source::With<'s, &MultipleOperatorError> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.oprs.iter().map(|t| self.trans(|_| t))).finish()
    }
}

impl<'s> Debug for source::With<'s, &MultiSegmentApp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entry(&self.trans(|_| &self.prefix))
            .entries(self.segments.iter().map(|t| self.trans(|_| t)))
            .finish()
    }
}

impl<'s> Debug for source::With<'s, &MultiSegmentAppSegment> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MultiSegmentAppSegment")
            .field("header", &self.trans(|_| &self.header))
            .field("body", &self.trans(|_| &self.body))
            .finish()
    }
}

fn token_to_ast(elem: TokenOrAst) -> Ast {
    match elem {
        TokenOrAst::Token(token) => match token.elem {
            lexer::Kind::Ident(ident) => elem.location().with_elem(AstData::Ident(ident)),
            _ => panic!(),
        },
        TokenOrAst::Ast(ast) => ast,
    }
}

fn matched_segments_into_multi_segment_app<'s>(
    lexer: &Lexer<'s>,
    mut prefix_tokens: Option<Vec<TokenOrAst>>,
    matched_segments: Vec<(Token, Vec<TokenOrAst>)>,
) -> Ast {
    let segments = matched_segments
        .into_iter()
        .map(|segment| {
            let header = segment.0;
            let body = (segment.1.len() > 0)
                .as_some_from(|| resolve_operator_precedence(lexer, segment.1));
            MultiSegmentAppSegment { header, body }
        })
        .collect_vec();
    if let Ok(mut segments) = NonEmptyVec::try_from(segments) {
        let left_offset_token =
            if let Some(first) = prefix_tokens.as_mut().and_then(|t| t.first_mut()) {
                first.trim_left()
            } else {
                let first = segments.first_mut();
                let (left_offset_token, left_trimmed_token) = first.header.split_at_start();
                first.header = left_trimmed_token;
                left_offset_token
            };
        let last_segment = segments.last();
        let total = if let Some(last_segment_body) = &last_segment.body {
            left_offset_token.extended_to(last_segment_body)
        } else {
            left_offset_token.extended_to(&last_segment.header)
        };
        let prefix = prefix_tokens.map(|t| Box::new(resolve_operator_precedence(lexer, t)));
        let data = AstData::MultiSegmentApp(MultiSegmentApp { prefix, segments });
        total.with_elem(data)
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
        Box::new(Pattern::Item(macros::Item { has_rhs_spacing: Some(false) })),
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

// #[test]
mod test {
    use super::*;

    pub fn ident(repr: &str) -> Ast {
        match lexer::Kind::parse_ident(repr) {
            lexer::Kind::Ident(ident) =>
                location::With::new_with_len(Bytes::from(repr.len()), AstData::Ident(ident)),
            _ => panic!(),
        }
    }

    pub fn app_segment(header: Token, body: Option<Ast>) -> MultiSegmentAppSegment {
        MultiSegmentAppSegment { header, body }
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
    let str = "foo (a)";
    let mut lexer = Lexer::new(str);
    lexer.run();

    let mut root_macro_map = builtin_macros();

    event!(TRACE, "Registered macros:\n{:#?}", root_macro_map);

    let mut resolver = Resolver::new_root();
    let ast = resolver.run(
        &lexer,
        &root_macro_map,
        lexer.output.borrow_vec().iter().map(|t| (*t).into()).collect_vec(),
    );
    println!("{:#?}", source::With::new(str, &ast));

    let seg1 =
        test::app_segment(Token::symbol("("), Some(Ast::opr_section_boundary(test::ident("a"))));
    let seg2 = test::app_segment(Token::symbol(")"), None);

    let ast2 = Ast::opr_section_boundary(location::With::new_with_len(
        Bytes::from(0),
        AstData::MultiSegmentApp(MultiSegmentApp {
            prefix:   None,
            segments: NonEmptyVec::try_from(vec![seg1, seg2]).unwrap(),
        }),
    ));

    println!("{:#?}", &ast2);
    foo();

    // TestVisitor {}.visit(&ast2);

    // let a = 5;
    // let b = 5;
    // let c = a + / b;
}

macro_rules! test_macro_build {
    ([[$($item1:tt)*] [$($item2:tt)*]] $($ts:tt)*) => {
        test_macro_build!{[[Ast::app($($item1)*,$($item2)*)]] $($ts)*}
    };
    ([[$($item:tt)*]]) => {
        $($item)*
    };
    ([$($stack:tt)*] $a:ident $($ts:tt)*) => {
        test_macro_build!{[$($stack)* [test::ident(stringify!{$a})]] $($ts)*}
    };
    ([$($stack:tt)*] [$($ss:tt)*] $($ts:tt)*) => {
        test_macro_build!{[$($stack)* [test_macro_build_multi_app!{$($ss)*}]] $($ts)*}
    };
}

macro_rules! test_macro_build_multi_app {
    ($($section:literal $($argument:ident)?)*) => {
        location::With::new_with_len(
            Bytes::from(0),
            AstData::MultiSegmentApp(MultiSegmentApp {
                prefix:   None,
                segments: NonEmptyVec::try_from(vec![$(
                    test::app_segment(Token::symbol($section),
                    test_macro_build_multi_app_argument!{$($argument)?}
                    )
                ),*]).unwrap(),
            }),
        )
    };
}

macro_rules! test_macro_build_multi_app_argument {
    ($argument:ident) => {
        Some(Ast::opr_section_boundary(test::ident(stringify!($argument))))
    };
    () => {
        None
    };
}

fn foo() {
    println!("{:#?}", test_macro_build! { [] foo ["(" a ")"] });
    // println!("{:#?}", test_macro_build! { [] a b });
}

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
fn tmp() {}

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
