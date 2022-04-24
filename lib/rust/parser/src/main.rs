#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]
#![feature(generic_associated_types)]
#![recursion_limit = "256"]
#![feature(specialization)]


use crate::prelude::*;

pub mod lexer;
pub mod location;
pub mod macros;
pub mod source;

use enso_data_structures::list;
use enso_data_structures::list::List;
use lexer::Token;
use macros::Pattern;
use source::WithSources;

pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_types::traits::*;
    pub use enso_types::Bytes;
}



// ==================================



use lexer::Lexer;

#[derive(Clone, Debug)]
pub enum TokenOrAst {
    Token(Token),
    Ast(Ast),
}

impl From<Token> for TokenOrAst {
    fn from(t: Token) -> Self {
        Self::Token(t)
    }
}

impl From<Ast> for TokenOrAst {
    fn from(t: Ast) -> Self {
        Self::Ast(t)
    }
}



// ======================
// === MacroMatchTree ===
// ======================

#[derive(Default, Debug, Deref, DerefMut)]
pub struct MacroMatchTree<'a> {
    map: HashMap<&'a str, Vec<PartiallyMatchedMacro<'a>>>,
}

#[derive(Clone, Debug)]
pub struct PartiallyMatchedMacro<'a> {
    required_segments: List<macros::SegmentDefinition<'a>>,
    definition:        Rc<macros::Definition<'a>>,
}



// =====================
// === MacroResolver ===
// =====================

#[derive(Debug)]
pub struct MacroResolver<'a> {
    current_segment:        MatchedSegment,
    resolved_segments:      Vec<MatchedSegment>,
    possible_next_segments: MacroMatchTree<'a>,
}

#[derive(Debug)]
pub struct MatchedSegment {
    header: Token,
    body:   Vec<TokenOrAst>,
}

impl MatchedSegment {
    pub fn new(header: Token) -> Self {
        let body = default();
        Self { header, body }
    }
}


#[derive(Debug)]
pub struct Resolver<'a> {
    leading_tokens:    Vec<Token>,
    current_macro:     Option<MacroResolver<'a>>,
    macro_stack:       Vec<MacroResolver<'a>>,
    matched_macro_def: Option<Rc<macros::Definition<'a>>>,
    root_macro_map:    MacroMatchTree<'a>,
}

#[derive(Clone, Copy, Debug)]
pub enum ResolverStep {
    NormalToken,
    NewSegmentStarted,
    MacroStackPop,
}

macro_rules! if_let {
    (($match1_cons:ident($($match1:tt)*), $match2_cons:ident($($match2:tt)*))
        = ($expr1:expr, $expr2:expr) $matched:tt else $($not_matched:tt)*) => {
            let mut matched = false;
            let mut out = None;
            if let $match1_cons($($match1)*) = $expr1 {
                if let $match2_cons($($match2)*) = $expr2 {
                    matched = true;
                    out = Some($matched);
                }
            }
            if matched {
                out.unwrap()
            } else {
                $($not_matched)*
            }
    };
}

impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        let leading_tokens = default();
        let current_macro = default();
        let macro_stack = default();
        let matched_macro_def = default();
        let mut root_macro_map = MacroMatchTree::default();

        let if_then = macro_if_then();
        let if_then_else = macro_if_then_else();
        let if_then = PartiallyMatchedMacro {
            required_segments: if_then.segments.tail.clone(),
            definition:        Rc::new(if_then),
        };
        let if_then_else = PartiallyMatchedMacro {
            required_segments: if_then_else.segments.tail.clone(),
            definition:        Rc::new(if_then_else),
        };
        root_macro_map.entry("if").or_default().push(if_then);
        root_macro_map.entry("if").or_default().push(if_then_else);

        println!("{:#?}", root_macro_map);
        Self { leading_tokens, current_macro, macro_stack, matched_macro_def, root_macro_map }
    }

    pub fn run(&mut self, lexer: &Lexer, tokens: &[Token]) -> Ast {
        let mut stream = tokens.into_iter();
        let mut opt_token = stream.next().copied();
        loop {
            if let Some(token) = opt_token {
                let repr = lexer.repr(token);
                println!("\n>> '{}' = {:#?}", repr, token);
                match self.enter(lexer, token) {
                    ResolverStep::NormalToken => {
                        match self.current_macro.as_mut() {
                            Some(current_macro) =>
                                current_macro.current_segment.body.push(token.into()),
                            None => self.leading_tokens.push(token),
                        }
                        opt_token = stream.next().copied();
                    }
                    ResolverStep::NewSegmentStarted => {
                        opt_token = stream.next().copied();
                    }
                    ResolverStep::MacroStackPop => {}
                }
                println!("{:#?}", self.current_macro);
                println!("{:#?}", self.macro_stack);
            } else {
                break;
            }
        }
        self.resolve_current_macro()
    }

    pub fn resolve_current_macro(&mut self) -> Ast {
        println!("FINISH");
        let current_macro = mem::take(&mut self.current_macro).unwrap();
        let mut segments = current_macro.resolved_segments;
        segments.push(current_macro.current_segment);
        println!("{:#?}", segments);
        println!("{:#?}", self.matched_macro_def);

        if let Some(macro_def) = &self.matched_macro_def {
            let matched_sections = macro_def
                .segments
                .into_iter()
                .zip(segments)
                .map(|(segment_def, segment_match)| {
                    let pattern = &segment_def.pattern;
                    let token_stream = &segment_match.body;
                    (segment_match.header, pattern.resolve(&token_stream))
                })
                .collect_vec();
            (macro_def.body)(matched_sections)
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

    pub fn enter(&mut self, lexer: &Lexer, token: Token) -> ResolverStep {
        let repr = lexer.repr(token);
        let mut out = None;
        if let Some(current_macro) = self.current_macro.as_mut() {
            if let Some(subsegments) = current_macro.possible_next_segments.get(repr) {
                // Entering subsegments of the current macro.
                let mut new_match_tree = Self::enter_path(&mut self.matched_macro_def, subsegments);
                let mut current_segment = MatchedSegment::new(token);
                mem::swap(&mut new_match_tree, &mut current_macro.possible_next_segments);
                mem::swap(&mut current_macro.current_segment, &mut current_segment);
                current_macro.resolved_segments.push(current_segment);
                out = Some(ResolverStep::NewSegmentStarted);
            }
        }
        out.unwrap_or_else(|| {
            if let Some(mut current_macro) = self.pop_macro_stack_if_reserved(repr) {
                // Finishing the current macro because next token is reserved (by parent macro).
                let ast = self.resolve_current_macro();
                current_macro.current_segment.body.push(ast.into());
                self.current_macro = Some(current_macro);
                ResolverStep::MacroStackPop
            } else if let Some(segments) = self.root_macro_map.get(repr) {
                // Starting a new nested macro (the current token is the first token of a root
                // macro).
                println!("NEW ROOT macro started");
                let mut current_macro = Some(MacroResolver {
                    current_segment:        MatchedSegment { header: token, body: default() },
                    resolved_segments:      default(),
                    possible_next_segments: Self::enter_path(&mut self.matched_macro_def, segments),
                });
                mem::swap(&mut self.current_macro, &mut current_macro);
                current_macro.map(|t| self.macro_stack.push(t));
                ResolverStep::NewSegmentStarted
            } else {
                ResolverStep::NormalToken
            }
        })
    }

    fn enter_path(
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
                new_section_tree.entry(&first.header).or_default().push(x);
            } else {
                if matched_macro_def.is_some() {
                    // warning
                }
                *matched_macro_def = Some(v.definition.clone_ref());
            }
        }
        new_section_tree
    }
}


pub type Ast = location::With<AstData>;

#[derive(Clone, Debug)]
pub enum AstData {
    Ident(lexer::Ident),
    MultiSegmentApp(MultiSegmentApp),
}

#[derive(Clone, Debug)]
pub struct MultiSegmentApp {
    segments: NonEmptyVec<MultiSegmentAppSegment>,
}

#[derive(Clone, Debug)]
pub struct MultiSegmentAppSegment {
    header: Token,
    body:   Ast,
}

impl<'s> Debug for WithSources<'s, &AstData> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            AstData::Ident(t) => f.debug_tuple("Ident").field(&self.trans(|_| t)).finish(),
            AstData::MultiSegmentApp(t) =>
                f.debug_tuple("MultiSegmentApp").field(&self.trans(|_| t)).finish(),
        }
    }
}

impl<'s> Debug for WithSources<'s, &MultiSegmentApp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.segments.iter().map(|t| self.trans(|_| t))).finish()
    }
}

impl<'s> Debug for WithSources<'s, &MultiSegmentAppSegment> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MultiSegmentAppSegment")
            .field("header", &self.trans(|_| &self.header))
            .field("body", &self.trans(|_| &self.body))
            .finish()
    }
}

fn tokens_to_ast(tokens: Vec<TokenOrAst>) -> Ast {
    let mut tokens = tokens.into_iter();
    match tokens.next() {
        None => panic!(),
        Some(first) => {
            if let Some(elem) = tokens.next() {
                panic!("Got element: {:#?}", elem);
            }
            match first {
                TokenOrAst::Token(token) => match token.elem {
                    lexer::Kind::Ident(ident) => token.with_elem(AstData::Ident(ident)),
                    _ => panic!(),
                },
                TokenOrAst::Ast(ast) => ast,
            }
        }
    }
}

fn matched_segments_into_multi_segment_app(matched_segments: Vec<(Token, Vec<TokenOrAst>)>) -> Ast {
    let mut segments = matched_segments
        .into_iter()
        .map(|segment| {
            let header = segment.0;
            let body = tokens_to_ast(segment.1);
            MultiSegmentAppSegment { header, body }
        })
        .collect_vec();
    if let Ok(mut segments) = NonEmptyVec::try_from(segments) {
        let first = segments.first_mut();
        let (left_offset_token, left_trimmed_token) = first.header.split_at_start();
        first.header = left_trimmed_token;
        let last_segment = segments.last();
        let total = left_offset_token.extended_to(&last_segment.body);
        let data = AstData::MultiSegmentApp(MultiSegmentApp { segments });
        total.with_elem(data)
    } else {
        panic!()
    }
}

fn macro_if_then_else<'a>() -> macros::Definition<'a> {
    let section1 = macros::SegmentDefinition::new("if", Pattern::Everything);
    let section2 = macros::SegmentDefinition::new("then", Pattern::Everything);
    let section3 = macros::SegmentDefinition::new("else", Pattern::Everything);
    macros::Definition {
        rev_prefix_pattern: None,
        segments:           list::NonEmpty::singleton(section3)
            .with_head(section2)
            .with_head(section1),
        body:               Rc::new(|t| matched_segments_into_multi_segment_app(t)),
    }
}

fn macro_if_then<'a>() -> macros::Definition<'a> {
    let section1 = macros::SegmentDefinition::new("if", Pattern::Everything);
    let section2 = macros::SegmentDefinition::new("then", Pattern::Everything);
    macros::Definition {
        rev_prefix_pattern: None,
        segments:           list::NonEmpty::singleton(section2).with_head(section1),
        body:               Rc::new(|t| matched_segments_into_multi_segment_app(t)),
    }
}



fn main() {
    // let str = "if a then b else c";
    let str = "if if x then y then b";
    let mut lexer = Lexer::new(str);
    println!("{:#?}", lexer.run());
    println!("{:#?}", lexer.output);

    println!("\n---\n");


    let mut resolver = Resolver::new();
    let ast = resolver.run(&lexer, lexer.output.borrow_vec());

    println!("\n---\n");

    println!("{:#?}", WithSources::new(str, &ast));
}

// var1 = if (a > b) then if x then y else z else w
