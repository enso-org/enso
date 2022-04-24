#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]
#![feature(generic_associated_types)]
#![recursion_limit = "256"]
#![feature(specialization)]


use crate::prelude::*;

pub mod lexer;
pub mod macros;
pub mod source;

use enso_data_structures::list;
use enso_data_structures::list::List;
use lexer::Token;
use macros::Pattern;
use source::WithSources;

pub mod prelude {
    pub use enso_prelude::*;
}



// ==================================

pub struct Stream<'a> {
    in_bounds: bool,
    index:     usize,
    tokens:    &'a [Token],
}

impl<'a> Stream<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        let in_bounds = default();
        let index = default();
        Self { in_bounds, index, tokens }.init()
    }

    fn init(mut self) -> Self {
        self.check_current_bounds();
        self
    }

    fn check_current_bounds(&mut self) {
        self.in_bounds = self.index < self.tokens.len();
    }

    pub fn first(&self) -> Option<&'a Token> {
        self.in_bounds.as_some_from(|| &self.tokens[self.index])
    }

    pub fn next(&mut self) {
        if self.in_bounds {
            self.index += 1;
            self.check_current_bounds();
        }
    }
}



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

#[derive(Default, Debug)]
pub struct MacroMatchTree<'a> {
    subsections: HashMap<&'a str, Vec<PartiallyMatchedMacro<'a>>>,
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
pub struct MacroResolver {
    current_segment:   MatchedSegment,
    resolved_segments: Vec<MatchedSegment>,
}

#[derive(Debug)]
pub struct MatchedSegment {
    header: Token,
    body:   Vec<TokenOrAst>,
}



#[derive(Debug)]
pub struct Resolver<'a> {
    leading_tokens:       Vec<Token>,
    current_macro:        Option<MacroResolver>,
    macro_stack:          Vec<MacroResolver>,
    matched_macro_def:    Option<Rc<macros::Definition<'a>>>,
    parent_segment_trees: Vec<MacroMatchTree<'a>>,
    macro_match_tree:     MacroMatchTree<'a>,
    root_segment_tree:    MacroMatchTree<'a>,
}

#[derive(Clone, Copy, Debug)]
pub enum ResolverStep {
    NormalToken,
    NewSegmentStarted,
    MacroStackPop,
}

impl<'a> Resolver<'a> {
    pub fn new() -> Self {
        let leading_tokens = default();
        let current_macro = default();
        let macro_stack = default();
        let matched_macro_def = default();
        let parent_segment_trees = default();
        let macro_match_tree = MacroMatchTree::default();
        let mut root_segment_tree = MacroMatchTree::default();

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
        root_segment_tree.subsections.entry("if").or_default().push(if_then);
        root_segment_tree.subsections.entry("if").or_default().push(if_then_else);

        println!("{:#?}", root_segment_tree);
        Self {
            leading_tokens,
            current_macro,
            macro_stack,
            matched_macro_def,
            macro_match_tree,
            root_segment_tree,
            parent_segment_trees,
        }
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
                println!("{:#?}", self.macro_match_tree);
                println!("{:#?}", self.parent_segment_trees);
            } else {
                break;
            }
        }
        self.finish()
    }

    pub fn finish(&mut self) -> Ast {
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

    pub fn is_reserved(&self, repr: &'a str) -> bool {
        self.parent_segment_trees.iter().any(|p| p.subsections.contains_key(repr))
    }

    pub fn enter(&mut self, lexer: &Lexer, token: Token) -> ResolverStep {
        let repr = lexer.repr(token);
        match self.macro_match_tree.subsections.get(repr) {
            Some(list) => {
                let current_macro = self.current_macro.as_mut().unwrap(); // has to be there
                let mut current_segment = MatchedSegment { header: token, body: default() };
                mem::swap(&mut current_macro.current_segment, &mut current_segment);
                current_macro.resolved_segments.push(current_segment);
                self.enter_path(list.clone());
                ResolverStep::NewSegmentStarted
            }
            None =>
                if self.is_reserved(repr) {
                    println!("RESERVED");
                    if let Some(mut current_macro) = self.macro_stack.pop() {
                        self.macro_match_tree = self.parent_segment_trees.pop().unwrap();
                        let ast = self.finish();
                        current_macro.current_segment.body.push(ast.into());
                        self.current_macro = Some(current_macro);
                        ResolverStep::MacroStackPop
                    } else {
                        panic!()
                    }
                } else {
                    match self.root_segment_tree.subsections.get(repr) {
                        Some(list) => {
                            println!("NEW ROOT macro started");
                            let current_segment =
                                MatchedSegment { header: token, body: default() };
                            let mut current_macro = Some(MacroResolver {
                                current_segment,
                                resolved_segments: default(),
                            });
                            let old_match_tree = self.enter_path(list.clone());
                            mem::swap(&mut self.current_macro, &mut current_macro);
                            if let Some(current_macro) = current_macro {
                                self.macro_stack.push(current_macro);
                            }
                            self.parent_segment_trees.push(old_match_tree);
                            ResolverStep::NewSegmentStarted
                        }
                        None => ResolverStep::NormalToken,
                    }
                },
        }
    }

    fn enter_path(&mut self, path: Vec<PartiallyMatchedMacro<'a>>) -> MacroMatchTree<'a> {
        self.matched_macro_def = None;
        let mut new_section_tree = MacroMatchTree::default();
        for v in path {
            if let Some(first) = v.required_segments.head() {
                let tail = v.required_segments.tail().cloned().unwrap_or_default();
                let definition = v.definition.clone_ref();
                let x = PartiallyMatchedMacro { required_segments: tail, definition };
                new_section_tree.subsections.entry(&first.header).or_default().push(x);
            } else {
                if self.matched_macro_def.is_some() {
                    // warning
                }
                self.matched_macro_def = Some(v.definition.clone_ref());
            }
        }
        // fixme: new_section_tree is created which is too much, we are using only subsections
        mem::swap(&mut new_section_tree, &mut self.macro_match_tree);
        new_section_tree
    }
}


pub type Ast = Token<AstData>;

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
