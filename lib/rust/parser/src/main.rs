#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]
#![feature(generic_associated_types)]
#![recursion_limit = "256"]
#![feature(specialization)]

use crate::prelude::*;

pub mod lexer;
pub mod source;

use enso_data_structures::list;
use enso_data_structures::list::List;
use lexer::Token;
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

#[derive(Clone, Debug)]
pub enum Pattern {
    Everything,
    // TokenVariant(lexer::KindVariant),
    Seq(Box<Pattern>, Box<Pattern>),
}

impl Pattern {
    pub fn resolve(&self, stream: &[TokenOrAst]) -> Vec<TokenOrAst> {
        let mut stream = stream.into_iter();
        self.resolve_internal(&mut stream)
    }

    fn resolve_internal(&self, stream: &mut slice::Iter<TokenOrAst>) -> Vec<TokenOrAst> {
        match self {
            // todo:perf of clone?
            Self::Everything => stream.cloned().collect(),
            // Self::TokenVariant(token_variant_pattern) =>
            //     if let Some(token) = stream.next() {
            //         let token = *token;
            //         if token.variant() == *token_variant_pattern {
            //             vec![token]
            //         } else {
            //             panic!()
            //         }
            //     } else {
            //         default()
            //     },
            Self::Seq(first, second) =>
                first.resolve_internal(stream).extended(second.resolve_internal(stream)),
        }
    }
}



#[derive(Derivative)]
#[derivative(Debug)]
pub struct Macro<'a> {
    prefix:   Option<Pattern>,
    segments: list::NonEmpty<MacroSegment<'a>>,
    #[derivative(Debug = "ignore")]
    body:     Rc<dyn Fn(Vec<(Token, Vec<TokenOrAst>)>) -> Ast>,
}

#[derive(Clone, Debug)]
pub struct MacroSegment<'a> {
    repr:    &'a str,
    pattern: Pattern,
}

#[derive(Debug)]
pub struct MatchedSegment {
    header: Token,
    body:   Vec<TokenOrAst>,
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

#[derive(Debug)]
pub struct MacroSegmentTreeDataRecord<'a> {
    list: List<MacroSegment<'a>>,
    def:  Rc<Macro<'a>>,
}

#[derive(Default, Debug)]
pub struct MacroSegmentTreeData<'a> {
    subsections: HashMap<&'a str, Vec<MacroSegmentTreeDataRecord<'a>>>,
    parents:     Vec<HashMap<&'a str, Vec<MacroSegmentTreeDataRecord<'a>>>>,
}

#[derive(Default, Debug, Deref, DerefMut)]
pub struct MacroSegmentTree<'a> {
    tree: MacroSegmentTreeData<'a>,
}

#[derive(Debug)]
pub struct MacroResolver {
    current_segment: MatchedSegment,
    segments:        Vec<MatchedSegment>,
}

#[derive(Default, Debug)]
pub struct Resolver<'a> {
    leading_tokens:    Vec<Token>,
    current_macro:     Option<MacroResolver>,
    macro_stack:       Vec<MacroResolver>,
    matched_macro_def: Option<Rc<Macro<'a>>>,
}

#[derive(Clone, Copy, Debug)]
pub enum ResolverStep {
    NormalToken,
    NewSegmentStarted,
    MacroStackPop,
}

impl<'a> Resolver<'a> {
    pub fn run(&mut self, lexer: &Lexer, tokens: &[Token]) -> Ast {
        let mut segment_tree = MacroSegmentTree::default();
        let mut root_segment_tree = MacroSegmentTree::default();

        let if_then = macro_if_then();
        let if_then_else = macro_if_then_else();
        let if_then = MacroSegmentTreeDataRecord {
            list: if_then.segments.tail.clone(),
            def:  Rc::new(if_then),
        };
        let if_then_else = MacroSegmentTreeDataRecord {
            list: if_then_else.segments.tail.clone(),
            def:  Rc::new(if_then_else),
        };
        root_segment_tree.subsections.entry("if").or_default().push(if_then);
        root_segment_tree.subsections.entry("if").or_default().push(if_then_else);

        println!("{:#?}", root_segment_tree);

        let mut stream = tokens.into_iter();
        let mut opt_token = stream.next().copied();
        loop {
            if let Some(token) = opt_token {
                let repr = lexer.repr(token);
                println!("\n>> '{}' = {:#?}", repr, token);
                match self.enter(lexer, &mut segment_tree, &root_segment_tree, token) {
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
                println!("{:#?}", segment_tree);
            } else {
                break;
            }
        }
        self.finish()
    }

    pub fn finish(&mut self) -> Ast {
        println!("FINISH");
        let current_macro = mem::take(&mut self.current_macro).unwrap();
        let mut segments = current_macro.segments;
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

    pub fn enter(
        &mut self,
        lexer: &Lexer,
        stack: &mut MacroSegmentTree<'a>,
        root: &MacroSegmentTree<'a>,
        token: Token,
    ) -> ResolverStep {
        let repr = lexer.repr(token);
        let mut new_root = false;
        let list = match stack.subsections.get(repr) {
            Some(list) => {
                let current_macro = self.current_macro.as_mut().unwrap(); // has to be there
                let mut current_segment = MatchedSegment { header: token, body: default() };
                mem::swap(&mut current_macro.current_segment, &mut current_segment);
                current_macro.segments.push(current_segment);
                Some(list)
            }
            None =>
                if stack.is_reserved(repr) {
                    println!("RESERVED");
                    if let Some(mut current_macro) = self.macro_stack.pop() {
                        stack.subsections = stack.parents.pop().unwrap();
                        let ast = self.finish();
                        current_macro.current_segment.body.push(ast.into());
                        self.current_macro = Some(current_macro);
                        return ResolverStep::MacroStackPop;
                    } else {
                        panic!()
                    }
                } else {
                    match root.subsections.get(repr) {
                        Some(list) => {
                            new_root = true;
                            println!("NEW ROOT macro started");
                            let current_segment =
                                MatchedSegment { header: token, body: default() };
                            let mut current_macro =
                                Some(MacroResolver { current_segment, segments: default() });
                            mem::swap(&mut self.current_macro, &mut current_macro);
                            if let Some(current_macro) = current_macro {
                                self.macro_stack.push(current_macro);
                            }
                            Some(list)
                        }
                        None => None,
                    }
                },
        };

        let out = list.is_some();
        if let Some(list) = list {
            self.matched_macro_def = None;
            let mut new_section_tree = MacroSegmentTreeData::default();
            for v in list {
                if v.list.is_empty() {
                    self.matched_macro_def = Some(v.def.clone_ref());
                }
                if let Some(first) = v.list.head() {
                    let tail = v.list.tail().cloned().unwrap_or_default();
                    let def = v.def.clone_ref();
                    let x = MacroSegmentTreeDataRecord { list: tail, def };
                    new_section_tree.subsections.entry(&first.repr).or_default().push(x);
                } else {
                    // todo!()
                }
            }
            // fixme: new_section_tree is created which is too much, we are using only subsections
            mem::swap(&mut new_section_tree, &mut stack.tree);
            stack.parents.push(new_section_tree.subsections);
        }
        if out {
            ResolverStep::NewSegmentStarted
        } else {
            ResolverStep::NormalToken
        }
    }
}

impl<'a> MacroSegmentTreeData<'a> {
    pub fn is_reserved(&self, repr: &'a str) -> bool {
        self.parents.iter().any(|p| p.contains_key(repr))
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

fn macro_if_then_else<'a>() -> Macro<'a> {
    let section1 = MacroSegment { repr: "if", pattern: Pattern::Everything };
    let section2 = MacroSegment { repr: "then", pattern: Pattern::Everything };
    let section3 = MacroSegment { repr: "else", pattern: Pattern::Everything };
    Macro {
        prefix:   None,
        segments: list::NonEmpty::singleton(section3).with_head(section2).with_head(section1),
        body:     Rc::new(|t| matched_segments_into_multi_segment_app(t)),
    }
}

fn macro_if_then<'a>() -> Macro<'a> {
    let section1 = MacroSegment { repr: "if", pattern: Pattern::Everything };
    let section2 = MacroSegment { repr: "then", pattern: Pattern::Everything };
    Macro {
        prefix:   None,
        segments: list::NonEmpty::singleton(section2).with_head(section1),
        body:     Rc::new(|t| matched_segments_into_multi_segment_app(t)),
    }
}



fn main() {
    // let str = "if a then b else c";
    let str = "if if x then y then b";
    let mut lexer = Lexer::new(str);
    println!("{:#?}", lexer.run());
    println!("{:#?}", lexer.output);

    println!("\n---\n");


    let mut resolver = Resolver::default();
    let ast = resolver.run(&lexer, lexer.output.borrow_vec());

    println!("\n---\n");

    println!("{:#?}", WithSources::new(str, &ast));
}

// var1 = if (a > b) then if x then y else z else w
