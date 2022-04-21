#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]
#![feature(generic_associated_types)]
#![recursion_limit = "256"]

use crate::prelude::*;

pub mod lexer;

use enso_data_structures::hash_map_tree::HashMapTree;
use lexer::Token;

pub mod prelude {
    pub use enso_prelude::*;
}


#[derive(Clone, Debug)]
pub struct Node<T> {
    head: T,
    tail: List<T>,
}

impl<T> Node<T> {
    pub fn singleton(head: T) -> Self {
        let tail = default();
        Self { head, tail }
    }
}


#[derive(Derivative, Deref)]
#[derivative(Clone(bound = ""))]
pub struct NonEmptyList<T> {
    node: Rc<Node<T>>,
}

#[derive(Derivative, Deref)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
pub struct List<T> {
    data: Option<NonEmptyList<T>>,
}

impl<T> NonEmptyList<T> {
    pub fn singleton(head: T) -> Self {
        let node = Rc::new(Node::singleton(head));
        Self { node }
    }

    pub fn into_list(self) -> List<T> {
        let data = Some(self);
        List { data }
    }

    pub fn with_head(self, head: T) -> Self {
        self.into_list().with_head(head)
    }

    pub fn head(&self) -> &T {
        &self.head
    }

    pub fn tail(&self) -> &List<T> {
        &self.tail
    }

    pub fn is_empty(&self) -> bool {
        false
    }

    fn to_vec(&self) -> Vec<&T> {
        let mut out: Vec<&T> = vec![&self.head];
        let mut list = self.tail();
        loop {
            match list.head() {
                None => break,
                Some(head) => {
                    out.push(head);
                    match list.tail() {
                        None => break,
                        Some(tail) => list = tail,
                    }
                }
            }
        }
        out
    }
}
impl<T> List<T> {
    pub fn with_head(self, head: T) -> NonEmptyList<T> {
        let tail = self;
        let node = Rc::new(Node { head, tail });
        NonEmptyList { node }
    }

    pub fn head(&self) -> Option<&T> {
        self.as_ref().map(|t| t.head())
    }

    pub fn tail(&self) -> Option<&List<T>> {
        self.as_ref().map(|t| t.tail())
    }

    pub fn is_empty(&self) -> bool {
        self.is_none()
    }

    fn to_vec(&self) -> Vec<&T> {
        self.data.as_ref().map(|t| t.to_vec()).unwrap_or_default()
    }
}

impl<T> From<NonEmptyList<T>> for List<T> {
    fn from(list: NonEmptyList<T>) -> Self {
        list.into_list()
    }
}

impl<T: Debug> Debug for List<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.to_vec(), f)
    }
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
    TokenVariant(lexer::KindVariant),
    Seq(Box<Pattern>, Box<Pattern>),
}

impl Pattern {
    pub fn next(&self, stream: &mut Stream) -> bool {
        stream
            .first()
            .map(|token| match self {
                Self::TokenVariant(token_variant_pattern) =>
                    if token.variant() == *token_variant_pattern {
                        stream.next();
                        true
                    } else {
                        false
                    },
                Self::Seq(first, second) =>
                    if first.next(stream) {
                        second.next(stream)
                    } else {
                        false
                    },
                _ => false,
            })
            .unwrap_or(false)
    }
}



#[derive(Debug)]
pub struct Macro<'a> {
    prefix:   Option<Pattern>,
    segments: NonEmptyList<MacroSegment<'a>>,
}

#[derive(Clone, Debug)]
pub struct MacroSegment<'a> {
    repr:    &'a str,
    pattern: Pattern,
}

#[derive(Debug)]
pub struct MatchedSegment {
    header: Token,
    body:   Vec<Token>,
}

use lexer::Lexer;

fn macro_if_then_else<'a>() -> Macro<'a> {
    let section1 = MacroSegment { repr: "if", pattern: Pattern::Everything };
    let section2 = MacroSegment { repr: "then", pattern: Pattern::Everything };
    let section3 = MacroSegment { repr: "else", pattern: Pattern::Everything };
    Macro {
        prefix:   None,
        segments: NonEmptyList::singleton(section3).with_head(section2).with_head(section1),
    }
}

fn macro_if_then<'a>() -> Macro<'a> {
    let section1 = MacroSegment { repr: "if", pattern: Pattern::Everything };
    let section2 = MacroSegment { repr: "then", pattern: Pattern::Everything };
    Macro { prefix: None, segments: NonEmptyList::singleton(section2).with_head(section1) }
}

#[derive(Debug)]
pub struct MacroSegmentTreeDataRecord<'a> {
    list: List<MacroSegment<'a>>,
    def:  Rc<Macro<'a>>,
}

#[derive(Default, Debug)]
pub struct MacroSegmentTreeData<'a> {
    subsections: HashMap<&'a str, Vec<MacroSegmentTreeDataRecord<'a>>>,
    parent:      Option<Box<MacroSegmentTreeData<'a>>>,
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
    leading_tokens: Vec<Token>,
    current_macro:  Option<MacroResolver>,
    macro_stack:    Vec<MacroResolver>,
    matched_macro:  Option<Rc<Macro<'a>>>,
}

impl<'a> Resolver<'a> {
    pub fn run(&mut self, lexer: &Lexer, tokens: &[Token]) {
        let mut segment_tree = MacroSegmentTree::default();
        let mut root_segment_tree = MacroSegmentTree::default();

        let if_then = macro_if_then();
        let if_then_else = macro_if_then_else();
        let if_then =
            MacroSegmentTreeDataRecord { list: if_then.segments.clone(), def: Rc::new(if_then) };
        let if_then_else = MacroSegmentTreeDataRecord {
            list: if_then_else.segments.clone(),
            def:  Rc::new(if_then_else),
        };
        root_segment_tree.subsections.entry("if").or_default().push(if_then);
        root_segment_tree.subsections.entry("if").or_default().push(if_then_else);

        println!("{:#?}", root_segment_tree);

        for token in tokens {
            let token = *token;
            let repr = lexer.repr(token);
            println!("\n>> '{}' = {:#?}", repr, token);
            println!("reserved: {}", segment_tree.is_reserved(repr));
            if !self.enter(lexer, &mut segment_tree, &root_segment_tree, token) {
                match self.current_macro.as_mut() {
                    Some(current_macro) => current_macro.current_segment.body.push(token),
                    None => self.leading_tokens.push(token),
                }
            }
            println!("{:#?}", segment_tree);
        }
        self.finish();
    }

    pub fn finish(&mut self) {
        println!("FINISH");
        let current_macro = mem::take(&mut self.current_macro).unwrap();
        let mut segments = current_macro.segments;
        segments.push(current_macro.current_segment);
        println!("{:#?}", segments);
        println!("{:#?}", self.matched_macro);
    }

    pub fn enter(
        &mut self,
        lexer: &Lexer,
        stack: &mut MacroSegmentTree<'a>,
        root: &MacroSegmentTree<'a>,
        token: Token,
    ) -> bool {
        let repr = lexer.repr(token);
        let list = match stack.subsections.get(repr) {
            Some(list) => {
                let current_macro = self.current_macro.as_mut().unwrap(); // has to be there
                let mut current_segment = MatchedSegment { header: token, body: default() };
                mem::swap(&mut current_macro.current_segment, &mut current_segment);
                current_macro.segments.push(current_segment);
                Some(list)
            }
            None => match root.subsections.get(repr) {
                Some(list) => {
                    let current_segment = MatchedSegment { header: token, body: default() };
                    let mut current_macro =
                        Some(MacroResolver { current_segment, segments: default() });
                    mem::swap(&mut self.current_macro, &mut current_macro);
                    if let Some(current_macro) = current_macro {
                        self.macro_stack.push(current_macro);
                    }
                    Some(list)
                }
                None => None,
            },
        };

        let out = list.is_some();
        if let Some(list) = list {
            self.matched_macro = None;
            let mut new_section_tree = MacroSegmentTreeData::default();
            for v in list {
                if v.list.is_empty() {
                    self.matched_macro = Some(v.def.clone_ref());
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
            mem::swap(&mut new_section_tree, &mut stack.tree);
            stack.tree.parent = Some(Box::new(new_section_tree));
        }
        out
    }
}

impl<'a> MacroSegmentTreeData<'a> {
    pub fn is_reserved(&self, repr: &'a str) -> bool {
        self.parent
            .as_ref()
            .map(|parent| parent.subsections.contains_key(repr) || parent.is_reserved(repr))
            .unwrap_or(false)
    }
}



pub struct Ast {}

fn main() {
    let str = "if a then b c else d e f";
    let mut lexer = Lexer::new(str);
    println!("{:#?}", lexer.run());
    println!("{:#?}", lexer.output);
    // let mut stream = Stream::new(lexer.output.borrow_vec().as_slice());
    // let pattern = Pattern::Seq(
    //     Box::new(Pattern::TokenVariant(lexer::KindVariant::Ident)),
    //     Box::new(Pattern::Seq(
    //         Box::new(Pattern::TokenVariant(lexer::KindVariant::Operator)),
    //         Box::new(Pattern::TokenVariant(lexer::KindVariant::Ident)),
    //     )),
    // );

    println!("\n---\n");


    let mut resolver = Resolver::default();
    resolver.run(&lexer, lexer.output.borrow_vec());

    println!("\n---\n");

    println!("{:#?}", resolver);
    // println!("{:#?}", segment_tree);
    // let if_then_else =
    // println!("{:#?}", pattern.next(&mut stream))
}

// var1 = if (a > b) then if x then y else z else w
