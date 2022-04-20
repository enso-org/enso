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



#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = ""))]
pub struct List<T> {
    node: Option<Rc<Node<T>>>,
}

impl<T> List<T> {
    pub fn with_head(self, head: T) -> Self {
        let tail = self;
        let node = Some(Rc::new(Node { head, tail }));
        Self { node }
    }

    pub fn head(&self) -> Option<&T> {
        self.node.as_ref().map(|t| &t.head)
    }

    pub fn tail(&self) -> Option<&List<T>> {
        self.node.as_ref().map(|t| &t.tail)
    }

    pub fn is_empty(&self) -> bool {
        self.node.is_none()
    }

    fn to_vec(&self) -> Vec<&T> {
        let mut out: Vec<&T> = default();
        let mut this = self;
        loop {
            match this.head() {
                None => break,
                Some(head) => {
                    out.push(head);
                    match this.tail() {
                        None => break,
                        Some(tail) => this = tail,
                    }
                }
            }
        }
        out
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
    section:  MacroSegment<'a>,
    segments: List<MacroSegment<'a>>,
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
        section:  section1,
        segments: List::default().with_head(section3).with_head(section2),
    }
}

fn macro_if_then<'a>() -> Macro<'a> {
    let section1 = MacroSegment { repr: "if", pattern: Pattern::Everything };
    let section2 = MacroSegment { repr: "then", pattern: Pattern::Everything };
    Macro { prefix: None, section: section1, segments: List::default().with_head(section2) }
}

#[derive(Default, Debug)]
pub struct MacroSegmentTreeData<'a> {
    subsections: HashMap<&'a str, Vec<List<MacroSegment<'a>>>>,
    parent:      Option<Box<MacroSegmentTreeData<'a>>>,
}

#[derive(Default, Debug, Deref, DerefMut)]
pub struct MacroSegmentTree<'a> {
    tree: MacroSegmentTreeData<'a>,
}

#[derive(Default, Debug)]
pub struct MacroResolver {
    current_segment: Option<MatchedSegment>,
    segments:        Vec<MatchedSegment>,
}

#[derive(Default, Debug)]
pub struct Resolver {
    current_macro:  MacroResolver,
    macro_stack:    Vec<MacroResolver>,
    is_final_state: bool,
}

impl Resolver {
    pub fn run(&mut self, lexer: &Lexer, tokens: &[Token]) {
        let mut segment_tree = MacroSegmentTree::default();
        let mut root_segment_tree = MacroSegmentTree::default();

        let if_then = macro_if_then();
        let if_then_else = macro_if_then_else();
        root_segment_tree.subsections.entry("if").or_default().push(if_then.segments);
        root_segment_tree.subsections.entry("if").or_default().push(if_then_else.segments);

        println!("{:#?}", root_segment_tree);

        for token in tokens {
            let token = *token;
            let repr = lexer.repr(token);
            println!("\n>> '{}' = {:#?}", repr, token);
            println!("reserved: {}", segment_tree.is_reserved(repr));
            self.enter(lexer, &mut segment_tree, &root_segment_tree, token);
            self.current_macro.current_segment.as_mut().unwrap().body.push(token);
            println!("{:#?}", segment_tree);
        }
    }

    pub fn enter<'a>(
        &mut self,
        lexer: &Lexer,
        stack: &mut MacroSegmentTree<'a>,
        root: &MacroSegmentTree<'a>,
        token: Token,
    ) {
        let repr = lexer.repr(token);
        if let Some(list) = stack.subsections.get(repr).or_else(|| root.subsections.get(repr)) {
            let mut current_macro = Some(MatchedSegment { header: token, body: default() });
            mem::swap(&mut self.current_macro.current_segment, &mut current_macro);
            if let Some(current_macro) = current_macro {
                self.current_macro.segments.push(current_macro);
            }

            self.is_final_state = false;
            let mut new_section_tree = MacroSegmentTreeData::default();
            for v in list {
                if v.is_empty() {
                    self.is_final_state = true;
                }
                if let Some(first) = v.head() {
                    let tail = v.tail().cloned().unwrap_or_default();
                    new_section_tree.subsections.entry(&first.repr).or_default().push(tail);
                } else {
                    // todo!()
                }
            }
            mem::swap(&mut new_section_tree, &mut stack.tree);
            stack.tree.parent = Some(Box::new(new_section_tree));
        }
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
    let str = "if a then b else c";
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

    println!("{:#?}", resolver);
    // println!("{:#?}", segment_tree);
    // let if_then_else =
    // println!("{:#?}", pattern.next(&mut stream))
}

// var1 = if (a > b) then if x then y else z else w
