#![feature(allocator_api)]
#![feature(slice_index_methods)]
#![feature(test)]
#![feature(generic_associated_types)]
#![recursion_limit = "256"]

use crate::prelude::*;

pub mod lexer;

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
pub struct Segment<'a> {
    prefix:   Option<Pattern>,
    section:  Section<'a>,
    sections: List<Section<'a>>,
}

#[derive(Clone, Debug)]
pub struct Section<'a> {
    repr:    &'a str,
    pattern: Pattern,
}

use lexer::Lexer;

fn segment_if_then_else<'a>() -> Segment<'a> {
    let section1 = Section { repr: "if", pattern: Pattern::Everything };
    let section2 = Section { repr: "then", pattern: Pattern::Everything };
    let section3 = Section { repr: "else", pattern: Pattern::Everything };
    Segment {
        prefix:   None,
        section:  section1,
        sections: List::default().with_head(section3).with_head(section2),
    }
}

#[derive(Default, Debug)]
pub struct SectionTree<'a> {
    subsections: HashMap<&'a str, Vec<List<Section<'a>>>>,
    parent:      Option<Box<SectionTree<'a>>>,
}

#[derive(Default, Debug, Deref, DerefMut)]
pub struct SectionTreeStack<'a> {
    tree: SectionTree<'a>,
}

impl<'a> SectionTreeStack<'a> {
    pub fn enter(&mut self, repr: &'a str) {
        if let Some(list) = self.subsections.get(repr) {
            let mut new_section_tree = SectionTree::default();
            for v in list {
                if let Some(first) = v.head() {
                    let vv = v.tail().cloned().unwrap_or_default();
                    new_section_tree.subsections.entry(&first.repr).or_default().push(vv);
                } else {
                    // todo!()
                }
            }
            mem::swap(&mut new_section_tree, &mut self.tree);
            self.tree.parent = Some(Box::new(new_section_tree));
        }
    }
}

impl<'a> SectionTree<'a> {
    pub fn is_reserved(&self, repr: &'a str) -> bool {
        self.parent
            .as_ref()
            .map(|parent| parent.subsections.contains_key(repr) || parent.is_reserved(repr))
            .unwrap_or(false)
    }
}

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

    let mut section_tree = SectionTreeStack::default();

    let if_then_else = segment_if_then_else();
    section_tree.subsections.entry("if").or_default().push(if_then_else.sections);

    for token in lexer.output.borrow_vec() {
        let repr = lexer.repr(*token);
        println!("\n>> '{}' = {:#?}", repr, token);
        println!("reserved: {}", section_tree.is_reserved(repr));
        section_tree.enter(repr);
        println!("{:#?}", section_tree);
    }

    // println!("{:#?}", section_tree);
    // let if_then_else =
    // println!("{:#?}", pattern.next(&mut stream))
}

// var1 = if (a > b) then if x then y else z else w
