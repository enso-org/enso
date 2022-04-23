use crate::prelude::*;
use crate::Ast;
use crate::Token;
use crate::TokenOrAst;
use enso_data_structures::list;
use enso_data_structures::list::List;


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
pub struct Definition<'a> {
    pub rev_prefix_pattern: Option<Pattern>,
    pub segments:           list::NonEmpty<SegmentDefinition<'a>>,
    #[derivative(Debug = "ignore")]
    pub body:               Rc<dyn Fn(Vec<(Token, Vec<TokenOrAst>)>) -> Ast>,
}



// =========================
// === SegmentDefinition ===
// =========================

#[derive(Clone, Debug)]
pub struct SegmentDefinition<'a> {
    pub header:  &'a str,
    pub pattern: Pattern,
}

impl<'a> SegmentDefinition<'a> {
    pub fn new(header: &'a str, pattern: Pattern) -> Self {
        Self { header, pattern }
    }
}
