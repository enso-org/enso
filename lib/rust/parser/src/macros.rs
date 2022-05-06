use crate::prelude::*;
use crate::token::Token;
use crate::token_or_ast::TokenOrAst;
use crate::Ast;
use crate::Lexer;
use crate::TokenOrAstOrMacroResolver;
use enso_data_structures::im_list;

pub mod pattern;

use pattern::Pattern;



#[derive(Derivative)]
#[derivative(Debug)]
pub struct Definition<'a> {
    pub rev_prefix_pattern: Option<Pattern>,
    pub segments:           im_list::NonEmpty<SegmentDefinition<'a>>,
    #[derivative(Debug = "ignore")]
    pub body: Rc<
        dyn for<'b> Fn(
            &Lexer<'b>,
            Option<Vec<TokenOrAst>>,
            NonEmptyVec<(Token, Vec<TokenOrAst>)>,
        ) -> Ast,
    >,
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



// ===================
// === Rust Macros ===
// ===================

#[macro_export]
macro_rules! macro_definition {
    ( ($($section:literal, $pattern:expr),* $(,)?) $body:expr ) => {
        $crate::macro_definition!{[None] ($($section, $pattern),*) $body}
    };
    ( ($prefix:expr, $($section:literal, $pattern:expr),* $(,)?) $body:expr ) => {
        $crate::macro_definition!{[Some($prefix)] ($($section, $pattern),*) $body}
    };
    ( [$prefix:expr] ($($section:literal, $pattern:expr),* $(,)?) $body:expr ) => {
        macros::Definition {
            rev_prefix_pattern: $prefix,
            segments: im_list::NonEmpty::try_from(vec![
                $(macros::SegmentDefinition::new($section, $pattern)),*]).unwrap(),
            body: Rc::new($body),
        }
    };
}
