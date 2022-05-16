//! Enso macro utilities. The parser contains a powerful macro resolution engine and a lot of the
//! language constructs are defined as macros. This module contains macro definition structs and
//! utilities allowing macros management.
//! Read the docs of the main module of this crate to learn more about the parsing process.

//

use crate::prelude::*;

use crate::syntax;
use crate::syntax::token::Token;

use enso_data_structures::im_list;
use pattern::Pattern;


// ==============
// === Export ===
// ==============

pub mod pattern;



// ==================
// === Definition ===
// ==================

/// Macro definition. It contains list of macro segments and optional macro prefix.
///
/// For example, the macro `if ... then ... else ...` contains three segments and no prefix. On the
/// other hand, the macro `... -> ...` contains one segment (starting with the `->` token) and a
/// prefix (it consumes tokens on the left of its first segment).
///
/// If you want to create macro definition in Rust, use the [`macro_definition`] macro instead,
/// which for a nice and concise definitions.
#[derive(Derivative)]
#[derivative(Debug)]
#[allow(missing_docs)]
pub struct Definition<'a> {
    /// The pattern in this field will be matched from right to left, unlike patterns in segments.
    pub rev_prefix_pattern: Option<Pattern>,
    pub segments:           im_list::NonEmpty<SegmentDefinition<'a>>,
    #[derivative(Debug = "ignore")]
    pub body:               Rc<Body>,
}

/// All the tokens matched as prefix of the resolved macro.
pub type PrefixTokens<'s> = Option<Vec<syntax::Item<'s>>>;

/// All the sections of the resolved macro.
pub type MatchedSections<'s> = NonEmptyVec<(Token<'s>, Vec<syntax::Item<'s>>)>;

/// A function that transforms matched macro tokens into [`syntax::Tree`].
pub type Body = dyn for<'s> Fn(PrefixTokens<'s>, MatchedSections<'s>) -> syntax::Tree<'s>;



// =========================
// === SegmentDefinition ===
// =========================

/// Definition of macro segment. Contains header, such as `if`, or `->` and pattern that following
/// tokens have to match.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct SegmentDefinition<'a> {
    pub header:  &'a str,
    pub pattern: Pattern,
}

impl<'a> SegmentDefinition<'a> {
    /// Constructor.
    pub fn new(header: &'a str, pattern: Pattern) -> Self {
        Self { header, pattern }
    }
}



// ===================
// === Rust Macros ===
// ===================

/// Macro allowing for nice macro [`Definition`] generation. For example, the following code defines
/// the `if ... then .. else ...` macro:
///
/// ```text
/// macro_definition! {
//      ("if", Pattern::Everything, "then", Pattern::Everything, "else", Pattern::Everything)
//      body_handler_fn
//  }
/// ```
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
