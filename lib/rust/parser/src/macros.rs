//! Enso macro utilities. The parser contains a powerful macro resolution engine and a lot of the
//! language constructs are defined as macros. This module contains macro definition structs and
//! utilities allowing macros management.
//! Read the docs of the main module of this crate to learn more about the parsing process.

use crate::prelude::*;

use crate::syntax;

use enso_data_structures::im_list;


// ==============
// === Export ===
// ==============

pub mod built_in;
pub mod expand;
pub mod pattern;
pub mod resolver;

pub use pattern::Pattern;



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
    pub segments: im_list::NonEmpty<SegmentDefinition<'a>>,
    #[derivative(Debug = "ignore")]
    pub body:     Rc<DefinitionBody>,
}

/// A function that transforms matched macro tokens into [`syntax::Tree`].
pub type DefinitionBody = dyn for<'s> Fn(pattern::MatchedSegments<'s>) -> syntax::Tree<'s>;



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
    ($def:tt) => {
        $crate::macro_definition!{$def $crate::macros::matched_segments_into_multi_segment_app}
    };
    (($($section:literal, $pattern:expr),* $(,)?) $body:expr) => {
        $crate::macros::Definition {
            segments: im_list::NonEmpty::try_from(vec![
                $($crate::macros::SegmentDefinition::new($section, $pattern)),*]).unwrap(),
            body: Rc::new($body),
        }
    };
}



fn matched_segments_into_multi_segment_app(
    matched_segments: NonEmptyVec<pattern::MatchedSegment<'_>>,
) -> syntax::Tree<'_> {
    let segments = matched_segments.mapped(|segment| {
        let header = segment.header;
        let tokens = segment.result.tokens();
        let body = (!tokens.is_empty())
            .as_some_from(|| syntax::operator::resolve_operator_precedence(tokens));
        syntax::tree::MultiSegmentAppSegment { header, body }
    });
    syntax::Tree::multi_segment_app(segments)
}
