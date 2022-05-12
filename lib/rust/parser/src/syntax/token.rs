//! Token implementation. Tokens are output from the lexer. Read the docs of the main module to
//! learn more about the parsing process steps.
use crate::prelude::*;

use crate::source;

use enso_shapely_macros::tagged_enum;

use source::Lexeme;


// =============
// === Token ===
// =============


/// Parsing token, output of lexing. Read the docs in the main lib file to learn more about the
/// parsing pipeline.
pub type Token<'s> = Lexeme<'s, Type>;



// ============
// === Type ===
// ============

/// Macro providing [`Token`] type definition. It is used to both define the token [`Type`], and to
/// define impls for every token type in other modules.
#[macro_export]
macro_rules! with_token_definition { ($f:ident ($($args:tt)*)) => { $f! { $($args)*
    /// [`Token`] type defining elements that can be found in the source code.
    #[tagged_enum]
    #[derive(Clone, Copy, PartialEq, Eq)]
    #[allow(missing_docs)]
    pub enum Type {
        Newline,
        Symbol,
        BlockStart,
        BlockEnd,
        Wildcard {
            pub lift_level: usize
        },
        Ident {
            pub is_free: bool,
            pub lift_level: usize
        },
        Operator,
        Modifier,
        Comment,
        DocComment,
        Number,
        TextStart,
        TextEnd,
        TextSection,
        TextEscape,
    }
}}}

macro_rules! identity {
    ($($ts:tt)*) => {
        $($ts)*
    };
}

with_token_definition!(identity());
