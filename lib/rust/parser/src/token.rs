use crate::prelude::*;

use crate::source;
use crate::span;

use enso_shapely_macros::tagged_enum;



// =============
// === Token ===
// =============

/// Parsing token, output of lexing. Read the docs in the main lib file to learn more about the
/// parsing pipeline.
pub type Token = span::With<Type>;



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

impl<'s> Debug for source::With<'s, &Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.data, f)
    }
}

macro_rules! generate_debug_impls {
    (
        $(#$enum_meta:tt)*
        pub enum $enum:ident {
            $(
                $(#$variant_meta:tt)*
                $variant:ident $({$($fields:tt)*})?
            ),* $(,)?
        }
    ) => {
        $(
            impl<'s> Debug for source::With<'s, &$variant> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    Debug::fmt(&self.data, f)
                }
            }
        )*
    };
}

macro_rules! define_token_type {
    ($($ts:tt)*) => {
        $($ts)*
        generate_debug_impls! { $($ts)* }
    };
}
with_token_definition!(define_token_type());
