use crate::prelude::*;

use crate::location;
use crate::source;

use enso_shapely_macros::tagged_enum;

// =============
// === Token ===
// =============

pub type Token = location::With<Kind>;



// ============
// === Kind ===
// ============

#[macro_export]
macro_rules! with_token_definition {
    ($($f:tt)*) => {
        $($f)*! {
            #[tagged_enum]
            #[derive(Clone, Copy, PartialEq, Eq)]
            pub enum Kind {
                Newline,
                Symbol,
                BlockStart,
                BlockEnd,
                Wildcard { lift_level: usize },
                Ident { is_free: bool, lift_level: usize },
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
        }
    }
}

impl<'s> Debug for source::With<'s, &Kind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.data, f)
    }
}

macro_rules! id {
    ($($ts:tt)*) => { $($ts)* };
}
with_token_definition!(id);

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
with_token_definition!(generate_debug_impls);
