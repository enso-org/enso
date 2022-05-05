use crate::prelude::*;

use crate::location;
use crate::source;



// =============
// === Token ===
// =============

pub type Token = location::With<Kind>;


// ============
// === Kind ===
// ============

macro_rules! tagged_enum {
    ($name:ident {
        $($variant:ident $({$($arg:ident : $arg_tp:ty),* $(,)?})? ),* $(,)?
    }) => {paste! {
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub enum $name {
            $($variant($variant)),*
        }

        $(
            #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
            pub struct $variant {
                $($($arg : $arg_tp),*)?
            }

            impl From<$variant> for $name {
                #[inline(always)]
                fn from(t: $variant) -> Self {
                    Self::$variant(t)
                }
            }

            impl PartialEq<$variant> for &$variant {
                #[inline(always)]
                fn eq(&self, other: &$variant) -> bool {
                    $variant::eq(*self, other)
                }
            }

            impl<'s> Debug for source::With<'s, &$variant> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    Debug::fmt(&self.data, f)
                }
            }
        )*

        impl $name {
            /// Variant of this element.
            pub fn variant(&self) -> [<$name Variant>] {
                self.into()
            }

            /// Check whether this element is the given variant.
            pub fn is(&self, variant:[<$name Variant>]) -> bool {
                self.variant() == variant
            }

            $(
                /// Constructor.
                #[inline(always)]
                pub fn [<$variant:snake:lower>]($($($arg : $arg_tp),*)?) -> Self {
                    Self::$variant($variant { $($($arg),*)? })
                }

                /// Check whether this element is the given variant.
                #[inline(always)]
                pub fn [<is_ $variant:snake:lower>](&self) -> bool {
                    self.is([<$name Variant>]::$variant)
                }
            )*
        }

        impl Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$variant(t) => Debug::fmt(&t,f)),*
                }
            }
        }

        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum [<$name Variant>] {
            $($variant),*
        }

        impl From<&$name> for [<$name Variant>] {
            fn from(t:&$name) -> Self {
                match t {
                    $(
                        $name::$variant(_) => Self::$variant
                    ),*
                }
            }
        }
    }};
}

#[macro_export]
macro_rules! with_token_definitions {
    ($($f:tt)*) => {
        $($f)*! {
            Kind {
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

with_token_definitions!(tagged_enum);

impl<'s> Debug for source::With<'s, &Kind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.data, f)
    }
}
