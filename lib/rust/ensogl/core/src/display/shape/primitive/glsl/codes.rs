//! GLSL codes, numbers which are used to identify errors and display modes. These are shared
//! between Rust and GLSL code.

use crate::prelude::*;



// =============
// === Codes ===
// =============

macro_rules! include_codes {
    ($($name:ident),* $(,)?) => { paste! {
        /// Enum describing possible GLSL codes.
        #[derive(Clone, Copy, Debug)]
        #[allow(missing_docs)]
        pub enum Codes {
            $([<$name:camel>]),*
        }

        impl Codes {
            /// The numeric representation of the code.
            pub const fn value(&self) -> u32 {
                match self {$(
                    Self::[<$name:camel>] => include!(concat!("codes/", stringify!($name), ".txt")),
                )*}
            }

            /// Conversion from the numeric representation of the code to the code variant.
            ///
            /// This is implemented as multiple ifs because macros don't allow usage of the
            /// [`include!`] macro as a pattern match arm.
            pub const fn from_value(number:u32) -> Option<Self> {
                $(
                    if Self::[<$name:camel>].value() == number {
                        return Some(Self::[<$name:camel>])
                    }
                )*
                return None;
            }

            /// Name of the code.
            pub const fn name(&self) -> &'static str {
                match self {$(
                    Self::[<$name:camel>] => stringify!($name),
                )*}
            }

            /// All registered codes.
            pub const fn all() -> &'static [Self] {
                &[$( Self::[<$name:camel>] ),*]
            }
        }
    }};
}

include_codes!(
    display_mode_debug_instance_id,
    display_mode_debug_sdf,
    display_mode_debug_shape_aa_span,
    display_mode_debug_sprite_uv,
    display_mode_normal,
    id_encoding_overflow_error
);
