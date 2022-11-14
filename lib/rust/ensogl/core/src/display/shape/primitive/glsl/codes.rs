//! GLSL codes, numbers which are used to identify errors and display modes. These are shared
//! between Rust and GLSL code.

use crate::prelude::*;



// =============
// === Codes ===
// =============

macro_rules! include_codes {
    ($($name:ident),* $(,)?) => { paste! {
        $(
            #[doc = "The value of the '`"]
            #[doc = stringify!($name)]
            #[doc = "`' GLSL code."]
            pub const [<$name:snake:upper>]: u32 =
                include!(concat!("codes/", stringify!($name), ".txt"));
        )*
        /// Mapping between the GLSL code names and their values.
        pub const MAP: &[(&str, u32)] =
            &[$( (stringify!($name), [<$name:snake:upper>]) ),*];
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
