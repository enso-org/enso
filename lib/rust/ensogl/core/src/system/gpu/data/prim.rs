//! This module exports primitive data and associated utils.



// =============
// === Types ===
// =============

/// `Identity<A>` resolves to `A`.
pub type Identity<T> = T;

macro_rules! gen_unsupported_types {
    ( $($name:ident),* $(,)? ) => {$(
        #[derive(Copy,Clone,Debug)]
        pub struct $name {}

        #[allow(unsafe_code)]
        unsafe impl bytemuck::Pod for $name {}
        #[allow(unsafe_code)]
        unsafe impl bytemuck::Zeroable for $name {}
    )*}
}

/// Types which are used in WebGL but are not (yet) bound to Rust types.
#[allow(non_camel_case_types)]
#[allow(missing_docs)]
pub mod unsupported_types {
    gen_unsupported_types! { f16, f32_u24_u8_REV, u16_4_4_4_4, u16_5_5_5_1, u16_5_6_5, u32_f10_f11_f11_REV, u32_24_8
    , u32_2_10_10_10_REV, u32_5_9_9_9_REV
    }
}
pub use unsupported_types::*;



// ==============
// === Macros ===
// ==============

/// Evaluates the argument macro with a list of pairs `[container item]` for all container and for
/// all primitive types supported on GPU. One of the container type is `Identity` which just
/// resolves to it's argument.
#[macro_export]
macro_rules! with_all_prim_types {
    ([[$f:path] $args:tt]) => {
        $f! { $args
            [[Identity u32] [Identity i32]  [Identity f32]  [Identity bool]
             [Vector2  f32]  [Vector3  f32]  [Vector4  f32]
             [Vector2  i32]  [Vector3  i32]  [Vector4  i32]
             [Vector2  u32]  [Vector3  u32]  [Vector4  u32]
             [Vector2  bool] [Vector3  bool] [Vector4  bool]
             [Matrix2  f32]  [Matrix3  f32]  [Matrix4 f32]
             [Matrix2x3 f32] [Matrix2x4 f32]
             [Matrix3x2 f32] [Matrix3x4 f32]
             [Matrix4x2 f32] [Matrix4x3 f32]
            ]
        }
    };
}
