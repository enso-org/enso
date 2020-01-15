//! This module defines a wrapper for WebGL enums and associated utils.

use crate::prelude::*;
use crate::system::gpu::shader::Context;
use crate::system::gpu::data::prim::*;



// ==============
// === GlEnum ===
// ==============

/// The newtype for WebGL enums.
#[derive(Copy,Clone,Debug,Default,Display)]
pub struct GlEnum(pub u32);

impl From<GlEnum> for u32 {
    fn from(t:GlEnum) -> u32 {
        t.0
    }
}



// ==================
// === Extensions ===
// ==================

/// Extension methods.
pub mod traits {
    use super::*;

    /// Methods for every object which implements `Into<GlEnum>`.
    pub trait IntoGlEnum {
        /// Converts the current value to `GlEnum`.
        fn into_gl_enum(&self) -> GlEnum;
    }

    impl<T> IntoGlEnum for T where for<'a> &'a T:Into<GlEnum> {
        fn into_gl_enum(&self) -> GlEnum {
            self.into()
        }
    }

    /// Methods for every object which implements `PhantomInto<GlEnum>`.
    pub trait PhantomIntoGlEnum {
        /// Converts the current value to `GlEnum`.
        fn gl_enum() -> GlEnum;
    }

    impl<T> PhantomIntoGlEnum for T where T:PhantomInto<GlEnum> {
        fn gl_enum() -> GlEnum {
            T::phantom_into::<GlEnum>()
        }
    }
}



// ==============
// === Macros ===
// ==============

/// Combination of `define_singletons` and `define_gl_enum_conversions`.
#[macro_export]
macro_rules! define_singletons_gl {
    ( $( $(#$meta:tt)* $name:ident = $expr:expr ),* $(,)? ) => {
        shapely::define_singletons!{ $( $(#$meta)* $name),* }
        $crate::define_gl_enum_conversions!{ $( $(#$meta)* $name = $expr ),* }
    }
}


/// Defines conversions `From<$type>` and `From<PhantomData<$type>>` for every provided type.
#[macro_export]
macro_rules! define_gl_enum_conversions {
    ( $( $(#$meta:tt)* $type:ty = $expr:expr ),* $(,)? ) => {
        $(
            impl From<$type> for GlEnum {
                fn from(_:$type) -> Self {
                    GlEnum($expr)
                }
            }

            impl From<PhantomData<$type>> for GlEnum {
                fn from(_:PhantomData<$type>) -> Self {
                    GlEnum($expr)
                }
            }
        )*
    }
}

/// Combination of `define_singletons_gl` and `define_singleton_enum_gl_from`.
#[macro_export]
macro_rules! define_singleton_enum_gl {
    (
        $(#$meta:tt)*
        $name:ident {
            $( $(#$field_meta:tt)* $field:ident = $expr:expr),* $(,)?
        }
    ) => {
        $crate :: define_singletons_gl!          { $($(#$field_meta)* $field = $expr),* }
        $crate :: define_singleton_enum_gl_from! { $(#$meta)* $name {$($(#$field_meta)* $field),*}}
    }
}


/// Defines associated enum type for the provided variants, just like `define_singleton_enum_from`.
/// It also defines conversions `From<$singleton>` and `From<PhantomData<$singleton>>` the enum
/// type.
#[macro_export]
macro_rules! define_singleton_enum_gl_from {
    (
        $(#$meta:tt)*
        $name:ident {
            $( $(#$field_meta:tt)* $field:ident),* $(,)?
        }
    ) => {
        shapely::define_singleton_enum_from! { $(#$meta)* $name {$($(#$field_meta)* $field),*}}

        impl From<&$name> for GlEnum {
            fn from(t:&$name) -> Self {
                match t {
                    $($name::$field => $field.into()),*
                }
            }
        }

        impl From<$name> for GlEnum {
            fn from(t:$name) -> Self {
                match t {
                    $($name::$field => $field.into()),*
                }
            }
        }
    }
}


// ================================
// === Primitive Type Instances ===
// ================================

define_gl_enum_conversions! {
    bool                = Context::BOOL,
    u8                  = Context::UNSIGNED_BYTE,
    u16                 = Context::UNSIGNED_SHORT,
    u32                 = Context::UNSIGNED_INT,
    i8                  = Context::BYTE,
    i16                 = Context::SHORT,
    i32                 = Context::INT,
    f16                 = Context::HALF_FLOAT,
    f32                 = Context::FLOAT,
    f32_u24_u8_REV      = Context::FLOAT_32_UNSIGNED_INT_24_8_REV,
    u16_4_4_4_4         = Context::UNSIGNED_SHORT_4_4_4_4,
    u16_5_5_5_1         = Context::UNSIGNED_SHORT_5_5_5_1,
    u16_5_6_5           = Context::UNSIGNED_SHORT_5_6_5,
    u32_f10_f11_f11_REV = Context::UNSIGNED_INT_10F_11F_11F_REV,
    u32_24_8            = Context::UNSIGNED_INT_24_8,
    u32_2_10_10_10_REV  = Context::UNSIGNED_INT_2_10_10_10_REV,
    u32_5_9_9_9_REV     = Context::UNSIGNED_INT_5_9_9_9_REV,
}
