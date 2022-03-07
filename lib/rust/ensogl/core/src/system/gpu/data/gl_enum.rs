//! This module defines a wrapper for WebGL enums and associated utils.

use crate::prelude::*;
use crate::system::gpu::data::prim::*;
use crate::system::Context;



// ==============
// === GlEnum ===
// ==============

/// The newtype for WebGL enums.
#[derive(Copy, Clone, Debug, Default, Display)]
pub struct GlEnum(pub u32);

impl From<GlEnum> for u32 {
    fn from(t: GlEnum) -> u32 {
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
    pub trait ToGlEnum {
        /// Converts the current value to `GlEnum`.
        fn to_gl_enum(&self) -> GlEnum;
    }

    impl<T> ToGlEnum for T
    where for<'a> &'a T: Into<GlEnum>
    {
        fn to_gl_enum(&self) -> GlEnum {
            self.into()
        }
    }

    /// Methods for every object which implements `PhantomInto<GlEnum>`.
    pub trait PhantomIntoGlEnum {
        /// Converts the current value to `GlEnum`.
        fn gl_enum() -> GlEnum;
    }

    impl<T> PhantomIntoGlEnum for T
    where T: PhantomInto<GlEnum>
    {
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
    ( $target:tt $( $(#$meta:tt)* $name:ident = $expr:expr ),* $(,)? ) => {
        enso_shapely::define_singletons!{ $( $(#$meta)* $name),* }
        $crate::define_gl_enum_conversions!{ $target $( $(#$meta)* $name = $expr ),* }
    }
}


/// Defines conversions `From<$type>` and `From<PhantomData<$type>>` for every provided type.
#[macro_export]
macro_rules! define_gl_enum_conversions {
    ( $target:tt $( $(#$meta:tt)* $type:ty = $expr:expr ),* $(,)? ) => {
        $(
            $crate::define_gl_enum_conversions_2! {$target $(#$meta)* $type = $expr }
        )*
    }
}

/// ...
#[macro_export]
macro_rules! define_gl_enum_conversions_2 {
    ( [$($target:tt)*] $(#$meta:tt)* $type:ty = $expr:expr ) => {
        impl From<$type> for $($target)* {
            fn from(_:$type) -> Self {
                $expr
            }
        }

        impl From<PhantomData<$type>> for $($target)* {
            fn from(_:PhantomData<$type>) -> Self {
                $expr
            }
        }
    }
}


/// Combination of `define_singletons_gl` and `define_singleton_enum_gl_from`.
#[macro_export]
macro_rules! define_singleton_enum_gl {
    (
        $target:tt
        $(#$meta:tt)*
        $name:ident {
            $( $(#$field_meta:tt)* $field:ident = $expr:expr),* $(,)?
        }
    ) => {
        $crate :: define_singletons_gl!          { $target $($(#$field_meta)* $field = $expr),* }
        $crate :: define_singleton_enum_gl_from! { $target $(#$meta)* $name {$($(#$field_meta)* $field),*}}
    }
}


/// Defines associated enum type for the provided variants, just like `define_singleton_enum_from`.
/// It also defines conversions `From<$singleton>` and `From<PhantomData<$singleton>>` the enum
/// type.
#[macro_export]
macro_rules! define_singleton_enum_gl_from {
    (   [$($target:tt)*]
        $(#$meta:tt)*
        $name:ident {
            $( $(#$field_meta:tt)* $field:ident),* $(,)?
        }
    ) => {
        enso_shapely::define_singleton_enum_from! { $(#$meta)* $name {$($(#$field_meta)* $field),*}}

        impl From<&$name> for $($target)* {
            fn from(t:&$name) -> Self {
                match t {
                    $($name::$field => PhantomData::<$field>.into()),*
                }
            }
        }

        impl From<$name> for $($target)* {
            fn from(t:$name) -> Self {
                match t {
                    $($name::$field => PhantomData::<$field>.into()),*
                }
            }
        }
    }
}



// ================================
// === Primitive Type Instances ===
// ================================

define_gl_enum_conversions! { [GlEnum]
    bool                = GlEnum(Context::BOOL),
    u8                  = GlEnum(Context::UNSIGNED_BYTE),
    u16                 = GlEnum(Context::UNSIGNED_SHORT),
    u32                 = GlEnum(Context::UNSIGNED_INT),
    i8                  = GlEnum(Context::BYTE),
    i16                 = GlEnum(Context::SHORT),
    i32                 = GlEnum(Context::INT),
    f16                 = GlEnum(Context::HALF_FLOAT),
    f32                 = GlEnum(Context::FLOAT),
    f32_u24_u8_REV      = GlEnum(Context::FLOAT_32_UNSIGNED_INT_24_8_REV),
    u16_4_4_4_4         = GlEnum(Context::UNSIGNED_SHORT_4_4_4_4),
    u16_5_5_5_1         = GlEnum(Context::UNSIGNED_SHORT_5_5_5_1),
    u16_5_6_5           = GlEnum(Context::UNSIGNED_SHORT_5_6_5),
    u32_f10_f11_f11_REV = GlEnum(Context::UNSIGNED_INT_10F_11F_11F_REV),
    u32_24_8            = GlEnum(Context::UNSIGNED_INT_24_8),
    u32_2_10_10_10_REV  = GlEnum(Context::UNSIGNED_INT_2_10_10_10_REV),
    u32_5_9_9_9_REV     = GlEnum(Context::UNSIGNED_INT_5_9_9_9_REV),
}
