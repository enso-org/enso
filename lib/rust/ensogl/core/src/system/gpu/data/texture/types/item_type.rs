//! This module defines bindings to every item data type which can be stored in a texture.

use crate::prelude::*;
use crate::system::gpu::data::gl_enum::*;
use crate::system::gpu::data::prim::*;



// ============
// === Item ===
// ============

/// Trait describing every texture item type.
pub trait ItemType = Debug + PhantomInto<AnyItemType> + PhantomInto<GlEnum> + 'static;

crate::define_singleton_enum_gl_from! { [GlEnum]
    /// Any data type which can be stored in a texture.
    #[allow(non_camel_case_types)]
    AnyItemType
        {u8,u16,u32,i8,i16,i32,f16,f32,f32_u24_u8_REV,u16_4_4_4_4,u16_5_5_5_1,u16_5_6_5
        ,u32_f10_f11_f11_REV,u32_24_8,u32_2_10_10_10_REV,u32_5_9_9_9_REV}
}
