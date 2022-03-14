//! Texture formats. A `GlEnum` specifying the format of the texel data. Follow the link to learn
//! more: https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texImage2D

use crate::prelude::*;
use crate::system::gpu::data::texture::gl_enums::*;

use crate::system::gpu::data::gl_enum::GlEnum;



// ==============
// === Format ===
// ==============

/// Trait for every format of a texture.
pub trait Format = Default + Into<AnyFormat> + PhantomInto<GlEnum>;

crate::define_singleton_enum_gl_from! { [GlEnum]
    AnyFormat
        { Alpha, DepthComponent, DepthStencil, Luminance, LuminanceAlpha, Red, RedInteger, Rg
        , Rgb, Rgba, RgbaInteger, RgbInteger, RgInteger,
        }
}
