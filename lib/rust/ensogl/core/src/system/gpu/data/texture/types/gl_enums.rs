//! This module defines all GLEnum bindings used by textures.


use crate::prelude::*;

use crate::system::gpu::data::gl_enum::GlEnum;
use crate::system::gpu::Context;



// ===============
// === GLEnums ===
// ===============

crate::define_singletons_gl! { [GlEnum]
    Alpha             = GlEnum(Context::ALPHA),
    Depth24Stencil8   = GlEnum(Context::DEPTH24_STENCIL8),
    Depth32fStencil8  = GlEnum(Context::DEPTH32F_STENCIL8),
    DepthComponent    = GlEnum(Context::DEPTH_COMPONENT),
    DepthComponent16  = GlEnum(Context::DEPTH_COMPONENT16),
    DepthComponent24  = GlEnum(Context::DEPTH_COMPONENT24),
    DepthComponent32f = GlEnum(Context::DEPTH_COMPONENT32F),
    DepthStencil      = GlEnum(Context::DEPTH_STENCIL),
    Luminance         = GlEnum(Context::LUMINANCE),
    LuminanceAlpha    = GlEnum(Context::LUMINANCE_ALPHA),
    R11fG11fB10f      = GlEnum(Context::R11F_G11F_B10F),
    R16f              = GlEnum(Context::R16F),
    R16i              = GlEnum(Context::R16I),
    R16ui             = GlEnum(Context::R16UI),
    R32f              = GlEnum(Context::R32F),
    R32i              = GlEnum(Context::R32I),
    R32ui             = GlEnum(Context::R32UI),
    R8                = GlEnum(Context::R8),
    R8i               = GlEnum(Context::R8I),
    R8SNorm           = GlEnum(Context::R8_SNORM),
    R8ui              = GlEnum(Context::R8UI),
    Red               = GlEnum(Context::RED),
    RedInteger        = GlEnum(Context::RED_INTEGER),
    Rg                = GlEnum(Context::RG),
    Rg16f             = GlEnum(Context::RG16F),
    Rg16i             = GlEnum(Context::RG16I),
    Rg16ui            = GlEnum(Context::RG16UI),
    Rg32f             = GlEnum(Context::RG32F),
    Rg32i             = GlEnum(Context::RG32I),
    Rg32ui            = GlEnum(Context::RG32UI),
    Rg8               = GlEnum(Context::RG8),
    Rg8i              = GlEnum(Context::RG8I),
    Rg8SNorm          = GlEnum(Context::RG8_SNORM),
    Rg8ui             = GlEnum(Context::RG8UI),
    Rgb               = GlEnum(Context::RGB),
    Rgb10A2           = GlEnum(Context::RGB10_A2),
    Rgb10A2ui         = GlEnum(Context::RGB10_A2UI),
    Rgb16f            = GlEnum(Context::RGB16F),
    Rgb16i            = GlEnum(Context::RGB16I),
    Rgb16ui           = GlEnum(Context::RGB16UI),
    Rgb32f            = GlEnum(Context::RGB32F),
    Rgb32i            = GlEnum(Context::RGB32I),
    Rgb32ui           = GlEnum(Context::RGB32UI),
    Rgb565            = GlEnum(Context::RGB565),
    Rgb5A1            = GlEnum(Context::RGB5_A1),
    Rgb8              = GlEnum(Context::RGB8),
    Rgb8i             = GlEnum(Context::RGB8I),
    Rgb8SNorm         = GlEnum(Context::RGB8_SNORM),
    Rgb8ui            = GlEnum(Context::RGB8UI),
    Rgb9E5            = GlEnum(Context::RGB9_E5),
    Rgba              = GlEnum(Context::RGBA),
    Rgba16f           = GlEnum(Context::RGBA16F),
    Rgba16i           = GlEnum(Context::RGBA16I),
    Rgba16ui          = GlEnum(Context::RGBA16UI),
    Rgba32f           = GlEnum(Context::RGBA32F),
    Rgba32i           = GlEnum(Context::RGBA32I),
    Rgba32ui          = GlEnum(Context::RGBA32UI),
    Rgba4             = GlEnum(Context::RGBA4),
    Rgba8             = GlEnum(Context::RGBA8),
    Rgba8i            = GlEnum(Context::RGBA8I),
    Rgba8SNorm        = GlEnum(Context::RGBA8_SNORM),
    Rgba8ui           = GlEnum(Context::RGBA8UI),
    RgbaInteger       = GlEnum(Context::RGBA_INTEGER),
    RgbInteger        = GlEnum(Context::RGB_INTEGER),
    RgInteger         = GlEnum(Context::RG_INTEGER),
    SRgb8             = GlEnum(Context::SRGB8),
    SRgb8Alpha8       = GlEnum(Context::SRGB8_ALPHA8),
}
