//! A GLenum specifying the color components in the texture. Follow the link to learn more:
//! https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texImage2D

use crate::prelude::*;
use crate::system::gpu::data::gl_enum::*;
use crate::system::gpu::data::prim::*;
use crate::system::gpu::data::texture::format::*;
use crate::system::gpu::data::texture::gl_enums::*;
use crate::system::gpu::data::texture::sampler::*;
use nalgebra::*;



// ====================
// === InternalItem ===
// ====================

/// Provides information about the size of a texture element for a given `InternalFormat`.
pub trait InternalItem<Type> {
    /// The size in bytes of a single element of the texture.
    type ByteSize: DimName;
}



// ======================
// === InternalFormat ===
// ======================

crate::define_singleton_enum_gl_from! { [GlEnum]
    AnyInternalFormat
        { Alpha, Luminance, LuminanceAlpha, Rgb, Rgba, R8, R8SNorm, R16f, R32f, R8ui, R8i
        , R16ui, R16i, R32ui, R32i, Rg8, Rg8SNorm, Rg16f, Rg32f, Rg8ui, Rg8i, Rg16ui, Rg16i
        , Rg32ui, Rg32i, Rgb8, SRgb8, Rgb565, Rgb8SNorm, R11fG11fB10f, Rgb9E5, Rgb16f, Rgb32f
        , Rgb8ui, Rgb8i, Rgb16ui, Rgb16i, Rgb32ui, Rgb32i, Rgba8, SRgb8Alpha8, Rgba8SNorm
        , Rgb5A1, Rgba4, Rgb10A2, Rgba16f, Rgba32f, Rgba8ui, Rgba8i, Rgb10A2ui, Rgba16ui
        , Rgba16i, Rgba32i, Rgba32ui, DepthComponent16, DepthComponent24, DepthComponent32f
        , Depth24Stencil8, Depth32fStencil8
        }
}

/// Provides information about the suitable format and checks if the texture is color renderable
/// and filterable for a given `InternalFormat`.
pub trait InternalFormat: Default + Into<AnyInternalFormat> + 'static {
    /// The `Format` associated with this `InternalFormat`. Please note that `InternalFormat`
    /// dictates which `Format` to use, but this relation is asymmetrical.
    type Format: Format;

    /// The sampler associated to this `InternalFormat`. If the sampler used in GLSL does not match
    /// the internal format of the texture, an undefined value is returned:
    /// https://www.khronos.org/registry/webgl/specs/latest/2.0/#5.22
    type Sampler: Sampler;

    /// Checks if the texture format can be rendered from shaders.
    type ColorRenderable: KnownTypeValue<Value = bool>;

    /// Checks it he texture can be filtered.
    type Filterable: KnownTypeValue<Value = bool>;

    /// Checks wether blending applies to this texture when rendering from shaders.
    type ColorBlendable: KnownTypeValue<Value = bool>;

    /// Checks if the texture format can be rendered as color.
    fn color_renderable() -> bool {
        <Self::ColorRenderable as KnownTypeValue>::value()
    }

    /// Checks it he texture can be filtered.
    fn filterable() -> bool {
        <Self::Filterable as KnownTypeValue>::value()
    }
}



// =================
// === Instances ===
// =================

/// Generates `InternalItem` and `InternalFormat` instances. Please note that the relation
/// between internal format, format, and possible client texel types is very strict and you are
/// not allowed to choose them arbitrary. Follow the link to learn more about possible relations and
/// how the values were composed below:
/// https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texImage2D
#[macro_export]
macro_rules! generate_internal_format_instances {
    ([] $( $internal_format:ident $format:ident $sampler:ident
           $renderable:tt $filterable:tt $blendable:tt $elem_descs:tt
    )*) => {
        $(
            $crate::generate_internal_format_instances_item!
            { $internal_format $format $sampler $renderable $filterable $blendable $elem_descs }
        )*
    }
}

/// See docs of `generate_internal_format_instances`.
#[macro_export]
macro_rules! generate_internal_format_instances_item {
    ( $internal_format:ident $format:ident $sampler:ident
      $renderable:tt $filterable:tt $blendable:tt
      [$($possible_types:ident : $bytes_per_element:ident),*]
    ) => {
        $(impl InternalItem<$possible_types> for $internal_format {
            type ByteSize = $bytes_per_element;
        })*

        impl InternalFormat for $internal_format {
            type Format          = $format;
            type Sampler         = $sampler;
            type ColorRenderable = $renderable;
            type ColorBlendable  = $blendable;
            type Filterable      = $filterable;
        }
    }
}

crate::with_texture_format_relations!(generate_internal_format_instances []);

impl AnyInternalFormat {
    pub fn format(self) -> AnyFormat {
        // TODO: Generate this.
        match self {
            AnyInternalFormat::Alpha => AnyFormat::Alpha,
            AnyInternalFormat::Luminance => AnyFormat::Luminance,
            AnyInternalFormat::LuminanceAlpha => AnyFormat::LuminanceAlpha,
            AnyInternalFormat::Rgb => AnyFormat::Rgb,
            AnyInternalFormat::Rgba => AnyFormat::Rgba,
            AnyInternalFormat::R8 => AnyFormat::Red,
            AnyInternalFormat::R8SNorm => AnyFormat::Red,
            AnyInternalFormat::R16f => AnyFormat::Red,
            AnyInternalFormat::R32f => AnyFormat::Red,
            AnyInternalFormat::R8ui => AnyFormat::RedInteger,
            AnyInternalFormat::R8i => AnyFormat::RedInteger,
            AnyInternalFormat::R16ui => AnyFormat::RedInteger,
            AnyInternalFormat::R16i => AnyFormat::RedInteger,
            AnyInternalFormat::R32ui => AnyFormat::RedInteger,
            AnyInternalFormat::R32i => AnyFormat::RedInteger,
            AnyInternalFormat::Rg8 => AnyFormat::Rg,
            AnyInternalFormat::Rg8SNorm => AnyFormat::Rg,
            AnyInternalFormat::Rg16f => AnyFormat::Rg,
            AnyInternalFormat::Rg32f => AnyFormat::Rg,
            AnyInternalFormat::Rg8ui => AnyFormat::RgInteger,
            AnyInternalFormat::Rg8i => AnyFormat::RgInteger,
            AnyInternalFormat::Rg16ui => AnyFormat::RgInteger,
            AnyInternalFormat::Rg16i => AnyFormat::RgInteger,
            AnyInternalFormat::Rg32ui => AnyFormat::RgInteger,
            AnyInternalFormat::Rg32i => AnyFormat::RgInteger,
            AnyInternalFormat::Rgb8 => AnyFormat::Rgb,
            AnyInternalFormat::SRgb8 => AnyFormat::Rgb,
            AnyInternalFormat::Rgb565 => AnyFormat::Rgb,
            AnyInternalFormat::Rgb8SNorm => AnyFormat::Rgb,
            AnyInternalFormat::R11fG11fB10f => AnyFormat::Rgb,
            AnyInternalFormat::Rgb9E5 => AnyFormat::Rgb,
            AnyInternalFormat::Rgb16f => AnyFormat::Rgb,
            AnyInternalFormat::Rgb32f => AnyFormat::Rgb,
            AnyInternalFormat::Rgb8ui => AnyFormat::RgbInteger,
            AnyInternalFormat::Rgb8i => AnyFormat::RgbInteger,
            AnyInternalFormat::Rgb16ui => AnyFormat::RgbInteger,
            AnyInternalFormat::Rgb16i => AnyFormat::RgbInteger,
            AnyInternalFormat::Rgb32ui => AnyFormat::RgbInteger,
            AnyInternalFormat::Rgb32i => AnyFormat::RgbInteger,
            AnyInternalFormat::Rgba8 => AnyFormat::Rgba,
            AnyInternalFormat::SRgb8Alpha8 => AnyFormat::Rgba,
            AnyInternalFormat::Rgba8SNorm => AnyFormat::Rgba,
            AnyInternalFormat::Rgb5A1 => AnyFormat::Rgba,
            AnyInternalFormat::Rgba4 => AnyFormat::Rgba,
            AnyInternalFormat::Rgb10A2 => AnyFormat::Rgba,
            AnyInternalFormat::Rgba16f => AnyFormat::Rgba,
            AnyInternalFormat::Rgba32f => AnyFormat::Rgba,
            AnyInternalFormat::Rgba8ui => AnyFormat::RgbaInteger,
            AnyInternalFormat::Rgba8i => AnyFormat::RgbaInteger,
            AnyInternalFormat::Rgb10A2ui => AnyFormat::RgbaInteger,
            AnyInternalFormat::Rgba16ui => AnyFormat::RgbaInteger,
            AnyInternalFormat::Rgba16i => AnyFormat::RgbaInteger,
            AnyInternalFormat::Rgba32i => AnyFormat::RgbaInteger,
            AnyInternalFormat::Rgba32ui => AnyFormat::RgbaInteger,
            AnyInternalFormat::DepthComponent16 => AnyFormat::DepthComponent,
            AnyInternalFormat::DepthComponent24 => AnyFormat::DepthComponent,
            AnyInternalFormat::DepthComponent32f => AnyFormat::DepthComponent,
            AnyInternalFormat::Depth24Stencil8 => AnyFormat::DepthStencil,
            AnyInternalFormat::Depth32fStencil8 => AnyFormat::DepthStencil,
        }
    }
}
