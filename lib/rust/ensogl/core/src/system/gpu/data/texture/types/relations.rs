//! This module defines relations between internal format, format, sampler type, and other
//! properties of textures.



// ==============
// === Macros ===
// ==============

/// Runs the provided macro with all texture format relations. In order to learn more about the
/// possible relations, refer to the source code and to the guide:
/// https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texImage2D
///
/// Legend:
/// - COL (Color Renderable) True if you can render to this format of texture.
///
/// - FILT (Texture Filterable) True if you can filter the texture, false if you can ony use
///   `Nearest`.
///
/// - BLEND (Color Blendable)
///   True if you can use color blending when writing to this texture from shaders.
///   According to https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture_integer.txt :
///   Blending is dependent on the incoming fragment's alpha value and that of the corresponding
///   currently stored pixel. Blending applies only in RGBA mode and only if the color buffer has a
///   fixed-point or floating-point format; in color index mode or if the color buffer has an
///   integer format, it is bypassed.
///
/// The features of some texture formats can be modified through extensions. For example, the
/// extension `EXT_color_buffer_float` can make the group of float texture (e.g., `Rgba32f`) color
/// renderable.
#[macro_export]
macro_rules! with_texture_format_relations { ($f:ident $args:tt) => { $crate::$f! { $args
//  INTERNAL_FORMAT   FORMAT         SAMPLER      COL   FILT  BLEND [POSSIBLE_TYPE:BYTES_PER_TEXTURE_ELEM]
    Alpha             Alpha          FloatSampler True  True  False [u8:U1,f16:U2,f32:U4]
    Luminance         Luminance      FloatSampler True  True  False [u8:U1,f16:U2,f32:U4]
    LuminanceAlpha    LuminanceAlpha FloatSampler True  True  False [u8:U2,f16:U4,f32:U8]
    Rgb               Rgb            FloatSampler True  True  False [u8:U3,f16:U6,f32:U12,u16_5_6_5:U2]
    Rgba              Rgba           FloatSampler True  True  True  [u8:U4,f16:U8,f32:U16,u16_4_4_4_4:U2,u16_5_5_5_1:U2]
    R8                Red            FloatSampler True  True  False [u8:U1]
    R8SNorm           Red            FloatSampler False True  False [i8:U1]
    R16f              Red            FloatSampler False True  False [f32:U4,f16:U2]
    R32f              Red            FloatSampler False False False [f32:U4]
    R8ui              RedInteger     UIntSampler  True  False False [u8:U1]
    R8i               RedInteger     IntSampler   True  False False [i8:U1]
    R16ui             RedInteger     UIntSampler  True  False False [u16:U2]
    R16i              RedInteger     IntSampler   True  False False [i16:U2]
    R32ui             RedInteger     UIntSampler  True  False False [u32:U4]
    R32i              RedInteger     IntSampler   True  False False [i32:U4]
    Rg8               Rg             FloatSampler True  True  False [u8:U2]
    Rg8SNorm          Rg             FloatSampler False True  False [i8:U2]
    Rg16f             Rg             FloatSampler False True  False [f32:U8,f16:U4]
    Rg32f             Rg             FloatSampler False False False [f32:U8]
    Rg8ui             RgInteger      UIntSampler  True  False False [u8:U2]
    Rg8i              RgInteger      IntSampler   True  False False [i8:U2]
    Rg16ui            RgInteger      UIntSampler  True  False False [u16:U4]
    Rg16i             RgInteger      IntSampler   True  False False [i16:U4]
    Rg32ui            RgInteger      UIntSampler  True  False False [u32:U8]
    Rg32i             RgInteger      IntSampler   True  False False [i32:U8]
    Rgb8              Rgb            FloatSampler True  True  False [u8:U3]
    SRgb8             Rgb            FloatSampler False True  False [u8:U3]
    Rgb565            Rgb            FloatSampler True  True  False [u8:U3,u16_5_6_5:U2]
    Rgb8SNorm         Rgb            FloatSampler False True  False [i8:U3]
    R11fG11fB10f      Rgb            FloatSampler False True  False [f32:U12,f16:U6,u32_f10_f11_f11_REV:U4]
    Rgb9E5            Rgb            FloatSampler False True  False [f32:U12,f16:U6,u32_5_9_9_9_REV:U4]
    Rgb16f            Rgb            FloatSampler False True  False [f32:U12,f16:U6]
    Rgb32f            Rgb            FloatSampler False False False [f32:U12]
    Rgb8ui            RgbInteger     UIntSampler  False False False [u8:U3]
    Rgb8i             RgbInteger     IntSampler   False False False [i8:U3]
    Rgb16ui           RgbInteger     UIntSampler  False False False [u16:U6]
    Rgb16i            RgbInteger     IntSampler   False False False [i16:U6]
    Rgb32ui           RgbInteger     UIntSampler  False False False [u32:U12]
    Rgb32i            RgbInteger     IntSampler   False False False [i32:U12]
    Rgba8             Rgba           FloatSampler True  True  True  [u8:U4]
    SRgb8Alpha8       Rgba           FloatSampler True  True  True  [u8:U4]
    Rgba8SNorm        Rgba           FloatSampler False True  True  [i8:U4]
    Rgb5A1            Rgba           FloatSampler True  True  True  [u8:U4,u16_5_5_5_1:U2,u32_2_10_10_10_REV:U4]
    Rgba4             Rgba           FloatSampler True  True  True  [u8:U4,u16_4_4_4_4:U2]
    Rgb10A2           Rgba           FloatSampler True  True  True  [u32_2_10_10_10_REV:U4]
    Rgba16f           Rgba           FloatSampler False True  True  [f32:U16,f16:U8]
    Rgba32f           Rgba           FloatSampler False False True  [f32:U16]
    Rgba8ui           RgbaInteger    UIntSampler  True  False False [u8:U4]
    Rgba8i            RgbaInteger    IntSampler   True  False False [i8:U4]
    Rgb10A2ui         RgbaInteger    UIntSampler  True  False False [u32_2_10_10_10_REV:U4]
    Rgba16ui          RgbaInteger    UIntSampler  True  False False [u16:U8]
    Rgba16i           RgbaInteger    IntSampler   True  False False [i16:U8]
    Rgba32i           RgbaInteger    IntSampler   True  False False [i32:U16]
    Rgba32ui          RgbaInteger    UIntSampler  True  False False [u32:U16]
    DepthComponent16  DepthComponent FloatSampler True  False False [u16:U2,u32:U4]
    DepthComponent24  DepthComponent FloatSampler True  False False [u32:U4]
    DepthComponent32f DepthComponent FloatSampler True  False False [f32:U4]
    Depth24Stencil8   DepthStencil   FloatSampler True  False False [u32_24_8:U4]
    Depth32fStencil8  DepthStencil   FloatSampler True  False False [f32_u24_u8_REV:U4]
}}}



// ======================
// === Meta Iterators ===
// ======================

/// See docs of `with_all_texture_types`.
#[macro_export]
macro_rules! with_all_texture_types_cartesians {
    ([$f:ident] [$($out:tt)*]) => {
        shapely::cartesian! { [[$f]] [Owned GpuOnly RemoteImage] [$($out)*] }
    };
    ([$f:ident _] $out:tt) => {
        $f! { $out }
    };
    ($f:tt $out:tt [$a:tt []] $($in:tt)*) => {
        $crate::with_all_texture_types_cartesians! {$f $out $($in)*}
    };
    ($f:tt [$($out:tt)*] [$a:tt [$b:tt $($bs:tt)*]] $($in:tt)*) => {
        $crate::with_all_texture_types_cartesians! {$f [$($out)* [$a $b]] [$a [$($bs)*]]  $($in)* }
    };
}

/// See docs of `with_all_texture_types`.
#[macro_export]
macro_rules! with_all_texture_types_impl {
    ( $f:tt
     $( $internal_format : ident
        $format          : ident
        $sampler         : ident
        $renderable      : tt
        $filterable      : tt
        $blendable       : tt
        [$($possible_types:ident : $bytes_per_element:ident),*]
    )*) => {
        $crate::with_all_texture_types_cartesians!
            { $f [] $([$internal_format [$($possible_types)*]])* }
    }
}

/// Runs the argument macro providing it with list of all possible texture types:
/// `arg! { [Alpha u8] [Alpha f16] [Alpha f32] [Luminance u8] ... }`
#[macro_export]
macro_rules! with_all_texture_types {
    ($f:tt) => {
        $crate::with_texture_format_relations! { with_all_texture_types_impl $f }
    };
}
