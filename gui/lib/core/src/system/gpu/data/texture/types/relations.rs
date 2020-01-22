//! This module defines relations between internal format, format, sampler type, and other
//! properties of textures.



// ==============
// === Macros ===
// ==============

/// Runs the provided macro with all texture format relations. In order to learn more about the
/// possible relations, refer to the source code and to the guide:
/// https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texImage2D
#[macro_export]
macro_rules! with_texture_format_relations { ($f:ident $args:tt) => { $crate::$f! { $args
//  INTERNAL_FORMAT   FORMAT         SAMPLER      COL   FILT  [POSSIBLE_TYPE:BYTES_PER_TEXTURE_ELEM]
    Alpha             Alpha          FloatSampler True  True  [u8:U1,f16:U2,f32:U4]
    Luminance         Luminance      FloatSampler True  True  [u8:U1,f16:U2,f32:U4]
    LuminanceAlpha    LuminanceAlpha FloatSampler True  True  [u8:U2,f16:U4,f32:U8]
    Rgb               Rgb            FloatSampler True  True  [u8:U3,f16:U6,f32:U12,u16_5_6_5:U2]
    Rgba              Rgba           FloatSampler True  True  [u8:U4,f16:U8,f32:U16,u16_4_4_4_4:U2,u16_5_5_5_1:U2]
    R8                Red            FloatSampler True  True  [u8:U1]
    R8SNorm           Red            FloatSampler False True  [i8:U1]
    R16f              Red            FloatSampler False True  [f32:U4,f16:U2]
    R32f              Red            FloatSampler False False [f32:U4]
    R8ui              RedInteger     UIntSampler  True  False [u8:U1]
    R8i               RedInteger     IntSampler   True  False [i8:U1]
    R16ui             RedInteger     UIntSampler  True  False [u16:U2]
    R16i              RedInteger     IntSampler   True  False [i16:U2]
    R32ui             RedInteger     UIntSampler  True  False [u32:U4]
    R32i              RedInteger     IntSampler   True  False [i32:U4]
    Rg8               Rg             FloatSampler True  True  [u8:U2]
    Rg8SNorm          Rg             FloatSampler False True  [i8:U2]
    Rg16f             Rg             FloatSampler False True  [f32:U8,f16:U4]
    Rg32f             Rg             FloatSampler False False [f32:U8]
    Rg8ui             RgInteger      UIntSampler  True  False [u8:U2]
    Rg8i              RgInteger      IntSampler   True  False [i8:U2]
    Rg16ui            RgInteger      UIntSampler  True  False [u16:U4]
    Rg16i             RgInteger      IntSampler   True  False [i16:U4]
    Rg32ui            RgInteger      UIntSampler  True  False [u32:U8]
    Rg32i             RgInteger      IntSampler   True  False [i32:U8]
    Rgb8              Rgb            FloatSampler True  True  [u8:U3]
    SRgb8             Rgb            FloatSampler False True  [u8:U3]
    Rgb565            Rgb            FloatSampler True  True  [u8:U3,u16_5_6_5:U2]
    Rgb8SNorm         Rgb            FloatSampler False True  [i8:U3]
    R11fG11fB10f      Rgb            FloatSampler False True  [f32:U12,f16:U6,u32_f10_f11_f11_REV:U4]
    Rgb9E5            Rgb            FloatSampler False True  [f32:U12,f16:U6,u32_5_9_9_9_REV:U4]
    Rgb16f            Rgb            FloatSampler False True  [f32:U12,f16:U6]
    Rgb32f            Rgb            FloatSampler False False [f32:U12]
    Rgb8ui            RgbInteger     UIntSampler  False False [u8:U3]
    Rgb8i             RgbInteger     IntSampler   False False [i8:U3]
    Rgb16ui           RgbInteger     UIntSampler  False False [u16:U6]
    Rgb16i            RgbInteger     IntSampler   False False [i16:U6]
    Rgb32ui           RgbInteger     UIntSampler  False False [u32:U12]
    Rgb32i            RgbInteger     IntSampler   False False [i32:U12]
    Rgba8             Rgba           FloatSampler True  True  [u8:U4]
    SRgb8Alpha8       Rgba           FloatSampler True  True  [u8:U4]
    Rgba8SNorm        Rgba           FloatSampler False True  [i8:U4]
    Rgb5A1            Rgba           FloatSampler True  True  [u8:U4,u16_5_5_5_1:U2,u32_2_10_10_10_REV:U4]
    Rgba4             Rgba           FloatSampler True  True  [u8:U4,u16_4_4_4_4:U2]
    Rgb10A2           Rgba           FloatSampler True  True  [u32_2_10_10_10_REV:U4]
    Rgba16f           Rgba           FloatSampler False True  [f32:U16,f16:U8]
    Rgba32f           Rgba           FloatSampler False False [f32:U16]
    Rgba8ui           RgbaInteger    UIntSampler  True  False [u8:U4]
    Rgba8i            RgbaInteger    IntSampler   True  False [i8:U4]
    Rgb10A2ui         RgbaInteger    UIntSampler  True  False [u32_2_10_10_10_REV:U4]
    Rgba16ui          RgbaInteger    UIntSampler  True  False [u16:U8]
    Rgba16i           RgbaInteger    IntSampler   True  False [i16:U8]
    Rgba32i           RgbaInteger    IntSampler   True  False [i32:U16]
    Rgba32ui          RgbaInteger    UIntSampler  True  False [u32:U16]
    DepthComponent16  DepthComponent FloatSampler True  False [u16:U2,u32:U4]
    DepthComponent24  DepthComponent FloatSampler True  False [u32:U4]
    DepthComponent32f DepthComponent FloatSampler True  False [f32:U4]
    Depth24Stencil8   DepthStencil   FloatSampler True  False [u32_24_8:U4]
    Depth32fStencil8  DepthStencil   FloatSampler True  False [f32_u24_u8_REV:U4]
}}}



// ======================
// === Meta Iterators ===
// ======================

/// See docs of `with_all_texture_types`.
#[macro_export]
macro_rules! with_all_texture_types_cartesians {
    ($f:ident [$($out:tt)*]) => {
        shapely::cartesian! { [[$f]] [Owned GpuOnly RemoteImage] [$($out)*] }
    };
    ($f:ident $out:tt [$a:tt []] $($in:tt)*) => {
        $crate::with_all_texture_types_cartesians! {$f $out $($in)*}
    };
    ($f:ident [$($out:tt)*] [$a:tt [$b:tt $($bs:tt)*]] $($in:tt)*) => {
        $crate::with_all_texture_types_cartesians! {$f [$($out)* [$a $b]] [$a [$($bs)*]]  $($in)* }
    };
}

/// See docs of `with_all_texture_types`.
#[macro_export]
macro_rules! with_all_texture_types_impl {
    ( [$f:ident]
     $( $internal_format:ident $format:ident $color_renderable:tt $filterable:tt
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
    ($f:ident) => {
        $crate::with_texture_format_relations! { with_all_texture_types_impl [$f] }
    }
}
