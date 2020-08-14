//! This module contains definitions of various color spaces, including `Rgb`, `Hsl`, `Lch`, etc.

use super::super::data::*;
use super::super::component::*;



// ==============
// === Macros ===
// ==============

macro_rules! define_color_space {
    ($(#[$($meta:tt)*])* $name:ident $a_name:ident $data_name:ident [$($comp:ident)*]) => {
        $(#[$($meta)*])*
        pub type $name = Color<$data_name>;

        $(#[$($meta)*])*
        pub type $a_name = Color<Alpha<$data_name>>;

        $(#[$($meta)*])*
        #[derive(Clone,Copy,Debug,Default,PartialEq)]
        #[allow(missing_docs)]
        pub struct $data_name {
            $(pub $comp : f32),*
        }

        impl $data_name {
            /// Constructor.
            pub const fn new($($comp:f32),*) -> Self {
                Self {$($comp),*}
            }
        }

        impl $name {
            /// Constructor.
            pub const fn new($($comp:f32),*) -> Self {
                let data = $data_name::new($($comp),*);
                Self {data}
            }
        }

        impl $a_name {
            /// Constructor.
            pub const fn new($($comp:f32),*,alpha:f32) -> Self {
                let opaque = Color {data : $data_name::new($($comp),*)};
                let data   = Alpha {alpha,opaque};
                Self {data}
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let comps = vec![$(self.$comp.to_string()),*].join(",");
                write!(f,"{}({})",stringify!($name),comps)
            }
        }

        impl std::fmt::Debug for $a_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let comps = vec![$(self.$comp.to_string()),*,self.alpha.to_string()].join(",");
                write!(f,"{}({})",stringify!($a_name),comps)
            }
        }

        impl HasComponentsRepr for $data_name{
            type ComponentsRepr = ($(enso_shapely::replace!($comp,f32)),*,);
        }

        impl From<$data_name> for ComponentsOf<$data_name> {
            fn from(data:$data_name) -> Self {
                Components(($(data.$comp.clone()),*,))
            }
        }

        impl From<ComponentsOf<$data_name>> for $data_name {
            fn from(Components{tuple:($($comp),*,)}:ComponentsOf<Self>) -> Self {
                Self {$($comp),*}
            }
        }

        impl ComponentMap for $data_name {
            fn map<F:Fn(f32)->f32>(&self, f:F) -> Self {
                $(let $comp = f(self.$comp);)*
                Self {$($comp),*}
            }
        }
    };
}



// ===========
// === Rgb ===
// ===========

define_color_space! {
    /// The most common color space, when it comes to computer graphics, and it's defined as an
    /// additive mixture of red, green and blue light, where gray scale colors are created when
    /// these three channels are equal in strength.
    ///
    /// Many conversions and operations on this color space requires that it's linear, meaning that
    /// gamma correction is required when converting to and from a displayable `RGB` to `LinearRgb`.
    ///
    /// ## Parameters
    ///
    /// - `red` [0.0 - 1.0]
    ///   The amount of red light, where 0.0 is no red light and 1.0 is the highest displayable
    ///   amount.
    ///
    /// - `blue` [0.0 - 1.0]
    ///   The amount of blue light, where 0.0 is no blue light and 1.0 is the highest displayable
    ///   amount.
    ///
    /// - `green` [0.0 - 1.0]
    ///   The amount of green light, where 0.0 is no green light and 1.0 is the highest displayable
    ///   amount.
    Rgb Rgba RgbData [red green blue]
}

impl Rgb {
    /// Converts the color to `LinearRgb` representation.
    pub fn into_linear(self) -> LinearRgb {
        self.into()
    }
}

impl Rgba {
    /// Converts the color to `LinearRgba` representation.
    pub fn into_linear(self) -> LinearRgba {
        self.into()
    }
}



// =================
// === LinearRgb ===
// =================

define_color_space! {
    /// Linear sRGBv space. See `Rgb` to learn more.
    LinearRgb LinearRgba LinearRgbData [red green blue]
}



// ===========
// === Hsl ===
// ===========

define_color_space! {
    /// Linear HSL color space.
    ///
    /// The HSL color space can be seen as a cylindrical version of RGB, where the hue is the angle
    /// around the color cylinder, the saturation is the distance from the center, and the lightness
    /// is the height from the bottom. Its composition makes it especially good for operations like
    /// changing green to red, making a color more gray, or making it darker.
    ///
    /// See `Hsv` for a very similar color space, with brightness instead of lightness.
    ///
    /// ## Parameters
    ///
    /// - `hue` [0.0 - 1.0]
    ///   The hue of the color. Decides if it's red, blue, purple, etc. You can use `hue_degrees`
    ///   or `hue_radians` to gen hue in non-normalized form. Most implementations use value range
    ///   of [0 .. 360] instead. It was rescaled for convenience.
    ///
    /// - `saturation` [0.0 - 1.0]
    ///   The colorfulness of the color. 0.0 gives gray scale colors and 1.0 will give absolutely
    ///   clear colors.
    ///
    /// - `lightness` [0.0 - 1.0]
    ///   Decides how light the color will look. 0.0 will be black, 0.5 will give a clear color,
    ///   and 1.0 will give white.
    Hsl Hsla HslData [hue saturation lightness]
}



// ===========
// === Xyz ===
// ===========

define_color_space! {
    /// The CIE 1931 XYZ color space.
    ///
    /// XYZ links the perceived colors to their wavelengths and simply makes it possible to describe
    /// the way we see colors as numbers. It's often used when converting from one color space to an
    /// other, and requires a standard illuminant and a standard observer to be defined.
    ///
    /// Conversions and operations on this color space depend on the defined white point. This
    /// implementation uses the `D65` white point by default.
    ///
    /// ## Parameters
    ///
    /// - `x` [0.0 - 0.95047] for the default `D65` white point.
    ///   Scale of what can be seen as a response curve for the cone cells in the human eye. Its
    ///   range depends on the white point.
    ///
    /// - `y` [0.0 - 1.0]
    ///   Luminance of the color, where 0.0 is black and 1.0 is white.
    ///
    /// - `z` [0.0 - 1.08883] for the default `D65` white point.
    ///   Scale of what can be seen as the blue stimulation. Its range depends on the white point.
    Xyz Xyza XyzData [x y z]
}



// ===========
// === Lab ===
// ===========

define_color_space! {
    /// The CIE L*a*b* (CIELAB) color space.
    ///
    /// CIE L*a*b* is a device independent color space which includes all perceivable colors. It's
    /// sometimes used to convert between other color spaces, because of its ability to represent
    /// all of their colors, and sometimes in color manipulation, because of its perceptual
    /// uniformity. This means that the perceptual difference between two colors is equal to their
    /// numerical difference.
    ///
    /// ## Parameters
    /// The parameters of L*a*b* are quite different, compared to many other color spaces, so
    /// manipulating them manually may be unintuitive.
    ///
    /// - `lightness` [0.0 - 1.0]
    ///   Lightness of 0.0 gives absolute black and 1.0 gives the brightest white. Most
    ///   implementations use value range of [0 .. 100] instead. It was rescaled for convenience.
    ///
    /// - `a` [-1.0 - 1.0]
    ///   a* goes from red at -1.0 to green at 1.0. Most implementations use value range of
    ///   [-128 .. 127] instead. It was rescaled for convenience.
    ///
    /// - `b` [-1.0 - 1.0]
    ///   b* goes from yellow at -1.0 to blue at 1.0. Most implementations use value range of
    ///   [-128 .. 127] instead. It was rescaled for convenience.
    Lab Laba LabData [lightness a b]
}

impl LabData {
    /// Computes the `hue` in degrees of the current color.
    pub fn hue(&self) -> Option<f32> {
        if self.a == 0.0 && self.b == 0.0 {
            None
        } else {
            let mut hue = self.b.atan2(self.a) * 180.0 / std::f32::consts::PI;
            if hue < 0.0 { hue += 360.0 }
            Some(hue)
        }
    }
}



// ===========
// === Lch ===
// ===========

define_color_space! {
    /// CIE L*C*h°, a polar version of CIE L*a*b*.
    ///
    /// L*C*h° shares its range and perceptual uniformity with L*a*b*, but it's a cylindrical color
    /// space, like HSL and HSV. This gives it the same ability to directly change the hue and
    /// colorfulness of a color, while preserving other visual aspects.
    ///
    /// **WARNING**
    /// You should be aware that the `CIE L*C*h°` is much wider than sRGB space which most monitors
    /// are limited to. Many combinations of valid values of the parameters will escape the sRGB
    /// space and will be clamped to it. Sometimes the value can be as low as 0.3 for chroma,
    /// which in combination with lightness of 0.6 and hue of 0.57 (blue) escapes sRGB space. It
    /// does not for other hues though! In most cases, escaping the space does not give us bad
    /// visual artifacts, but shifting colors would not be perceptual uniform anymore. Moreover,
    /// there is more and more monitors on the market which are able to display broader color space,
    /// `P3` or `Rec.2020`. There are combinations of values which escape even these color spaces.
    /// In order to visually play which values are OK, we suggest using the online tool:
    /// https://css.land/lch . Please note, however, that this tool gives slightly different values
    /// than this implementation. Our implementation gives the same values as the following tools:
    /// - http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    /// - https://www.easyrgb.com/en/convert.php
    ///
    /// ## Parameters
    ///
    /// - `lightness` [0.0 - 1.0]
    ///   Lightness of 0.0 gives absolute black and 100.0 gives the brightest white. Most
    ///   implementations use value range of [0 .. 100] instead. It was rescaled for convenience.
    ///
    /// - `chroma` [0.0 - 1.0]
    ///   The colorfulness of the color. It's similar to saturation. 0.0 gives gray scale colors,
    ///   and numbers around 128-181 gives fully saturated colors. The upper limit should include
    ///   the whole L*a*b* space and some more. You can use higher values than 1.0 to target `P3`,
    ///   `Rec.2020`, or even larger color spaces. Most implementations use value range of
    ///   [0 .. 132] instead. It was rescaled for convenience.
    ///
    /// - `hue` [0.0 - 1.0]
    ///   The hue of the color. Decides if it's red, blue, purple, etc. You can use `hue_degrees`
    ///   or `hue_radians` to gen hue in non-normalized form. Most implementations use value range
    ///   of [0 .. 360] instead. It was rescaled for convenience.
    Lch Lcha LchData [lightness chroma hue]
}

#[allow(missing_docs)]
impl Lch {
    pub fn pink_hue       () -> f32 { 0.0   } // approx.   0.0 degrees
    pub fn red_hue        () -> f32 { 0.111 } // approx.  40.0 degrees
    pub fn orange_hue     () -> f32 { 0.18  } // approx.  65.0 degrees
    pub fn yellow_hue     () -> f32 { 0.236 } // approx.  85.0 degrees
    pub fn olive_hue      () -> f32 { 0.291 } // approx. 105.0 degrees
    pub fn green_hue      () -> f32 { 0.378 } // approx. 136.0 degrees
    pub fn blue_green_hue () -> f32 { 0.6   } // approx. 216.0 degrees
    pub fn blue_hue       () -> f32 { 0.672 } // approx. 242.0 degrees
    pub fn violet_hue     () -> f32 { 0.847 } // approx. 305.0 degrees
}

#[allow(missing_docs)]
impl Lch {
    pub fn white      ()             -> Lch { Lch::new(1.0,0.0,0.0) }
    pub fn black      ()             -> Lch { Lch::new(0.0,0.0,0.0) }
    pub fn pink       (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::pink_hue())       }
    pub fn red        (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::red_hue())        }
    pub fn orange     (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::orange_hue())     }
    pub fn yellow     (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::yellow_hue())     }
    pub fn olive      (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::olive_hue())      }
    pub fn green      (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::green_hue())      }
    pub fn blue_green (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::blue_green_hue()) }
    pub fn blue       (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::blue_hue())       }
    pub fn violet     (l:f32, c:f32) -> Lch { Lch::new(l,c,Lch::violet_hue())     }
}

#[allow(missing_docs)]
impl Lcha {
    pub fn white      ()             -> Lcha { Lch::white      ()    . into() }
    pub fn black      ()             -> Lcha { Lch::black      ()    . into() }
    pub fn pink       (l:f32, c:f32) -> Lcha { Lch::pink       (l,c) . into() }
    pub fn red        (l:f32, c:f32) -> Lcha { Lch::red        (l,c) . into() }
    pub fn orange     (l:f32, c:f32) -> Lcha { Lch::orange     (l,c) . into() }
    pub fn yellow     (l:f32, c:f32) -> Lcha { Lch::yellow     (l,c) . into() }
    pub fn olive      (l:f32, c:f32) -> Lcha { Lch::olive      (l,c) . into() }
    pub fn green      (l:f32, c:f32) -> Lcha { Lch::green      (l,c) . into() }
    pub fn blue_green (l:f32, c:f32) -> Lcha { Lch::blue_green (l,c) . into() }
    pub fn blue       (l:f32, c:f32) -> Lcha { Lch::blue       (l,c) . into() }
    pub fn violet     (l:f32, c:f32) -> Lcha { Lch::violet     (l,c) . into() }
}
