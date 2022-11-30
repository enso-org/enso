//! This module contains definitions of various color spaces, including `Rgb`, `Hsl`, `Lch`, etc.

use super::super::component::*;
use super::super::data::*;
use crate::prelude::*;



// ==============
// === Macros ===
// ==============

macro_rules! define_color_parsing {
    ($name:ident) => {
        impl FromStr for $name {
            type Err = ParseError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let (head, args) = generic_parse(s)?;
                if &head != stringify!($name) {
                    return Err(ParseError::new(format!(
                        "No '{}' header found.",
                        stringify!($name)
                    )));
                }
                Ok($name::from_slice(&args))
            }
        }
    };
}

macro_rules! define_color_spaces {
    ($($(#$meta:tt)* $name:ident $a_name:ident $data_name:ident $comps:tt)*) => {
        $(define_color_space!{ $(#$meta)* $name $a_name $data_name $comps })*

        /// A struct that can contain color in any supported format, like Rgba, or Lch.
        #[derive(Clone,Copy,Debug,PartialEq)]
        #[allow(missing_docs)]
        pub enum AnyFormat {
            $(
                $name($name),
                $a_name($a_name),
            )*
        }

        impl FromStr for AnyFormat {
            type Err = ParseError;
            fn from_str(s:&str) -> Result<Self, Self::Err> {
                let (head,args) = generic_parse(s)?;
                match head.as_str() {
                    $(
                        stringify!($name)   => Ok(AnyFormat::$name($name::from_slice(&args))),
                        stringify!($a_name) => Ok(AnyFormat::$a_name($a_name::from_slice(&args))),
                    )*
                    _ => panic!("Impossible.")
                }
            }
        }

        // TODO[WD]: This should be uncommented in the future. See the TODO comment below to
        //   learn more.
        // impl<C> From<AnyFormat> for Color<C>
        // where $(
        //     $name   : Into<Color<C>>,
        //     $a_name : Into<Color<C>>,
        // )* {
        //     fn from(c:AnyFormat) -> Self {
        //         match c {
        //             $(
        //                 AnyFormat::$name(t)   => t.into(),
        //                 AnyFormat::$a_name(t) => t.into(),
        //             )*
        //         }
        //     }
        // }
    }
}

impl<C> From<AnyFormat> for Color<C>
where
    Rgb: Into<Color<C>>,
    Rgba: Into<Color<C>>,
    Lch: Into<Color<C>>,
    Lcha: Into<Color<C>>,
{
    fn from(c: AnyFormat) -> Self {
        match c {
            AnyFormat::Rgb(t) => t.into(),
            AnyFormat::Rgba(t) => t.into(),
            AnyFormat::Lch(t) => t.into(),
            AnyFormat::Lcha(t) => t.into(),
            // TODO[WD]: This should be implemented by the commented out macro above, however,
            //   it requires a lot more conversions than we support currently. To be implemented
            //   one day.
            //   https://github.com/enso-org/ide/issues/1404
            _ => panic!("Not implemented."),
        }
    }
}

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

        /// Constructor.
        #[allow(non_snake_case)]
        pub const fn $name($($comp:f32),*) -> $name {
            $name::new($($comp),*)
        }

        impl $name {
            /// Constructor.
            pub const fn new($($comp:f32),*) -> Self {
                let data = $data_name::new($($comp),*);
                Self {data}
            }

            /// Constructor.
            pub fn from_slice(comps:&[f32]) -> Self {
                let mut iter = comps.iter().copied();
                $(let $comp = iter.next().unwrap_or_default();)*
                Self::new($($comp),*)
            }
        }

        /// Constructor.
        #[allow(non_snake_case)]
        pub const fn $a_name($($comp:f32),*,alpha:f32) -> $a_name {
            $a_name::new($($comp),*,alpha)
        }

        impl $a_name {
            /// Constructor.
            pub const fn new($($comp:f32),*,alpha:f32) -> Self {
                let opaque = Color {data : $data_name::new($($comp),*)};
                let data   = Alpha {alpha,opaque};
                Self {data}
            }

            /// Constructor.
            pub fn from_slice(comps:&[f32]) -> Self {
                let mut iter = comps.iter().copied();
                $(let $comp = iter.next().unwrap_or_default();)*
                let alpha = iter.next().unwrap_or(1.0);
                Self::new($($comp),*,alpha)
            }
        }

        impl Display for $name {
            fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let comps = vec![$(self.$comp.to_string()),*].join(",");
                write!(f,"{}({})",stringify!($name),comps)
            }
        }

        impl Display for $a_name {
            fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let comps = vec![$(self.$comp.to_string()),*,self.alpha.to_string()].join(",");
                write!(f,"{}({})",stringify!($a_name),comps)
            }
        }

        impl Debug for $name {
            fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                <Self as Display>::fmt(self,f)
            }
        }

        impl Debug for $a_name {
            fn fmt(&self, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                <Self as Display>::fmt(self,f)
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

        define_color_parsing!{$name}
        define_color_parsing!{$a_name}
    };
}



// ====================
// === Color Spaces ===
// ====================

define_color_spaces! {

    // === Rgb ===

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


    // === Hsl ===

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


    // === Xyz ===

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


    // === Lab ===

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


    // === Lch ===

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
    ///   The colorfulness of the color. It's similar to saturation. 0.0 gives grayscale colors,
    ///   while bigger values gives saturated ones. This value was scaled in such way, that the
    ///   chroma of 1.0 is the maximum saturation of any hue shade in sRGB color space (which often
    ///   corresponds to `LCH_MAX_CHROMA_IN_SRGB_IN_STD_EQUATIONS` value in such tools as
    ///   https://css.land/lch. You can use higher values than 1.0 to target `P3`, `Rec.2020`, or
    ///   even larger color spaces. Please, be aware that for some hue values, even small chroma
    ///   values are outside of the sRGB color space. For example, for dark greens, even the value
    ///   of 0.3 can not be displayed properly on most screens nowadays.
    ///
    /// - `hue` [0.0 - 1.0]
    ///   The hue of the color. Decides if it's red, blue, purple, etc. You can use `hue_degrees`
    ///   or `hue_radians` to gen hue in non-normalized form. Most implementations use value range
    ///   of [0 .. 360] instead. It was rescaled for convenience.
    Lch Lcha LchData [lightness chroma hue]
}



// ===========
// === Rgb ===
// ===========

impl Rgb {
    /// Construct RGB color by mapping [0 – 255] value range into [0.0 – 1.0].
    pub fn from_base_255(r: impl Into<f32>, g: impl Into<f32>, b: impl Into<f32>) -> Self {
        Self::new(r.into() / 255.0, g.into() / 255.0, b.into() / 255.0)
    }

    /// Return a color if the argument is a string matching one of the formats: `#RGB`, `#RRGGBB`,
    /// `RGB`, or `RRGGBB`, where `R`, `G`, `B` represent lower- or upper-case hexadecimal digits.
    /// The `RR`, `GG`, `BB` color components are mapped from `[00 - ff]` value range into `[0.0 -
    /// 1.0]` (a three-digit string matching a `#RGB` or `RGB` format is equivalent to a six-digit
    /// string matching a `#RRGGBB` format, constructed by duplicating the digits).
    ///
    /// The format is based on the hexadecimal color notation used in CSS (see:
    /// https://developer.mozilla.org/en-US/docs/Web/CSS/hex-color), with the following changes:
    /// - the `#` character is optional,
    /// - formats containing an alpha color component are not supported.
    /// ```
    /// # use ensogl_core::data::color::Rgb;
    /// fn color_to_u8_tuple(c: Rgb) -> (u8, u8, u8) {
    ///     ((c.red * 255.0) as u8, (c.green * 255.0) as u8, (c.blue * 255.0) as u8)
    /// }
    ///
    /// assert_eq!(Rgb::from_css_hex("#C047AB").map(color_to_u8_tuple), Some((0xC0, 0x47, 0xAB)));
    /// assert_eq!(Rgb::from_css_hex("#fff").map(color_to_u8_tuple), Some((0xff, 0xff, 0xff)));
    /// assert_eq!(Rgb::from_css_hex("fff").map(color_to_u8_tuple), Some((0xff, 0xff, 0xff)));
    /// assert_eq!(Rgb::from_css_hex("C047AB").map(color_to_u8_tuple), Some((0xC0, 0x47, 0xAB)));
    /// assert!(Rgb::from_css_hex("red").is_none());
    /// assert!(Rgb::from_css_hex("yellow").is_none());
    /// assert!(Rgb::from_css_hex("#red").is_none());
    /// assert!(Rgb::from_css_hex("#yellow").is_none());
    /// assert!(Rgb::from_css_hex("#").is_none());
    /// assert!(Rgb::from_css_hex("").is_none());
    /// ```
    pub fn from_css_hex(css_hex: &str) -> Option<Self> {
        let hex_bytes = css_hex.strip_prefix('#').unwrap_or(css_hex).as_bytes();
        let hex_color_components = match hex_bytes.len() {
            3 => Some(Vector3([hex_bytes[0]; 2], [hex_bytes[1]; 2], [hex_bytes[2]; 2])),
            6 => {
                let (chunks, _) = hex_bytes.as_chunks::<2>();
                Some(Vector3(chunks[0], chunks[1], chunks[2]))
            }
            _ => None,
        };
        hex_color_components.and_then(|components| {
            let red = byte_from_hex(components.x)?;
            let green = byte_from_hex(components.y)?;
            let blue = byte_from_hex(components.z)?;
            Some(Rgb::from_base_255(red, green, blue))
        })
    }

    /// Converts the color to `LinearRgb` representation.
    pub fn into_linear(self) -> LinearRgb {
        self.into()
    }

    /// Convert the color to JavaScript representation.
    pub fn to_javascript_string(self) -> String {
        let red = (self.red * 255.0).round() as i32;
        let green = (self.green * 255.0).round() as i32;
        let blue = (self.blue * 255.0).round() as i32;
        format!("rgb({},{},{})", red, green, blue)
    }
}


// === Rgb Helpers ===

/// Decode an 8-bit number from its big-endian hexadecimal encoding in ASCII. Return `None` if any
/// of the bytes stored in the argument array is not an upper- or lower-case hexadecimal digit in
/// ASCII.
fn byte_from_hex(s: [u8; 2]) -> Option<u8> {
    let first_digit = (s[0] as char).to_digit(16)? as u8;
    let second_digit = (s[1] as char).to_digit(16)? as u8;
    Some(first_digit << 4 | second_digit)
}


// === Rgba ===

impl Rgba {
    /// Constructor.
    pub fn black() -> Self {
        Self::new(0.0, 0.0, 0.0, 1.0)
    }

    /// Constructor.
    pub fn white() -> Self {
        Self::new(1.0, 1.0, 1.0, 1.0)
    }

    /// Constructor.
    pub fn red() -> Self {
        Self::new(1.0, 0.0, 0.0, 1.0)
    }

    /// Constructor.
    pub fn green() -> Self {
        Self::new(0.0, 1.0, 0.0, 1.0)
    }

    /// Constructor.
    pub fn blue() -> Self {
        Self::new(0.0, 0.0, 1.0, 1.0)
    }

    /// Fully transparent color constructor.
    pub fn transparent() -> Self {
        Self::new(0.0, 0.0, 0.0, 0.0)
    }

    /// Convert the color to `LinearRgba` representation.
    pub fn into_linear(self) -> LinearRgba {
        self.into()
    }

    /// Convert the color to JavaScript representation.
    pub fn to_javascript_string(self) -> String {
        let red = (self.red * 255.0).round() as i32;
        let green = (self.green * 255.0).round() as i32;
        let blue = (self.blue * 255.0).round() as i32;
        format!("rgba({},{},{},{})", red, green, blue, self.alpha)
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
// === Lab ===
// ===========

impl LabData {
    /// Computes the `hue` in degrees of the current color.
    pub fn hue(&self) -> Option<f32> {
        if self.a == 0.0 && self.b == 0.0 {
            None
        } else {
            let mut hue = self.b.atan2(self.a) * 180.0 / std::f32::consts::PI;
            if hue < 0.0 {
                hue += 360.0
            }
            Some(hue)
        }
    }
}



// ===========
// === Lch ===
// ===========

/// The maximum value of chroma in LCH space that can be displayed in sRGB color space. The value
/// uses scale used by popular color-conversion math equations, such as https://css.land/lch, or
/// http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html. Used internally for color
/// conversions.
pub(crate) const LCH_MAX_CHROMA_IN_SRGB_IN_STD_EQUATIONS: usize = 120;

#[allow(missing_docs)]
impl Lch {
    pub fn pink_hue() -> f32 {
        0.0
    } // approx.   0.0 degrees
    pub fn red_hue() -> f32 {
        0.111
    } // approx.  40.0 degrees
    pub fn orange_hue() -> f32 {
        0.18
    } // approx.  65.0 degrees
    pub fn yellow_hue() -> f32 {
        0.236
    } // approx.  85.0 degrees
    pub fn olive_hue() -> f32 {
        0.291
    } // approx. 105.0 degrees
    pub fn green_hue() -> f32 {
        0.378
    } // approx. 136.0 degrees
    pub fn blue_green_hue() -> f32 {
        0.6
    } // approx. 216.0 degrees
    pub fn blue_hue() -> f32 {
        0.672
    } // approx. 242.0 degrees
    pub fn violet_hue() -> f32 {
        0.847
    } // approx. 305.0 degrees
}

#[allow(missing_docs)]
impl Lch {
    pub fn white() -> Lch {
        Lch::new(1.0, 0.0, 0.0)
    }
    pub fn black() -> Lch {
        Lch::new(0.0, 0.0, 0.0)
    }
    pub fn pink(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::pink_hue())
    }
    pub fn red(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::red_hue())
    }
    pub fn orange(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::orange_hue())
    }
    pub fn yellow(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::yellow_hue())
    }
    pub fn olive(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::olive_hue())
    }
    pub fn green(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::green_hue())
    }
    pub fn blue_green(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::blue_green_hue())
    }
    pub fn blue(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::blue_hue())
    }
    pub fn violet(l: f32, c: f32) -> Lch {
        Lch::new(l, c, Lch::violet_hue())
    }
}

#[allow(missing_docs)]
impl Lcha {
    pub fn transparent() -> Lcha {
        Lcha::new(0.0, 0.0, 0.0, 0.0)
    }
    pub fn white() -> Lcha {
        Lch::white().into()
    }
    pub fn black() -> Lcha {
        Lch::black().into()
    }
    pub fn pink(l: f32, c: f32) -> Lcha {
        Lch::pink(l, c).into()
    }
    pub fn red(l: f32, c: f32) -> Lcha {
        Lch::red(l, c).into()
    }
    pub fn orange(l: f32, c: f32) -> Lcha {
        Lch::orange(l, c).into()
    }
    pub fn yellow(l: f32, c: f32) -> Lcha {
        Lch::yellow(l, c).into()
    }
    pub fn olive(l: f32, c: f32) -> Lcha {
        Lch::olive(l, c).into()
    }
    pub fn green(l: f32, c: f32) -> Lcha {
        Lch::green(l, c).into()
    }
    pub fn blue_green(l: f32, c: f32) -> Lcha {
        Lch::blue_green(l, c).into()
    }
    pub fn blue(l: f32, c: f32) -> Lcha {
        Lch::blue(l, c).into()
    }
    pub fn violet(l: f32, c: f32) -> Lcha {
        Lch::violet(l, c).into()
    }

    /// Convert the color to JavaScript representation.
    pub fn to_javascript_string(self) -> String {
        Rgba::from(self).to_javascript_string()
    }

    /// Convert the color to grayscale by setting chroma to zero.
    pub fn to_grayscale(mut self) -> Lcha {
        self.data.opaque.chroma = 0.0;
        self
    }
}


/// LCH color space is very limited in sRGB gammut. In the LCH color space, for the given lightness,
/// there is a maximum chroma value which allows all hue colors to exist in the sRGB color space.
/// This also means that for a given chroma, there exist maximum lightness. The values here were
/// checked manually by using the online LCH color picker https://css.land/lch. We did not found any
/// equations which allow for mathematical approximations of those, but in case you are aware of
/// such equations, you are more than welcome to improve this code.
///
/// ## WARNING
/// Please note that for convenience, the value of lightness is scaled by 100 and the value of
/// chroma is scaled by `LCH_MAX_CHROMA_IN_SRGB_IN_STD_EQUATIONS`.
///
///
/// ```text
///                                                     ••••                     ├ 40
///                                                  ••••   •                    │
///                                               ••••       •                   │
///                                            ••••           ••                 │
///                                         •••                •                 │
///                                    •••••                    ••               ├ 30
///                                 •••                           •              │     M
///                              •••                              •              │     A
///                           •••                                  ••            │     X
///                        •••                                       •           │
///                   •••••                                          ••          ├ 20  C
///               ••••                                                 •         │     H
///            ••••                                                     •        │     R
///         ••••                                                        •        │     O
///       •••                                                            ••      │     M
///      •                                                                 •     ├ 10  A
///     •                                                                   •    │
///   ••                                                                     ••  │
///   •                                                                       •  │
/// ••                                                                         ••│
/// ┬────────┬─────────┬─────────┬────────┬─────────┬─────────┬────────┬─────────┤
/// 0       12.5      25.0      37.5     50.0      62.5      75.0     75.5     100.0
///                                     LIGHTNESS
/// ```
pub const LCH_MAX_LIGHTNESS_CHROMA_IN_SRGB_CORRELATION: &[(usize, usize)] = &[
    (0, 0),
    (1, 1),
    (2, 2),
    (3, 5),
    (4, 5),
    (5, 6),
    (6, 8),
    (7, 9),
    (8, 10),
    (9, 11),
    (10, 12),
    (11, 13),
    (12, 13),
    (13, 14),
    (14, 14),
    (15, 15),
    (16, 15),
    (17, 16),
    (18, 16),
    (19, 17),
    (20, 17),
    (21, 18),
    (22, 18),
    (23, 18),
    (24, 19),
    (25, 19),
    (26, 20),
    (27, 20),
    (28, 21),
    (29, 21),
    (30, 22),
    (31, 22),
    (32, 23),
    (33, 23),
    (34, 24),
    (35, 24),
    (36, 25),
    (37, 25),
    (38, 26),
    (39, 26),
    (40, 27),
    (41, 27),
    (42, 28),
    (43, 28),
    (44, 29),
    (45, 29),
    (46, 30),
    (47, 30),
    (48, 31),
    (49, 31),
    (50, 32),
    (51, 32),
    (52, 33),
    (53, 33),
    (54, 34),
    (55, 34),
    (56, 35),
    (57, 35),
    (58, 36),
    (59, 36),
    (60, 36),
    (61, 37),
    (62, 37),
    (63, 38),
    (64, 38),
    (65, 39),
    (66, 39),
    (67, 40),
    (68, 40),
    (69, 41),
    (70, 41),
    (71, 42),
    (72, 42),
    (73, 41),
    (74, 39),
    (75, 38),
    (76, 36),
    (77, 35),
    (78, 33),
    (79, 31),
    (80, 30),
    (81, 28),
    (82, 27),
    (83, 25),
    (84, 24),
    (85, 22),
    (86, 20),
    (87, 19),
    (88, 17),
    (89, 16),
    (90, 14),
    (91, 12),
    (92, 11),
    (93, 9),
    (94, 8),
    (95, 6),
    (96, 5),
    (97, 4),
    (98, 2),
    (99, 1),
    (100, 0),
];

lazy_static! {
    /// Map from LCH lightness to max chroma, so every hue value will be included in the sRGB color
    /// space. Read docs of `LCH_MAX_LIGHTNESS_CHROMA_IN_SRGB_CORRELATION` to learn more.
    ///
    /// ## WARNING
    /// Please note that for convenience, the value of lightness is scaled by 100 and the value of
    /// chroma is scaled by `LCH_MAX_CHROMA_IN_SRGB_IN_STD_EQUATIONS`.
    pub static ref LCH_LIGHTNESS_TO_MAX_CHROMA_IN_SRGB : HashMap<usize,usize> = {
        let mut m = HashMap::new();
        for (lightness,chroma) in LCH_MAX_LIGHTNESS_CHROMA_IN_SRGB_CORRELATION {
            m.insert(*lightness,*chroma);
        }
        m
    };

    /// Map from LCH chroma to max lightness, so every hue value will be included in the sRGB color
    /// space. Read docs of `LCH_MAX_LIGHTNESS_CHROMA_IN_SRGB_CORRELATION` to learn more.
    ///
    /// ## WARNING
    /// Please note that for convenience, the value of lightness is scaled by 100 and the value of
    /// chroma is scaled by `LCH_MAX_CHROMA_IN_SRGB_IN_STD_EQUATIONS`.
    pub static ref LCH_CHROMA_TO_MAX_LIGHTNESS_IN_SRGB : HashMap<usize,usize> = {
        let mut m = HashMap::new();
        for (lightness,chroma) in LCH_MAX_LIGHTNESS_CHROMA_IN_SRGB_CORRELATION {
            m.insert(*chroma,*lightness);
        }
        m
    };
}

/// For a given LCH lightness, compute the max chroma value, so every hue value will be included in
/// the sRGB color space. Please read the docs of `LCH_MAX_LIGHTNESS_CHROMA_IN_SRGB_CORRELATION` to
/// learn more.
fn lch_lightness_to_max_chroma_in_srgb(l: f32) -> f32 {
    let l = l.clamp(0.0, 100.0);
    let l_scaled = l * 100.0;
    let l_scaled_floor = l_scaled.floor();
    let l_scaled_ceil = l_scaled.ceil();
    let coeff = (l_scaled - l_scaled_floor) / (l_scaled_ceil - l_scaled_floor);
    let l_scaled_floor_u = l_scaled_floor as usize;
    let l_scaled_ceil_u = l_scaled_ceil as usize;
    let c_scaled_floor_u = LCH_LIGHTNESS_TO_MAX_CHROMA_IN_SRGB.get(&l_scaled_floor_u);
    let c_scaled_ceil_u = LCH_LIGHTNESS_TO_MAX_CHROMA_IN_SRGB.get(&l_scaled_ceil_u);
    let c_scaled_floor = c_scaled_floor_u.copied().unwrap_or(0) as f32;
    let c_scaled_ceil = c_scaled_ceil_u.copied().unwrap_or(0) as f32;
    let c_scaled = c_scaled_floor + (c_scaled_ceil - c_scaled_floor) * coeff;
    c_scaled / LCH_MAX_CHROMA_IN_SRGB_IN_STD_EQUATIONS as f32
}



// ===============
// === Parsing ===
// ===============

/// String to color parse error.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ParseError {
    pub reason: String,
}

impl ParseError {
    /// Constructor.
    pub fn new(reason: impl Into<String>) -> Self {
        let reason = reason.into();
        Self { reason }
    }
}

impl From<std::num::ParseFloatError> for ParseError {
    fn from(_: std::num::ParseFloatError) -> Self {
        ParseError::new("Improper numeric argument.")
    }
}

fn uppercase_first_letter(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

/// Consume the input string and return the header and list of args. For example, for the input
/// `rgba(1.0,0.0,0.0,0.5)`, the header will be `"rgba"`, and the nubers will be arguments.
fn generic_parse(s: &str) -> Result<(String, Vec<f32>), ParseError> {
    let mut splitter = s.splitn(2, '(');
    match splitter.next() {
        None => Err(ParseError::new("Empty input.")),
        Some(head) => match splitter.next() {
            None => Err(ParseError::new("No arguments provided.")),
            Some(rest) => {
                let head = uppercase_first_letter(&head.to_lowercase());
                if !rest.ends_with(')') {
                    Err(ParseError::new("Expression does not end with ')'."))
                } else {
                    let rest = &rest[..rest.len() - 1];
                    let args: Result<Vec<f32>, std::num::ParseFloatError> =
                        rest.split(',').map(|t| t.parse::<f32>()).collect();
                    let args = args?;
                    Ok((head, args))
                }
            }
        },
    }
}
