//! Definition of style sheet values.

use crate::prelude::*;

use crate::data::color;



// ============
// === Data ===
// ============

/// Type of values in the style sheet.
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
pub enum Data {
    Invalid(String),
    Number(f32),
    Color(color::Rgba),
    Text(String),
}


// === Constructors ===

impl Data {
    /// Parse a [`Data`] value encoded in a string.
    ///
    /// The contents of the string are parsed by the following rules:
    ///  - strings successfully parsed by [`f32::from_str`] result in a [`Number`] value;
    ///  - strings successfully parsed by [`color::AnyFormat::from_str`] result in a [`Color`]
    ///    value;
    ///  - strings starting and ending with a double-quote character (`"`) result in a [`Text`]
    ///    value (with the surrounding double-quotes stripped);
    ///  - other strings result in `None`.
    /// See below for some examples:
    /// ```
    /// # use ensogl_core::data::color;
    /// # use ensogl_core::display::style::data::*;
    /// assert_eq!(Data::parse("123.4"), Some(Data::Number(123.4)));
    /// let red = color::Rgba(1.0, 0.0, 0.0, 1.0);
    /// assert_eq!(Data::parse("rgba(1.0,0.0,0.0,1.0)"), Some(Data::Color(red)));
    /// assert_eq!(Data::parse("\"some string\""), Some(Data::Text("some string".to_string())));
    /// assert_eq!(Data::parse("bad-format"), None);
    /// ```
    pub fn parse(s: &str) -> Option<Data> {
        if let Ok(t) = s.parse::<f32>() {
            return Some(Data::Number(t));
        }
        if let Ok(t) = s.parse::<color::AnyFormat>() {
            return Some(Data::Color(t.into()));
        }
        if s.starts_with('"') && s.ends_with('"') {
            return Some(Data::Text(s[1..s.len() - 1].to_string()));
        }
        None
    }
}

/// Smart constructor for `Data`.
pub fn data<T: Into<Data>>(t: T) -> Data {
    t.into()
}

impl From<f32> for Data {
    fn from(t: f32) -> Data {
        Data::Number(t)
    }
}

impl From<i32> for Data {
    fn from(t: i32) -> Data {
        Data::Number(t as f32)
    }
}

impl<C> From<color::Color<C>> for Data
where color::Color<C>: Into<color::Rgba>
{
    fn from(color: color::Color<C>) -> Data {
        Data::Color(color.into())
    }
}

/// A conversion from a string slice to [`Data`]. Needed to allow entering text literals as values
/// in the hard-coded themes (see the `define_themes` macro in the `ensogl_hardcoded_theme` crate).
impl From<&str> for Data {
    fn from(t: &str) -> Data {
        Data::Text(t.to_owned())
    }
}


// === Impls ===

impl Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid(s) => write!(f, "{}", s),
            Self::Number(t) => write!(f, "Number({})", t),
            Self::Color(t) => write!(f, "Color({:?})", t),
            Self::Text(t) => write!(f, "Text({:?})", t),
        }
    }
}


// FIXME: Make use of this macro and allow themes to modify colors, including:
// lighten,darken,saturate,desaturate,with_hue,shift_hue, etc.
macro_rules! _define_color_transform {
    ($($name:ident),*) => {$(
        impl Data {
            /// Transform the color: $name.
            pub fn $name(&self, amount:Data) -> Self {
                match (self,amount) {
                    (Data::Invalid(s) , _)                => Data::Invalid(s.clone()),
                    (_                , Data::Invalid(s)) => Data::Invalid(s.clone()),
                    (Data::Color(t)   , Data::Number(f))  => Data::Color(t.$name(f)),
                    (this             , t)                => Data::Invalid
                        (format!(concat!("Cannot apply",stringify!($name),"({}) to {}."),t,this))
                }
            }
        }
    )*};
}

// define_color_transform!(lighten,darken,saturate,desaturate,with_hue,shift_hue);


// === Color Getters ===

macro_rules! define_color_getter {
    ($($space:ident :: $name:ident),*) => {$(
        impl Data {
            /// Component getter.
            pub fn $name(&self) -> Data {
                match self {
                    Data::Invalid(s) => Data::Invalid(s.clone()),
                    Data::Color(t)   => Data::Number(color::$space::from(*t).$name),
                    this             => Data::Invalid (format!
                        (concat!("Cannot access ",stringify!($name)," property of {}."),this))
                }
            }
        }
    )*};
}

define_color_getter!(Lcha::alpha);
define_color_getter!(LinearRgba::red, LinearRgba::green, LinearRgba::blue);


// === Operators ===

macro_rules! define_binary_number_operator {
    ($($toks:tt)*) => {
        _define_binary_number_operator! { [&Data] [&Data] $($toks)* }
        _define_binary_number_operator! { [ Data] [&Data] $($toks)* }
        _define_binary_number_operator! { [&Data] [ Data] $($toks)* }
        _define_binary_number_operator! { [ Data] [ Data] $($toks)* }
    };
}

macro_rules! _define_binary_number_operator {
    ([$($t1:tt)*] [$($t2:tt)*] $name:ident :: $fn:ident, $($err:tt)*) => {
        impl $name<$($t2)*> for $($t1)* {
            type Output = Data;
            #[allow(clippy::redundant_closure_call)]
            fn $fn(self, rhs:$($t2)*) -> Self::Output {
                match(self,rhs) {
                    (Data::Invalid(t),_) => Data::Invalid(t.clone()),
                    (_,Data::Invalid(t)) => Data::Invalid(t.clone()),
                    (Data::Number(lhs),Data::Number(rhs)) => Data::Number(lhs.$fn(rhs)),
                    (lhs,rhs) => Data::Invalid(($($err)*)(lhs,rhs))
                }
            }
        }
    };
}

define_binary_number_operator!(Mul::mul, |lhs, rhs| format!("Cannot multiply {} by {}.", lhs, rhs));
define_binary_number_operator!(Div::div, |lhs, rhs| format!("Cannot divide {} by {}.", lhs, rhs));
define_binary_number_operator!(Add::add, |lhs, rhs| format!("Cannot add {} to {}.", lhs, rhs));
define_binary_number_operator!(Sub::sub, |lhs, rhs| format!(
    "Cannot subtract {} from {}.",
    rhs, lhs
));



// =================
// === DataMatch ===
// =================

/// Smart `Data` deconstructors.
#[allow(missing_docs)]
pub trait DataMatch {
    fn invalid(&self) -> Option<&String>;
    fn number(&self) -> Option<f32>;
    fn color(&self) -> Option<color::Rgba>;
    fn text(&self) -> Option<String>;

    fn number_or_else(&self, f: impl FnOnce() -> f32) -> f32 {
        self.number().unwrap_or_else(f)
    }

    fn color_or_else(&self, f: impl FnOnce() -> color::Rgba) -> color::Rgba {
        self.color().unwrap_or_else(f)
    }

    fn text_or_else(&self, f: impl FnOnce() -> String) -> String {
        self.text().unwrap_or_else(f)
    }
}

impl DataMatch for Data {
    fn invalid(&self) -> Option<&String> {
        match self {
            Self::Invalid(t) => Some(t),
            _ => None,
        }
    }
    fn number(&self) -> Option<f32> {
        match self {
            Self::Number(t) => Some(*t),
            _ => None,
        }
    }
    fn color(&self) -> Option<color::Rgba> {
        match self {
            Self::Color(t) => Some(*t),
            _ => None,
        }
    }
    fn text(&self) -> Option<String> {
        match self {
            Self::Text(t) => Some(t.clone()),
            _ => None,
        }
    }
}

impl DataMatch for Option<Data> {
    fn invalid(&self) -> Option<&String> {
        self.as_ref().and_then(|t| t.invalid())
    }
    fn number(&self) -> Option<f32> {
        self.as_ref().and_then(|t| t.number())
    }
    fn color(&self) -> Option<color::Rgba> {
        self.as_ref().and_then(|t| t.color())
    }
    fn text(&self) -> Option<String> {
        self.as_ref().and_then(|t| t.text())
    }
}
