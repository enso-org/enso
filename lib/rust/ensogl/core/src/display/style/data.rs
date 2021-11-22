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
}


// === Constructors ===

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

impl TryFrom<String> for Data {
    type Error = ();
    fn try_from(s: String) -> Result<Self, Self::Error> {
        match s.parse::<f32>() {
            Ok(t) => Ok(Data::Number(t)),
            _ => match s.parse::<color::AnyFormat>() {
                Ok(t) => Ok(Data::Color(t.into())),
                _ => Err(()),
            },
        }
    }
}


// === Impls ===

impl Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid(s) => write!(f, "{}", s),
            Self::Number(t) => write!(f, "Number({})", t),
            Self::Color(t) => write!(f, "Color({:?})", t),
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

    fn number_or_else(&self, f: impl FnOnce() -> f32) -> f32 {
        self.number().unwrap_or_else(f)
    }

    fn color_or_else(&self, f: impl FnOnce() -> color::Rgba) -> color::Rgba {
        self.color().unwrap_or_else(f)
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
}
