//! Definition of style sheet values.

use crate::prelude::*;

use crate::data::color;



// ============
// === Data ===
// ============

/// Type of values in the style sheet.
#[derive(Debug,Clone,PartialEq)]
#[allow(missing_docs)]
pub enum Data {
    Invalid(String),
    Number(f32),
    Rgba(color::LinSrgba),
}


// === Constructors ===

/// Smart constructor for `Data`.
pub fn data<T:Into<Data>>(t:T) -> Data {
    t.into()
}

impl From<f32> for Data {
    fn from(t:f32) -> Data {
        Data::Number(t)
    }
}


// === Impls ===

//impl Eq for Data {}

impl Display for Data {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid(s) => write!(f,"{}",s),
            Self::Number(t)  => write!(f,"Number({})",t),
            Self::Rgba(t)    => write!(f,"Color({:?})",t),
        }
    }
}


impl Data {
    /// Lighten the color by `amount`.
    pub fn lighten(&self, amount:f32) -> Self {
        match self {
            Data::Invalid(s) => Data::Invalid(s.clone()),
            Data::Rgba(t) => Data::Rgba(palette::Shade::lighten(t,amount)),
            this => Data::Invalid(format!("Cannot use method lighten on {}.",this))
        }
    }
}


// === Color Getters ===

macro_rules! define_color_getter {
    ($($name:ident),*) => {$(
        impl Data {
            /// Component getter.
            pub fn $name(&self) -> Data {
                match self {
                    Data::Invalid(s) => Data::Invalid(s.clone()),
                    Data::Rgba(t)    => Data::Number(t.$name),
                    this             => Data::Invalid (format!
                        (concat!("Cannot access ",stringify!($name)," property of {}."),this))
                }
            }
        }
    )*};
}

define_color_getter!(red,green,blue,alpha);



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

define_binary_number_operator!(Mul::mul,|lhs,rhs| format!("Cannot multiply {} by {}.",lhs,rhs));
define_binary_number_operator!(Div::div,|lhs,rhs| format!("Cannot divide {} by {}.",lhs,rhs));
define_binary_number_operator!(Add::add,|lhs,rhs| format!("Cannot add {} to {}.",lhs,rhs));
define_binary_number_operator!(Sub::sub,|lhs,rhs| format!("Cannot subtract {} from {}.",rhs,lhs));
