//! Color mixing utilities.

use super::*;
use crate::prelude::*;

use crate::data::mix;
use crate::data::mix::Mixable;

pub use crate::data::mix::mix;



// =================
// === Color Mix ===
// =================

/// Defines a set of impls, in the form of:
///
/// ```ignore
/// impl Mixable for Lch { type Repr = Vector3; }
/// impl From<Lch> for mix::Space<Lch> {
///     fn from(value: Lch) -> mix::Space<Lch> {
///         mix::Space::new(<Lab>::from(value).into())
///     }
/// }
/// ```
macro_rules! define_mix_impls {
    ($($tp:ident => $via_tp:ident;)*) => {$(
        define_mix_impl_repr! {$tp                      => $via_tp                      [Vector3]}
        define_mix_impl_repr! {Color<Alpha<Model<$tp>>> => Color<Alpha<Model<$via_tp>>> [Vector4]}
    )*}
}

macro_rules! define_mix_impl_repr {
    ($tp:ty => $via_tp:ty [$repr:ident]) => {
        impl Mixable for $tp {
            type Repr = $repr;
        }

        impl From<$tp> for mix::Space<$tp> {
            fn from(value: $tp) -> mix::Space<$tp> {
                mix::Space::new(<$via_tp>::from(value).into())
            }
        }

        impl From<mix::Space<$tp>> for $tp {
            fn from(t: mix::Space<$tp>) -> Self {
                <$via_tp>::from(t.value).into()
            }
        }
    };
}


// === Impls ===

define_mix_impls! {
    Lab => Lab;
    Lch => Lab;
    Rgb => LinearRgb;
}
