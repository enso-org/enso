//! Abstraction for values that can be mixed together. For numbers this is just a simple linear
//! interpolation. For other types it can get more complex. For example, mixing colors requires
//! to convert them to a linear space first, do the interpolation and then performing a conversion
//! back.

use crate::prelude::*;



// =============
// === Space ===
// =============

/// Strongly typed value representation in the mix space.
#[allow(missing_docs)]
pub struct Space<T: Mixable> {
    pub value: Repr<T>,
}

impl<T: Mixable> Space<T> {
    /// Constructor.
    pub fn new(value: Repr<T>) -> Self {
        Self { value }
    }
}

impl<T: Mixable> Debug for Space<T>
where
    T: Mixable,
    Repr<T>: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Space({:?})", self.value)
    }
}



// ===============
// === Mixable ===
// ===============

/// Type association between a value and its representation used for mixing. For example, for colors
/// in sRGB space, the representation may be RGB in the linear color space.
#[allow(missing_docs)]
pub trait Mixable: BiInto<Space<Self>> {
    type Repr: Value;
}

/// Mixable::Repr getter.
pub type Repr<T> = <T as Mixable>::Repr;

/// Trait for values that can be mixed.
pub trait Value = Sized + Mul<f32, Output = Self> + Add<Output = Self>;


// === Utils ===

/// Convert the mix space representation to corresponding value.
pub fn from_space<T: Mixable>(value: Repr<T>) -> T {
    Space { value }.into()
}

/// Convert value to corresponding mix space representation.
pub fn into_space<T: Mixable>(t: T) -> Repr<T> {
    t.into().value
}

/// Perform a mix of two values. See module docs to learn more.
pub fn mix<T: Mixable>(t1: T, t2: T, coefficient: f32) -> T {
    let v1 = into_space(t1);
    let v2 = into_space(t2);
    let v = v1 * (1.0 - coefficient) + v2 * coefficient;
    from_space(v)
}



// =============
// === Impls ===
// =============

/// Macro for defining `Mixable` impls for types whose mix space is the same as the types
/// themselves.
macro_rules! define_self_mixables {
    ($($type:ty),*) => {$(
        impl Mixable for $type {
            type Repr = $type;
        }

        impl From<$type> for Space<$type> {
            fn from(value:$type) -> Self {
                 Space{value}
            }
        }

        impl From<Space<$type>> for $type {
            fn from(value:Space<$type>) -> Self {
                value.value
            }
        }
    )*}
}

define_self_mixables!(f32, Vector2, Vector3, Vector4);
