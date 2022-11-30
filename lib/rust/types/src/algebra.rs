//! This module gathers common math types which are widely used in this project.

// === Non-Standard Linter Configuration ===
#![allow(non_snake_case)]

use nalgebra;
use nalgebra::storage::Storage;
use nalgebra::ComplexField;
use nalgebra::Dim;
use nalgebra::Matrix;
use nalgebra::Scalar;


// ==============
// === Export ===
// ==============

pub use crate::dim::*;
pub use nalgebra::Matrix2;
pub use nalgebra::Matrix2x3;
pub use nalgebra::Matrix2x4;
pub use nalgebra::Matrix3;
pub use nalgebra::Matrix3x2;
pub use nalgebra::Matrix3x4;
pub use nalgebra::Matrix4;
pub use nalgebra::Matrix4x2;
pub use nalgebra::Matrix4x3;
pub use nalgebra::OMatrix;
pub use std::ops::Add;
pub use std::ops::Div;
pub use std::ops::Mul;
pub use std::ops::Neg;
pub use std::ops::Sub;



// ==========================
// === Smart Constructors ===
// ==========================

#[allow(missing_docs)]
mod vectors {
    use super::*;

    pub type Vector2<T = f32> = nalgebra::Vector2<T>;
    pub type Vector3<T = f32> = nalgebra::Vector3<T>;
    pub type Vector4<T = f32> = nalgebra::Vector4<T>;

    pub type Rotation2<T = f32> = nalgebra::Rotation2<T>;
    pub type Rotation3<T = f32> = nalgebra::Rotation3<T>;

    pub const fn Vector2<T: Scalar>(t1: T, t2: T) -> Vector2<T> {
        Vector2::new(t1, t2)
    }
    pub fn Vector3<T: Scalar>(t1: T, t2: T, t3: T) -> Vector3<T> {
        Vector3::new(t1, t2, t3)
    }
    pub fn Vector4<T: Scalar>(t1: T, t2: T, t3: T, t4: T) -> Vector4<T> {
        Vector4::new(t1, t2, t3, t4)
    }
}
pub use vectors::*;



// ==============
// === Traits ===
// ==============

/// Describes types that have a zero value.
pub trait Zero {
    /// A zero value of this type.
    fn zero() -> Self;
}

/// Smart constructor for the `Zero` trait.
pub fn zero<T: Zero>() -> T {
    <T as Zero>::zero()
}


// === Impls ===

macro_rules! gen_zero {
    ([$($ty:ident),*] = $value:expr) => {$(
        impl Zero for $ty {
            fn zero() -> Self {
                $value
            }
        }
    )*};
}

macro_rules! gen_zero_nalgebra {
    ([$($ty:ident),*]) => {$(
        impl<T:Scalar> Zero for $ty<T>
        where $ty<T> : num_traits::Zero {
            fn zero() -> Self {
                nalgebra::zero()
            }
        }
    )*};
}

gen_zero!([f32, f64] = 0.0);
gen_zero!([i32, i64, usize] = 0);
gen_zero_nalgebra!([
    Vector2, Vector3, Vector4, Matrix2, Matrix3, Matrix4, Matrix2x3, Matrix2x4, Matrix3x2,
    Matrix3x4, Matrix4x2, Matrix4x3
]);



// ===========
// === Abs ===
// ===========

/// Types which have an absolute value.
pub trait Abs {
    /// Absolute value.
    fn abs(&self) -> Self;
}

impl Abs for usize {
    fn abs(&self) -> Self {
        *self
    }
}


// === Impls ===

macro_rules! gen_abs {
    ([$($ty:ident),*]) => {$(
        impl Abs for $ty {
            fn abs(&self) -> Self {
                if *self < Self::zero() { -self } else { *self }
            }
        }
    )*};
}

gen_abs!([f32, f64, i32, i64]);



// ===========
// === Min ===
// ===========

/// Types where minimum of the values can be found.
pub trait Min {
    /// Lesser of the two values.
    fn min(a: Self, b: Self) -> Self;
}


// === Impls ===

macro_rules! gen_min {
    ([$($ty:ident),*]) => {$(
        impl Min for $ty {
            fn min(a:Self, b:Self) -> Self {
                min(a,b)
            }
        }
    )*};
}

gen_min!([f32, f64, i32, i64, usize]);



// ===========
// === Max ===
// ===========

/// Types where maximum of the values can be found.
pub trait Max {
    /// Greater of the two values.
    fn max(a: Self, b: Self) -> Self;
}


// === Impls ===

macro_rules! gen_max {
    ([$($ty:ident),*]) => {$(
        impl Max for $ty {
            fn max(a:Self, b:Self) -> Self {
                max(a,b)
            }
        }
    )*};
}

gen_max!([f32, f64, i32, i64, usize]);



// ===========
// === Pow ===
// ===========

/// Types which can be raised to the given power.
#[allow(missing_docs)]
pub trait Pow<T = Self> {
    type Output;
    fn pow(self, t: T) -> Self::Output;
}

impl Pow<f32> for f32 {
    type Output = f32;
    fn pow(self, t: f32) -> Self::Output {
        self.powf(t)
    }
}



// =================
// === Magnitude ===
// =================

/// Types which have magnitude value.
#[allow(missing_docs)]
pub trait Magnitude {
    type Output;
    fn magnitude(&self) -> Self::Output;
}


// === Impls ===

impl Magnitude for f32 {
    type Output = f32;
    fn magnitude(&self) -> Self::Output {
        self.abs()
    }
}

impl<N: ComplexField, R: Dim, C: Dim, S: Storage<N, R, C>> Magnitude for Matrix<N, R, C, S> {
    type Output = N::RealField;
    fn magnitude(&self) -> Self::Output {
        self.norm()
    }
}



// ==============
// === Signum ===
// ==============

/// Computes the signum of the value. Returns +1 if its positive, -1 if its negative, 0 if its zero.
/// It can also return other values for specific types like `NaN` for `NaN`.
#[allow(missing_docs)]
pub trait Signum {
    type Output;
    fn signum(self) -> Self::Output;
}


// === Impls ===

impl Signum for f32 {
    type Output = f32;
    fn signum(self) -> f32 {
        f32::signum(self)
    }
}



// =============
// === Clamp ===
// =============

/// Clamps the value to [min..max] range.
#[allow(missing_docs)]
pub trait Clamp {
    type Output;
    fn clamp(self, min: Self, max: Self) -> Self::Output;
}


// === Impls ===

impl Clamp for f32 {
    type Output = f32;
    fn clamp(self, min: f32, max: f32) -> f32 {
        self.clamp(min, max)
    }
}



// =================
// === Min / Max ===
// =================

/// Compares and returns the minimum of two values.
pub fn min<T: PartialOrd>(a: T, b: T) -> T {
    if b < a {
        b
    } else {
        a
    }
}

/// Compares and returns the maximum of two values.
pub fn max<T: PartialOrd>(a: T, b: T) -> T {
    if b > a {
        b
    } else {
        a
    }
}



// =================
// === Normalize ===
// =================

/// Types which can be normalized.
#[allow(missing_docs)]
pub trait Normalize {
    fn normalize(&self) -> Self;
}


// === Impls ===

impl Normalize for f32 {
    fn normalize(&self) -> f32 {
        self.signum()
    }
}

impl Normalize for Vector2<f32> {
    fn normalize(&self) -> Self {
        self.normalize()
    }
}

impl Normalize for Vector3<f32> {
    fn normalize(&self) -> Self {
        self.normalize()
    }
}

impl Normalize for Vector4<f32> {
    fn normalize(&self) -> Self {
        self.normalize()
    }
}



// ===================
// === Square Root ===
// ===================

/// Types from which a square root can be calculated.
#[allow(missing_docs)]
pub trait Sqrt {
    type Output;
    fn sqrt(&self) -> Self::Output;
}


// === Impls ===

impl Sqrt for f32 {
    type Output = f32;
    fn sqrt(&self) -> f32 {
        f32::sqrt(*self)
    }
}



// ===========
// === Cos ===
// ===========

/// Types from which a cosine can be calculated.
#[allow(missing_docs)]
pub trait Cos {
    type Output;
    fn cos(&self) -> Self;
}


// === Impls ===

impl Cos for f32 {
    type Output = f32;
    fn cos(&self) -> f32 {
        f32::cos(*self)
    }
}



// ===========
// === Sin ===
// ===========

/// Types from which a sine can be calculated
#[allow(missing_docs)]
pub trait Sin {
    type Output;
    fn sin(&self) -> Self::Output;
}


// === Impls ===

impl Sin for f32 {
    type Output = f32;
    fn sin(&self) -> f32 {
        f32::sin(*self)
    }
}



// ============
// === Asin ===
// ============

/// Types from which a asin can be calculated
#[allow(missing_docs)]
pub trait Asin {
    type Output;
    fn asin(&self) -> Self::Output;
}


// === Impls ===

impl Asin for f32 {
    type Output = f32;
    fn asin(&self) -> f32 {
        f32::asin(*self)
    }
}



// ============
// === Acos ===
// ============

/// Types from which a asin can be calculated
#[allow(missing_docs)]
pub trait Acos {
    type Output;
    fn acos(&self) -> Self::Output;
}


// === Impls ===

impl Acos for f32 {
    type Output = f32;
    fn acos(&self) -> f32 {
        f32::acos(*self)
    }
}



// =============================
// === Saturating Operations ===
// =============================

/// Saturating addition. Computes self + rhs, saturating at the numeric bounds instead of
/// overflowing.
#[const_trait]
#[allow(missing_docs)]
pub trait SaturatingAdd<Rhs = Self> {
    type Output;
    fn saturating_add(self, rhs: Rhs) -> Self::Output;
}

/// Saturating subtraction. Computes self - rhs, saturating at the numeric bounds instead of
/// overflowing.
#[allow(missing_docs)]
#[const_trait]
pub trait SaturatingSub<Rhs = Self> {
    type Output;
    fn saturating_sub(self, rhs: Rhs) -> Self::Output;
}

/// Saturating multiplication. Computes self * rhs, saturating at the numeric bounds instead of
/// overflowing.
#[allow(missing_docs)]
#[const_trait]
pub trait SaturatingMul<Rhs = Self> {
    type Output;
    fn saturating_mul(self, rhs: Rhs) -> Self::Output;
}

/// Saturating power. Computes self ^ exp, saturating at the numeric bounds instead of overflowing.
#[allow(missing_docs)]
#[const_trait]
pub trait SaturatingPow {
    type Output;
    fn saturating_pow(self, exp: u32) -> Self::Output;
}


// === Impls ===

macro_rules! impl_saturating_opr {
    ($name:ident :: $opr:ident for $tgt:ident) => {
        impl $name<$tgt> for $tgt {
            type Output = $tgt;
            fn $opr(self, rhs: $tgt) -> Self::Output {
                self.$opr(rhs)
            }
        }

        impl $name<$tgt> for &$tgt {
            type Output = $tgt;
            fn $opr(self, rhs: $tgt) -> Self::Output {
                (*self).$opr(rhs)
            }
        }

        impl $name<&$tgt> for $tgt {
            type Output = $tgt;
            fn $opr(self, rhs: &$tgt) -> Self::Output {
                self.$opr(*rhs)
            }
        }

        impl $name<&$tgt> for &$tgt {
            type Output = $tgt;
            fn $opr(self, rhs: &$tgt) -> Self::Output {
                (*self).$opr(*rhs)
            }
        }
    };
}

macro_rules! impl_saturating_integer {
    ($($name:ident),* $(,)?) => {
        $(impl_saturating_opr! {SaturatingAdd::saturating_add for $name})*
        $(impl_saturating_opr! {SaturatingSub::saturating_sub for $name})*
        $(impl_saturating_opr! {SaturatingMul::saturating_mul for $name})*
    }
}

impl_saturating_integer!(u8, u16, u32, u64, u128, usize);
