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
#[allow(missing_docs)]
pub trait SaturatingAdd<Rhs = Self> {
    type Output;
    fn saturating_add(self, rhs: Rhs) -> Self::Output;
}

/// Saturating subtraction. Computes self - rhs, saturating at the numeric bounds instead of
/// overflowing.
#[allow(missing_docs)]
pub trait SaturatingSub<Rhs = Self> {
    type Output;
    fn saturating_sub(self, rhs: Rhs) -> Self::Output;
}

/// Saturating multiplication. Computes self * rhs, saturating at the numeric bounds instead of
/// overflowing.
#[allow(missing_docs)]
pub trait SaturatingMul<Rhs = Self> {
    type Output;
    fn saturating_mul(self, rhs: Rhs) -> Self::Output;
}

/// Saturating power. Computes self ^ exp, saturating at the numeric bounds instead of overflowing.
#[allow(missing_docs)]
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



// ==================
// === Dimensions ===
// ==================

/// Component accessors and swizzling for 1-dimensional types.
#[allow(missing_docs)]
pub trait Dim1 {
    /// The type of 1-dimensional projection of this type. For example, for `Vector4<f32>` this is
    /// `f32`.
    type Dim1Type;
    fn x(&self) -> Self::Dim1Type;
}

/// Component accessors and swizzling for 2-dimensional types.
#[allow(missing_docs)]
pub trait Dim2: Dim1 {
    /// The type of 2-dimensional projection of this type. For example, for `Vector4<f32>` this is
    /// `Vector2<f32>`.
    type Dim2Type;
    fn y(&self) -> Self::Dim1Type;
    fn xx(&self) -> Self::Dim2Type;
    fn xy(&self) -> Self::Dim2Type;
    fn yy(&self) -> Self::Dim2Type;
    fn yx(&self) -> Self::Dim2Type;
}

/// Component accessors and swizzling for 3-dimensional types.
#[allow(missing_docs)]
pub trait Dim3: Dim2 {
    /// The type of 3-dimensional projection of this type. For example, for `Vector4<f32>` this is
    /// `Vector3<f32>`.
    type Dim3Type;
    fn z(&self) -> Self::Dim1Type;
    fn zz(&self) -> Self::Dim2Type;
    fn xz(&self) -> Self::Dim2Type;
    fn zx(&self) -> Self::Dim2Type;
    fn yz(&self) -> Self::Dim2Type;
    fn zy(&self) -> Self::Dim2Type;

    fn xxx(&self) -> Self::Dim3Type;
    fn xxy(&self) -> Self::Dim3Type;
    fn xxz(&self) -> Self::Dim3Type;
    fn xyx(&self) -> Self::Dim3Type;
    fn xyy(&self) -> Self::Dim3Type;
    fn xyz(&self) -> Self::Dim3Type;
    fn xzx(&self) -> Self::Dim3Type;
    fn xzy(&self) -> Self::Dim3Type;
    fn xzz(&self) -> Self::Dim3Type;
    fn yxx(&self) -> Self::Dim3Type;
    fn yxy(&self) -> Self::Dim3Type;
    fn yxz(&self) -> Self::Dim3Type;
    fn yyx(&self) -> Self::Dim3Type;
    fn yyy(&self) -> Self::Dim3Type;
    fn yyz(&self) -> Self::Dim3Type;
    fn yzx(&self) -> Self::Dim3Type;
    fn yzy(&self) -> Self::Dim3Type;
    fn yzz(&self) -> Self::Dim3Type;
    fn zxx(&self) -> Self::Dim3Type;
    fn zxy(&self) -> Self::Dim3Type;
    fn zxz(&self) -> Self::Dim3Type;
    fn zyx(&self) -> Self::Dim3Type;
    fn zyy(&self) -> Self::Dim3Type;
    fn zyz(&self) -> Self::Dim3Type;
    fn zzx(&self) -> Self::Dim3Type;
    fn zzy(&self) -> Self::Dim3Type;
    fn zzz(&self) -> Self::Dim3Type;
}

/// Component accessors and swizzling for 4-dimensional types.
#[allow(missing_docs)]
pub trait Dim4: Dim3 {
    /// The type of 4-dimensional projection of this type. For example, for `Vector4<f32>` this is
    /// `Vector4<f32>`.
    type Dim4Type;
    fn w(&self) -> Self::Dim1Type;
    fn ww(&self) -> Self::Dim2Type;
    fn xw(&self) -> Self::Dim2Type;
    fn wx(&self) -> Self::Dim2Type;
    fn yw(&self) -> Self::Dim2Type;
    fn wy(&self) -> Self::Dim2Type;
    fn zw(&self) -> Self::Dim2Type;
    fn wz(&self) -> Self::Dim2Type;

    fn xxw(&self) -> Self::Dim3Type;
    fn xyw(&self) -> Self::Dim3Type;
    fn xzw(&self) -> Self::Dim3Type;
    fn yxw(&self) -> Self::Dim3Type;
    fn yyw(&self) -> Self::Dim3Type;
    fn yzw(&self) -> Self::Dim3Type;
    fn zxw(&self) -> Self::Dim3Type;
    fn zyw(&self) -> Self::Dim3Type;
    fn zzw(&self) -> Self::Dim3Type;
    fn xwx(&self) -> Self::Dim3Type;
    fn xwy(&self) -> Self::Dim3Type;
    fn xwz(&self) -> Self::Dim3Type;
    fn ywx(&self) -> Self::Dim3Type;
    fn ywy(&self) -> Self::Dim3Type;
    fn ywz(&self) -> Self::Dim3Type;
    fn zwx(&self) -> Self::Dim3Type;
    fn zwy(&self) -> Self::Dim3Type;
    fn zwz(&self) -> Self::Dim3Type;
    fn wxx(&self) -> Self::Dim3Type;
    fn wxy(&self) -> Self::Dim3Type;
    fn wxz(&self) -> Self::Dim3Type;
    fn wyx(&self) -> Self::Dim3Type;
    fn wyy(&self) -> Self::Dim3Type;
    fn wyz(&self) -> Self::Dim3Type;
    fn wzx(&self) -> Self::Dim3Type;
    fn wzy(&self) -> Self::Dim3Type;
    fn wzz(&self) -> Self::Dim3Type;

    fn xxxx(&self) -> Self::Dim4Type;
    fn xxxy(&self) -> Self::Dim4Type;
    fn xxxz(&self) -> Self::Dim4Type;
    fn xxxw(&self) -> Self::Dim4Type;
    fn xxyx(&self) -> Self::Dim4Type;
    fn xxyy(&self) -> Self::Dim4Type;
    fn xxyz(&self) -> Self::Dim4Type;
    fn xxyw(&self) -> Self::Dim4Type;
    fn xxzx(&self) -> Self::Dim4Type;
    fn xxzy(&self) -> Self::Dim4Type;
    fn xxzz(&self) -> Self::Dim4Type;
    fn xxzw(&self) -> Self::Dim4Type;
    fn xxwx(&self) -> Self::Dim4Type;
    fn xxwy(&self) -> Self::Dim4Type;
    fn xxwz(&self) -> Self::Dim4Type;
    fn xxww(&self) -> Self::Dim4Type;
    fn xyxx(&self) -> Self::Dim4Type;
    fn xyxy(&self) -> Self::Dim4Type;
    fn xyxz(&self) -> Self::Dim4Type;
    fn xyxw(&self) -> Self::Dim4Type;
    fn xyyx(&self) -> Self::Dim4Type;
    fn xyyy(&self) -> Self::Dim4Type;
    fn xyyz(&self) -> Self::Dim4Type;
    fn xyyw(&self) -> Self::Dim4Type;
    fn xyzx(&self) -> Self::Dim4Type;
    fn xyzy(&self) -> Self::Dim4Type;
    fn xyzz(&self) -> Self::Dim4Type;
    fn xyzw(&self) -> Self::Dim4Type;
    fn xywx(&self) -> Self::Dim4Type;
    fn xywy(&self) -> Self::Dim4Type;
    fn xywz(&self) -> Self::Dim4Type;
    fn xyww(&self) -> Self::Dim4Type;
    fn xzxx(&self) -> Self::Dim4Type;
    fn xzxy(&self) -> Self::Dim4Type;
    fn xzxz(&self) -> Self::Dim4Type;
    fn xzxw(&self) -> Self::Dim4Type;
    fn xzyx(&self) -> Self::Dim4Type;
    fn xzyy(&self) -> Self::Dim4Type;
    fn xzyz(&self) -> Self::Dim4Type;
    fn xzyw(&self) -> Self::Dim4Type;
    fn xzzx(&self) -> Self::Dim4Type;
    fn xzzy(&self) -> Self::Dim4Type;
    fn xzzz(&self) -> Self::Dim4Type;
    fn xzzw(&self) -> Self::Dim4Type;
    fn xzwx(&self) -> Self::Dim4Type;
    fn xzwy(&self) -> Self::Dim4Type;
    fn xzwz(&self) -> Self::Dim4Type;
    fn xzww(&self) -> Self::Dim4Type;
    fn xwxx(&self) -> Self::Dim4Type;
    fn xwxy(&self) -> Self::Dim4Type;
    fn xwxz(&self) -> Self::Dim4Type;
    fn xwxw(&self) -> Self::Dim4Type;
    fn xwyx(&self) -> Self::Dim4Type;
    fn xwyy(&self) -> Self::Dim4Type;
    fn xwyz(&self) -> Self::Dim4Type;
    fn xwyw(&self) -> Self::Dim4Type;
    fn xwzx(&self) -> Self::Dim4Type;
    fn xwzy(&self) -> Self::Dim4Type;
    fn xwzz(&self) -> Self::Dim4Type;
    fn xwzw(&self) -> Self::Dim4Type;
    fn xwwx(&self) -> Self::Dim4Type;
    fn xwwy(&self) -> Self::Dim4Type;
    fn xwwz(&self) -> Self::Dim4Type;
    fn xwww(&self) -> Self::Dim4Type;
    fn yxxx(&self) -> Self::Dim4Type;
    fn yxxy(&self) -> Self::Dim4Type;
    fn yxxz(&self) -> Self::Dim4Type;
    fn yxxw(&self) -> Self::Dim4Type;
    fn yxyx(&self) -> Self::Dim4Type;
    fn yxyy(&self) -> Self::Dim4Type;
    fn yxyz(&self) -> Self::Dim4Type;
    fn yxyw(&self) -> Self::Dim4Type;
    fn yxzx(&self) -> Self::Dim4Type;
    fn yxzy(&self) -> Self::Dim4Type;
    fn yxzz(&self) -> Self::Dim4Type;
    fn yxzw(&self) -> Self::Dim4Type;
    fn yxwx(&self) -> Self::Dim4Type;
    fn yxwy(&self) -> Self::Dim4Type;
    fn yxwz(&self) -> Self::Dim4Type;
    fn yxww(&self) -> Self::Dim4Type;
    fn yyxx(&self) -> Self::Dim4Type;
    fn yyxy(&self) -> Self::Dim4Type;
    fn yyxz(&self) -> Self::Dim4Type;
    fn yyxw(&self) -> Self::Dim4Type;
    fn yyyx(&self) -> Self::Dim4Type;
    fn yyyy(&self) -> Self::Dim4Type;
    fn yyyz(&self) -> Self::Dim4Type;
    fn yyyw(&self) -> Self::Dim4Type;
    fn yyzx(&self) -> Self::Dim4Type;
    fn yyzy(&self) -> Self::Dim4Type;
    fn yyzz(&self) -> Self::Dim4Type;
    fn yyzw(&self) -> Self::Dim4Type;
    fn yywx(&self) -> Self::Dim4Type;
    fn yywy(&self) -> Self::Dim4Type;
    fn yywz(&self) -> Self::Dim4Type;
    fn yyww(&self) -> Self::Dim4Type;
    fn yzxx(&self) -> Self::Dim4Type;
    fn yzxy(&self) -> Self::Dim4Type;
    fn yzxz(&self) -> Self::Dim4Type;
    fn yzxw(&self) -> Self::Dim4Type;
    fn yzyx(&self) -> Self::Dim4Type;
    fn yzyy(&self) -> Self::Dim4Type;
    fn yzyz(&self) -> Self::Dim4Type;
    fn yzyw(&self) -> Self::Dim4Type;
    fn yzzx(&self) -> Self::Dim4Type;
    fn yzzy(&self) -> Self::Dim4Type;
    fn yzzz(&self) -> Self::Dim4Type;
    fn yzzw(&self) -> Self::Dim4Type;
    fn yzwx(&self) -> Self::Dim4Type;
    fn yzwy(&self) -> Self::Dim4Type;
    fn yzwz(&self) -> Self::Dim4Type;
    fn yzww(&self) -> Self::Dim4Type;
    fn ywxx(&self) -> Self::Dim4Type;
    fn ywxy(&self) -> Self::Dim4Type;
    fn ywxz(&self) -> Self::Dim4Type;
    fn ywxw(&self) -> Self::Dim4Type;
    fn ywyx(&self) -> Self::Dim4Type;
    fn ywyy(&self) -> Self::Dim4Type;
    fn ywyz(&self) -> Self::Dim4Type;
    fn ywyw(&self) -> Self::Dim4Type;
    fn ywzx(&self) -> Self::Dim4Type;
    fn ywzy(&self) -> Self::Dim4Type;
    fn ywzz(&self) -> Self::Dim4Type;
    fn ywzw(&self) -> Self::Dim4Type;
    fn ywwx(&self) -> Self::Dim4Type;
    fn ywwy(&self) -> Self::Dim4Type;
    fn ywwz(&self) -> Self::Dim4Type;
    fn ywww(&self) -> Self::Dim4Type;
    fn zxxx(&self) -> Self::Dim4Type;
    fn zxxy(&self) -> Self::Dim4Type;
    fn zxxz(&self) -> Self::Dim4Type;
    fn zxxw(&self) -> Self::Dim4Type;
    fn zxyx(&self) -> Self::Dim4Type;
    fn zxyy(&self) -> Self::Dim4Type;
    fn zxyz(&self) -> Self::Dim4Type;
    fn zxyw(&self) -> Self::Dim4Type;
    fn zxzx(&self) -> Self::Dim4Type;
    fn zxzy(&self) -> Self::Dim4Type;
    fn zxzz(&self) -> Self::Dim4Type;
    fn zxzw(&self) -> Self::Dim4Type;
    fn zxwx(&self) -> Self::Dim4Type;
    fn zxwy(&self) -> Self::Dim4Type;
    fn zxwz(&self) -> Self::Dim4Type;
    fn zxww(&self) -> Self::Dim4Type;
    fn zyxx(&self) -> Self::Dim4Type;
    fn zyxy(&self) -> Self::Dim4Type;
    fn zyxz(&self) -> Self::Dim4Type;
    fn zyxw(&self) -> Self::Dim4Type;
    fn zyyx(&self) -> Self::Dim4Type;
    fn zyyy(&self) -> Self::Dim4Type;
    fn zyyz(&self) -> Self::Dim4Type;
    fn zyyw(&self) -> Self::Dim4Type;
    fn zyzx(&self) -> Self::Dim4Type;
    fn zyzy(&self) -> Self::Dim4Type;
    fn zyzz(&self) -> Self::Dim4Type;
    fn zyzw(&self) -> Self::Dim4Type;
    fn zywx(&self) -> Self::Dim4Type;
    fn zywy(&self) -> Self::Dim4Type;
    fn zywz(&self) -> Self::Dim4Type;
    fn zyww(&self) -> Self::Dim4Type;
    fn zzxx(&self) -> Self::Dim4Type;
    fn zzxy(&self) -> Self::Dim4Type;
    fn zzxz(&self) -> Self::Dim4Type;
    fn zzxw(&self) -> Self::Dim4Type;
    fn zzyx(&self) -> Self::Dim4Type;
    fn zzyy(&self) -> Self::Dim4Type;
    fn zzyz(&self) -> Self::Dim4Type;
    fn zzyw(&self) -> Self::Dim4Type;
    fn zzzx(&self) -> Self::Dim4Type;
    fn zzzy(&self) -> Self::Dim4Type;
    fn zzzz(&self) -> Self::Dim4Type;
    fn zzzw(&self) -> Self::Dim4Type;
    fn zzwx(&self) -> Self::Dim4Type;
    fn zzwy(&self) -> Self::Dim4Type;
    fn zzwz(&self) -> Self::Dim4Type;
    fn zzww(&self) -> Self::Dim4Type;
    fn zwxx(&self) -> Self::Dim4Type;
    fn zwxy(&self) -> Self::Dim4Type;
    fn zwxz(&self) -> Self::Dim4Type;
    fn zwxw(&self) -> Self::Dim4Type;
    fn zwyx(&self) -> Self::Dim4Type;
    fn zwyy(&self) -> Self::Dim4Type;
    fn zwyz(&self) -> Self::Dim4Type;
    fn zwyw(&self) -> Self::Dim4Type;
    fn zwzx(&self) -> Self::Dim4Type;
    fn zwzy(&self) -> Self::Dim4Type;
    fn zwzz(&self) -> Self::Dim4Type;
    fn zwzw(&self) -> Self::Dim4Type;
    fn zwwx(&self) -> Self::Dim4Type;
    fn zwwy(&self) -> Self::Dim4Type;
    fn zwwz(&self) -> Self::Dim4Type;
    fn zwww(&self) -> Self::Dim4Type;
    fn wxxx(&self) -> Self::Dim4Type;
    fn wxxy(&self) -> Self::Dim4Type;
    fn wxxz(&self) -> Self::Dim4Type;
    fn wxxw(&self) -> Self::Dim4Type;
    fn wxyx(&self) -> Self::Dim4Type;
    fn wxyy(&self) -> Self::Dim4Type;
    fn wxyz(&self) -> Self::Dim4Type;
    fn wxyw(&self) -> Self::Dim4Type;
    fn wxzx(&self) -> Self::Dim4Type;
    fn wxzy(&self) -> Self::Dim4Type;
    fn wxzz(&self) -> Self::Dim4Type;
    fn wxzw(&self) -> Self::Dim4Type;
    fn wxwx(&self) -> Self::Dim4Type;
    fn wxwy(&self) -> Self::Dim4Type;
    fn wxwz(&self) -> Self::Dim4Type;
    fn wxww(&self) -> Self::Dim4Type;
    fn wyxx(&self) -> Self::Dim4Type;
    fn wyxy(&self) -> Self::Dim4Type;
    fn wyxz(&self) -> Self::Dim4Type;
    fn wyxw(&self) -> Self::Dim4Type;
    fn wyyx(&self) -> Self::Dim4Type;
    fn wyyy(&self) -> Self::Dim4Type;
    fn wyyz(&self) -> Self::Dim4Type;
    fn wyyw(&self) -> Self::Dim4Type;
    fn wyzx(&self) -> Self::Dim4Type;
    fn wyzy(&self) -> Self::Dim4Type;
    fn wyzz(&self) -> Self::Dim4Type;
    fn wyzw(&self) -> Self::Dim4Type;
    fn wywx(&self) -> Self::Dim4Type;
    fn wywy(&self) -> Self::Dim4Type;
    fn wywz(&self) -> Self::Dim4Type;
    fn wyww(&self) -> Self::Dim4Type;
    fn wzxx(&self) -> Self::Dim4Type;
    fn wzxy(&self) -> Self::Dim4Type;
    fn wzxz(&self) -> Self::Dim4Type;
    fn wzxw(&self) -> Self::Dim4Type;
    fn wzyx(&self) -> Self::Dim4Type;
    fn wzyy(&self) -> Self::Dim4Type;
    fn wzyz(&self) -> Self::Dim4Type;
    fn wzyw(&self) -> Self::Dim4Type;
    fn wzzx(&self) -> Self::Dim4Type;
    fn wzzy(&self) -> Self::Dim4Type;
    fn wzzz(&self) -> Self::Dim4Type;
    fn wzzw(&self) -> Self::Dim4Type;
    fn wzwx(&self) -> Self::Dim4Type;
    fn wzwy(&self) -> Self::Dim4Type;
    fn wzwz(&self) -> Self::Dim4Type;
    fn wzww(&self) -> Self::Dim4Type;
    fn wwxx(&self) -> Self::Dim4Type;
    fn wwxy(&self) -> Self::Dim4Type;
    fn wwxz(&self) -> Self::Dim4Type;
    fn wwxw(&self) -> Self::Dim4Type;
    fn wwyx(&self) -> Self::Dim4Type;
    fn wwyy(&self) -> Self::Dim4Type;
    fn wwyz(&self) -> Self::Dim4Type;
    fn wwyw(&self) -> Self::Dim4Type;
    fn wwzx(&self) -> Self::Dim4Type;
    fn wwzy(&self) -> Self::Dim4Type;
    fn wwzz(&self) -> Self::Dim4Type;
    fn wwzw(&self) -> Self::Dim4Type;
    fn wwwx(&self) -> Self::Dim4Type;
    fn wwwy(&self) -> Self::Dim4Type;
    fn wwwz(&self) -> Self::Dim4Type;
    fn wwww(&self) -> Self::Dim4Type;
}

impl<T: Scalar + Copy> Dim1 for Vector4<T> {
    type Dim1Type = T;
    fn x(&self) -> Self::Dim1Type {
        self.x
    }
}

impl<T: Scalar + Copy> Dim2 for Vector4<T> {
    type Dim2Type = Vector2<T>;
    fn y(&self) -> Self::Dim1Type {
        self.y
    }
    fn xx(&self) -> Self::Dim2Type {
        Vector2::new(self.x, self.x)
    }
    fn xy(&self) -> Self::Dim2Type {
        Vector2::new(self.x, self.y)
    }
    fn yy(&self) -> Self::Dim2Type {
        Vector2::new(self.y, self.y)
    }
    fn yx(&self) -> Self::Dim2Type {
        Vector2::new(self.y, self.x)
    }
}

impl<T: Scalar + Copy> Dim3 for Vector4<T> {
    type Dim3Type = Vector3<T>;
    fn z(&self) -> Self::Dim1Type {
        self.z
    }
    fn zz(&self) -> Self::Dim2Type {
        Vector2::new(self.z, self.z)
    }
    fn xz(&self) -> Self::Dim2Type {
        Vector2::new(self.x, self.z)
    }
    fn zx(&self) -> Self::Dim2Type {
        Vector2::new(self.z, self.x)
    }
    fn yz(&self) -> Self::Dim2Type {
        Vector2::new(self.y, self.z)
    }
    fn zy(&self) -> Self::Dim2Type {
        Vector2::new(self.z, self.y)
    }

    fn xxx(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.x, self.x)
    }
    fn xxy(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.x, self.y)
    }
    fn xxz(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.x, self.z)
    }
    fn xyx(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.y, self.x)
    }
    fn xyy(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.y, self.y)
    }
    fn xyz(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.y, self.z)
    }
    fn xzx(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.z, self.x)
    }
    fn xzy(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.z, self.y)
    }
    fn xzz(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.z, self.z)
    }
    fn yxx(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.x, self.x)
    }
    fn yxy(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.x, self.y)
    }
    fn yxz(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.x, self.z)
    }
    fn yyx(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.y, self.x)
    }
    fn yyy(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.y, self.y)
    }
    fn yyz(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.y, self.z)
    }
    fn yzx(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.z, self.x)
    }
    fn yzy(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.z, self.y)
    }
    fn yzz(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.z, self.z)
    }
    fn zxx(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.x, self.x)
    }
    fn zxy(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.x, self.y)
    }
    fn zxz(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.x, self.z)
    }
    fn zyx(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.y, self.x)
    }
    fn zyy(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.y, self.y)
    }
    fn zyz(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.y, self.z)
    }
    fn zzx(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.z, self.x)
    }
    fn zzy(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.z, self.y)
    }
    fn zzz(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.z, self.z)
    }
}

impl<T: Scalar + Copy> Dim4 for Vector4<T> {
    type Dim4Type = Vector4<T>;
    fn w(&self) -> Self::Dim1Type {
        self.w
    }
    fn ww(&self) -> Self::Dim2Type {
        Vector2::new(self.w, self.w)
    }
    fn xw(&self) -> Self::Dim2Type {
        Vector2::new(self.x, self.w)
    }
    fn wx(&self) -> Self::Dim2Type {
        Vector2::new(self.w, self.x)
    }
    fn yw(&self) -> Self::Dim2Type {
        Vector2::new(self.y, self.w)
    }
    fn wy(&self) -> Self::Dim2Type {
        Vector2::new(self.w, self.y)
    }
    fn zw(&self) -> Self::Dim2Type {
        Vector2::new(self.z, self.w)
    }
    fn wz(&self) -> Self::Dim2Type {
        Vector2::new(self.w, self.z)
    }

    fn xxw(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.x, self.w)
    }
    fn xyw(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.y, self.w)
    }
    fn xzw(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.z, self.w)
    }
    fn yxw(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.x, self.w)
    }
    fn yyw(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.y, self.w)
    }
    fn yzw(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.z, self.w)
    }
    fn zxw(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.x, self.w)
    }
    fn zyw(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.y, self.w)
    }
    fn zzw(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.z, self.w)
    }
    fn xwx(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.w, self.x)
    }
    fn xwy(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.w, self.y)
    }
    fn xwz(&self) -> Self::Dim3Type {
        Vector3::new(self.x, self.w, self.z)
    }
    fn ywx(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.w, self.x)
    }
    fn ywy(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.w, self.y)
    }
    fn ywz(&self) -> Self::Dim3Type {
        Vector3::new(self.y, self.w, self.z)
    }
    fn zwx(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.w, self.x)
    }
    fn zwy(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.w, self.y)
    }
    fn zwz(&self) -> Self::Dim3Type {
        Vector3::new(self.z, self.w, self.z)
    }
    fn wxx(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.x, self.x)
    }
    fn wxy(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.x, self.y)
    }
    fn wxz(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.x, self.z)
    }
    fn wyx(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.y, self.x)
    }
    fn wyy(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.y, self.y)
    }
    fn wyz(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.y, self.z)
    }
    fn wzx(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.z, self.x)
    }
    fn wzy(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.z, self.y)
    }
    fn wzz(&self) -> Self::Dim3Type {
        Vector3::new(self.w, self.z, self.z)
    }

    fn xxxx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.x, self.x)
    }
    fn xxxy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.x, self.y)
    }
    fn xxxz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.x, self.z)
    }
    fn xxxw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.x, self.w)
    }
    fn xxyx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.y, self.x)
    }
    fn xxyy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.y, self.y)
    }
    fn xxyz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.y, self.z)
    }
    fn xxyw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.y, self.w)
    }
    fn xxzx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.z, self.x)
    }
    fn xxzy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.z, self.y)
    }
    fn xxzz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.z, self.z)
    }
    fn xxzw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.z, self.w)
    }
    fn xxwx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.w, self.x)
    }
    fn xxwy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.w, self.y)
    }
    fn xxwz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.w, self.z)
    }
    fn xxww(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.x, self.w, self.w)
    }
    fn xyxx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.x, self.x)
    }
    fn xyxy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.x, self.y)
    }
    fn xyxz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.x, self.z)
    }
    fn xyxw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.x, self.w)
    }
    fn xyyx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.y, self.x)
    }
    fn xyyy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.y, self.y)
    }
    fn xyyz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.y, self.z)
    }
    fn xyyw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.y, self.w)
    }
    fn xyzx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.z, self.x)
    }
    fn xyzy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.z, self.y)
    }
    fn xyzz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.z, self.z)
    }
    fn xyzw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.z, self.w)
    }
    fn xywx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.w, self.x)
    }
    fn xywy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.w, self.y)
    }
    fn xywz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.w, self.z)
    }
    fn xyww(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.y, self.w, self.w)
    }
    fn xzxx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.x, self.x)
    }
    fn xzxy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.x, self.y)
    }
    fn xzxz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.x, self.z)
    }
    fn xzxw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.x, self.w)
    }
    fn xzyx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.y, self.x)
    }
    fn xzyy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.y, self.y)
    }
    fn xzyz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.y, self.z)
    }
    fn xzyw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.y, self.w)
    }
    fn xzzx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.z, self.x)
    }
    fn xzzy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.z, self.y)
    }
    fn xzzz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.z, self.z)
    }
    fn xzzw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.z, self.w)
    }
    fn xzwx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.w, self.x)
    }
    fn xzwy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.w, self.y)
    }
    fn xzwz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.w, self.z)
    }
    fn xzww(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.z, self.w, self.w)
    }
    fn xwxx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.x, self.x)
    }
    fn xwxy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.x, self.y)
    }
    fn xwxz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.x, self.z)
    }
    fn xwxw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.x, self.w)
    }
    fn xwyx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.y, self.x)
    }
    fn xwyy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.y, self.y)
    }
    fn xwyz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.y, self.z)
    }
    fn xwyw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.y, self.w)
    }
    fn xwzx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.z, self.x)
    }
    fn xwzy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.z, self.y)
    }
    fn xwzz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.z, self.z)
    }
    fn xwzw(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.z, self.w)
    }
    fn xwwx(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.w, self.x)
    }
    fn xwwy(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.w, self.y)
    }
    fn xwwz(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.w, self.z)
    }
    fn xwww(&self) -> Self::Dim4Type {
        Vector4::new(self.x, self.w, self.w, self.w)
    }
    fn yxxx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.x, self.x)
    }
    fn yxxy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.x, self.y)
    }
    fn yxxz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.x, self.z)
    }
    fn yxxw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.x, self.w)
    }
    fn yxyx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.y, self.x)
    }
    fn yxyy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.y, self.y)
    }
    fn yxyz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.y, self.z)
    }
    fn yxyw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.y, self.w)
    }
    fn yxzx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.z, self.x)
    }
    fn yxzy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.z, self.y)
    }
    fn yxzz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.z, self.z)
    }
    fn yxzw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.z, self.w)
    }
    fn yxwx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.w, self.x)
    }
    fn yxwy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.w, self.y)
    }
    fn yxwz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.w, self.z)
    }
    fn yxww(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.x, self.w, self.w)
    }
    fn yyxx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.x, self.x)
    }
    fn yyxy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.x, self.y)
    }
    fn yyxz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.x, self.z)
    }
    fn yyxw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.x, self.w)
    }
    fn yyyx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.y, self.x)
    }
    fn yyyy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.y, self.y)
    }
    fn yyyz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.y, self.z)
    }
    fn yyyw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.y, self.w)
    }
    fn yyzx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.z, self.x)
    }
    fn yyzy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.z, self.y)
    }
    fn yyzz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.z, self.z)
    }
    fn yyzw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.z, self.w)
    }
    fn yywx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.w, self.x)
    }
    fn yywy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.w, self.y)
    }
    fn yywz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.w, self.z)
    }
    fn yyww(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.y, self.w, self.w)
    }
    fn yzxx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.x, self.x)
    }
    fn yzxy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.x, self.y)
    }
    fn yzxz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.x, self.z)
    }
    fn yzxw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.x, self.w)
    }
    fn yzyx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.y, self.x)
    }
    fn yzyy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.y, self.y)
    }
    fn yzyz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.y, self.z)
    }
    fn yzyw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.y, self.w)
    }
    fn yzzx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.z, self.x)
    }
    fn yzzy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.z, self.y)
    }
    fn yzzz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.z, self.z)
    }
    fn yzzw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.z, self.w)
    }
    fn yzwx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.w, self.x)
    }
    fn yzwy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.w, self.y)
    }
    fn yzwz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.w, self.z)
    }
    fn yzww(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.z, self.w, self.w)
    }
    fn ywxx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.x, self.x)
    }
    fn ywxy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.x, self.y)
    }
    fn ywxz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.x, self.z)
    }
    fn ywxw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.x, self.w)
    }
    fn ywyx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.y, self.x)
    }
    fn ywyy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.y, self.y)
    }
    fn ywyz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.y, self.z)
    }
    fn ywyw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.y, self.w)
    }
    fn ywzx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.z, self.x)
    }
    fn ywzy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.z, self.y)
    }
    fn ywzz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.z, self.z)
    }
    fn ywzw(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.z, self.w)
    }
    fn ywwx(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.w, self.x)
    }
    fn ywwy(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.w, self.y)
    }
    fn ywwz(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.w, self.z)
    }
    fn ywww(&self) -> Self::Dim4Type {
        Vector4::new(self.y, self.w, self.w, self.w)
    }
    fn zxxx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.x, self.x)
    }
    fn zxxy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.x, self.y)
    }
    fn zxxz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.x, self.z)
    }
    fn zxxw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.x, self.w)
    }
    fn zxyx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.y, self.x)
    }
    fn zxyy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.y, self.y)
    }
    fn zxyz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.y, self.z)
    }
    fn zxyw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.y, self.w)
    }
    fn zxzx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.z, self.x)
    }
    fn zxzy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.z, self.y)
    }
    fn zxzz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.z, self.z)
    }
    fn zxzw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.z, self.w)
    }
    fn zxwx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.w, self.x)
    }
    fn zxwy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.w, self.y)
    }
    fn zxwz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.w, self.z)
    }
    fn zxww(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.x, self.w, self.w)
    }
    fn zyxx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.x, self.x)
    }
    fn zyxy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.x, self.y)
    }
    fn zyxz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.x, self.z)
    }
    fn zyxw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.x, self.w)
    }
    fn zyyx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.y, self.x)
    }
    fn zyyy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.y, self.y)
    }
    fn zyyz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.y, self.z)
    }
    fn zyyw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.y, self.w)
    }
    fn zyzx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.z, self.x)
    }
    fn zyzy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.z, self.y)
    }
    fn zyzz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.z, self.z)
    }
    fn zyzw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.z, self.w)
    }
    fn zywx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.w, self.x)
    }
    fn zywy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.w, self.y)
    }
    fn zywz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.w, self.z)
    }
    fn zyww(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.y, self.w, self.w)
    }
    fn zzxx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.x, self.x)
    }
    fn zzxy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.x, self.y)
    }
    fn zzxz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.x, self.z)
    }
    fn zzxw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.x, self.w)
    }
    fn zzyx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.y, self.x)
    }
    fn zzyy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.y, self.y)
    }
    fn zzyz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.y, self.z)
    }
    fn zzyw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.y, self.w)
    }
    fn zzzx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.z, self.x)
    }
    fn zzzy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.z, self.y)
    }
    fn zzzz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.z, self.z)
    }
    fn zzzw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.z, self.w)
    }
    fn zzwx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.w, self.x)
    }
    fn zzwy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.w, self.y)
    }
    fn zzwz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.w, self.z)
    }
    fn zzww(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.z, self.w, self.w)
    }
    fn zwxx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.x, self.x)
    }
    fn zwxy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.x, self.y)
    }
    fn zwxz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.x, self.z)
    }
    fn zwxw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.x, self.w)
    }
    fn zwyx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.y, self.x)
    }
    fn zwyy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.y, self.y)
    }
    fn zwyz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.y, self.z)
    }
    fn zwyw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.y, self.w)
    }
    fn zwzx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.z, self.x)
    }
    fn zwzy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.z, self.y)
    }
    fn zwzz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.z, self.z)
    }
    fn zwzw(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.z, self.w)
    }
    fn zwwx(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.w, self.x)
    }
    fn zwwy(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.w, self.y)
    }
    fn zwwz(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.w, self.z)
    }
    fn zwww(&self) -> Self::Dim4Type {
        Vector4::new(self.z, self.w, self.w, self.w)
    }
    fn wxxx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.x, self.x)
    }
    fn wxxy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.x, self.y)
    }
    fn wxxz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.x, self.z)
    }
    fn wxxw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.x, self.w)
    }
    fn wxyx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.y, self.x)
    }
    fn wxyy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.y, self.y)
    }
    fn wxyz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.y, self.z)
    }
    fn wxyw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.y, self.w)
    }
    fn wxzx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.z, self.x)
    }
    fn wxzy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.z, self.y)
    }
    fn wxzz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.z, self.z)
    }
    fn wxzw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.z, self.w)
    }
    fn wxwx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.w, self.x)
    }
    fn wxwy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.w, self.y)
    }
    fn wxwz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.w, self.z)
    }
    fn wxww(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.x, self.w, self.w)
    }
    fn wyxx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.x, self.x)
    }
    fn wyxy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.x, self.y)
    }
    fn wyxz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.x, self.z)
    }
    fn wyxw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.x, self.w)
    }
    fn wyyx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.y, self.x)
    }
    fn wyyy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.y, self.y)
    }
    fn wyyz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.y, self.z)
    }
    fn wyyw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.y, self.w)
    }
    fn wyzx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.z, self.x)
    }
    fn wyzy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.z, self.y)
    }
    fn wyzz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.z, self.z)
    }
    fn wyzw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.z, self.w)
    }
    fn wywx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.w, self.x)
    }
    fn wywy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.w, self.y)
    }
    fn wywz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.w, self.z)
    }
    fn wyww(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.y, self.w, self.w)
    }
    fn wzxx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.x, self.x)
    }
    fn wzxy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.x, self.y)
    }
    fn wzxz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.x, self.z)
    }
    fn wzxw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.x, self.w)
    }
    fn wzyx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.y, self.x)
    }
    fn wzyy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.y, self.y)
    }
    fn wzyz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.y, self.z)
    }
    fn wzyw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.y, self.w)
    }
    fn wzzx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.z, self.x)
    }
    fn wzzy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.z, self.y)
    }
    fn wzzz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.z, self.z)
    }
    fn wzzw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.z, self.w)
    }
    fn wzwx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.w, self.x)
    }
    fn wzwy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.w, self.y)
    }
    fn wzwz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.w, self.z)
    }
    fn wzww(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.z, self.w, self.w)
    }
    fn wwxx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.x, self.x)
    }
    fn wwxy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.x, self.y)
    }
    fn wwxz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.x, self.z)
    }
    fn wwxw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.x, self.w)
    }
    fn wwyx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.y, self.x)
    }
    fn wwyy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.y, self.y)
    }
    fn wwyz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.y, self.z)
    }
    fn wwyw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.y, self.w)
    }
    fn wwzx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.z, self.x)
    }
    fn wwzy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.z, self.y)
    }
    fn wwzz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.z, self.z)
    }
    fn wwzw(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.z, self.w)
    }
    fn wwwx(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.w, self.x)
    }
    fn wwwy(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.w, self.y)
    }
    fn wwwz(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.w, self.z)
    }
    fn wwww(&self) -> Self::Dim4Type {
        Vector4::new(self.w, self.w, self.w, self.w)
    }
}
