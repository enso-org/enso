//! This module gathers common math types which are widely used in this project.

use enso_prelude::*;

pub use nalgebra::Vector2;
pub use nalgebra::Vector3;
pub use nalgebra::Vector4;

pub use nalgebra::Matrix2;
pub use nalgebra::Matrix3;
pub use nalgebra::Matrix4;

pub use nalgebra::Matrix2x3;
pub use nalgebra::Matrix2x4;
pub use nalgebra::Matrix3x2;
pub use nalgebra::Matrix3x4;
pub use nalgebra::Matrix4x2;
pub use nalgebra::Matrix4x3;

use nalgebra;
use nalgebra::Scalar;
use nalgebra::Matrix;
use nalgebra::ComplexField;
use nalgebra::Dim;
use nalgebra::storage::Storage;

use std::ops::AddAssign;



// ==============
// === Traits ===
// ==============

/// Describes types that have a zero value.
pub trait Zero {
    /// A zero value of this type.
    fn zero() -> Self;
}

/// Smart constructor for the `Zero` trait.
pub fn zero<T:Zero>() -> T {
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
        impl<T:Scalar+num_traits::Zero> Zero for $ty<T> {
            fn zero() -> Self {
                nalgebra::zero()
            }
        }
    )*};
}

gen_zero!([f32,f64] = 0.0);
gen_zero!([i32,i64,usize] = 0);
gen_zero_nalgebra!([Vector2,Vector3,Vector4,Matrix2,Matrix3,Matrix4,Matrix2x3,Matrix2x4,Matrix3x2
                   ,Matrix3x4,Matrix4x2,Matrix4x3]);



// =====================
// === HasComponents ===
// =====================

/// Every type which has components, like `Vector<f32>`.
pub trait HasComponents {
    /// The component type.
    type Component;
}


// ============
// === Dim1 ===
// ============

/// Describes types that have the first dimension component.
pub trait Dim1 : HasComponents {
    /// X-axis component getter.
    fn x(&self) -> Self::Component;
}

/// Describes types that have the second dimension component.
pub trait Dim2 : Dim1 {
    /// Y-axis component getter.
    fn y(&self) -> Self::Component;
}

/// Describes types that have the third dimension component.
pub trait Dim3 : Dim2 {
    /// Z-axis component getter.
    fn z(&self) -> Self::Component;
}



// ===========
// === Abs ===
// ===========

/// Types which have an absolute value.
pub trait Abs {
    /// Absolute value.
    fn abs(&self) -> Self;
}

impl Abs for usize {
    fn abs(&self) -> Self { *self }
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

gen_abs!([f32,f64,i32,i64]);



// ===========
// === Pow ===
// ===========

/// Types which can be raised to the given power.
#[allow(missing_docs)]
pub trait Pow<T=Self> {
    type Output;
    fn pow(self, t:T) -> Self::Output;
}

impl Pow<f32> for f32 {
    type Output = f32;
    fn pow(self, t:f32) -> Self::Output {
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

impl<N:ComplexField, R:Dim, C:Dim, S:Storage<N,R,C>> Magnitude for Matrix<N,R,C,S> {
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
    fn clamp(self, min:Self, max:Self) -> Self::Output;
}


// === Impls ===

impl Clamp for f32 {
    type Output = f32;
    fn clamp(self, min:f32, max:f32) -> f32 {
        self.clamp(min,max)
    }
}



// =============
// === Min ===
// =============

#[allow(missing_docs)]
pub trait Min {
    fn min(self, other:Self) -> Self;
}


// === Impls ===

impl Min for f32 {
    fn min(self, other:Self) -> Self {
        self.min(other)
    }
}



// =============
// === Max ===
// =============

#[allow(missing_docs)]
pub trait Max {
    fn max(self, other:Self) -> Self;
}


// === Impls ===

impl Max for f32 {
    fn max(self, other:Self) -> Self {
        self.max(other)
    }
}




// =================
// === Normalize ===
// =================

/// Types which can be normalized.
pub trait Normalize {
    /// Normalized value.
    fn normalize(&self) -> Self;
}


// === Impls ===

impl Normalize for f32 {
    fn normalize(&self) -> f32 {
        self.signum()
    }
}



// ===================
// === Square Root ===
// ===================

/// Types from which a square root can be calculated.
pub trait Sqrt {
    /// The output type of the computation.
    type Output;
    /// Compute the square root of the given number.
    fn sqrt(&self) -> Self::Output;
}


// === Impls ===

impl Sqrt for f32 {
    type Output = f32;
    fn sqrt(&self) -> f32 {
        f32::sqrt(*self)
    }
}


// ==============
// === Cosine ===
// ==============

/// Types from which a cosine can be calculated.
pub trait Cos {
    /// The output type of the computation.
    type Output;
    /// Compute the cosine of the given number.
    fn cos(&self) -> Self;
}


// === Impls ===

impl Cos for f32 {
    type Output = f32;
    fn cos(&self) -> f32 {
        f32::cos(*self)
    }
}



// ============
// === Sine ===
// ============

/// Types from which a sine can be calculated
pub trait Sin {
    /// The output type of the computation.
    type Output;
    /// Compute the sine of the given number.
    fn sin(&self) -> Self::Output;
}


// === Impls ===

impl Sin for f32 {
    type Output = f32;
    fn sin(&self) -> f32 {
        f32::sin(*self)
    }
}



// =============
// === Asine ===
// =============

/// Types from which a asin can be calculated
pub trait Asin {
    /// The output type of the computation.
    type Output;
    /// Compute the asin of the given number.
    fn asin(&self) -> Self::Output;
}


// === Impls ===

impl Asin for f32 {
    type Output = f32;
    fn asin(&self) -> f32 {
        f32::asin(*self)
    }
}



// ===============
// === Acosine ===
// ===============

/// Types from which a asin can be calculated
pub trait Acos {
    /// The output type of the computation.
    type Output;
    /// Compute the asin of the given number.
    fn acos(&self) -> Self::Output;
}


// === Impls ===

impl Acos for f32 {
    type Output = f32;
    fn acos(&self) -> f32 {
        f32::acos(*self)
    }
}




macro_rules! define_vector {
    ($name:ident {$($field:ident),*}) => {
        /// A coordinate in space.
        #[derive(Clone,Copy,Debug,Default,PartialEq)]
        #[repr(C)]
        pub struct $name<T=f32> {
            $(
                /// Vector component.
                pub $field : T
            ),*
        }

        /// Smart constructor.
        #[allow(non_snake_case)]
        pub fn $name<T>($($field:T),*) -> $name<T> {
            $name {$($field),*}
        }

        impl<T> $name<T> {
            /// Constructor.
            pub fn new($($field:T),*) -> Self {
                Self {$($field),*}
            }

            /// Converts the struct to slice.
            ///
            /// # Safety
            /// The code is safe as the struct is implemented as `repr(C)`.
            #[allow(unsafe_code)]
            #[allow(trivial_casts)]
            pub fn as_slice(&self) -> &[T] {
                // Safe, because $name is defined as `#[repr(C)]`.
                let ptr = self as *const $name<T> as *const T;
                unsafe {
                    std::slice::from_raw_parts(ptr, std::mem::size_of::<T>())
                }
            }
        }

        impl Magnitude for $name {
            type Output = f32;
            fn magnitude(&self) -> Self::Output {
                $(let $field = self.$field * self.$field;)*
                let sum = 0.0 $(+$field)*;
                sum.sqrt()
            }
        }

        impl Normalize for $name {
            fn normalize(&self) -> Self {
                let magnitude = self.magnitude();
                $(let $field = self.$field / magnitude;)*
                Self {$($field),*}
            }
        }

        impl AddAssign<f32> for $name {
            fn add_assign(&mut self, rhs:f32) {
                $(self.$field += rhs;)*
            }
        }

        impl<T,S> AddAssign<$name<S>> for $name<T>
        where T:AddAssign<S> {
            fn add_assign(&mut self, rhs:$name<S>) {
                $(self.$field += rhs.$field;)*
            }
        }

        impl<T,S> Add<$name<S>> for $name<T>
        where T:Add<S> {
            type Output = $name<<T as Add<S>>::Output>;
            fn add(self,rhs:$name<S>) -> Self::Output {
                $(let $field = self.$field.add(rhs.$field);)*
                $name {$($field),*}
            }
        }

        impl<T,S> Sub<$name<S>> for $name<T>
        where T:Sub<S> {
            type Output = $name<<T as Sub<S>>::Output>;
            fn sub(self,rhs:$name<S>) -> Self::Output {
                $(let $field = self.$field.sub(rhs.$field);)*
                $name {$($field),*}
            }
        }

        impl<T> Neg for $name<T>
        where T:Neg {
            type Output = $name<<T as Neg>::Output>;
            fn neg(self) -> Self::Output {
                $(let $field = self.$field.neg();)*
                $name {$($field),*}
            }
        }

        impl Mul<f32> for $name {
            type Output = $name;
            fn mul(self, rhs:f32) -> Self::Output {
                $(let $field = self.$field.mul(rhs);)*
                $name {$($field),*}
            }
        }

        impl Mul<&f32> for $name {
            type Output = $name;
            fn mul(self, rhs:&f32) -> Self::Output {
                $(let $field = self.$field.mul(rhs);)*
                $name {$($field),*}
            }
        }

        impl Mul<f32> for &$name {
            type Output = $name;
            fn mul(self, rhs:f32) -> Self::Output {
                $(let $field = self.$field.mul(rhs);)*
                $name {$($field),*}
            }
        }

        impl Mul<&f32> for &$name {
            type Output = $name;
            fn mul(self, rhs:&f32) -> Self::Output {
                $(let $field = self.$field.mul(rhs);)*
                $name {$($field),*}
            }
        }

        impl Div<f32> for $name {
            type Output = $name;
            fn div(self, rhs:f32) -> Self::Output {
                $(let $field = self.$field.div(rhs);)*
                $name {$($field),*}
            }
        }

        impl Div<&f32> for $name {
            type Output = $name;
            fn div(self, rhs:&f32) -> Self::Output {
                $(let $field = self.$field.div(rhs);)*
                $name {$($field),*}
            }
        }

        impl Div<f32> for &$name {
            type Output = $name;
            fn div(self, rhs:f32) -> Self::Output {
                $(let $field = self.$field.div(rhs);)*
                $name {$($field),*}
            }
        }

        impl Div<&f32> for &$name {
            type Output = $name;
            fn div(self, rhs:&f32) -> Self::Output {
                $(let $field = self.$field.div(rhs);)*
                $name {$($field),*}
            }
        }

        impl Mul<$name> for f32 {
            type Output = $name;
            fn mul(self, rhs:$name) -> Self::Output {
                $(let $field = self.mul(rhs.$field);)*
                $name {$($field),*}
            }
        }

        impl Mul<&$name> for f32 {
            type Output = $name;
            fn mul(self, rhs:&$name) -> Self::Output {
                $(let $field = self.mul(rhs.$field);)*
                $name {$($field),*}
            }
        }

        impl Mul<$name> for &f32 {
            type Output = $name;
            fn mul(self, rhs:$name) -> Self::Output {
                $(let $field = self.mul(rhs.$field);)*
                $name {$($field),*}
            }
        }

        impl Mul<&$name> for &f32 {
            type Output = $name;
            fn mul(self, rhs:&$name) -> Self::Output {
                $(let $field = self.mul(rhs.$field);)*
                $name {$($field),*}
            }
        }
    };
}

define_vector! {V2 {x,y}}
define_vector! {V3 {x,y,z}}
define_vector! {V4 {x,y,z,w}}

impl<T:Default> From<V2<T>> for V3<T> {
    fn from(t:V2<T>) -> Self {
        V3(t.x,t.y,default())
    }
}

impl<T:Default> From<V2<T>> for V4<T> {
    fn from(t:V2<T>) -> Self {
        V4(t.x,t.y,default(),default())
    }
}

impl<T:Default> From<V3<T>> for V4<T> {
    fn from(t:V3<T>) -> Self {
        V4(t.x,t.y,t.z,default())
    }
}



impl<T:Scalar> From<Vector2<T>> for V2<T> {
    fn from(t:Vector2<T>) -> Self {
        V2(t.x,t.y)
    }
}

impl<T:Scalar> Into<Vector2<T>> for V2<T> {
    fn into(self) -> Vector2<T> {
        Vector2::new(self.x,self.y)
    }
}

impl<T:Scalar> Into<Vector2<T>> for &V2<T> {
    fn into(self) -> Vector2<T> {
        Vector2::new(self.x,self.y)
    }
}



impl<T:Scalar> From<Vector3<T>> for V3<T> {
    fn from(t:Vector3<T>) -> Self {
        V3(t.x,t.y,t.z)
    }
}

impl<T:Scalar> Into<Vector3<T>> for V3<T> {
    fn into(self) -> Vector3<T> {
        Vector3::new(self.x,self.y,self.z)
    }
}

impl<T:Scalar> Into<Vector3<T>> for &V3<T> {
    fn into(self) -> Vector3<T> {
        Vector3::new(self.x,self.y,self.z)
    }
}



impl<T:Scalar> From<Vector4<T>> for V4<T> {
    fn from(t:Vector4<T>) -> Self {
        V4(t.x,t.y,t.z,t.w)
    }
}

impl<T:Scalar> Into<Vector4<T>> for V4<T> {
    fn into(self) -> Vector4<T> {
        Vector4::new(self.x,self.y,self.z,self.w)
    }
}

impl<T:Scalar> Into<Vector4<T>> for &V4<T> {
    fn into(self) -> Vector4<T> {
        Vector4::new(self.x,self.y,self.z,self.w)
    }
}




// ============================
// === Algebraic Structures ===
// ============================
// TODO evaluate for correctness and usefulness.

/// Trait that describes a set of numbers that define addition, subtraction, multiplication,
/// and division.
pub trait Field<T> = Add<T,Output=T> + Sub<T,Output=T> + Mul<T,Output=T> + Div<T,Output=T>;
