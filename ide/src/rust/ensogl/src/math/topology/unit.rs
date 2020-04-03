//! Defines unit of measurement abstraction. See: https://en.wikipedia.org/wiki/Unit_of_measurement

use crate::prelude::*;

use crate::system::gpu::shader::glsl;
use crate::system::gpu::shader::glsl::Glsl;
use crate::system::gpu::shader::glsl::traits::*;

use nalgebra::*;
use std::ops::*;



// ============
// === Unit ===
// ============

/// Abstraction for any unit type. It is parametrized by:
///   - Quantity, like distance, angle, or mass. See https://en.wikipedia.org/wiki/Quantity .
///   - Type, like pixels, degrees, or radians.
///   - Repr, like f32, or f64.
#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Unit<Quantity=Anything,Type=Anything,Repr=f32> {
    /// The raw value of this unit.
    pub value : Repr,
    _quantity : PhantomData<Quantity>,
    _type     : PhantomData<Type>,
}

impl<Quantity,Type,Repr> Unit<Quantity,Type,Repr> {
    /// Constructor.
    pub fn new(value:Repr) -> Self {
        let _quantity = PhantomData;
        let _type     = PhantomData;
        Self {value,_quantity,_type}
    }
}


// === Num ===

impl<Quantity,Type,Repr> Abs for Unit<Quantity,Type,Repr> where Repr:Abs {
    fn abs(&self) -> Self {
        Self { value:self.value.abs(), ..*self }
    }
}


// === Operators ===

impls! { [Quantity,Type,Repr] From<Repr> for Unit<Quantity,Type,Repr>  { |t| {Self::new(t)} } }
impls! { [Quantity,Type]      From<Unit<Quantity,Type,f32>> for f32    { |t| {t.value} } }

macro_rules! impl_operator_for_unit {
    ( $name:ident $fn:ident <$rhs:ty> for $lhs:ty ) => {
        impl_operator_for_unit_with_ref_rhs! { $name $fn <$rhs> for $lhs [value] }
        impl_operator_for_unit_with_ref_lhs! { $name $fn <$rhs> for $lhs }
    }
}

macro_rules! impl_operator_for_unit_with_ref_rhs {
    ( $name:ident $fn:ident <$rhs:ty> for $lhs:ty $([$rhs_accessor:ident])? ) => {
        impl<Quantity,Type,Lhs,Rhs> $name<$rhs> for $lhs
        where Lhs : $name<Rhs> {
            type Output = Unit<Quantity,Type,<Lhs as $name<Rhs>>::Output>;
            fn $fn(self, rhs:$rhs) -> Self::Output {
                (self.value.$fn(rhs $(.$rhs_accessor)?)).into()
            }
        }

        impl<'t,Quantity,Type,Lhs,Rhs> $name<$rhs> for &'t $lhs
        where &'t Lhs : $name<Rhs> {
            type Output = Unit<Quantity,Type,<&'t Lhs as $name<Rhs>>::Output>;
            fn $fn(self, rhs:$rhs) -> Self::Output {
                ((&self.value).$fn(rhs $(.$rhs_accessor)?)).into()
            }
        }
    }
}

macro_rules! impl_operator_for_unit_with_ref_lhs {
    ( $name:ident $fn:ident <$rhs:ty> for $lhs:ty ) => {
        impl<'t,Quantity,Type,Lhs,Rhs> $name<&'t $rhs> for &'t $lhs
        where &'t Lhs : $name<&'t Rhs> {
            type Output = Unit<Quantity,Type,<&'t Lhs as $name<&'t Rhs>>::Output>;
            fn $fn(self, rhs:&'t $rhs) -> Self::Output {
                ((&self.value).$fn(&rhs.value)).into()
            }
        }

        impl<'t,Quantity,Type,Lhs,Rhs> $name<&'t $rhs> for $lhs
            where Lhs : $name<&'t Rhs> {
            type Output = Unit<Quantity,Type,<Lhs as $name<&'t Rhs>>::Output>;
            fn $fn(self, rhs:&'t $rhs) -> Self::Output {
                (self.value.$fn(&rhs.value)).into()
            }
        }
    }
}

impl_operator_for_unit! { Add add <Unit<Quantity,Type,Rhs>> for Unit<Quantity,Type,Lhs> }
impl_operator_for_unit! { Sub sub <Unit<Quantity,Type,Rhs>> for Unit<Quantity,Type,Lhs> }

impl_operator_for_unit_with_ref_rhs! { Mul mul <Rhs> for Unit<Quantity,Type,Lhs> }
impl_operator_for_unit_with_ref_rhs! { Div div <Rhs> for Unit<Quantity,Type,Lhs> }

impl<Quantity,Type> Mul<Unit<Quantity,Type,f32>> for f32 {
    type Output = Unit<Quantity,Type,f32>;
    fn mul(self, rhs:Unit<Quantity,Type,f32>) -> Self::Output {
        (self * rhs.value).into()
    }
}

impl<'t,Quantity,Type> Mul<&'t Unit<Quantity,Type,f32>> for f32 {
    type Output = Unit<Quantity,Type,f32>;
    fn mul(self, rhs:&'t Unit<Quantity,Type,f32>) -> Self::Output {
        (self * rhs.value).into()
    }
}

impl<'t,Quantity,Type> Mul<Unit<Quantity,Type,f32>> for &'t f32 {
    type Output = Unit<Quantity,Type,f32>;
    fn mul(self, rhs:Unit<Quantity,Type,f32>) -> Self::Output {
        (self * rhs.value).into()
    }
}

impl<'t,Quantity,Type> Mul<&'t Unit<Quantity,Type,f32>> for &'t f32 {
    type Output = Unit<Quantity,Type,f32>;
    fn mul(self, rhs:&'t Unit<Quantity,Type,f32>) -> Self::Output {
        (self * rhs.value).into()
    }
}

impl<Quantity,Type,V> Neg for Unit<Quantity,Type,V>
    where V:Neg<Output=V> {
    type Output = Unit<Quantity,Type,V>;
    fn neg(self) -> Self::Output {
        (-self.value).into()
    }
}

impl<'t,Quantity,Type,V> Neg for &'t Unit<Quantity,Type,V>
    where &'t V : Neg {
    type Output = Unit<Quantity,Type,<&'t V as Neg>::Output>;
    fn neg(self) -> Self::Output {
        (-&self.value).into()
    }
}


// ================
// === Quantity ===
// ================

macro_rules! define_quantities {
    ( $($name:ident),* $(,)? ) => {
        /// Module containing popular quantity marker types.
        #[allow(missing_docs)]
        pub mod quantity {$(
            #[derive(Clone,Copy,Debug,Eq,PartialEq)]
            pub struct $name {}
        )*}
    }
}

define_quantities! {Distance,Angle}



// ================
// === Distance ===
// ================

/// Distance parametrized by the unit type.
pub type Distance<Type=Anything,Repr=f32> = Unit<quantity::Distance,Type,Repr>;


// === Pixels ===

/// Pixel distance unit type.
#[derive(Clone,Copy,Debug,Eq,PartialEq)]
pub struct Pixels;

/// Provides a `px` method to every unit that can be converted to a pixel distance.
pub trait PixelDistance {
    /// Distance in pixels.
    fn px(&self) -> Distance<Pixels>;
}

impl PixelDistance for f32 {
    fn px(&self) -> Distance<Pixels> {
        Distance::new(*self)
    }
}

impl PixelDistance for i32 {
    fn px(&self) -> Distance<Pixels> {
        Distance::new(*self as f32)
    }
}

impls! { From + &From <Distance<Pixels>> for Glsl { |t| { t.value.into() } }}

impls! { From<PhantomData<Distance<Pixels>>> for glsl::PrimType {
    |_|  { PhantomData::<f32>.into() }
}}

impls! { From<PhantomData<Vector2<Distance<Pixels>>>> for glsl::PrimType {
    |_|  { PhantomData::<Vector2<f32>>.into() }
}}

impls! { From<PhantomData<Vector3<Distance<Pixels>>>> for glsl::PrimType {
    |_|  { PhantomData::<Vector3<f32>>.into() }
}}

impls! { From<PhantomData<Vector4<Distance<Pixels>>>> for glsl::PrimType {
    |_|  { PhantomData::<Vector4<f32>>.into() }
}}



// =============
// === Angle ===
// =============

/// Angle parametrized by the unit type.
pub type Angle<Type=Anything,Repr=f32> = Unit<quantity::Angle,Type,Repr>;

/// Degrees angle unit type.
#[derive(Clone,Copy,Debug,Eq,PartialEq)]
pub struct Degrees;

/// Radians angle unit type.
#[derive(Clone,Copy,Debug,Eq,PartialEq)]
pub struct Radians;

/// Provides a `degrees` and `radians` methods to every unit that can be converted to an angle.
pub trait AngleOps {
    /// Distance in degrees.
    fn degrees(&self) -> Angle<Degrees>;

    /// Distance in radians.
    fn radians(&self) -> Angle<Radians>;

    /// Distance in degrees.
    fn deg(&self) -> Angle<Degrees> { self.degrees() }

    /// Distance in radians.
    fn rad(&self) -> Angle<Radians> { self.radians() }
}

impl AngleOps for f32 {
    fn degrees(&self) -> Angle<Degrees> {
        Angle::new(*self)
    }

    fn radians(&self) -> Angle<Radians> {
        Angle::new(*self)
    }
}

impl AngleOps for i32 {
    fn degrees(&self) -> Angle<Degrees> {
        Angle::new(*self as f32)
    }

    fn radians(&self) -> Angle<Radians> {
        Angle::new(*self as f32)
    }
}

impls! { From< Angle<Radians>> for Glsl { |t| { iformat!("Radians({t.value.glsl()})").into() } }}
impls! { From<&Angle<Radians>> for Glsl { |t| { iformat!("Radians({t.value.glsl()})").into() } }}
impls! { From< Angle<Degrees>> for Glsl { |t| { iformat!("radians(Degrees({t.value.glsl()}))").into() } }}
impls! { From<&Angle<Degrees>> for Glsl { |t| { iformat!("radians(Degrees({t.value.glsl()}))").into() } }}
impls! { From<PhantomData<Angle<Radians>>> for glsl::PrimType {
    |_|  { "Radians".into() }
}}



// ==============
// === Traits ===
// ==============

/// Commonly used traits.
pub mod traits {
    pub use super::PixelDistance;
    pub use super::AngleOps;
}
