//! Defines unit of measurement abstraction. See: https://en.wikipedia.org/wiki/Unit_of_measurement

use crate::algebra::*;

use std::ops::*;
use std::marker::PhantomData;



// ============
// === Unit ===
// ============

/// Abstraction for any unit type. It is parametrized by:
///   - Quantity, like distance, angle, or mass. See https://en.wikipedia.org/wiki/Quantity .
///   - Type, like pixels, degrees, or radians.
///   - Repr, like f32, or f64.
#[derive(Clone,Copy,Debug,PartialEq)]
pub struct Unit<Quantity,Type,Repr=f32> {
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

impl<Quantity,Type,Repr> From<Repr> for Unit<Quantity,Type,Repr> {
    fn from(t:Repr) -> Self {
        Self::new(t)
    }
}

impl<Quantity,Type> From<Unit<Quantity,Type,f32>> for f32 {
    fn from(t:Unit<Quantity,Type,f32>) -> Self {
        t.value
    }
}

macro_rules! impl_operator_for_unit {
    ( $name:ident $fn:ident $ln:ident $rn:ident <$rhs:ty> for $lhs:ty ) => {
        impl_operator_for_unit_with_ref_rhs! { $name $fn $ln $rn <$rhs> for $lhs [value] }
        impl_operator_for_unit_with_ref_lhs! { $name $fn $ln $rn <$rhs> for $lhs }
    }
}

macro_rules! impl_operator_for_unit_with_ref_rhs {
    ( $name:ident $fn:ident $ln:ident $rn:ident <$rhs:ty> for $lhs:ty $([$rhs_accessor:ident])? ) => {
        impl<Quantity,Type>/*,$ln,$rn>*/ $name<$rhs> for $lhs
        where $ln : $name<$rn> {
            type Output = Unit<Quantity,Type,<$ln as $name<$rn>>::Output>;
            fn $fn(self, rhs:$rhs) -> Self::Output {
                (self.value.$fn(rhs $(.$rhs_accessor)?)).into()
            }
        }

        impl<'t,Quantity,Type>/*,$ln,$rn>*/ $name<$rhs> for &'t $lhs
        where &'t $ln : $name<$rn> {
            type Output = Unit<Quantity,Type,<&'t $ln as $name<$rn>>::Output>;
            fn $fn(self, rhs:$rhs) -> Self::Output {
                ((&self.value).$fn(rhs $(.$rhs_accessor)?)).into()
            }
        }
    }
}

macro_rules! impl_operator_for_unit_with_ref_lhs {
    ( $name:ident $fn:ident $ln:ident $rn:ident <$rhs:ty> for $lhs:ty ) => {
        impl<'t,Quantity,Type>/*,$ln,$rn>*/ $name<&'t $rhs> for &'t $lhs
        where &'t $ln : $name<&'t $rn> {
            type Output = Unit<Quantity,Type,<&'t $ln as $name<&'t $rn>>::Output>;
            fn $fn(self, rhs:&'t $rhs) -> Self::Output {
                ((&self.value).$fn(&rhs.value)).into()
            }
        }

        impl<'t,Quantity,Type>/*,$ln,$rn>*/ $name<&'t $rhs> for $lhs
            where $ln : $name<&'t $rn> {
            type Output = Unit<Quantity,Type,<$ln as $name<&'t $rn>>::Output>;
            fn $fn(self, rhs:&'t $rhs) -> Self::Output {
                (self.value.$fn(&rhs.value)).into()
            }
        }
    }
}


macro_rules! impl_operator_for_prim_type_rhs {
    ( $name:ident $fn:ident $t:ident ) => {
        impl<Quantity,Type> $name<$t> for Unit<Quantity,Type,$t> {
            type Output = Unit<Quantity,Type,$t>;
            fn $fn(self, rhs:$t) -> Self::Output {
                (self.value.$fn(rhs)).into()
            }
        }

        impl<Quantity,Type> $name<$t> for &Unit<Quantity,Type,$t> {
            type Output = Unit<Quantity,Type,$t>;
            fn $fn(self, rhs:$t) -> Self::Output {
                (self.value.$fn(rhs)).into()
            }
        }

        impl<Quantity,Type> $name<&$t> for Unit<Quantity,Type,$t> {
            type Output = Unit<Quantity,Type,$t>;
            fn $fn(self, rhs:&$t) -> Self::Output {
                (self.value.$fn(*rhs)).into()
            }
        }

        impl<Quantity,Type> $name<&$t> for &Unit<Quantity,Type,$t> {
            type Output = Unit<Quantity,Type,$t>;
            fn $fn(self, rhs:&$t) -> Self::Output {
                ((&self.value).$fn(*rhs)).into()
            }
        }
    }
}

impl_operator_for_prim_type_rhs!( Mul mul f32);
impl_operator_for_prim_type_rhs!( Div div f32);

// TODO: The following line (commented) is more generic, but is likely to introduce infinite
//       compilation loop rutc bug.
// impl_operator_for_unit! { Add add Lhs Rhs <Unit<Quantity,Type,Rhs>> for Unit<Quantity,Type,Lhs> }
impl_operator_for_unit! { Add add f32 f32 <Unit<Quantity,Type,f32>> for Unit<Quantity,Type,f32> }
impl_operator_for_unit! { Sub sub f32 f32 <Unit<Quantity,Type,f32>> for Unit<Quantity,Type,f32> }


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
pub type Distance<Type,Repr=f32> = Unit<quantity::Distance,Type,Repr>;


// === Pixels ===

/// Pixel distance unit type.
#[derive(Clone,Copy,Debug,Eq,PartialEq)]
pub struct Pixels;

/// Provides a `px` method to every unit that can be converted to a pixel distance.
#[allow(missing_docs)]
pub trait PixelDistance {
    type Output;
    /// Distance in pixels.
    fn px(&self) -> Self::Output;
}

impl PixelDistance for f32 {
    type Output = Distance<Pixels>;
    fn px(&self) -> Self::Output {
        Distance::new(*self)
    }
}

impl PixelDistance for i32 {
    type Output = Distance<Pixels>;
    fn px(&self) -> Self::Output {
        Distance::new(*self as f32)
    }
}

impl PixelDistance for Vector2<f32> {
    type Output = Vector2<Distance<Pixels>>;
    fn px(&self) -> Self::Output {
        Vector2(Distance::new(self.x),Distance::new(self.y))
    }
}



// =============
// === Angle ===
// =============

/// Angle parametrized by the unit type.
pub type Angle<Type,Repr=f32> = Unit<quantity::Angle,Type,Repr>;

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

impl AngleOps for Angle<Degrees> {
    fn degrees(&self) -> Angle<Degrees> {
        *self
    }

    fn radians(&self) -> Angle<Radians> {
        Angle::new(self.value.to_radians())
    }
}

impl AngleOps for Angle<Radians> {
    fn degrees(&self) -> Angle<Degrees> {
        Angle::new(self.value.to_degrees())
    }

    fn radians(&self) -> Angle<Radians> {
        *self
    }
}



// ==============
// === Traits ===
// ==============

/// Commonly used traits.
pub mod traits {
    pub use super::PixelDistance;
    pub use super::AngleOps;
}
