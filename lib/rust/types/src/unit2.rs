use std::marker::PhantomData;
use std::ops::Deref;


mod ops {
    pub use crate::algebra::*;
    pub use std::ops::*;
}

// ================
// === UnitData ===
// ================

pub trait Variant {
    type Repr;
}

pub type Unit<V> = UnitData<V, <V as Variant>::Repr>;

pub struct UnitData<V, R = f32> {
    repr:    R,
    variant: PhantomData<V>,
}

impl<V, R> UnitData<V, R> {
    pub const fn new(repr: R) -> Self {
        let variant = PhantomData;
        Self { repr, variant }
    }
}

impl<V, R: Copy> Copy for UnitData<V, R> {}
impl<V, R: Copy> Clone for UnitData<V, R> {
    fn clone(&self) -> Self {
        *self
    }
}



// =================
// === IsNotUnit ===
// =================

auto trait IsNotUnit {}
impl<V, R> !IsNotUnit for UnitData<V, R> {}



// ===============
// === Default ===
// ===============

impl<V, R: Default> Default for UnitData<V, R> {
    fn default() -> Self {
        UnitData::new(Default::default())
    }
}


// =======================
// === Debug / Display ===
// =======================

impl<V, R: std::fmt::Debug> std::fmt::Debug for UnitData<V, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Unit({:?})", self.repr)
    }
}

impl<V, R: std::fmt::Display> std::fmt::Display for UnitData<V, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.repr.fmt(f)
    }
}



// =====================
// === Deref / AsRef ===
// =====================

impl<V, R> Deref for UnitData<V, R> {
    type Target = R;
    fn deref(&self) -> &Self::Target {
        &self.repr
    }
}

impl<V, R> AsRef<UnitData<V, R>> for UnitData<V, R> {
    fn as_ref(&self) -> &UnitData<V, R> {
        &self
    }
}

impl<V, R> AsRef<R> for UnitData<V, R> {
    fn as_ref(&self) -> &R {
        &self.repr
    }
}



// ==========
// === Eq ===
// ==========

impl<V, R: PartialEq> Eq for UnitData<V, R> {}
impl<V, R: PartialEq> PartialEq for UnitData<V, R> {
    fn eq(&self, other: &Self) -> bool {
        self.repr.eq(&other.repr)
    }
}



// ============
// === From ===
// ============

impl<V, R> From<&UnitData<V, R>> for UnitData<V, R>
where R: Copy
{
    fn from(t: &UnitData<V, R>) -> Self {
        *t
    }
}

impl<V, R, T> From<T> for UnitData<V, R>
where T: IsNotUnit + Into<R>
{
    fn from(t: T) -> Self {
        Self::new(t.into())
    }
}



// ===========
// === Mul ===
// ===========


macro_rules! gen_ops {
    ($rev_trait:ident, $trait:ident, $op:ident) => {
        pub trait $trait<T> {
            type Output;
        }

        pub trait $rev_trait<T> {
            type Output;
        }

        impl<V, R> const ops::$trait<UnitData<V, R>> for f32
        where
            R: Copy,
            V: $rev_trait<f32>,
            f32: ~const ops::$trait<R>,
        {
            type Output = UnitData<<V as $rev_trait<f32>>::Output, <f32 as ops::$trait<R>>::Output>;
            fn $op(self, rhs: UnitData<V, R>) -> Self::Output {
                UnitData::new(self.$op(rhs.repr))
            }
        }

        impl<V, R, T> const ops::$trait<T> for UnitData<V, R>
        where
            V: $trait<T>,
            R: ~const ops::$trait<T> + Copy,
            T: IsNotUnit,
        {
            type Output = UnitData<<V as $trait<T>>::Output, <R as ops::$trait<T>>::Output>;
            fn $op(self, rhs: T) -> Self::Output {
                UnitData::new(self.repr.$op(rhs))
            }
        }

        impl<V1, V2, R1, R2> const ops::$trait<UnitData<V2, R2>> for UnitData<V1, R1>
        where
            V1: $trait<V2>,
            R1: ~const ops::$trait<R2> + Copy,
            R2: Copy,
        {
            type Output = UnitData<<V1 as $trait<V2>>::Output, <R1 as ops::$trait<R2>>::Output>;
            fn $op(self, rhs: UnitData<V2, R2>) -> Self::Output {
                UnitData::new(self.repr.$op(rhs.repr))
            }
        }
    };
}

gen_ops!(RevAdd, Add, add);
gen_ops!(RevSub, Sub, sub);
gen_ops!(RevMul, Mul, mul);
gen_ops!(RevDiv, Div, div);
gen_ops!(SaturatingRevAdd, SaturatingAdd, saturating_add);
gen_ops!(SaturatingRevSub, SaturatingSub, saturating_sub);
gen_ops!(SaturatingRevMul, SaturatingMul, saturating_mul);


#[macro_export]
macro_rules! define_single_op {
    ($lhs:ident + $rhs:ident = $out:ident) => {
        impl Add<$rhs> for $lhs {
            type Output = $out;
        }
    };

    ($lhs:ident - $rhs:ident = $out:ident) => {
        impl Sub<$rhs> for $lhs {
            type Output = $out;
        }
    };

    ($lhs:ident * $rhs:ident = $out:ident) => {
        impl Mul<$rhs> for $lhs {
            type Output = $out;
        }
    };

    ($lhs:ident / $rhs:ident = $out:ident) => {
        impl Div<$rhs> for $lhs {
            type Output = $out;
        }
    };

    ($lhs:ident [$($op:tt),* $(,)?] $rhs:ident = $out:ident) => {
        $(
            $crate::define_single_op!{$lhs $op $rhs = $out}
        )*
    };
}

#[macro_export]
macro_rules! define_ops {
    ($($lhs:ident $op:tt $rhs:ident = $out:ident),* $(,)?) => {
        $(
            $crate::define_single_op!{ $lhs $op $rhs = $out }
        )*
    };
}

#[macro_export]
macro_rules! define_single_rev_op {
    ($lhs:ident + $rhs:ident = $out:ident) => {
        impl RevAdd<$rhs> for $lhs {
            type Output = $out;
        }
    };

    ($lhs:ident - $rhs:ident = $out:ident) => {
        impl RevSub<$rhs> for $lhs {
            type Output = $out;
        }
    };

    ($lhs:ident * $rhs:ident = $out:ident) => {
        impl RevMul<$rhs> for $lhs {
            type Output = $out;
        }
    };

    ($lhs:ident / $rhs:ident = $out:ident) => {
        impl RevDiv<$rhs> for $lhs {
            type Output = $out;
        }
    };

    ($lhs:ident [$($op:tt),* $(,)?] $rhs:ident = $out:ident) => {
        $(
            $crate::define_single_rev_op!{$lhs $op $rhs = $out}
        )*
    };
}

#[macro_export]
macro_rules! define_rev_ops {
    ($($lhs:ident $op:tt $rhs:ident = $out:ident),* $(,)?) => {
        $(
            $crate::define_single_rev_op!{ $lhs $op $rhs = $out }
        )*
    };
}


#[macro_export]
macro_rules! define {
    ($name:ident = $variant:ident ($tp:ident)) => {
        pub type $name = Unit<$variant>;
        pub fn $name(val: $tp) -> $name {
            $name::new(val)
        }
        pub struct $variant;
        impl Variant for $variant {
            type Repr = $tp;
        }
    };
}



// ======================
// === Standard Units ===
// ======================

// === Milliseconds ===

define!(Milliseconds = MILLISECONDS(f32));
define_ops![MILLISECONDS [+,-] MILLISECONDS = MILLISECONDS, MILLISECONDS [*,/] f32 = MILLISECONDS];


// === Meters ===

define!(Meters = METERS(f32));
define_ops![METERS [+,-,*,/] f32 = METERS, METERS * METERS = SQUARE_METERS];


// === SquareMeters ===

define!(SquareMeters = SQUARE_METERS(f32));
define_ops![SQUARE_METERS [+,-,*,/] f32 = SQUARE_METERS, SQUARE_METERS / METERS = METERS];
