use std::marker::PhantomData;


mod ops {
    pub use crate::algebra::*;
    pub use std::ops::*;
}

pub mod traits {
    pub use super::DurationNumberOps;
    pub use super::DurationOps;
}



// =====================
// === UncheckedInto ===
// =====================

impl<T> const UncheckedInto<T> for T {
    fn unchecked_into(self) -> T {
        self
    }
}

impl<V, R> const UncheckedInto<UnitData<V, R>> for R {
    fn unchecked_into(self) -> UnitData<V, R> {
        UnitData::unchecked_from(self)
    }
}



// ================
// === UnitData ===
// ================

pub trait Variant {
    type Repr;
}

pub type Unit<V> = UnitData<V, <V as Variant>::Repr>;

pub struct UnitData<V, R> {
    repr:    R,
    variant: PhantomData<V>,
}

impl<V, R> UnitData<V, R> {
    pub const fn unchecked_from(repr: R) -> Self {
        let variant = PhantomData;
        Self { repr, variant }
    }
}

impl<V, R: Copy> UnitData<V, R> {
    pub const fn unchecked_raw(self) -> R {
        self.repr
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
        UnitData::unchecked_from(Default::default())
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

impl<V, R> AsRef<UnitData<V, R>> for UnitData<V, R> {
    fn as_ref(&self) -> &UnitData<V, R> {
        &self
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



// ===========
// === Ord ===
// ===========

impl<V, R: Ord> Ord for UnitData<V, R> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.repr.cmp(&other.repr)
    }
}

impl<V, R: PartialOrd> PartialOrd for UnitData<V, R> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.repr.partial_cmp(&other.repr)
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

        // Please note that this impl is not as generic as the following ones because Rust compiler
        // is unable to compile the more generic version.
        impl<V, R> const ops::$trait<UnitData<V, R>> for f32
        where
            R: Copy,
            V: $rev_trait<f32>,
            f32: ~const ops::$trait<R>,
        {
            type Output = UnitData<<V as $rev_trait<f32>>::Output, <f32 as ops::$trait<R>>::Output>;
            fn $op(self, rhs: UnitData<V, R>) -> Self::Output {
                UnitData::unchecked_from(self.$op(rhs.repr))
            }
        }

        impl<V, R, T> const ops::$trait<T> for UnitData<V, R>
        where
            UnitData<V, R>: $trait<T>,
            R: ~const ops::$trait<T> + Copy,
            T: IsNotUnit,
            <R as ops::$trait<T>>::Output:
                ~const UncheckedInto<<UnitData<V, R> as $trait<T>>::Output>,
        {
            type Output = <UnitData<V, R> as $trait<T>>::Output;
            fn $op(self, rhs: T) -> Self::Output {
                self.repr.$op(rhs).unchecked_into()
            }
        }

        impl<V1, V2, R1, R2> const ops::$trait<UnitData<V2, R2>> for UnitData<V1, R1>
        where
            UnitData<V1, R1>: $trait<UnitData<V2, R2>>,
            R1: ~const ops::$trait<R2> + Copy,
            R2: Copy,
            <R1 as ops::$trait<R2>>::Output:
                ~const UncheckedInto<<UnitData<V1, R1> as $trait<UnitData<V2, R2>>>::Output>,
        {
            type Output = <UnitData<V1, R1> as $trait<UnitData<V2, R2>>>::Output;
            fn $op(self, rhs: UnitData<V2, R2>) -> Self::Output {
                self.repr.$op(rhs.repr).unchecked_into()
            }
        }
    };
}

macro_rules! gen_ops_mut {
    ($rev_trait:ident, $trait:ident, $trait_mut:ident, $op:ident) => {
        impl<V, R> const ops::$trait_mut<UnitData<V, R>> for f32
        where
            f32: ~const ops::$trait_mut<R>,
            R: Copy,
            UnitData<V, R>: $rev_trait<f32>,
        {
            fn $op(&mut self, rhs: UnitData<V, R>) {
                self.$op(rhs.repr)
            }
        }
        impl<V, R, T> const ops::$trait_mut<T> for UnitData<V, R>
        where
            T: IsNotUnit,
            R: ~const ops::$trait_mut<T>,
            UnitData<V, R>: $trait<T>,
        {
            fn $op(&mut self, rhs: T) {
                self.repr.$op(rhs)
            }
        }
        impl<V1, V2, R1, R2> const ops::$trait_mut<UnitData<V2, R2>> for UnitData<V1, R1>
        where
            R1: ~const ops::$trait_mut<R2>,
            R2: Copy,
            UnitData<V1, R1>: $trait<UnitData<V2, R2>>,
        {
            fn $op(&mut self, rhs: UnitData<V2, R2>) {
                self.repr.$op(rhs.repr)
            }
        }
    };
}

pub trait UncheckedInto<T> {
    fn unchecked_into(self) -> T;
}

gen_ops!(RevAdd, Add, add);
gen_ops!(RevSub, Sub, sub);
gen_ops!(RevMul, Mul, mul);
gen_ops!(RevDiv, Div, div);
gen_ops!(SaturatingRevAdd, SaturatingAdd, saturating_add);
gen_ops!(SaturatingRevSub, SaturatingSub, saturating_sub);
gen_ops!(SaturatingRevMul, SaturatingMul, saturating_mul);
gen_ops_mut!(RevAdd, Add, AddAssign, add_assign);
gen_ops_mut!(RevSub, Sub, SubAssign, sub_assign);
gen_ops_mut!(RevMul, Mul, MulAssign, mul_assign);
gen_ops_mut!(RevDiv, Div, DivAssign, div_assign);



// ==============================
// === Unit Definition Macros ===
// ==============================

#[macro_export]
macro_rules! define_single_op_switch {
    (f32 $op:tt $rhs:ident = $out:ident) => {
        $crate::define_single_rev_op! {f32 $op $rhs = $out}
    };
    (f64 $op:tt $rhs:ident = $out:ident) => {
        $crate::define_single_rev_op! {f64 $op $rhs = $out}
    };
    ($lhs:ident $op:tt $rhs:ident = $out:ident) => {
        $crate::define_single_op! {$lhs $op $rhs = $out}
    };
}

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
            $crate::define_single_op_switch!{ $lhs $op $rhs = $out }
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
macro_rules! define {
    ($name:ident = $variant:ident ($tp:ident)) => {
        pub type $name = Unit<$variant>;
        pub struct $variant;
        impl Variant for $variant {
            type Repr = $tp;
        }
    };
}



// ================
// === Duration ===
// ================

define!(Duration = DURATION(f32));
define_ops![
    Duration [+,-] Duration = Duration,
    Duration [*,/] f32 = Duration,
    Duration / Duration = f32,
    f32 * Duration = Duration,
];

pub trait DurationOps {
    fn ms(t: f32) -> Duration;
    fn s(t: f32) -> Duration;
    fn min(t: f32) -> Duration;
    fn h(t: f32) -> Duration;
    fn as_ms(&self) -> f32;
    fn as_s(&self) -> f32;
    fn as_min(&self) -> f32;
    fn as_h(&self) -> f32;
}

impl const DurationOps for Duration {
    fn ms(t: f32) -> Duration {
        Self::unchecked_from(t)
    }
    fn s(t: f32) -> Duration {
        Self::ms(t * 1000.0)
    }
    fn min(t: f32) -> Duration {
        Self::s(t * 60.0)
    }
    fn h(t: f32) -> Duration {
        Self::min(t * 60.0)
    }

    fn as_ms(&self) -> f32 {
        self.unchecked_raw()
    }
    fn as_s(&self) -> f32 {
        self.as_ms() / 1000.0
    }
    fn as_min(&self) -> f32 {
        self.as_s() / 60.0
    }
    fn as_h(&self) -> f32 {
        self.as_min() / 60.0
    }
}

pub trait DurationNumberOps {
    fn ms(self) -> Duration;
    fn s(self) -> Duration;
}

impl const DurationNumberOps for f32 {
    fn ms(self) -> Duration {
        Duration::ms(self)
    }

    fn s(self) -> Duration {
        Duration::s(self)
    }
}
