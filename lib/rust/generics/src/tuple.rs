//! This module contains implementations of generic operations on tuples.

use crate as hlist;
use nalgebra::base::dimension::*;



// ====================
// === HasTupleRepr ===
// ====================

/// All types which have a tuple representation.
#[allow(missing_docs)]
pub trait HasTupleRepr {
    type TupleRepr;
}

/// Tuple representation of a type.
pub type TupleRepr<T> = <T as HasTupleRepr>::TupleRepr;

/// Conversion of the given type to its tuple representation.
#[allow(missing_docs)]
pub trait IntoTuple : HasTupleRepr + Into<TupleRepr<Self>> {
    fn into_tuple(self) -> TupleRepr<Self> {
        self.into()
    }
}

impl<T> IntoTuple for T where T : HasTupleRepr + Into<TupleRepr<T>> {}



// ===================
// === GenericRepr ===
// ===================

macro_rules! gen_as_hlist_for_tuples {
    () => {};
    ($t:ident $(,$($ts:ident),*)?) => {
        impl <$($($ts),*)?> $crate::HasRepr for ($($($ts,)*)?) {
            type GenericRepr = hlist::ty! { $($($ts),*)? };
        }
        gen_as_hlist_for_tuples! { $($($ts),*)? }
    }
}

gen_as_hlist_for_tuples! {T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12}




// =============================
// === KnownLast / KnownInit ===
// =============================

macro_rules! gen_known_last {
    () => {};
    ($t:ident $(,$($ts:ident),*)?) => {
        impl<X $(,$($ts),*)?> $crate::KnownLast for ($($($ts,)*)? X,) { type Last = X; }
        gen_known_last! { $($($ts),*)? }
    }
}

macro_rules! gen_known_init {
    () => {};
    ($t:ident $(,$($ts:ident),*)?) => {
        impl<X $(,$($ts),*)?> $crate::KnownInit for ($($($ts,)*)? X,) { type Init = ($($($ts,)*)?); }
        gen_known_init! { $($($ts),*)? }
    }
}

gen_known_last!{T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_known_init!{T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}




// ================
// === PushBack ===
// ================

impl<X> hlist::PushBack<X>
for () {
    type Output = (X,);
    fn push_back(self,x:X) -> Self::Output {
        (x,)
    }
}

impl<X,T0> hlist::PushBack<X>
for (T0,) {
    type Output = (T0,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,x)
    }
}

impl<X,T0,T1> hlist::PushBack<X>
for (T0,T1) {
    type Output = (T0,T1,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,x)
    }
}

impl<X,T0,T1,T2> hlist::PushBack<X>
for (T0,T1,T2) {
    type Output = (T0,T1,T2,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,x)
    }
}

impl<X,T0,T1,T2,T3> hlist::PushBack<X>
for (T0,T1,T2,T3) {
    type Output = (T0,T1,T2,T3,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,self.3,x)
    }
}

impl<X,T0,T1,T2,T3,T4> hlist::PushBack<X>
for (T0,T1,T2,T3,T4) {
    type Output = (T0,T1,T2,T3,T4,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,self.3,self.4,x)
    }
}

impl<X,T0,T1,T2,T3,T4,T5> hlist::PushBack<X>
for (T0,T1,T2,T3,T4,T5) {
    type Output = (T0,T1,T2,T3,T4,T5,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,self.3,self.4,self.5,x)
    }
}

impl<X,T0,T1,T2,T3,T4,T5,T6> hlist::PushBack<X>
for (T0,T1,T2,T3,T4,T5,T6) {
    type Output = (T0,T1,T2,T3,T4,T5,T6,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,self.3,self.4,self.5,self.6,x)
    }
}

impl<X,T0,T1,T2,T3,T4,T5,T6,T7> hlist::PushBack<X>
for (T0,T1,T2,T3,T4,T5,T6,T7) {
    type Output = (T0,T1,T2,T3,T4,T5,T6,T7,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,self.3,self.4,self.5,self.6,self.7,x)
    }
}

impl<X,T0,T1,T2,T3,T4,T5,T6,T7,T8> hlist::PushBack<X>
for (T0,T1,T2,T3,T4,T5,T6,T7,T8) {
    type Output = (T0,T1,T2,T3,T4,T5,T6,T7,T8,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,self.3,self.4,self.5,self.6,self.7,self.8,x)
    }
}

impl<X,T0,T1,T2,T3,T4,T5,T6,T7,T8,T9> hlist::PushBack<X>
for (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9) {
    type Output = (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,self.3,self.4,self.5,self.6,self.7,self.8,self.9,x)
    }
}

impl<X,T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10> hlist::PushBack<X>
for (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) {
    type Output = (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,X);
    fn push_back(self,x:X) -> Self::Output {
        (self.0,self.1,self.2,self.3,self.4,self.5,self.6,self.7,self.8,self.9,self.10,x)
    }
}




// ===============
// === PopBack ===
// ===============

impl<T0> hlist::PopBack
for (T0,) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.0,())
    }
}

impl<T0,T1> hlist::PopBack
for (T0,T1) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.1,(self.0,))
    }
}

impl<T0,T1,T2> hlist::PopBack
for (T0,T1,T2) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.2,(self.0,self.1))
    }
}

impl<T0,T1,T2,T3> hlist::PopBack
for (T0,T1,T2,T3) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.3,(self.0,self.1,self.2))
    }
}

impl<T0,T1,T2,T3,T4> hlist::PopBack
for (T0,T1,T2,T3,T4) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.4,(self.0,self.1,self.2,self.3))
    }
}

impl<T0,T1,T2,T3,T4,T5> hlist::PopBack
for (T0,T1,T2,T3,T4,T5) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.5,(self.0,self.1,self.2,self.3,self.4))
    }
}

impl<T0,T1,T2,T3,T4,T5,T6> hlist::PopBack
for (T0,T1,T2,T3,T4,T5,T6) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.6,(self.0,self.1,self.2,self.3,self.4,self.5))
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7> hlist::PopBack
for (T0,T1,T2,T3,T4,T5,T6,T7) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.7,(self.0,self.1,self.2,self.3,self.4,self.5,self.6))
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8> hlist::PopBack
for (T0,T1,T2,T3,T4,T5,T6,T7,T8) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.8,(self.0,self.1,self.2,self.3,self.4,self.5,self.6,self.7))
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9> hlist::PopBack
for (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.9,(self.0,self.1,self.2,self.3,self.4,self.5,self.6,self.7,self.8))
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10> hlist::PopBack
for (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.10,(self.0,self.1,self.2,self.3,self.4,self.5,self.6,self.7,self.8,self.9))
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11> hlist::PopBack
for (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        (self.11,(self.0,self.1,self.2,self.3,self.4,self.5,self.6,self.7,self.8,self.9,self.10))
    }
}



// =================================
// === Conversion Tuple -> HList ===
// =================================

impl From<()>
for hlist::ty![] {
    #[inline(always)]
    fn from(_:()) -> Self {
        hlist::new![]
    }
}

impl<T0> From<(T0,)>
for hlist::ty![T0] {
    #[inline(always)]
    fn from(t:(T0,)) -> Self {
        hlist::new![t.0]
    }
}

impl<T0,T1> From<(T0,T1,)>
for hlist::ty![T0,T1] {
    #[inline(always)]
    fn from(t:(T0,T1,)) -> Self {
        hlist::new![t.0,t.1]
    }
}

impl<T0,T1,T2> From<(T0,T1,T2,)>
for hlist::ty![T0,T1,T2] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,)) -> Self {
        hlist::new![t.0,t.1,t.2]
    }
}

impl<T0,T1,T2,T3> From<(T0,T1,T2,T3,)>
for hlist::ty![T0,T1,T2,T3] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3]
    }
}

impl<T0,T1,T2,T3,T4> From<(T0,T1,T2,T3,T4,)>
for hlist::ty![T0,T1,T2,T3,T4] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,T4,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3,t.4]
    }
}

impl<T0,T1,T2,T3,T4,T5> From<(T0,T1,T2,T3,T4,T5,)>
for hlist::ty![T0,T1,T2,T3,T4,T5] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,T4,T5,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3,t.4,t.5]
    }
}

impl<T0,T1,T2,T3,T4,T5,T6> From<(T0,T1,T2,T3,T4,T5,T6,)>
for hlist::ty![T0,T1,T2,T3,T4,T5,T6] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,T4,T5,T6,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3,t.4,t.5,t.6]
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7> From<(T0,T1,T2,T3,T4,T5,T6,T7,)>
for hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,T4,T5,T6,T7,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3,t.4,t.5,t.6,t.7]
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8> From<(T0,T1,T2,T3,T4,T5,T6,T7,T8,)>
for hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,T4,T5,T6,T7,T8,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3,t.4,t.5,t.6,t.7,t.8]
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9> From<(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,)>
for hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3,t.4,t.5,t.6,t.7,t.8,t.9]
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10> From<(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,)>
for hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3,t.4,t.5,t.6,t.7,t.8,t.9,t.10]
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11> From<(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,)>
for hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] {
    #[inline(always)]
    fn from(t:(T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,)) -> Self {
        hlist::new![t.0,t.1,t.2,t.3,t.4,t.5,t.6,t.7,t.8,t.9,t.10,t.11]
    }
}




// =================================
// === Conversion HList -> Tuple ===
// =================================

impl From<hlist::ty![]>
for () {
    #[inline(always)]
    fn from(_:hlist::ty![]) -> Self {}
}

impl<T0> From<hlist::ty![T0]>
for (T0,) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,]) -> Self {
        let hlist::pat![t0] = value;
        (t0,)
    }
}

impl<T0,T1> From<hlist::ty![T0,T1]>
for (T0,T1) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1]) -> Self {
        let hlist::pat![t0,t1] = value;
        (t0,t1)
    }
}

impl<T0,T1,T2> From<hlist::ty![T0,T1,T2]>
for (T0,T1,T2) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2]) -> Self {
        let hlist::pat![t0,t1,t2] = value;
        (t0,t1,t2)
    }
}

impl<T0,T1,T2,T3> From<hlist::ty![T0,T1,T2,T3]>
for (T0,T1,T2,T3) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3]) -> Self {
        let hlist::pat![t0,t1,t2,t3] = value;
        (t0,t1,t2,t3)
    }
}

impl<T0,T1,T2,T3,T4> From<hlist::ty![T0,T1,T2,T3,T4]>
for (T0,T1,T2,T3,T4) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3,T4]) -> Self {
        let hlist::pat![t0,t1,t2,t3,t4] = value;
        (t0,t1,t2,t3,t4)
    }
}

impl<T0,T1,T2,T3,T4,T5> From<hlist::ty![T0,T1,T2,T3,T4,T5]>
for (T0,T1,T2,T3,T4,T5) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3,T4,T5]) -> Self {
        let hlist::pat![t0,t1,t2,t3,t4,t5] = value;
        (t0,t1,t2,t3,t4,t5)
    }
}

impl<T0,T1,T2,T3,T4,T5,T6> From<hlist::ty![T0,T1,T2,T3,T4,T5,T6]>
for (T0,T1,T2,T3,T4,T5,T6) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3,T4,T5,T6]) -> Self {
        let hlist::pat![t0,t1,t2,t3,t4,t5,t6] = value;
        (t0,t1,t2,t3,t4,t5,t6)
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7> From<hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7]>
for (T0,T1,T2,T3,T4,T5,T6,T7) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7]) -> Self {
        let hlist::pat![t0,t1,t2,t3,t4,t5,t6,t7] = value;
        (t0,t1,t2,t3,t4,t5,t6,t7)
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8> From<hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8]>
for (T0,T1,T2,T3,T4,T5,T6,T7,T8) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8]) -> Self {
        let hlist::pat![t0,t1,t2,t3,t4,t5,t6,t7,t8] = value;
        (t0,t1,t2,t3,t4,t5,t6,t7,t8)
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9> From<hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9]>
for (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9]) -> Self {
        let hlist::pat![t0,t1,t2,t3,t4,t5,t6,t7,t8,t9] = value;
        (t0,t1,t2,t3,t4,t5,t6,t7,t8,t9)
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10> From<hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]>
for (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) -> Self {
        let hlist::pat![t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10] = value;
        (t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
    }
}

impl<T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11> From<hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]>
for (T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) {
    #[inline(always)]
    fn from(value:hlist::ty![T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) -> Self {
        let hlist::pat![t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11] = value;
        (t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11)
    }
}



// ==============================
// === HasTupleRepr for HList ===
// ==============================

impl HasTupleRepr
for hlist::ty![] {
    type TupleRepr = ();
}

impl<T1> HasTupleRepr
for hlist::ty![T1] {
    type TupleRepr = (T1,);
}

impl<T1,T2> HasTupleRepr
for hlist::ty![T1,T2] {
    type TupleRepr = (T1,T2);
}

impl<T1,T2,T3> HasTupleRepr
for hlist::ty![T1,T2,T3] {
    type TupleRepr = (T1,T2,T3);
}

impl<T1,T2,T3,T4> HasTupleRepr
for hlist::ty![T1,T2,T3,T4] {
    type TupleRepr = (T1,T2,T3,T4);
}

impl<T1,T2,T3,T4,T5> HasTupleRepr
for hlist::ty![T1,T2,T3,T4,T5] {
    type TupleRepr = (T1,T2,T3,T4,T5);
}

impl<T1,T2,T3,T4,T5,T6> HasTupleRepr
for hlist::ty![T1,T2,T3,T4,T5,T6] {
    type TupleRepr = (T1,T2,T3,T4,T5,T6);
}

impl<T1,T2,T3,T4,T5,T6,T7> HasTupleRepr
for hlist::ty![T1,T2,T3,T4,T5,T6,T7] {
    type TupleRepr = (T1,T2,T3,T4,T5,T6,T7);
}

impl<T1,T2,T3,T4,T5,T6,T7,T8> HasTupleRepr
for hlist::ty![T1,T2,T3,T4,T5,T6,T7,T8] {
    type TupleRepr = (T1,T2,T3,T4,T5,T6,T7,T8);
}

impl<T1,T2,T3,T4,T5,T6,T7,T8,T9> HasTupleRepr
for hlist::ty![T1,T2,T3,T4,T5,T6,T7,T8,T9] {
    type TupleRepr = (T1,T2,T3,T4,T5,T6,T7,T8,T9);
}

impl<T1,T2,T3,T4,T5,T6,T7,T8,T9,T10> HasTupleRepr
for hlist::ty![T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] {
    type TupleRepr = (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10);
}

impl<T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11> HasTupleRepr
for hlist::ty![T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] {
    type TupleRepr = (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11);
}



// =================
// === HasItemAt ===
// =================

macro_rules! gen_has_item_at {
    ($at:ident $p:tt) => {};
    ($at:ident [$($p:ident),*] $t:ident $(,$($ts:ident),*)?) => {
        impl<$($p,)* X $(,$($ts),*)?> $crate::HasItemAt<$at> for ($($p,)*X,$($($ts,)*)?) {
            type Item = X;
        }
        gen_has_item_at! { $at [$($p),*] $($($ts),*)? }
    }
}

gen_has_item_at!{U0  [] T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_has_item_at!{U1  [T0] T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_has_item_at!{U2  [T0,T1] T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_has_item_at!{U3  [T0,T1,T2] T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_has_item_at!{U4  [T0,T1,T2,T3] T4,T5,T6,T7,T8,T9,T10,T11}
gen_has_item_at!{U5  [T0,T1,T2,T3,T4] T5,T6,T7,T8,T9,T10,T11}
gen_has_item_at!{U6  [T0,T1,T2,T3,T4,T5] T6,T7,T8,T9,T10,T11}
gen_has_item_at!{U7  [T0,T1,T2,T3,T4,T5,T6] T7,T8,T9,T10,T11}
gen_has_item_at!{U8  [T0,T1,T2,T3,T4,T5,T6,T7] T8,T9,T10,T11}
gen_has_item_at!{U9  [T0,T1,T2,T3,T4,T5,T6,T7,T8] T9,T10,T11}
gen_has_item_at!{U10 [T0,T1,T2,T3,T4,T5,T6,T7,T8,T9] T10,T11}
gen_has_item_at!{U11 [T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] T11}



// =================
// === GetItemAt ===
// =================

macro_rules! gen_get_item_at {
    ($at:ident $num:tt $p:tt) => {};
    ($at:ident $num:tt [$($p:ident),*] $t:ident $(,$($ts:ident),*)?) => {
        impl<$($p,)* X $(,$($ts),*)?> $crate::GetItemAt<$at> for ($($p,)*X,$($($ts,)*)?) {
            fn get_item_at(&self) -> &X { &self.$num }
        }
        gen_get_item_at! { $at $num [$($p),*] $($($ts),*)? }
    }
}

gen_get_item_at!{U0  0  [] T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_get_item_at!{U1  1  [T0] T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_get_item_at!{U2  2  [T0,T1] T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_get_item_at!{U3  3  [T0,T1,T2] T3,T4,T5,T6,T7,T8,T9,T10,T11}
gen_get_item_at!{U4  4  [T0,T1,T2,T3] T4,T5,T6,T7,T8,T9,T10,T11}
gen_get_item_at!{U5  5  [T0,T1,T2,T3,T4] T5,T6,T7,T8,T9,T10,T11}
gen_get_item_at!{U6  6  [T0,T1,T2,T3,T4,T5] T6,T7,T8,T9,T10,T11}
gen_get_item_at!{U7  7  [T0,T1,T2,T3,T4,T5,T6] T7,T8,T9,T10,T11}
gen_get_item_at!{U8  8  [T0,T1,T2,T3,T4,T5,T6,T7] T8,T9,T10,T11}
gen_get_item_at!{U9  9  [T0,T1,T2,T3,T4,T5,T6,T7,T8] T9,T10,T11}
gen_get_item_at!{U10 10 [T0,T1,T2,T3,T4,T5,T6,T7,T8,T9] T10,T11}
gen_get_item_at!{U11 11 [T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] T11}
