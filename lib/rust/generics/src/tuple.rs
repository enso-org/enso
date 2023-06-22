//! This module contains implementations of generic operations on tuples.

use crate::generic::*;

use crate as hlist;
use crate::HasHListRepr;

use paste::paste;



// ========================
// === Family of tuples ===
// ========================

/// The tuple family.
#[derive(Clone, Copy, Debug)]
pub struct Tuple;

macro_rules! impl_belongs_to_family_for_tuple {
    ($t:tt $(,$ts:tt)*) => {
        paste! {
            impl<$([<T $ts>]),*> BelongsToFamily for ($([<T $ts>],)*) {
                type Family = Tuple;
            }
        }
        impl_belongs_to_family_for_tuple! {$($ts),*}
    };
    () => {}
}

impl_belongs_to_family_for_tuple![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// =====================================
// === impl HasHListRepr for tuples  ===
// =====================================

macro_rules! gen_has_hlist_repr_for_tuples {
    ($($ts:literal),*) => { paste! {
        gen_has_hlist_repr_for_tuples! {@ $([<T $ts>])* }
    }};

    (@) => {};
    (@ $t:tt $($ts:tt)*) => {
        impl <$($ts,)*> HasHListRepr for ($($ts,)*) {
            type HListRepr = hlist::ty! { $($ts,)* };
        }

        impl <'a, $($ts,)*> HasHListRepr for &'a ($($ts,)*) {
            type HListRepr = hlist::ty! { $(&'a $ts),* };
        }

        impl <'a, $($ts,)*> HasHListRepr for &'a mut ($($ts,)*) {
            type HListRepr = hlist::ty! { $(&'a mut $ts,)* };
        }
        gen_has_hlist_repr_for_tuples! {@ $($ts)*}
    };
}

gen_has_hlist_repr_for_tuples![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// =================================
// === Conversion Tuple -> HList ===
// =================================

macro_rules! impl_from_tuple_for_hlist {
    ($($ts:literal),*) => {
        paste! {
            impl_from_tuple_for_hlist! {[] $( [[<T $ts>] $ts] )*}
       }
    };
    ([$( [$t:tt $tn:tt] )*] $r:tt $($rs:tt)*) => {
        impl<$($t),*> From<($($t,)*)> for hlist::ty![$($t),*] {
            #[inline(always)]
            fn from(_t: ($($t,)*)) -> Self {
                hlist::new![$(_t.$tn),*]
            }
        }

        impl<'a, $($t),*> From<&'a ($($t,)*)> for hlist::ty![$(&'a $t),*] {
            #[inline(always)]
            fn from(_t: &'a ($($t,)*)) -> Self {
                hlist::new![$(&_t.$tn),*]
            }
        }

        impl<'a, $($t),*> From<&'a mut ($($t,)*)> for hlist::ty![$(&'a mut $t),*] {
            #[inline(always)]
            fn from(_t: &'a mut ($($t,)*)) -> Self {
                hlist::new![$(&mut _t.$tn),*]
            }
        }

        impl_from_tuple_for_hlist! {[$([$t $tn])* $r] $($rs)*}
    };
    ($ts:tt) => {}
}

impl_from_tuple_for_hlist![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// =================================
// === Conversion HList -> Tuple ===
// =================================

macro_rules! impl_from_hlist_for_tuple {
    () => {};
    ($($ts:literal),*) => { paste! {
        impl_from_hlist_for_tuple! {$([<T $ts>])*}
    }};
    ($t:tt $($ts:tt)*) => {
        impl<$($ts),*> IntoFamily<Tuple> for crate::ty![$($ts),*] {
            type Output = ($($ts,)*);
            #[allow(clippy::unused_unit)]
            fn _into_family(self) -> Self::Output {
                #[allow(non_snake_case)]
                let crate::pat![$($ts),*] = self;
                ($($ts,)*)
            }
        }

        impl<'t, $($ts),*> IntoFamily<Tuple> for &'t crate::ty![$($ts),*] {
            type Output = ($(&'t $ts,)*);
            #[allow(clippy::unused_unit)]
            fn _into_family(self) -> Self::Output {
                #[allow(non_snake_case)]
                let crate::pat![$($ts),*] = self;
                ($($ts,)*)
            }
        }

        impl<'t, $($ts),*> IntoFamily<Tuple> for &'t mut crate::ty![$($ts),*] {
            type Output = ($(&'t mut $ts,)*);
            #[allow(clippy::unused_unit)]
            fn _into_family(self) -> Self::Output {
                #[allow(non_snake_case)]
                let crate::pat![$($ts),*] = self;
                ($($ts,)*)
            }
        }

        impl_from_hlist_for_tuple! { $($ts)* }
    };
}

impl_from_hlist_for_tuple![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
