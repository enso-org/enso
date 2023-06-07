//! This module contains implementations of generic operations on tuples.

use crate as hlist;
use paste::paste;

use crate::hlist::*;
use crate::HasHListRepr;


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
pub trait IntoTuple: HasTupleRepr + Into<TupleRepr<Self>> {
    #[inline(always)]
    fn into_tuple(self) -> TupleRepr<Self> {
        self.into()
    }
}

impl<T> IntoTuple for T where T: HasTupleRepr + Into<TupleRepr<T>> {}



// =====================================
// === impl HasHListRepr for tuples  ===
// =====================================

macro_rules! gen_has_hlist_repr_for_tuples {
    ($r:literal $(,$rs:literal)*) => {
        gen_has_hlist_repr_for_tuples! {[$r] $($rs)* X}
    };
    ([$($t:tt)*] $r:tt $($rs:tt)*) => {
        paste! {
            impl <$([<T $t>]),*> HasHListRepr for ($([<T $t>]),*,) {
                type HListRepr = hlist::ty! { $([<T $t>]),* };
            }

            impl <'a, $([<T $t>]),*> HasHListRepr for &'a ($([<T $t>]),*,) {
                type HListRepr = hlist::ty! { $(&'a [<T $t>]),* };
            }

            impl <'a, $([<T $t>]),*> HasHListRepr for &'a mut ($([<T $t>]),*,) {
                type HListRepr = hlist::ty! { $(&'a mut [<T $t>]),* };
            }
        }
        gen_has_hlist_repr_for_tuples! {[$($t)* $r] $($rs)*}
    };
    ([$($t:tt)*]) => {}
}

gen_has_hlist_repr_for_tuples![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// =================================
// === Conversion Tuple -> HList ===
// =================================

macro_rules! gen_from_tuple_to_hlist {
    ($($rs:literal),*) => {
        gen_from_tuple_to_hlist! {[] $($rs)* X}
    };
    ([$($t:tt)*] $r:tt $($rs:tt)*) => {
        paste! {
            impl<$([<T $t>]),*> From<($([<T $t>],)*)> for hlist::ty![$([<T $t>]),*] {
                #[inline(always)]
                fn from(t: ($([<T $t>],)*)) -> Self {
                    hlist::new![$(t.$t),*]
                }
            }

            impl<'a, $([<T $t>]),*> From<&'a ($([<T $t>],)*)> for hlist::ty![$(&'a [<T $t>]),*] {
                #[inline(always)]
                fn from(t: &'a ($([<T $t>],)*)) -> Self {
                    hlist::new![$(&t.$t),*]
                }
            }
        }
        gen_from_tuple_to_hlist! {[$($t)* $r] $($rs)*}
    };
    ([$($ts:tt)*]) => {}
}

gen_from_tuple_to_hlist![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// =================================
// === Conversion HList -> Tuple ===
// =================================

macro_rules! gen_from_hlist_to_tuple {
    ($($rs:literal),*) => {
        gen_from_hlist_to_tuple! {[] $($rs)* X}
    };
    ([$($t:tt)*] $r:tt $($rs:tt)*) => {
        paste! {
            impl<$([<T $t>]),*> From<hlist::ty![$([<T $t>]),*]> for ($([<T $t>],)*) {
                #[inline(always)]
                fn from(value: hlist::ty![$([<T $t>]),*]) -> Self {
                    let hlist::pat![$([<t $t>]),*] = value;
                    ($([<t $t>],)*)
                }
            }

            impl<'a, $([<T $t>]),*> From<&'a hlist::ty![$([<T $t>]),*]> for ($(&'a [<T $t>],)*) {
                #[inline(always)]
                fn from(value: &'a hlist::ty![$([<T $t>]),*]) -> Self {
                    let hlist::pat![$([<t $t>]),*] = value;
                    ($([<t $t>],)*)
                }
            }
        }
        gen_from_hlist_to_tuple! {[$($t)* $r] $($rs)*}
    };
    ([$($ts:tt)*]) => {}
}

gen_from_hlist_to_tuple![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// ==============================
// === HasTupleRepr for HList ===
// ==============================

macro_rules! impl_has_tuple_repr_for_hlist {
    ($($rs:literal),*) => {
        impl_has_tuple_repr_for_hlist! {[] $($rs)* X}
    };
    ([$($t:tt)*] $r:tt $($rs:tt)*) => {
        paste! {
            impl<$([<T $t>]),*> HasTupleRepr for hlist::ty![$([<T $t>]),*] {
                type TupleRepr = ($([<T $t>],)*);
            }

            impl<'a, $([<T $t>]),*> HasTupleRepr for &'a hlist::ty![$([<T $t>]),*] {
                type TupleRepr = ($(&'a [<T $t>],)*);
            }
        }
        impl_has_tuple_repr_for_hlist! {[$($t)* $r] $($rs)*}
    };
    ([$($ts:tt)*]) => {}
}

impl_has_tuple_repr_for_hlist![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];


#[cfg(test)]
mod tests {
    use super::*;
    use crate::traits::*;
    #[test]
    fn test_field_at() {
        let tuple = (1, "hello", 1);
        // assert_eq!(tuple.field_at::<0>(), 1);
        // assert_eq!(tuple.field_at::<1>(), "hello");
        // assert_eq!(tuple.field_at::<2>(), 1);
    }
}
