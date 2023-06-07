//! Generic representation of data types. Refer to the crate documentation to learn more.

// This crate defines many helper traits and uses this flag on purpose.

// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use super::hlist;
use std::ops::Not;

use hlist::Cons;
use hlist::Nil;
use paste::paste;

#[rustfmt::skip]
macro_rules! dec {
    (1) => { 0 };
    (2) => { 1 };
    (3) => { 2 };
    (4) => { 3 };
    (5) => { 4 };
    (6) => { 5 };
    (7) => { 6 };
    (8) => { 7 };
    (9) => { 8 };
    (10) => { 9 };
    (11) => { 10 };
    (12) => { 11 };
    (13) => { 12 };
    (14) => { 13 };
    (15) => { 14 };
    (16) => { 15 };
}

//
// struct PhantomConstUsize<const N: usize>;
//
// auto trait NotZeroCheck {}
// impl !NotZeroCheck for PhantomConstUsize<0> {}
//
// trait NotZero<const N: usize> = where PhantomConstUsize<N>: NotZeroCheck;


// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    pub use super::get_field_at_x_traits::*;

    pub use super::FieldAt as _TRAIT_FieldAt;
    pub use super::HasFieldAt as _TRAIT_HasFieldAt;
    pub use super::HasFieldCount as _TRAIT_HasFieldsCount;
    pub use super::HasHListRepr as _TRAIT_HasRepr;
    pub use super::IntoFieldAt as _TRAIT_GetFieldAt;
    pub use super::_IntoFieldAt as _TRAIT__IntoFieldAt;
}



// ====================
// === HasHListRepr ===
// ====================

/// A generic representation of the given type. This is a [`HList`] representation of all struct
/// fields.
pub trait HasHListRepr {
    type HListRepr: hlist::HList;
}

impl<'t, 's, T> HasHListRepr for &'t &'s T
where &'s T: HasHListRepr
{
    type HListRepr = <&'s T as HasHListRepr>::HListRepr;
}

/// A generic representation of the given type. This is a [`HList`] representation of all struct
/// fields.
pub type HListRepr<T> = <T as HasHListRepr>::HListRepr;

/// Converts the struct into its generic representation. Please note that this trait is implemented
/// automatically for every type which implements `Into<HListRepr<Self>>`.
pub trait IntoHList: HasHListRepr + Into<HListRepr<Self>> {
    #[inline(always)]
    fn into_hlist(self) -> HListRepr<Self> {
        self.into()
    }
}
impl<T: HasHListRepr + Into<HListRepr<T>>> IntoHList for T {}

// pub trait AsHList
// where for<'t> &'t Self: HasHListRepr + Into<HListRepr<&'t Self>> {
//     #[inline(always)]
//     fn as_hlist(&self) -> HListRepr<&Self> {
//         self.into()
//     }
// }
// impl<T> AsHList for T where for<'t> &'t T: HasHListRepr + Into<HListRepr<&'t T>> {}



// =====================
// === HasFieldCount ===
// =====================

/// Information of field count of any structure implementing [`HasHListRepr`]. This trait is
/// implemented automatically.
pub trait HasFieldCount {
    const FIELD_COUNT: usize;
    #[inline(always)]
    fn field_count() -> usize {
        Self::FIELD_COUNT
    }
}

impl<T: HasHListRepr> HasFieldCount for T {
    const FIELD_COUNT: usize = <HListRepr<T> as hlist::HasConstLength>::LEN;
}



// ==================
// === HasFieldAt ===
// ==================

/// Type of field at the given index. Please note that this is automatically implemented for every
/// struct that implements [`IntoHList`].
///
/// Please note that the ownership type is preserved. For example, `FieldAt<0, (usize, String)>`
/// resolves to `usize`, but `FieldAt<0, &'t(usize, String)>` resolves to `&'t usize`.
pub type FieldAt<const I: usize, T> = <T as HasFieldAt<I>>::Type;

/// Association between field index and field type. Please note that this is automatically
/// implemented for every struct that implements [`IntoHList`].
pub trait HasFieldAt<const I: usize> {
    /// The type of the field. Please note that the ownership should be preserved. For example,
    /// For `impl HasFieldAt<0> for (usize, String)`, the [`Self::Type`] should be resolved to
    /// `usize`, however, for `impl<'t> HasFieldAt<0> for &'t (usize, String)`, the [`Self::Type`]
    /// should be resolved to `&'t usize`.
    type Type;
}

impl<T: HasHListRepr, const N: usize> HasFieldAt<N> for T
where HListRepr<T>: HasFieldAt<N>
{
    type Type = <HListRepr<T> as HasFieldAt<N>>::Type;
}

impl<H, T> HasFieldAt<0> for Cons<H, T> {
    type Type = H;
}

impl<'t, H, T> HasFieldAt<0> for &'t Cons<H, T> {
    type Type = &'t H;
}

impl<'t, H, T> HasFieldAt<0> for &'t mut Cons<H, T> {
    type Type = &'t mut H;
}

macro_rules! impl_field_index_for_hlist {
    () => {};
    ($t:tt $(,$ts:tt)*) => {
        impl<H, T> HasFieldAt<$t> for Cons<H, T>
        where T: HasFieldAt<{dec!($t)}> {
            type Type = <T as HasFieldAt<{dec!($t)}>>::Type;
        }

        impl<'t, H, T> HasFieldAt<$t> for &'t Cons<H, T>
        where &'t T: HasFieldAt<{dec!($t)}> {
            type Type = <&'t T as HasFieldAt<{dec!($t)}>>::Type;
        }

        impl<'t, H, T> HasFieldAt<$t> for &'t mut Cons<H, T>
        where &'t mut T: HasFieldAt<{dec!($t)}> {
            type Type = <&'t mut T as HasFieldAt<{dec!($t)}>>::Type;
        }

        impl_field_index_for_hlist! { $($ts),* }
    };
}

// Rust does not allow for a generic implementation for now, see:
// https://github.com/rust-lang/rust/issues/112341
impl_field_index_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// ================================
// === IntoFieldAt / GetFieldAt ===
// ================================

/// Take the ownership of self and return its I-th field. If self was a reference, it will return
/// a reference to the field. Please note that this trait is implemented automatically for every
/// struct that implements [`IntoHList`].
pub trait IntoFieldAt<const I: usize>: Sized + HasFieldAt<I> {
    fn _into_field_at(self) -> FieldAt<I, Self>;
}

/// Take the ownership of self and return its I-th field. If self was a reference, it will return
/// a reference to the field. Please note that this trait is implemented automatically for every
/// struct that implements [`IntoHList`].
///
/// It is a wrapper for [`IntoFieldAt`] that enables syntax `struct.into_field_at::<0>()`.
impl<T> _IntoFieldAt for T {}
pub trait _IntoFieldAt {
    #[inline(always)]
    fn into_field_at<const I: usize>(self) -> FieldAt<I, Self>
    where Self: IntoFieldAt<I> {
        IntoFieldAt::<I>::_into_field_at(self)
    }
}

/// Return reference to the I-th field of self. Please note that this trait is implemented
/// automatically for every struct that implements [`IntoHList`].
///
/// It is a wrapper for [`IntoFieldAt`] that enables syntax `struct.field_at::<0>()`.
impl<T> _GetFieldAt for T {}
pub trait _GetFieldAt {
    #[inline(always)]
    fn field_at<'t, const I: usize>(&'t self) -> FieldAt<I, &'t Self>
    where &'t Self: IntoFieldAt<I> {
        IntoFieldAt::<I>::_into_field_at(self)
    }
}

/// Default implementation for every struct that implements [`IntoHList`].
impl<T, const N: usize> IntoFieldAt<N> for T
where
    T: HasFieldAt<N> + IntoHList,
    HListRepr<T>: IntoFieldAt<N, Type = FieldAt<N, Self>>,
{
    #[inline(always)]
    fn _into_field_at(self) -> FieldAt<N, Self> {
        self.into_hlist().into_field_at::<N>()
    }
}



// =====================
// === GetFieldAtMut ===
// =====================

/// Return mutable reference to the I-th field of self. Please note that this trait is implemented
/// automatically for every struct that implements [`IntoHList`].
pub trait GetFieldAtMut<'t, const I: usize>
where
    Self: 't,
    &'t mut Self: HasFieldAt<I>, {
    fn _field_at_mut(&'t mut self) -> FieldAt<I, &'t mut Self>;
}

/// Return mutable reference to the I-th field of self. Please note that this trait is implemented
/// automatically for every struct that implements [`IntoHList`].
///
/// It is a wrapper for [`GetFieldAtMut`] that enables syntax `struct.field_at_mut::<0>()`.
impl<T> _GetFieldAtMut for T {}
pub trait _GetFieldAtMut {
    #[inline(always)]
    fn field_at_mut<'t, const I: usize>(&'t mut self) -> FieldAt<I, &'t mut Self>
    where
        Self: GetFieldAtMut<'t, I>,
        &'t mut Self: HasFieldAt<I>, {
        GetFieldAtMut::<I>::_field_at_mut(self)
    }
}

/// Default implementation for every struct that implements [`IntoHList`].
impl<'t, T, const I: usize> GetFieldAtMut<'t, I> for T
where
    T: 't,
    &'t mut T: HasFieldAt<I> + IntoHList,
    HListRepr<&'t mut T>: IntoFieldAt<I, Type = FieldAt<I, &'t mut Self>>,
{
    fn _field_at_mut(&'t mut self) -> FieldAt<I, &'t mut Self> {
        self.into_hlist().into_field_at::<I>()
    }
}


// === HList impls ===

impl<H, T> IntoFieldAt<0> for Cons<H, T> {
    #[inline(always)]
    fn _into_field_at(self) -> FieldAt<0, Self> {
        self.0
    }
}

impl<'t, H, T> IntoFieldAt<0> for &'t Cons<H, T> {
    #[inline(always)]
    fn _into_field_at(self) -> FieldAt<0, Self> {
        &self.0
    }
}

impl<'t, H, T> GetFieldAtMut<'t, 0> for Cons<H, T>
where Self: 't
{
    #[inline(always)]
    fn _field_at_mut(&'t mut self) -> FieldAt<0, &'t mut Self> {
        &mut self.0
    }
}

macro_rules! impl_get_field_at_for_hlist {
    () => {};
    ($t:tt $(,$ts:tt)*) => {
        impl<H, T> IntoFieldAt<$t> for Cons<H, T>
        where
            T: IntoFieldAt<{dec!($t)}>,
            Cons<H, T>: HasFieldAt<$t, Type = <T as HasFieldAt<{dec!($t)}>>::Type>,
        {
            #[inline(always)]
            fn _into_field_at(self) -> FieldAt<$t, Self> {
                self.1.into_field_at::<{dec!($t)}>()
            }
        }

        impl<'t, H, T> IntoFieldAt<$t> for &'t Cons<H, T>
        where
            &'t T: IntoFieldAt<{dec!($t)}>,
            &'t Cons<H, T>: HasFieldAt<$t, Type = <&'t T as HasFieldAt<{dec!($t)}>>::Type>,
        {
            #[inline(always)]
            fn _into_field_at(self) -> FieldAt<$t, Self> {
                (&self.1).into_field_at::<{dec!($t)}>()
            }
        }

        impl<'t, H, T> GetFieldAtMut<'t, $t> for Cons<H, T>
        where
            Self: 't,
            &'t mut Cons<H, T>: HasFieldAt<$t, Type = <&'t mut T as HasFieldAt<{dec!($t)}>>::Type>,
            &'t mut T: HasFieldAt<{dec!($t)}>,
            T: GetFieldAtMut<'t, {dec!($t)}>,
        {
            #[inline(always)]
            fn _field_at_mut(&'t mut self) -> FieldAt<$t, &'t mut Self> {
                self.1.field_at_mut::<{dec!($t)}>()
            }
        }

        impl_get_field_at_for_hlist! { $($ts),* }
    };
}

// Rust does not allow to impl it with default impls, so we need to do it manually.
impl_get_field_at_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// ==================
// === GetFieldAtX ===
// ==================

macro_rules! impl_get_field_at_x_trait {
    () => {};
    ($($t:tt),* $(,)?) => {
        paste! {
            $(
                impl<T: IntoFieldAt<$t>> [<IntoFieldAt $t>] for T {}
                pub trait [<IntoFieldAt $t>]: IntoFieldAt<$t> {
                    #[inline(always)]
                    fn [<_ $t>](self) -> FieldAt<$t, Self> {
                        self._into_field_at()
                    }
                }

                // impl<'t, T: GetFieldAtMut<'t, $t>> [<GetFieldAtMut $t>] for T {}
                // pub trait [<GetFieldAtMut $t>] where Self: for<'t> GetFieldAtMut<'t $t> {
                //     #[inline(always)]
                //     fn [<_ $t _mut>](&'t mut self) -> FieldAt<$t, &'t mut Self> {
                //         self._field_at_mut()
                //     }
                // }
            )*

            mod get_field_at_x_traits {
                $(
                    pub use super::[<IntoFieldAt $t>] as [<_TRAIT_GetFieldAt $t>];
                    // pub use super::[<GetFieldAtMut $t>] as [<_TRAIT_GetFieldAtMut $t>];
                )*
            }
        }
    };
}

// Rust does not allow to impl it with default impls, so we need to do it manually.
impl_get_field_at_x_trait![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];

// // impl<T> GetFieldAtMut1 for T where Self: for<'t> GetFieldAtMut<'t, 1> {}
// pub trait GetFieldAtMut1
// where Self: for<'t> GetFieldAtMut<'t, 1> {
//     #[inline(always)]
//     fn _1_mut(&mut self) -> FieldAt<1, &mut Self> {
//         self._field_at_mut()
//     }
// }

type Test = FieldAt<2, crate::ty![usize, usize, usize, usize]>;
type Test2 = FieldAt<0, crate::ty![String]>;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_field_at() {
        let mut list = crate::new![1, 1.0, vec![2, 3]];
        let foo: Test = 1;
        assert_eq!(list.field_at::<0>(), &1);
        assert_eq!(list.field_at::<1>(), &1.0);
        assert_eq!(list.field_at::<2>(), &vec![2, 3]);
        *list.field_at_mut::<0>() += 1;
        *list.field_at_mut::<1>() += 1.0;
        list.field_at_mut::<2>().push(4);
        assert_eq!(list.field_at::<0>(), &2);
        assert_eq!(list.field_at::<1>(), &2.0);
        assert_eq!(list.field_at::<2>(), &vec![2, 3, 4]);
    }
}
