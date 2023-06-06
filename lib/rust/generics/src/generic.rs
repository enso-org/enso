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


struct PhantomConstUsize<const N: usize>;

auto trait NotZeroCheck {}
impl !NotZeroCheck for PhantomConstUsize<0> {}

trait NotZero<const N: usize> = where PhantomConstUsize<N>: NotZeroCheck;


// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    pub use super::get_field_at_x_traits::*;

    pub use super::FieldAt as _TRAIT_FieldAt;
    pub use super::FieldIndex as _TRAIT_HasFieldAt;
    pub use super::GetFieldAt as _TRAIT_GetFieldAt;
    pub use super::HasFieldCount as _TRAIT_HasFieldsCount;
    pub use super::HasHListRepr as _TRAIT_HasRepr;
    pub use super::_GetFieldAt as _TRAIT__GetFieldAt;
}



// ====================
// === HasHListRepr ===
// ====================

/// A generic representation of the given type. This is a [`HList`] representation of all struct
/// fields.
pub trait HasHListRepr {
    type HListRepr: hlist::HList;
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

pub trait AsHList
where for<'t> &'t Self: HasHListRepr + Into<HListRepr<&'t Self>> {
    #[inline(always)]
    fn as_hlist(&self) -> HListRepr<&Self> {
        self.into()
    }
}
impl<T> AsHList for T where for<'t> &'t T: HasHListRepr + Into<HListRepr<&'t T>> {}



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
// === FieldIndex ===
// ==================

/// Get the field type at the given index.
pub type FieldAt<const I: usize, T> = <T as FieldIndex<I>>::Type;

/// Trait allowing getting the field type at the given index. Please note that this is automatically
/// implemented for every struct that implements [`IntoHList`].
#[allow(missing_docs)]
pub trait FieldIndex<const I: usize> {
    type Type;
}

impl<T: IntoHList, const N: usize> FieldIndex<N> for T
where HListRepr<T>: FieldIndex<N>
{
    type Type = <HListRepr<T> as FieldIndex<N>>::Type;
}



macro_rules! impl_field_index_for_hlist {
    () => {};
    ($t:tt $(,$ts:tt)*) => {
        impl<H, T> FieldIndex<$t> for Cons<H, T>
        where T: FieldIndex<{ dec!($t) }> {
            type Type = <T as FieldIndex<{ dec!($t) }>>::Type;
        }
        impl_field_index_for_hlist! { $($ts),* }
    };
}

// Rust does not allow to impl it with default impls, so we need to do it manually.
// impl_field_index_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



impl<H, T> FieldIndex<0> for Cons<H, T> {
    type Type = H;
}

impl<H, T, const N: usize> FieldIndex<N> for Cons<H, T>
where
    (): NotZero<N>,
    T: FieldIndex<{ N - 1 }>,
{
    type Type = <T as FieldIndex<{ N - 1 }>>::Type;
}


// ==================
// === GetFieldAt ===
// ==================

/// Accessor for field at the given index. Please note that this trait is implemented automatically
/// for every struct that implements [`IntoHList`].
pub trait GetFieldAt<const I: usize>: FieldIndex<I> + _GetFieldAt {
    fn _field_at(&self) -> &FieldAt<I, Self>;
}

/// Mutable accessor for field at the given index. Please note that this trait is implemented
/// automatically for every struct that implements [`IntoHList`].
pub trait GetFieldAtMut<const I: usize>: FieldIndex<I> + _GetFieldAt {
    fn _field_at_mut(&mut self) -> &mut FieldAt<I, Self>;
}

/// Wrapper for [`GetFieldAt`] that enables syntax `struct.field_at::<0>()`.
impl<T> _GetFieldAt for T {}
pub trait _GetFieldAt {
    #[inline(always)]
    fn field_at<const I: usize>(&self) -> &FieldAt<I, Self>
    where Self: GetFieldAt<I> {
        GetFieldAt::<I>::_field_at(self)
    }
}

/// Wrapper for [`GetFieldAtMut`]. Enables syntax `struct.field_at_mut::<0>()`.
impl<T> _GetFieldAtMut for T {}
pub trait _GetFieldAtMut {
    #[inline(always)]
    fn field_at_mut<const I: usize>(&mut self) -> &mut FieldAt<I, Self>
    where Self: GetFieldAtMut<I> {
        GetFieldAtMut::<I>::_field_at_mut(self)
    }
}


// === HList impls ===

impl<H, T> GetFieldAt<0> for Cons<H, T> {
    #[inline(always)]
    fn _field_at(&self) -> &FieldAt<0, Self> {
        &self.0
    }
}

impl<H, T> GetFieldAtMut<0> for Cons<H, T> {
    #[inline(always)]
    fn _field_at_mut(&mut self) -> &mut FieldAt<0, Self> {
        &mut self.0
    }
}

macro_rules! impl_get_field_at_for_hlist {
    () => {};
    ($t:tt $(,$ts:tt)*) => {
        impl<H, T> GetFieldAt<$t> for Cons<H, T>
        where
            T: GetFieldAt<{dec!($t)}>,
            Cons<H, T>: FieldIndex<$t, Type = <T as FieldIndex<{dec!($t)}>>::Type>,
        {
            #[inline(always)]
            fn _field_at(&self) -> &FieldAt<$t, Self> {
                self.1.field_at::<{dec!($t)}>()
            }
        }

        impl<H, T> GetFieldAtMut<$t> for Cons<H, T>
        where
            T: GetFieldAtMut<{dec!($t)}>,
            Cons<H, T>: FieldIndex<$t, Type = <T as FieldIndex<{dec!($t)}>>::Type>,
        {
            #[inline(always)]
            fn _field_at_mut(&mut self) -> &mut FieldAt<$t, Self> {
                self.1.field_at_mut::<{dec!($t)}>()
            }
        }

        impl_get_field_at_for_hlist! { $($ts),* }
    };
}

// Rust does not allow to impl it with default impls, so we need to do it manually.
impl_get_field_at_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];

// impl<T, const N: usize> GetFieldAt<N> for T
// where
//     for<'t> &'t T: IntoHList,
//     for<'t> HListRepr<&'t T>: GetFieldAt<N>,
// {
//     #[inline(always)]
//     fn _field_at(&self) -> &FieldAt<N, &Self> {
//         self.as_hlist().field_at::<N>()
//     }
// }


// ==================
// === GetFieldAtX ===
// ==================

macro_rules! impl_get_field_at_x_trait {
    () => {};
    ($($t:tt),* $(,)?) => {
        paste! {
            $(
                impl<T: GetFieldAt<$t>> [<GetFieldAt $t>] for T {}
                pub trait [<GetFieldAt $t>]: GetFieldAt<$t> {
                    #[inline(always)]
                    fn [<_ $t>](&self) -> &FieldAt<$t, Self> {
                        self._field_at()
                    }
                }

                impl<T: GetFieldAtMut<$t>> [<GetFieldAtMut $t>] for T {}
                pub trait [<GetFieldAtMut $t>]: GetFieldAtMut<$t> {
                    #[inline(always)]
                    fn [<_ $t _mut>](&mut self) -> &mut FieldAt<$t, Self> {
                        self._field_at_mut()
                    }
                }
            )*

            mod get_field_at_x_traits {
                $(
                    pub use super::[<GetFieldAt $t>] as [<_TRAIT_GetFieldAt $t>];
                    pub use super::[<GetFieldAtMut $t>] as [<_TRAIT_GetFieldAtMut $t>];
                )*
            }
        }
    };
}

// Rust does not allow to impl it with default impls, so we need to do it manually.
impl_get_field_at_x_trait![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



type Test = FieldAt<1, crate::ty![usize, String]>;
type Test2 = FieldAt<0, crate::ty![String]>;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_field_at() {
        let list = crate::new![1, "test"];
        let foo: Test = "foo".into();
        assert_eq!(list.field_at::<0>(), &1);
        assert_eq!(list.field_at::<1>(), &"test");
        // assert_eq!(list.field_at::<2>(), &vec![2, 3]);
    }
}
