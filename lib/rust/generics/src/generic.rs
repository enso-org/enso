//! Generic representation of data types. Refer to the crate documentation to learn more.

// This crate defines many helper traits and uses this flag on purpose.

// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use super::hlist;
use std::marker::PhantomData;
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
    pub use super::GetFieldAt as _TRAIT_GetFieldAt;
    pub use super::HasFieldAt as _TRAIT_HasFieldAt;
    pub use super::HasFieldCount as _TRAIT_HasFieldsCount;
    pub use super::HasHListRepr as _TRAIT_HasRepr;
    pub use super::PushBack as _TRAIT_PushBack;
    pub use super::_GetFieldAt as _TRAIT__GetFieldAt;
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



// =================
// === IntoHList ===
// =================

/// Converts the struct into its generic representation. Please note that this trait is implemented
/// automatically for every type which implements `Into<HListRepr<Self>>`.
pub trait IntoHList: HasHListRepr + Into<HListRepr<Self>> {
    #[inline(always)]
    fn into_hlist(self) -> HListRepr<Self> {
        self.into()
    }
}
impl<T: HasHListRepr + Into<HListRepr<T>>> IntoHList for T {}



// ===============
// === AsHList ===
// ===============

pub trait AsHList
where for<'t> &'t Self: HasHListRepr + Into<HListRepr<&'t Self>> {
    #[inline(always)]
    fn as_hlist(&self) -> HListRepr<&Self> {
        self.into()
    }
}
impl<T> AsHList for T where for<'t> &'t T: HasHListRepr + Into<HListRepr<&'t T>> {}



// ==================
// === AsHListMut ===
// ==================

pub trait AsHListMut
where for<'t> &'t mut Self: HasHListRepr + Into<HListRepr<&'t mut Self>> {
    #[inline(always)]
    fn as_hlist_mut(&mut self) -> HListRepr<&mut Self> {
        self.into()
    }
}
impl<T> AsHListMut for T where for<'t> &'t mut T: HasHListRepr + Into<HListRepr<&'t mut T>> {}



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

macro_rules! impl_field_index_for_hlist {
    () => {};
    ($t:tt $(,$ts:tt)*) => {
        impl<H, T> HasFieldAt<$t> for Cons<H, T>
        where T: HasFieldAt<{dec!($t)}> {
            type Type = <T as HasFieldAt<{dec!($t)}>>::Type;
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

impl<H, T> IntoFieldAt<0> for Cons<H, T> {
    #[inline(always)]
    fn _into_field_at(self) -> FieldAt<0, Self> {
        self.0
    }
}

macro_rules! impl_into_field_at_for_hlist {
    () => {};
    ($($t:tt),*) => {
        $(
            impl<H, T> IntoFieldAt<$t> for Cons<H, T>
            where
                T: IntoFieldAt<{ dec!($t) }>,
                Cons<H, T>: HasFieldAt<$t, Type = <T as HasFieldAt<{ dec!($t) }>>::Type>,
            {
                #[inline(always)]
                fn _into_field_at(self) -> FieldAt<$t, Self> {
                    self.1.into_field_at::<{ dec!($t) }>()
                }
            }
        )*
    };
}

// Rust does not allow to impl it with default impls, so we need to do it manually.
impl_into_field_at_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// ==================
// === GetFieldAt ===
// ==================

pub trait GetFieldAt<const I: usize>: HasFieldAt<I> {
    fn _field_at(&self) -> &FieldAt<I, Self>;
}

impl<T> _GetFieldAt for T {}
pub trait _GetFieldAt {
    #[inline(always)]
    fn field_at<const I: usize>(&self) -> &FieldAt<I, Self>
    where Self: GetFieldAt<I> {
        GetFieldAt::<I>::_field_at(self)
    }
}

/// Needed because Rust is stupid.
trait GetFieldAtHelper<'t, T, const N: usize> = where
    Self: HasHListRepr,
    T: HasFieldAt<N>,
    <T as HasFieldAt<N>>::Type: 't,
    HListRepr<Self>: HasFieldAt<N, Type = &'t FieldAt<N, T>>;

/// Default implementation for every struct that implements [`IntoHList`].
impl<T, const N: usize> GetFieldAt<N> for T
where
    T: HasFieldAt<N>,
    for<'t> &'t T: IntoHList,
    for<'t> HListRepr<&'t T>: IntoFieldAt<N>,
    for<'t> &'t T: GetFieldAtHelper<'t, T, N>,
{
    #[inline(always)]
    fn _field_at(&self) -> &FieldAt<N, Self> {
        self.into_hlist().into_field_at::<N>()
    }
}

impl<H, T> GetFieldAt<0> for Cons<H, T> {
    #[inline(always)]
    fn _field_at(&self) -> &FieldAt<0, Self> {
        &self.0
    }
}

macro_rules! impl_get_field_at_for_hlist {
    () => {};
    ($($t:tt),*) => {
        $(
            impl<H, T> GetFieldAt<$t> for Cons<H, T>
            where
                T: GetFieldAt<{ dec!($t) }>,
                Cons<H, T>: HasFieldAt<$t, Type = <T as HasFieldAt<{ dec!($t) }>>::Type>
            {
                #[inline(always)]
                fn _field_at(&self) -> &FieldAt<$t, Self> {
                    self.1.field_at::<{ dec!($t) }>()
                }
            }
        )*
    };
}

// Rust does not allow to impl it with default impls, so we need to do it manually.
impl_get_field_at_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// =====================
// === GetFieldAtMut ===
// =====================

/// Return mutable reference to the I-th field of self. Please note that this trait is implemented
/// automatically for every struct that implements [`IntoHList`].
pub trait GetFieldAtMut<const I: usize>
where Self: HasFieldAt<I> {
    fn _field_at_mut(&mut self) -> &mut FieldAt<I, Self>;
}

impl<T> _GetFieldAtMut for T {}
pub trait _GetFieldAtMut {
    #[inline(always)]
    fn field_at_mut<const I: usize>(&mut self) -> &mut FieldAt<I, Self>
    where Self: GetFieldAtMut<I> {
        GetFieldAtMut::<I>::_field_at_mut(self)
    }
}

/// Needed because Rust is stupid.
trait GetFieldAtMutHelper<'t, T, const N: usize> = where
    Self: HasHListRepr,
    T: HasFieldAt<N>,
    <T as HasFieldAt<N>>::Type: 't,
    HListRepr<Self>: HasFieldAt<N, Type = &'t mut FieldAt<N, T>>;

/// Default implementation for every struct that implements [`IntoHList`].
impl<T, const N: usize> GetFieldAtMut<N> for T
where
    T: HasFieldAt<N>,
    for<'t> &'t mut T: IntoHList,
    for<'t> HListRepr<&'t mut T>: IntoFieldAt<N>,
    for<'t> &'t mut T: GetFieldAtMutHelper<'t, T, N>,
{
    #[inline(always)]
    fn _field_at_mut(&mut self) -> &mut FieldAt<N, Self> {
        self.into_hlist().into_field_at::<N>()
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
    ($($t:tt),*) => {
        $(
            impl<H, T> GetFieldAtMut <$t> for Cons<H, T>
            where
                T: GetFieldAtMut<{ dec!($t) }>,
                Cons<H, T>: HasFieldAt<$t, Type = <T as HasFieldAt<{ dec!($t) }>>::Type>
            {
                #[inline(always)]
                fn _field_at_mut(&mut self) -> &mut FieldAt<$t, Self> {
                    self.1.field_at_mut::<{ dec!($t) }>()
                }
            }
        )*
    };
}

// Rust does not allow to impl it with default impls, so we need to do it manually.
impl_get_field_at_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// ===================
// === GetFieldAtX ===
// ===================

macro_rules! impl_get_field_at_x_trait {
    () => {};
    ($($t:tt),* $(,)?) => {
        paste! {
            $(
                impl<T: GetFieldAt<$t>> [<GetFieldAt $t>] for T {}
                pub trait [<GetFieldAt $t>]: GetFieldAt<$t> {
                    #[inline(always)]
                    fn [<_ $t>](&self) -> &FieldAt<$t, Self> {
                        GetFieldAt::<$t>::_field_at(self)
                    }
                }

                // impl<T: GetFieldAtMut<$t>> [<GetFieldAtMut $t>] for T {}
                // pub trait [<GetFieldAtMut $t>]: GetFieldAtMut<$t> {
                //     #[inline(always)]
                //     fn [<_ $t>](&self) -> &FieldAt<$t, Self> {
                //         GetFieldAt::<$t>::_field_at(self)
                //     }
                // }
            )*

            mod get_field_at_x_traits {
                $(
                    pub use super::[<GetFieldAt $t>] as [<_TRAIT_GetFieldAt $t>];
                    // pub use super::[<GetFieldAtMut $t>] as [<_TRAIT_GetFieldAtMut $t>];
                )*
            }
        }
    };
}
//
// // Rust does not allow to impl it with default impls, so we need to do it manually.
impl_get_field_at_x_trait![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// impl<T> GetFieldAt0 for T {}
// pub trait GetFieldAt0 {
//     #[inline(always)]
//     fn _0<'t>(&'t self) -> FieldAt<0, &'t Self>
//         where &'t Self: IntoFieldAt<0> {
//         IntoFieldAt::<0>::_into_field_at(self)
//     }
// }

struct Get0Data<T>(T);

trait Data {}

trait EventOutput {
    type Output;
}

trait HasOutput {
    type Output;
}

// pub type FieldAt<const I: usize, T> = <T as HasFieldAt<I>>::Type;

impl<T1> HasOutput for Get0Data<T1>
where
    T1: EventOutput,
    T1::Output: HasFieldAt<0>,
    FieldAt<0, T1::Output>: Data,
{
    type Output = usize; //FieldAt<0, T1::Output>;
}



// ================
// === PushBack ===
// ================

pub trait HasAbstractType {
    type AbstractType;
}

pub type AbstractType<T> = <T as HasAbstractType>::AbstractType;

pub trait FromHList<A> {
    type Output;
    fn from_hlist(self, a: PhantomData<A>) -> Self::Output;
}

pub struct Tuple;
impl<T1, T2> HasAbstractType for (T1, T2) {
    type AbstractType = Tuple;
}

impl<T1, T2, T3> FromHList<Tuple> for crate::ty![T1, T2, T3] {
    type Output = (T1, T2, T3);
    fn from_hlist(self, _: PhantomData<Tuple>) -> Self::Output {
        let crate::pat![t1, t2, t3] = self;
        (t1, t2, t3)
    }
}


/// Add a new element to the back of the list.
#[allow(missing_docs)]
pub trait PushBack<T>: Sized {
    type Output; //: KnownLast<Last = T> + KnownInit<Init = Self>;
    fn push_back(self, t: T) -> Self::Output;
}

impl<X> PushBack<X> for Nil {
    type Output = Cons<X, Nil>;
    #[inline(always)]
    fn push_back(self, x: X) -> Self::Output {
        Cons(x, Nil)
    }
}

impl<X, H, T> PushBack<X> for Cons<H, T>
where T: PushBack<X>
{
    type Output = Cons<H, <T as PushBack<X>>::Output>;
    #[inline(always)]
    fn push_back(self, x: X) -> Self::Output {
        let Cons(head, tail) = self;
        Cons(head, tail.push_back(x))
    }
}


impl<T, X> PushBack<X> for T
where
    T: HasAbstractType + IntoHList,
    HListRepr<T>: PushBack<X>,
    <HListRepr<T> as PushBack<X>>::Output: FromHList<AbstractType<Self>>,
{
    type Output = <<HListRepr<T> as PushBack<X>>::Output as FromHList<AbstractType<Self>>>::Output;
    #[inline(always)]
    fn push_back(self, x: X) -> Self::Output {
        self.into_hlist().push_back(x).from_hlist(PhantomData::<AbstractType<Self>>)
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_field_at() {
        let mut list = crate::new![1, 1.0, vec![2, 3]];
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
