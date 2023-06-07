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
    pub use super::hlist::traits::*;

    pub use super::FieldAt as _TRAIT_FieldAt;
    pub use super::GetFieldAt as _TRAIT_GetFieldAt;
    pub use super::HasFieldAt as _TRAIT_HasFieldAt;
    pub use super::HasFieldCount as _TRAIT_HasFieldsCount;
    pub use super::PushBack as _TRAIT_PushBack;
    pub use super::_GetFieldAt as _TRAIT__GetFieldAt;
    pub use super::_IntoFieldAt as _TRAIT__IntoFieldAt;
}

use hlist::*;


// =====================
// === HasFieldCount ===
// =====================

/// Compile-time known length value.
#[allow(missing_docs)]
pub trait HasFieldCount {
    const FIELD_COUNT: usize;
    #[inline(always)]
    fn field_count() -> usize {
        Self::FIELD_COUNT
    }
}

#[inline(always)]
pub const fn field_count<T: HasFieldCount>() -> usize {
    <T as HasFieldCount>::FIELD_COUNT
}

impl HasFieldCount for Nil {
    const FIELD_COUNT: usize = 0;
}

impl<H, T: HasFieldCount> HasFieldCount for Cons<H, T> {
    const FIELD_COUNT: usize = 1 + field_count::<T>();
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

impl<H, T> HasFieldAt<0> for Cons<H, T> {
    type Type = H;
}

impl<T: HasHListRepr, const N: usize> HasFieldAt<N> for T
where HListRepr<T>: HasFieldAt<N>
{
    type Type = <HListRepr<T> as HasFieldAt<N>>::Type;
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

impl<H, T> IntoFieldAt<0> for Cons<H, T> {
    #[inline(always)]
    fn _into_field_at(self) -> FieldAt<0, Self> {
        self.0
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

impl<H, T> GetFieldAt<0> for Cons<H, T> {
    #[inline(always)]
    fn _field_at(&self) -> &FieldAt<0, Self> {
        &self.0
    }
}

/// Needed because Rust is stupid.
trait GetFieldAtHelper<'t, const N: usize> = where
    Self: 't + HasFieldAt<N>,
    &'t Self: IntoHList,
    HListRepr<&'t Self>: IntoFieldAt<N, Type = &'t FieldAt<N, Self>>;

/// Default implementation for every struct that implements [`IntoHList`].
impl<T, const N: usize> GetFieldAt<N> for T
where for<'t> Self: GetFieldAtHelper<'t, N>
{
    #[inline(always)]
    fn _field_at(&self) -> &FieldAt<N, Self> {
        self.into_hlist().into_field_at::<N>()
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
trait GetFieldAtMutHelper<'t, const N: usize> = where
    Self: 't + HasFieldAt<N>,
    &'t mut Self: IntoHList,
    HListRepr<&'t mut Self>: IntoFieldAt<N, Type = &'t mut FieldAt<N, Self>>;

/// Default implementation for every struct that implements [`IntoHList`].
impl<T, const N: usize> GetFieldAtMut<N> for T
where for<'t> Self: GetFieldAtMutHelper<'t, N>
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

impl<T1, T2, T3> HasAbstractType for (T1, T2, T3) {
    type AbstractType = Tuple;
}

impl<T1, T2, T3, T4> HasAbstractType for (T1, T2, T3, T4) {
    type AbstractType = Tuple;
}

impl<T1, T2> FromHList<Tuple> for crate::ty![T1, T2] {
    type Output = (T1, T2);
    fn from_hlist(self, _: PhantomData<Tuple>) -> Self::Output {
        let crate::pat![t1, t2] = self;
        (t1, t2)
    }
}

impl<T1, T2, T3> FromHList<Tuple> for crate::ty![T1, T2, T3] {
    type Output = (T1, T2, T3);
    fn from_hlist(self, _: PhantomData<Tuple>) -> Self::Output {
        let crate::pat![t1, t2, t3] = self;
        (t1, t2, t3)
    }
}

impl<T1, T2, T3, T4> FromHList<Tuple> for crate::ty![T1, T2, T3, T4] {
    type Output = (T1, T2, T3, T4);
    fn from_hlist(self, _: PhantomData<Tuple>) -> Self::Output {
        let crate::pat![t1, t2, t3, t4] = self;
        (t1, t2, t3, t4)
    }
}



// ================
// === PushBack ===
// ================

/// Add a new element to the back of the list.
#[allow(missing_docs)]
pub trait PushBack<T>: Sized {
    type Output;
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



// ===============
// === PopLastField ===
// ===============

/// Add a new element to the back of the list.
#[allow(missing_docs)]
pub trait PopLastField: Sized + HasLastField + HasInitFields {
    fn pop_last_field(self) -> (Self::LastField, Self::InitFields);
}

impl<H> PopLastField for Cons<H, Nil> {
    #[inline(always)]
    fn pop_last_field(self) -> (Self::LastField, Self::InitFields) {
        (self.0, Nil)
    }
}

impl<H, T> PopLastField for Cons<H, T>
where T: PopLastField
{
    #[inline(always)]
    fn pop_last_field(self) -> (Self::LastField, Self::InitFields) {
        let (last, init) = self.1.pop_last_field();
        (last, Cons(self.0, init))
    }
}

impl<T> PopLastField for T
where
    T: IntoHList + HasLastField + HasInitFields + HasAbstractType,
    HListRepr<T>: PopLastField + HasLastField<LastField = Self::LastField>,
    InitFields<HListRepr<T>>: FromHList<AbstractType<Self>, Output = Self::InitFields>,
{
    #[inline(always)]
    fn pop_last_field(self) -> (Self::LastField, Self::InitFields) {
        let (last, init) = self.into_hlist().pop_last_field();
        let init = init.from_hlist(PhantomData::<AbstractType<Self>>);
        (last, init)
    }
}



// ====================
// === HasLastField ===
// ====================

/// LastField element type accessor.
pub type LastField<T> = <T as HasLastField>::LastField;

/// Last element accessor.
#[allow(missing_docs)]
pub trait HasLastField {
    type LastField;
}

impl<H> HasLastField for Cons<H, Nil> {
    type LastField = H;
}

impl<H, T: HasLastField> HasLastField for Cons<H, T> {
    type LastField = LastField<T>;
}

impl<T> HasLastField for T
where
    T: HasHListRepr,
    HListRepr<T>: HasLastField,
{
    type LastField = LastField<HListRepr<T>>;
}



// =====================
// === IntoLastField ===
// =====================

pub trait IntoLastField: Sized + HasLastField {
    fn into_last_field(self) -> Self::LastField;
}

impl<H> IntoLastField for Cons<H, Nil> {
    #[inline(always)]
    fn into_last_field(self) -> Self::LastField {
        self.0
    }
}

impl<H, T> IntoLastField for Cons<H, T>
where T: IntoLastField
{
    #[inline(always)]
    fn into_last_field(self) -> Self::LastField {
        self.1.into_last_field()
    }
}

impl<T> IntoLastField for T
where
    T: IntoHList,
    HListRepr<T>: IntoLastField,
{
    #[inline(always)]
    fn into_last_field(self) -> Self::LastField {
        self.into_hlist().into_last_field()
    }
}



// ====================
// === GetLastField ===
// ====================

/// LastField element accessor.
#[allow(missing_docs)]
pub trait GetLastField: HasLastField {
    fn last_field(&self) -> &Self::LastField;
}

impl<H> GetLastField for Cons<H, Nil> {
    #[inline(always)]
    fn last_field(&self) -> &Self::LastField {
        &self.0
    }
}

impl<H, T> GetLastField for Cons<H, T>
where T: GetLastField
{
    #[inline(always)]
    fn last_field(&self) -> &Self::LastField {
        self.1.last_field()
    }
}

trait GetLastFieldHelper<'t> = where
    Self: 't + HasLastField,
    &'t Self: IntoHList,
    HListRepr<&'t Self>: IntoLastField<LastField = &'t LastField<Self>>;

impl<T> GetLastField for T
where for<'t> Self: GetLastFieldHelper<'t>
{
    #[inline(always)]
    fn last_field(&self) -> &LastField<Self> {
        self.into_hlist().into_last_field()
    }
}



// ======================
// === GetLastFieldMut===
// ======================

/// LastField element accessor.
#[allow(missing_docs)]
pub trait GetLastFieldMut: HasLastField {
    fn last_field_mut(&mut self) -> &mut Self::LastField;
}

impl<H> GetLastFieldMut for Cons<H, Nil> {
    #[inline(always)]
    fn last_field_mut(&mut self) -> &mut Self::LastField {
        &mut self.0
    }
}

impl<H, T> GetLastFieldMut for Cons<H, T>
where T: GetLastFieldMut
{
    #[inline(always)]
    fn last_field_mut(&mut self) -> &mut Self::LastField {
        self.1.last_field_mut()
    }
}

trait GetLastFieldMutHelper<'t> = where
    Self: 't + HasLastField,
    &'t mut Self: IntoHList,
    HListRepr<&'t mut Self>: IntoLastField<LastField = &'t mut LastField<Self>>;

impl<T> GetLastFieldMut for T
where for<'t> Self: GetLastFieldMutHelper<'t>
{
    #[inline(always)]
    fn last_field_mut(&mut self) -> &mut LastField<Self> {
        self.into_hlist().into_last_field()
    }
}


// =====================
// === HasInitFields ===
// =====================

/// LastField element type accessor.
pub type InitFields<T> = <T as HasInitFields>::InitFields;

/// Last element accessor.
#[allow(missing_docs)]
pub trait HasInitFields {
    type InitFields;
}

impl<H> HasInitFields for Cons<H, Nil> {
    type InitFields = Nil;
}
impl<H, T: HasInitFields> HasInitFields for Cons<H, T> {
    type InitFields = Cons<H, InitFields<T>>;
}

impl<T> HasInitFields for T
where
    T: HasHListRepr + HasAbstractType,
    HListRepr<T>: HasInitFields,
    InitFields<HListRepr<T>>: FromHList<AbstractType<Self>>,
{
    type InitFields = <InitFields<HListRepr<T>> as FromHList<AbstractType<Self>>>::Output;
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
