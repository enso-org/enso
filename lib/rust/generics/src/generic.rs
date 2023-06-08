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
    pub use super::PushLastField as _TRAIT_PushBack;
    pub use super::_GetFieldAt as _TRAIT__GetFieldAt;
    pub use super::_IntoFieldAt as _TRAIT__IntoFieldAt;
}

use hlist::*;


// ==============
// === Family ===
// ==============

/// Generic type families group similar types under a single umbrella. For example, all tuples
/// belong to the tuple type family. It is used to implement operations which are isomorphic for
/// those families, such as `push_last_field` (which takes a tuple and returns a tuple). Please
/// note, that result of this operation is of another type than the input. The generic marker is
/// used to keep relation between these types.
///
/// To learn more, see the implementation of generic markers for tuples (in the `tuple` module).
#[allow(missing_docs)]
pub trait BelongsToFamily {
    type Family;
}

/// Generic marker of the given type. See [`BelongsToFamily`] for more information.
pub type Family<T> = <T as BelongsToFamily>::Family;

/// Converts the generic representation (HList) to the concrete type of the given family. For
/// example, `t.into_family::<Tuple>()` converts the given data to appropriate tuple representation.
pub trait IntoFamily<M> {
    type Output;
    fn _into_family(self) -> Self::Output;
}

/// Convert the generic representation (HList) to the concrete type of the given family. For
/// example, `t.into_family::<Tuple>()` converts the given data to appropriate tuple representation.
///
/// This is wrapper for [`IntoFamily`] enabling the syntax `t.into_family::<F>()`.
pub trait _IntoFamily: Sized {
    /// Convert the generic representation (HList) to the concrete type of the given family.
    #[inline(always)]
    fn into_family<M>(self) -> Self::Output
    where Self: IntoFamily<M> {
        self._into_family()
    }

    /// Convert the generic representation (HList) to the same generic type family as the `T` type
    /// family.
    #[inline(always)]
    fn into_family_of<T>(self) -> Self::Output
    where
        T: BelongsToFamily,
        Self: IntoFamily<Family<T>>, {
        self._into_family()
    }
}
impl<T> _IntoFamily for T {}



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
// === FirstField ===
// ==================

/// The type of the first field of `T`. This is automatically implemented for every generic type.
pub type FirstField<T> = <T as HasFirstField>::FirstField;


// === HasFirstField ===

/// The association between the Self type and its first field. This is automatically implemented for
/// every generic type.
#[allow(missing_docs)]
pub trait HasFirstField {
    type FirstField;
}

impl<H, T> HasFirstField for Cons<H, T> {
    type FirstField = H;
}

impl<T> HasFirstField for T
where
    T: HasHListRepr,
    HListRepr<T>: HasFirstField,
{
    type FirstField = FirstField<HListRepr<T>>;
}


// === IntoFirstField ===

/// Take ownership of self and return its first field. This is automatically implemented for every
/// generic type.
pub trait IntoFirstField: Sized + HasFirstField {
    fn into_first_field(self) -> Self::FirstField;
}

impl<H, T> IntoFirstField for Cons<H, T>
where T: IntoFirstField
{
    #[inline(always)]
    fn into_first_field(self) -> Self::FirstField {
        self.0
    }
}

impl<T> IntoFirstField for T
where
    T: IntoHList,
    HListRepr<T>: IntoFirstField,
{
    #[inline(always)]
    fn into_first_field(self) -> Self::FirstField {
        self.into_hlist().into_first_field()
    }
}


// === GetFirstField ===

/// Return reference to the first field. This is automatically implemented for every generic type.
#[allow(missing_docs)]
pub trait GetFirstField: HasFirstField {
    fn first_field(&self) -> &Self::FirstField;
}

impl<H, T> GetFirstField for Cons<H, T> {
    #[inline(always)]
    fn first_field(&self) -> &Self::FirstField {
        &self.0
    }
}

trait GetFirstFieldHelper<'t> = where
    Self: 't + HasFirstField,
    &'t Self: IntoHList,
    HListRepr<&'t Self>: IntoFirstField<FirstField = &'t FirstField<Self>>;

impl<T> GetFirstField for T
where for<'t> Self: GetFirstFieldHelper<'t>
{
    #[inline(always)]
    fn first_field(&self) -> &FirstField<Self> {
        self.into_hlist().into_first_field()
    }
}


// === GetFirstFieldMut===

/// Return mutable reference to the first field. This is automatically implemented for every generic
/// type.
#[allow(missing_docs)]
pub trait GetFirstFieldMut: HasFirstField {
    fn first_field_mut(&mut self) -> &mut Self::FirstField;
}

impl<H, T> GetFirstFieldMut for Cons<H, T> {
    #[inline(always)]
    fn first_field_mut(&mut self) -> &mut Self::FirstField {
        &mut self.0
    }
}

trait GetFirstFieldMutHelper<'t> = where
    Self: 't + HasFirstField,
    &'t mut Self: IntoHList,
    HListRepr<&'t mut Self>: IntoFirstField<FirstField = &'t mut FirstField<Self>>;

impl<T> GetFirstFieldMut for T
where for<'t> Self: GetFirstFieldMutHelper<'t>
{
    #[inline(always)]
    fn first_field_mut(&mut self) -> &mut FirstField<Self> {
        self.into_hlist().into_first_field()
    }
}



// =================
// === LastField ===
// =================

/// The type of the last field of `T`. This is automatically implemented for every generic type.
pub type LastField<T> = <T as HasLastField>::LastField;


// === HasLastField ===

/// The association between the Self type and its last field. This is automatically implemented for
/// every generic type.
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


// === IntoLastField ===

/// Take ownership of self and return its last field. This is automatically implemented for every
/// generic type.
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


// === GetLastField ===

/// Return reference to the last field. This is automatically implemented for every generic type.
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


// === GetLastFieldMut===

/// Return mutable reference to the last field. This is automatically implemented for every generic
/// type.
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



// ===============
// === FieldAt ===
// ===============

/// FieldType of field at the given index. Please note that this is automatically implemented for
/// every generic type.
pub type FieldAt<const I: usize, T> = <T as HasFieldAt<I>>::FieldType;


// === HasFieldAt ===

/// Association between field index and field type. Please note that this is automatically
/// implemented for every generic type.
pub trait HasFieldAt<const I: usize> {
    type FieldType;
}

impl<H, T> HasFieldAt<0> for Cons<H, T> {
    type FieldType = H;
}

impl<T: HasHListRepr, const N: usize> HasFieldAt<N> for T
where HListRepr<T>: HasFieldAt<N>
{
    type FieldType = <HListRepr<T> as HasFieldAt<N>>::FieldType;
}

macro_rules! impl_field_index_for_hlist {
    () => {};
    ($t:tt $(,$ts:tt)*) => {
        impl<H, T> HasFieldAt<$t> for Cons<H, T>
        where T: HasFieldAt<{dec!($t)}> {
            type FieldType = <T as HasFieldAt<{dec!($t)}>>::FieldType;
        }

        impl_field_index_for_hlist! { $($ts),* }
    };
}

// Rust does not allow for a generic implementation yet, see:
// https://github.com/rust-lang/rust/issues/112341
impl_field_index_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];


// === IntoFieldAt ===

/// Take the ownership of self and return its I-th field. Please note that this is automatically
/// implemented for every generic type.
pub trait IntoFieldAt<const I: usize>: Sized + HasFieldAt<I> {
    fn _into_field_at(self) -> FieldAt<I, Self>;
}

/// Take the ownership of self and return its I-th field. This is automatically implemented for
/// every generic type. It is a wrapper for [`IntoFieldAt`] that enables syntax
/// `struct.into_field_at::<0>()`.
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

impl<T, const N: usize> IntoFieldAt<N> for T
where
    T: HasFieldAt<N> + IntoHList,
    HListRepr<T>: IntoFieldAt<N, FieldType = FieldAt<N, Self>>,
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
                Cons<H, T>: HasFieldAt<$t, FieldType = <T as HasFieldAt<{ dec!($t) }>>::FieldType>,
            {
                #[inline(always)]
                fn _into_field_at(self) -> FieldAt<$t, Self> {
                    self.1.into_field_at::<{ dec!($t) }>()
                }
            }
        )*
    };
}

// Rust does not allow for a generic implementation yet, see:
// https://github.com/rust-lang/rust/issues/112341
impl_into_field_at_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// === GetFieldAt ===

/// Return the I-th field reference. Please note that this is automatically implemented for every
/// generic type.
pub trait GetFieldAt<const I: usize>: HasFieldAt<I> {
    fn _field_at(&self) -> &FieldAt<I, Self>;
}

/// Return the I-th field reference. Please note that this is automatically implemented for every
/// generic type. It is a wrapper for [`GetFieldAt`] that enables syntax `struct.field_at::<0>()`.
#[allow(missing_docs)]
pub trait _GetFieldAt {
    #[inline(always)]
    fn field_at<const I: usize>(&self) -> &FieldAt<I, Self>
    where Self: GetFieldAt<I> {
        GetFieldAt::<I>::_field_at(self)
    }
}
impl<T> _GetFieldAt for T {}

impl<H, T> GetFieldAt<0> for Cons<H, T> {
    #[inline(always)]
    fn _field_at(&self) -> &FieldAt<0, Self> {
        &self.0
    }
}

trait GetFieldAtHelper<'t, const N: usize> = where
    Self: 't + HasFieldAt<N>,
    &'t Self: IntoHList,
    HListRepr<&'t Self>: IntoFieldAt<N, FieldType = &'t FieldAt<N, Self>>;

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
                Cons<H, T>: HasFieldAt<$t, FieldType = <T as HasFieldAt<{ dec!($t) }>>::FieldType>
            {
                #[inline(always)]
                fn _field_at(&self) -> &FieldAt<$t, Self> {
                    self.1.field_at::<{ dec!($t) }>()
                }
            }
        )*
    };
}

// Rust does not allow for a generic implementation yet, see:
// https://github.com/rust-lang/rust/issues/112341
impl_get_field_at_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// === GetFieldAtMut ===

/// Return the I-th field mutable reference. Please note that this is automatically implemented for
/// every generic type.
pub trait GetFieldAtMut<const I: usize>
where Self: HasFieldAt<I> {
    fn _field_at_mut(&mut self) -> &mut FieldAt<I, Self>;
}

/// Return the I-th field mutable reference. Please note that this is automatically implemented for
/// every generic type. It is a wrapper for [`GetFieldAtMut`] that enables syntax
/// `struct.field_at_mut::<0>()`.
pub trait _GetFieldAtMut {
    #[inline(always)]
    fn field_at_mut<const I: usize>(&mut self) -> &mut FieldAt<I, Self>
    where Self: GetFieldAtMut<I> {
        GetFieldAtMut::<I>::_field_at_mut(self)
    }
}
impl<T> _GetFieldAtMut for T {}

trait GetFieldAtMutHelper<'t, const N: usize> = where
    Self: 't + HasFieldAt<N>,
    &'t mut Self: IntoHList,
    HListRepr<&'t mut Self>: IntoFieldAt<N, FieldType = &'t mut FieldAt<N, Self>>;

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
                Cons<H, T>: HasFieldAt<$t, FieldType = <T as HasFieldAt<{ dec!($t) }>>::FieldType>
            {
                #[inline(always)]
                fn _field_at_mut(&mut self) -> &mut FieldAt<$t, Self> {
                    self.1.field_at_mut::<{ dec!($t) }>()
                }
            }
        )*
    };
}

// Rust does not allow for a generic implementation yet, see:
// https://github.com/rust-lang/rust/issues/112341
impl_get_field_at_for_hlist![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];


// === GetFieldAtX / GetFieldAtMutX  ===

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
// Rust does not allow for a generic implementation yet, see:
// https://github.com/rust-lang/rust/issues/112341
impl_get_field_at_x_trait![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// =====================
// === PushLastField ===
// =====================

/// Add a new element to the back of the list.
#[allow(missing_docs)]
pub trait PushLastField<T>: Sized {
    type Output;
    fn push_last_field(self, t: T) -> Self::Output;
}

impl<X> PushLastField<X> for Nil {
    type Output = Cons<X, Nil>;
    #[inline(always)]
    fn push_last_field(self, x: X) -> Self::Output {
        Cons(x, Nil)
    }
}

impl<X, H, T> PushLastField<X> for Cons<H, T>
where T: PushLastField<X>
{
    type Output = Cons<H, <T as PushLastField<X>>::Output>;
    #[inline(always)]
    fn push_last_field(self, x: X) -> Self::Output {
        let Cons(head, tail) = self;
        Cons(head, tail.push_last_field(x))
    }
}


impl<T, X> PushLastField<X> for T
where
    T: BelongsToFamily + IntoHList,
    HListRepr<T>: PushLastField<X>,
    <HListRepr<T> as PushLastField<X>>::Output: IntoFamily<Family<Self>>,
{
    type Output = <<HListRepr<T> as PushLastField<X>>::Output as IntoFamily<Family<Self>>>::Output;
    #[inline(always)]
    fn push_last_field(self, x: X) -> Self::Output {
        self.into_hlist().push_last_field(x).into_family_of::<Self>()
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
    T: IntoHList + HasLastField + HasInitFields + BelongsToFamily,
    HListRepr<T>: PopLastField + HasLastField<LastField = Self::LastField>,
    InitFields<HListRepr<T>>: IntoFamily<Family<Self>, Output = Self::InitFields>,
{
    #[inline(always)]
    fn pop_last_field(self) -> (Self::LastField, Self::InitFields) {
        let (last, init) = self.into_hlist().pop_last_field();
        let init = init.into_family_of::<Self>();
        (last, init)
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
    T: HasHListRepr + BelongsToFamily,
    HListRepr<T>: HasInitFields,
    InitFields<HListRepr<T>>: IntoFamily<Family<Self>>,
{
    type InitFields = <InitFields<HListRepr<T>> as IntoFamily<Family<Self>>>::Output;
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
