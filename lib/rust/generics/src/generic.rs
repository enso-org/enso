//! Generic representation of data types. Refer to the crate documentation to learn more.

use crate::hlist;

use derivative::Derivative;
use enso_zst::ZST;
use hlist::Cons;
use hlist::Nil;
use paste::paste;



// =============
// === Utils ===
// =============

/// Currently, Rust type system does not work well with type-level subtraction.
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



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    pub use super::get_field_at_x_traits::*;
    pub use super::hlist::traits::*;

    pub use super::AsMutFields as _TRAIT_AsMutFields;
    pub use super::AsRefFields as _TRAIT_AsRefFields;
    pub use super::BelongsToFamily as _TRAIT_BelongsToFamily;
    pub use super::FieldIter as _TRAIT_FieldIter;
    pub use super::FieldIterMut as _TRAIT_FieldIterMut;
    pub use super::GetFieldAt as _TRAIT_GetFieldAt;
    pub use super::GetFieldAtMut as _TRAIT_GetFieldAtMut;
    pub use super::GetFirstField as _TRAIT_GetFirstField;
    pub use super::GetFirstFieldMut as _TRAIT_GetFirstFieldMut;
    pub use super::GetLastField as _TRAIT_GetLastField;
    pub use super::GetLastFieldMut as _TRAIT_GetLastFieldMut;
    pub use super::HasFieldAt as _TRAIT_HasFieldAt;
    pub use super::HasFieldCount as _TRAIT_HasFieldsCount;
    pub use super::HasFirstField as _TRAIT_HasFirstField;
    pub use super::HasInitFields as _TRAIT_HasInitFields;
    pub use super::HasLastField as _TRAIT_HasLastField;
    pub use super::HasTailFields as _TRAIT_HasTailFields;
    pub use super::IntoFamily as _TRAIT_IntoFamily;
    pub use super::IntoFieldAt as _TRAIT_IntoFieldAt;
    pub use super::IntoFieldIter as _TRAIT_IntoFieldIter;
    pub use super::IntoFirstField as _TRAIT_IntoFirstField;
    pub use super::IntoLastField as _TRAIT_IntoLastField;
    pub use super::MapFields as _TRAIT_MapFields;
    pub use super::MapFieldsInto as _TRAIT_MapFieldsInto;
    pub use super::MapFieldsWith as _TRAIT_MapFieldsWith;
    pub use super::PopFirstField as _TRAIT_PopFirstField;
    pub use super::PopLastField as _TRAIT_PopLastField;
    pub use super::PushFirstField as _TRAIT_PushFirstField;
    pub use super::PushLastField as _TRAIT_PushBack;
    pub use super::PushLastField as _TRAIT_PushLastField;
    pub use super::_GetFieldAt as _TRAIT__GetFieldAt;
    pub use super::_GetFieldAtMut as _TRAIT__GetFieldAtMut;
    pub use super::_IntoFamily as _TRAIT__IntoFamily;
    pub use super::_IntoFieldAt as _TRAIT__IntoFieldAt;
    pub use super::_MapFields as _TRAIT__MapFields;
    pub use super::_MapFieldsInto as _TRAIT__MapFieldsInto;
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

/// Converts the generic representation [`HList`] to the concrete type of the given family.
#[allow(missing_docs)]
pub trait IntoFamily<M> {
    type Output;
    /// Instead of using this function, use the [`_IntoFamily::into_family`] which allows for a
    /// turbofish syntax.
    fn _into_family(self) -> Self::Output;
}

/// Convert the generic representation [`HList`] to the concrete type of the given family. For
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

    /// Convert the generic representation [`HList]` to the same generic type family as the `T` type
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
    fn field_count(&self) -> usize {
        Self::FIELD_COUNT
    }
}

impl HasFieldCount for Nil {
    const FIELD_COUNT: usize = 0;
}

impl<H, T: HasFieldCount> HasFieldCount for Cons<H, T> {
    const FIELD_COUNT: usize = 1 + T::FIELD_COUNT;
}

impl<T: HasHListRepr> HasFieldCount for T {
    const FIELD_COUNT: usize = <T as HasHListRepr>::HListRepr::FIELD_COUNT;
}



// ===================
// === AsRefFields ===
// ===================

/// Return the structure containing references to all of its fields. Please note that this works
/// only for structs that have all parameters generic, such as [`HList`] or tuples. In case you want
/// to get references to all fields of other structs, you can use [`AsHList`] instead.
#[allow(missing_docs)]
pub trait AsRefFields {
    type RefFields;
    #[allow(clippy::wrong_self_convention)]
    fn as_ref_fields(self) -> Self::RefFields;
}

/// Struct with all fields transformed to references. See docs of [`AsRefFields`] for more info.
pub type RefFields<T> = <T as AsRefFields>::RefFields;

impl<'t> AsRefFields for &'t Nil {
    type RefFields = Nil;
    #[inline(always)]
    fn as_ref_fields(self) -> Self::RefFields {
        Nil
    }
}

impl<'t, H, T> AsRefFields for &'t Cons<H, T>
where &'t T: AsRefFields
{
    type RefFields = Cons<&'t H, RefFields<&'t T>>;
    #[inline(always)]
    fn as_ref_fields(self) -> Self::RefFields {
        Cons(&self.0, (&self.1).as_ref_fields())
    }
}

impl<'t, T> AsRefFields for &'t T
where
    T: BelongsToFamily,
    &'t T: IntoHList,
    HListRepr<&'t T>: IntoFamily<Family<T>>,
{
    type RefFields = <HListRepr<&'t T> as IntoFamily<Family<T>>>::Output;
    #[inline(always)]
    fn as_ref_fields(self) -> Self::RefFields {
        self.into_hlist().into_family_of::<T>()
    }
}



// ===================
// === AsMutFields ===
// ===================

/// Return the structure containing mutable references to all of its fields. Please note that this
/// works only for structs that have all parameters generic, such as [`HList`] or tuples. In case
/// you want to get references to all fields of other structs, you can use [`AsHListMut`] instead.
#[allow(missing_docs)]
pub trait AsMutFields {
    type MutFields;
    #[allow(clippy::wrong_self_convention)]
    fn as_mut_fields(self) -> Self::MutFields;
}

/// Struct with all fields transformed to references. See docs of [`AsMutFields`] for more info.
pub type MutFields<T> = <T as AsMutFields>::MutFields;

impl<'t> AsMutFields for &'t mut Nil {
    type MutFields = Nil;
    #[inline(always)]
    fn as_mut_fields(self) -> Self::MutFields {
        Nil
    }
}

impl<'t, H, T> AsMutFields for &'t mut Cons<H, T>
where &'t mut T: AsMutFields
{
    type MutFields = Cons<&'t mut H, MutFields<&'t mut T>>;
    #[inline(always)]
    fn as_mut_fields(self) -> Self::MutFields {
        Cons(&mut self.0, (&mut self.1).as_mut_fields())
    }
}

impl<'t, T> AsMutFields for &'t mut T
where
    T: BelongsToFamily,
    &'t mut T: IntoHList,
    HListRepr<&'t mut T>: IntoFamily<Family<T>>,
{
    type MutFields = <HListRepr<&'t mut T> as IntoFamily<Family<T>>>::Output;
    #[inline(always)]
    fn as_mut_fields(self) -> Self::MutFields {
        self.into_hlist().into_family_of::<T>()
    }
}



// ==================
// === FirstField ===
// ==================

/// The type of the first field of `T`. This is automatically implemented for every type with a
/// generic representation.
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
#[allow(missing_docs)]
pub trait IntoFirstField: Sized + HasFirstField {
    fn into_first_field(self) -> Self::FirstField;
}

impl<H, T> IntoFirstField for Cons<H, T> {
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

/// Return reference to the first field. This is automatically implemented for every type with a
/// generic representation.
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

/// The type of the last field of `T`. This is automatically implemented for every type with a
/// generic representation.
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
#[allow(missing_docs)]
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

/// Return reference to the last field. This is automatically implemented for every type with a
/// generic representation.
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



// ==================
// === TailFields ===
// ==================

/// All but first field of a struct.
pub type TailFields<T> = <T as HasTailFields>::TailFields;

/// All but first field of a struct.
#[allow(missing_docs)]
pub trait HasTailFields {
    type TailFields;
}

impl<H, T> HasTailFields for Cons<H, T> {
    type TailFields = T;
}

impl<T> HasTailFields for T
where
    T: HasHListRepr + BelongsToFamily,
    HListRepr<T>: HasTailFields,
    TailFields<HListRepr<T>>: IntoFamily<Family<Self>>,
{
    type TailFields = <TailFields<HListRepr<T>> as IntoFamily<Family<Self>>>::Output;
}



// ==================
// === InitFields ===
// ==================

/// Accessor of all fields but the last one.
pub type InitFields<T> = <T as HasInitFields>::InitFields;

/// Accessor of all fields but the last one.
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



// ===============
// === FieldAt ===
// ===============

/// FieldType of field at the given index. Please note that this is automatically implemented for
/// every generic type.
pub type FieldAt<const I: usize, T> = <T as HasFieldAt<I>>::FieldType;


// === HasFieldAt ===

/// Association between field index and field type. Please note that this is automatically
/// implemented for every type with a generic representation.
#[allow(missing_docs)]
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
/// implemented for every type with a generic representation.
pub trait IntoFieldAt<const I: usize>: Sized + HasFieldAt<I> {
    /// Instead of using this function, use the [`_IntoFieldAt::into_field_at`] which allows for a
    /// turbofish syntax.
    fn _into_field_at(self) -> FieldAt<I, Self>;
}

/// Take the ownership of self and return its I-th field. This is automatically implemented for
/// every generic type. It is a wrapper for [`IntoFieldAt`] that enables syntax
/// `struct.into_field_at::<0>()`.
impl<T> _IntoFieldAt for T {}

#[allow(missing_docs)]
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
    /// Instead of using this function, use the [`_GetFieldAt::field_at`] which allows for a
    /// turbofish syntax.
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
    /// Instead of using this function, use the [`_HasFieldAt::field_at_mut`] which allows for a
    /// turbofish syntax.
    fn _field_at_mut(&mut self) -> &mut FieldAt<I, Self>;
}

/// Return the I-th field mutable reference. Please note that this is automatically implemented for
/// every generic type. It is a wrapper for [`GetFieldAtMut`] that enables syntax
/// `struct.field_at_mut::<0>()`.
#[allow(missing_docs)]
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

                /// Get reference to the field at the given index.
                #[allow(missing_docs)]
                pub trait [<GetFieldAt $t>]: GetFieldAt<$t> {
                    #[inline(always)]
                    fn [<_ $t>](&self) -> &FieldAt<$t, Self> {
                        GetFieldAt::<$t>::_field_at(self)
                    }
                }

                impl<T: GetFieldAtMut<$t>> [<GetFieldAtMut $t>] for T {}

                /// Get mutable reference to the field at the given index.
                #[allow(missing_docs)]
                pub trait [<GetFieldAtMut $t>]: GetFieldAtMut<$t> {
                    #[inline(always)]
                    fn [<_ $t>](&mut self) -> &mut FieldAt<$t, Self> {
                        GetFieldAtMut::<$t>::_field_at_mut(self)
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
//
// Rust does not allow for a generic implementation yet, see:
// https://github.com/rust-lang/rust/issues/112341
impl_get_field_at_x_trait![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];



// ======================
// === PushFirstField ===
// ======================

/// Add a new element to the front of the list.
#[allow(missing_docs)]
pub trait PushFirstField<T>: Sized {
    type Output;
    fn push_first_field(self, t: T) -> Self::Output;
}

impl<X> PushFirstField<X> for Nil {
    type Output = Cons<X, Nil>;
    #[inline(always)]
    fn push_first_field(self, x: X) -> Self::Output {
        Cons(x, Nil)
    }
}

impl<X, H, T> PushFirstField<X> for Cons<H, T> {
    type Output = Cons<X, Cons<H, T>>;
    #[inline(always)]
    fn push_first_field(self, x: X) -> Self::Output {
        Cons(x, self)
    }
}

impl<T, X> PushFirstField<X> for T
where
    T: BelongsToFamily + IntoHList,
    HListRepr<T>: PushFirstField<X>,
    <HListRepr<T> as PushFirstField<X>>::Output: IntoFamily<Family<Self>>,
{
    type Output = <<HListRepr<T> as PushFirstField<X>>::Output as IntoFamily<Family<Self>>>::Output;
    #[inline(always)]
    fn push_first_field(self, x: X) -> Self::Output {
        self.into_hlist().push_first_field(x).into_family_of::<Self>()
    }
}



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



// =====================
// === PopFirstField ===
// =====================

/// Add a new element to the back of the list.
#[allow(missing_docs)]
pub trait PopFirstField: Sized + HasFirstField + HasTailFields {
    fn pop_first_field(self) -> (Self::FirstField, Self::TailFields);
}

impl<H, T> PopFirstField for Cons<H, T> {
    #[inline(always)]
    fn pop_first_field(self) -> (Self::FirstField, Self::TailFields) {
        (self.0, self.1)
    }
}

impl<T> PopFirstField for T
where
    T: IntoHList + HasFirstField + HasTailFields + BelongsToFamily,
    HListRepr<T>: PopFirstField + HasFirstField<FirstField = Self::FirstField>,
    TailFields<HListRepr<T>>: IntoFamily<Family<Self>, Output = Self::TailFields>,
{
    #[inline(always)]
    fn pop_first_field(self) -> (Self::FirstField, Self::TailFields) {
        let (first, init) = self.into_hlist().pop_first_field();
        let init = init.into_family_of::<Self>();
        (first, init)
    }
}



// ====================
// === PopLastField ===
// ====================

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
// === MapFieldsWith ===
// =====================

/// Map all fields with the provided mapper [`M`]. There are multiple mappers defined out of the
/// box, such as [`MapperInto`], which maps all fields with [`Into`]. Alternatively, you can define
/// your own mapper.
#[allow(missing_docs)]
pub trait MapFieldsWith<M>: Sized {
    type MappedFieldsWith;
    /// Instead of using this function, use the [`_MapFieldsWith::map_fields_with`] which allows for
    /// a turbofish syntax.
    fn _map_fields_with(self, mapper: &M) -> Self::MappedFieldsWith;
}

/// The type `T` with all fields mapped with the mapper `M`.
pub type MappedFieldsWith<T, M> = <T as MapFieldsWith<M>>::MappedFieldsWith;

/// Map a single field with the provided mapper [`M`]. To learn more, see the docs of
/// [`MapFieldsWith`].
#[allow(missing_docs)]
pub trait MapFieldWith<M> {
    type MappedFieldWith;
    fn map_field_with(self, mapper: &M) -> Self::MappedFieldWith;
}

/// The field `T` mapped with the mapper `M`.
pub type MappedFieldWith<T, M> = <T as MapFieldWith<M>>::MappedFieldWith;

/// Wrapper for [`MapFieldsWith`] enabling the syntax `t.map_fields_with::<Mapper>()`. This trait is
/// automatically implemented for all structs.
#[allow(missing_docs)]
pub trait _MapFields: Sized {
    #[inline(always)]
    fn map_fields_with<M>(self, mapper: &M) -> MappedFieldsWith<Self, M>
    where Self: MapFieldsWith<M> {
        MapFieldsWith::<M>::_map_fields_with(self, mapper)
    }
}
impl<T> _MapFields for T {}

impl<M> MapFieldsWith<M> for Nil {
    type MappedFieldsWith = Nil;
    #[inline(always)]
    fn _map_fields_with(self, _mapper: &M) -> Self::MappedFieldsWith {
        Nil
    }
}

impl<M, H, T> MapFieldsWith<M> for Cons<H, T>
where
    H: MapFieldWith<M>,
    T: MapFieldsWith<M>,
{
    type MappedFieldsWith = Cons<MappedFieldWith<H, M>, MappedFieldsWith<T, M>>;
    #[inline(always)]
    fn _map_fields_with(self, mapper: &M) -> Self::MappedFieldsWith {
        Cons(
            MapFieldWith::<M>::map_field_with(self.0, mapper),
            MapFieldsWith::<M>::_map_fields_with(self.1, mapper),
        )
    }
}

impl<M, T> MapFieldsWith<M> for T
where
    T: BelongsToFamily,
    T: IntoHList,
    HListRepr<T>: MapFieldsWith<M>,
    MappedFieldsWith<HListRepr<T>, M>: IntoFamily<Family<T>>,
{
    type MappedFieldsWith = <MappedFieldsWith<HListRepr<T>, M> as IntoFamily<Family<T>>>::Output;
    #[inline(always)]
    fn _map_fields_with(self, mapper: &M) -> Self::MappedFieldsWith {
        self.into_hlist()._map_fields_with(mapper).into_family_of::<T>()
    }
}


// === MapperInto ===

/// Mapper which maps every field with [`Into`].
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Copy(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(Default(bound = ""))]
pub struct MapperInto<S>(ZST<S>);

impl<T: Into<S>, S> MapFieldWith<MapperInto<S>> for T {
    type MappedFieldWith = S;
    #[inline(always)]
    fn map_field_with(self, _mapper: &MapperInto<S>) -> Self::MappedFieldWith {
        self.into()
    }
}

/// Alias for [`MapFieldsWith<MapperInto<S>>`]. This trait is automatically implemented for every
/// struct.
pub trait MapFieldsInto<S>: MapFieldsWith<MapperInto<S>> {
    /// Instead of using this function, use the [`_MapFieldsInto::map_fields_into`] which allows for
    /// a turbofish syntax.
    #[inline(always)]
    fn _map_fields_into(self) -> MappedFieldsWith<Self, MapperInto<S>> {
        self.map_fields_with(&MapperInto::<S>::default())
    }
}
impl<T, S> MapFieldsInto<S> for T where T: MapFieldsWith<MapperInto<S>> {}

/// The result of mapping all fields of `T` with [`Into`].
pub type MappedFieldsInto<T, S> = MappedFieldsWith<T, MapperInto<S>>;

/// Wrapper for [`MapFieldsInto`] enabling syntax `t.map_fields_into::<S>()`. This trait is
/// automatically implemented for every struct.
#[allow(missing_docs)]
pub trait _MapFieldsInto {
    #[inline(always)]
    fn map_fields_into<S>(self) -> MappedFieldsWith<Self, MapperInto<S>>
    where Self: MapFieldsWith<MapperInto<S>> {
        self.map_fields_with(&MapperInto::<S>::default())
    }
}
impl<T> _MapFieldsInto for T {}



// =================
// === MapFields ===
// =================

#[allow(missing_docs)]
pub trait MapFields<F, F2> {
    type MappedFields;
    fn map_fields(self, f: impl FnMut(F) -> F2) -> Self::MappedFields;
}

/// The result of mapping all fields of `S` with the provided function `Fn(F) -> F2`.
pub type MappedFields<S, F, F2> = <S as MapFields<F, F2>>::MappedFields;

impl<F, F2> MapFields<F, F2> for Nil {
    type MappedFields = Nil;
    #[inline(always)]
    fn map_fields(self, _f: impl FnMut(F) -> F2) -> Self::MappedFields {
        Nil
    }
}

impl<F, F2, T> MapFields<F, F2> for Cons<F, T>
where T: MapFields<F, F2>
{
    type MappedFields = Cons<F2, MappedFields<T, F, F2>>;
    #[inline(always)]
    fn map_fields(self, mut f: impl FnMut(F) -> F2) -> Self::MappedFields {
        Cons(f(self.0), self.1.map_fields(f))
    }
}

impl<F, F2, T> MapFields<F, F2> for T
where
    T: BelongsToFamily + IntoHList,
    HListRepr<T>: MapFields<F, F2>,
    MappedFields<HListRepr<T>, F, F2>: IntoFamily<Family<T>>,
{
    type MappedFields = <MappedFields<HListRepr<T>, F, F2> as IntoFamily<Family<T>>>::Output;
    #[inline(always)]
    fn map_fields(self, f: impl FnMut(F) -> F2) -> Self::MappedFields {
        self.into_hlist().map_fields(f).into_family_of::<T>()
    }
}



// =====================
// === IntoFieldIter ===
// =====================

/// Take ownership of the structure and evaluate the provided function for every field. Please note
/// that all fields have to be of the same type. If you want to iterate over fields with different
/// types, use the [`MapFieldsWith`] interface with a custom defined mapper.
#[allow(missing_docs)]
pub trait IntoFieldIter<F> {
    fn into_field_iter(self, f: impl FnMut(F));
}

impl<F> IntoFieldIter<F> for Nil {
    #[inline(always)]
    fn into_field_iter(self, _f: impl FnMut(F)) {}
}

impl<H, T> IntoFieldIter<H> for Cons<H, T>
where T: IntoFieldIter<H>
{
    #[inline(always)]
    fn into_field_iter(self, mut f: impl FnMut(H)) {
        f(self.0);
        self.1.into_field_iter(f);
    }
}

impl<H, T> IntoFieldIter<H> for T
where
    T: IntoHList,
    HListRepr<T>: IntoFieldIter<H>,
{
    #[inline(always)]
    fn into_field_iter(self, f: impl FnMut(H)) {
        self.into_hlist().into_field_iter(f);
    }
}



// =================
// === FieldIter ===
// =================

/// Evaluate the provided function for every field reference. Please note that all fields have to be
/// of the same type. If you want to iterate over fields with different types, use the
/// [`MapFieldsWith`] interface with a custom defined mapper.
#[allow(missing_docs)]
pub trait FieldIter<F> {
    fn field_iter(&self, f: impl FnMut(&F));
}

impl<F> FieldIter<F> for Nil {
    #[inline(always)]
    fn field_iter(&self, _f: impl FnMut(&F)) {}
}

impl<H, T> FieldIter<H> for Cons<H, T>
where T: FieldIter<H>
{
    #[inline(always)]
    fn field_iter(&self, mut f: impl FnMut(&H)) {
        f(&self.0);
        self.1.field_iter(f);
    }
}

trait FieldIterHelper<'t, F> = where
    Self: 't,
    F: 't,
    &'t Self: IntoHList,
    HListRepr<&'t Self>: IntoFieldIter<&'t F>;

impl<F, T> FieldIter<F> for T
where
    for<'t> &'t T: HasHListRepr,
    for<'t> T: FieldIterHelper<'t, F>,
{
    #[inline(always)]
    fn field_iter(&self, f: impl FnMut(&F)) {
        self.into_hlist().into_field_iter(f)
    }
}



// ====================
// === FieldIterMut ===
// ====================

/// Evaluate the provided function for every field mutable reference. Please note that all fields
/// have to be of the same type. If you want to iterate over fields with different types, use the
/// [`MapFieldsWith`] interface with a custom defined mapper.
#[allow(missing_docs)]
pub trait FieldIterMut<F> {
    fn field_iter_mut(&mut self, f: impl FnMut(&mut F));
}

impl<F> FieldIterMut<F> for Nil {
    #[inline(always)]
    fn field_iter_mut(&mut self, _f: impl FnMut(&mut F)) {}
}

impl<H, T> FieldIterMut<H> for Cons<H, T>
where T: FieldIterMut<H>
{
    #[inline(always)]
    fn field_iter_mut(&mut self, mut f: impl FnMut(&mut H)) {
        f(&mut self.0);
        self.1.field_iter_mut(f);
    }
}

trait FieldIterMutHelper<'t, F> = where
    Self: 't,
    F: 't,
    &'t mut Self: IntoHList,
    HListRepr<&'t mut Self>: IntoFieldIter<&'t mut F>;

impl<F, T> FieldIterMut<F> for T
where
    for<'t> &'t mut T: HasHListRepr,
    for<'t> T: FieldIterMutHelper<'t, F>,
{
    #[inline(always)]
    fn field_iter_mut(&mut self, f: impl FnMut(&mut F)) {
        self.into_hlist().into_field_iter(f)
    }
}
