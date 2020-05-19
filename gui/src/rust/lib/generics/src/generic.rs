//! Generic representation of data types. Refer to the crate documentation to learn more.

// This crate defines many helper traits and uses this flag on purpose.
#![allow(missing_docs)]

use super::hlist;
pub use nalgebra::base::dimension::*;



// ==============
// === Traits ===
// ==============

/// Common traits.
pub mod traits {
    pub use super::HasRepr        as _TRAIT_HasRepr;
    pub use super::HasFieldsCount as _TRAIT_HasFieldsCount;

    pub use super::HasItemAt      as _TRAIT_HasItemAt;
    pub use super::HasItemAt0     as _TRAIT_HasItemAt0;
    pub use super::HasItemAt1     as _TRAIT_HasItemAt1;
    pub use super::HasItemAt2     as _TRAIT_HasItemAt2;
    pub use super::HasItemAt3     as _TRAIT_HasItemAt3;
    pub use super::HasItemAt4     as _TRAIT_HasItemAt4;
    pub use super::HasItemAt5     as _TRAIT_HasItemAt5;
    pub use super::HasItemAt6     as _TRAIT_HasItemAt6;
    pub use super::HasItemAt7     as _TRAIT_HasItemAt7;
    pub use super::HasItemAt8     as _TRAIT_HasItemAt8;
    pub use super::HasItemAt9     as _TRAIT_HasItemAt9;
    pub use super::HasItemAt10    as _TRAIT_HasItemAt10;
    pub use super::HasItemAt11    as _TRAIT_HasItemAt11;
    pub use super::HasItemAt12    as _TRAIT_HasItemAt12;
    pub use super::HasItemAt13    as _TRAIT_HasItemAt13;
    pub use super::HasItemAt14    as _TRAIT_HasItemAt14;
    pub use super::HasItemAt15    as _TRAIT_HasItemAt15;

    pub use super::_GetItemAt     as _TRAIT__GetItemAt;
    pub use super::GetItemAt      as _TRAIT_GetItemAt;
    pub use super::GetItemAt0     as _TRAIT_GetItemAt0;
    pub use super::GetItemAt1     as _TRAIT_GetItemAt1;
    pub use super::GetItemAt2     as _TRAIT_GetItemAt2;
    pub use super::GetItemAt3     as _TRAIT_GetItemAt3;
    pub use super::GetItemAt4     as _TRAIT_GetItemAt4;
    pub use super::GetItemAt5     as _TRAIT_GetItemAt5;
    pub use super::GetItemAt6     as _TRAIT_GetItemAt6;
    pub use super::GetItemAt7     as _TRAIT_GetItemAt7;
    pub use super::GetItemAt8     as _TRAIT_GetItemAt8;
    pub use super::GetItemAt9     as _TRAIT_GetItemAt9;
    pub use super::GetItemAt10    as _TRAIT_GetItemAt10;
    pub use super::GetItemAt11    as _TRAIT_GetItemAt11;
    pub use super::GetItemAt12    as _TRAIT_GetItemAt12;
    pub use super::GetItemAt13    as _TRAIT_GetItemAt13;
    pub use super::GetItemAt14    as _TRAIT_GetItemAt14;
    pub use super::GetItemAt15    as _TRAIT_GetItemAt15;

    pub use super::ItemAt         as _TRAIT_ItemAt;
    pub use super::ItemAt0        as _TRAIT_ItemAt0;
    pub use super::ItemAt1        as _TRAIT_ItemAt1;
    pub use super::ItemAt2        as _TRAIT_ItemAt2;
    pub use super::ItemAt3        as _TRAIT_ItemAt3;
    pub use super::ItemAt4        as _TRAIT_ItemAt4;
    pub use super::ItemAt5        as _TRAIT_ItemAt5;
    pub use super::ItemAt6        as _TRAIT_ItemAt6;
    pub use super::ItemAt7        as _TRAIT_ItemAt7;
    pub use super::ItemAt8        as _TRAIT_ItemAt8;
    pub use super::ItemAt9        as _TRAIT_ItemAt9;
    pub use super::ItemAt10       as _TRAIT_ItemAt10;
    pub use super::ItemAt11       as _TRAIT_ItemAt11;
    pub use super::ItemAt12       as _TRAIT_ItemAt12;
    pub use super::ItemAt13       as _TRAIT_ItemAt13;
    pub use super::ItemAt14       as _TRAIT_ItemAt14;
    pub use super::ItemAt15       as _TRAIT_ItemAt15;
}



// ===============
// === HasRepr ===
// ===============

/// Association of a given type with its generic representation.
pub trait HasRepr {
    type GenericRepr : hlist::HList;
}

/// Type level accessor of a generic representation of the given type.
pub type Repr<T> = <T as HasRepr>::GenericRepr;

/// Converts the type to its generic representation. Please note that this trait is implemented
/// automatically for every type which implements `Into<Repr<Self>>`.
pub trait IntoGeneric : HasRepr + Into<Repr<Self>> {
    fn into_generic(self) -> Repr<Self> {
        self.into()
    }
}
impl<T> IntoGeneric for T where T : HasRepr + Into<Repr<T>> {}



// ======================
// === HasFieldsCount ===
// ======================

/// Information of field count of any structure implementing `Generics`. This trait is implemented
/// automatically.
pub trait HasFieldsCount {
    const FIELDS_COUNT : usize;
    fn fields_count() -> usize {
        Self::FIELDS_COUNT
    }
}

impl<T> HasFieldsCount for T
where T:HasRepr {
    const FIELDS_COUNT : usize = <Repr<T> as hlist::HasLength>::LEN;
}



// ================
// === HasIndex ===
// ================

/// Trait for heterogeneous containers like tuples which contain element at index `Ix`.
pub trait HasItemAt<Ix> { type Item; }
pub trait HasItemAt0  = HasItemAt<U0>;
pub trait HasItemAt1  = HasItemAt<U1>;
pub trait HasItemAt2  = HasItemAt<U2>;
pub trait HasItemAt3  = HasItemAt<U3>;
pub trait HasItemAt4  = HasItemAt<U4>;
pub trait HasItemAt5  = HasItemAt<U5>;
pub trait HasItemAt6  = HasItemAt<U6>;
pub trait HasItemAt7  = HasItemAt<U7>;
pub trait HasItemAt8  = HasItemAt<U8>;
pub trait HasItemAt9  = HasItemAt<U9>;
pub trait HasItemAt10 = HasItemAt<U10>;
pub trait HasItemAt11 = HasItemAt<U11>;
pub trait HasItemAt12 = HasItemAt<U12>;
pub trait HasItemAt13 = HasItemAt<U13>;
pub trait HasItemAt14 = HasItemAt<U14>;
pub trait HasItemAt15 = HasItemAt<U15>;

/// Type of element at index `Ix`. Useful for heterogeneous containers like tuples.
pub type ItemAt<Ix,T> = <T as HasItemAt<Ix>>::Item;
pub type ItemAt0  <T> = ItemAt <U0  , T>;
pub type ItemAt1  <T> = ItemAt <U1  , T>;
pub type ItemAt2  <T> = ItemAt <U2  , T>;
pub type ItemAt3  <T> = ItemAt <U3  , T>;
pub type ItemAt4  <T> = ItemAt <U4  , T>;
pub type ItemAt5  <T> = ItemAt <U5  , T>;
pub type ItemAt6  <T> = ItemAt <U6  , T>;
pub type ItemAt7  <T> = ItemAt <U7  , T>;
pub type ItemAt8  <T> = ItemAt <U8  , T>;
pub type ItemAt9  <T> = ItemAt <U9  , T>;
pub type ItemAt10 <T> = ItemAt <U10 , T>;
pub type ItemAt11 <T> = ItemAt <U11 , T>;
pub type ItemAt12 <T> = ItemAt <U12 , T>;
pub type ItemAt13 <T> = ItemAt <U13 , T>;
pub type ItemAt14 <T> = ItemAt <U14 , T>;
pub type ItemAt15 <T> = ItemAt <U15 , T>;

/// Accessor for element at index `Ix`.
pub trait GetItemAt<Ix> : HasItemAt<Ix> + _GetItemAt {
    fn get_item_at(&self) -> &ItemAt<Ix,Self>;
}

/// Smart wrapper for `GetItemAt`. Enables syntax `lst.item_at::<U0>()`.
impl<T>   _GetItemAt for T {}
pub trait _GetItemAt {
    fn item_at<Ix>(&self) -> &ItemAt<Ix,Self> where Self:GetItemAt<Ix> {
        GetItemAt::<Ix>::get_item_at(self)
    }
}

pub trait GetItemAt0  : GetItemAt <U0> { fn  _0(&self) -> &ItemAt0  <Self> {self.item_at::<U0> ()} }
pub trait GetItemAt1  : GetItemAt <U1> { fn  _1(&self) -> &ItemAt1  <Self> {self.item_at::<U1> ()} }
pub trait GetItemAt2  : GetItemAt <U2> { fn  _2(&self) -> &ItemAt2  <Self> {self.item_at::<U2> ()} }
pub trait GetItemAt3  : GetItemAt <U3> { fn  _3(&self) -> &ItemAt3  <Self> {self.item_at::<U3> ()} }
pub trait GetItemAt4  : GetItemAt <U4> { fn  _4(&self) -> &ItemAt4  <Self> {self.item_at::<U4> ()} }
pub trait GetItemAt5  : GetItemAt <U5> { fn  _5(&self) -> &ItemAt5  <Self> {self.item_at::<U5> ()} }
pub trait GetItemAt6  : GetItemAt <U6> { fn  _6(&self) -> &ItemAt6  <Self> {self.item_at::<U6> ()} }
pub trait GetItemAt7  : GetItemAt <U7> { fn  _7(&self) -> &ItemAt7  <Self> {self.item_at::<U7> ()} }
pub trait GetItemAt8  : GetItemAt <U8> { fn  _8(&self) -> &ItemAt8  <Self> {self.item_at::<U8> ()} }
pub trait GetItemAt9  : GetItemAt <U9> { fn  _9(&self) -> &ItemAt9  <Self> {self.item_at::<U9> ()} }
pub trait GetItemAt10 : GetItemAt<U10> { fn _10(&self) -> &ItemAt10 <Self> {self.item_at::<U10>()} }
pub trait GetItemAt11 : GetItemAt<U11> { fn _11(&self) -> &ItemAt11 <Self> {self.item_at::<U11>()} }
pub trait GetItemAt12 : GetItemAt<U12> { fn _12(&self) -> &ItemAt12 <Self> {self.item_at::<U12>()} }
pub trait GetItemAt13 : GetItemAt<U13> { fn _13(&self) -> &ItemAt13 <Self> {self.item_at::<U13>()} }
pub trait GetItemAt14 : GetItemAt<U14> { fn _14(&self) -> &ItemAt14 <Self> {self.item_at::<U14>()} }
pub trait GetItemAt15 : GetItemAt<U15> { fn _15(&self) -> &ItemAt15 <Self> {self.item_at::<U15>()} }

impl<T:GetItemAt<U0>>  GetItemAt0  for T {}
impl<T:GetItemAt<U1>>  GetItemAt1  for T {}
impl<T:GetItemAt<U2>>  GetItemAt2  for T {}
impl<T:GetItemAt<U3>>  GetItemAt3  for T {}
impl<T:GetItemAt<U4>>  GetItemAt4  for T {}
impl<T:GetItemAt<U5>>  GetItemAt5  for T {}
impl<T:GetItemAt<U6>>  GetItemAt6  for T {}
impl<T:GetItemAt<U7>>  GetItemAt7  for T {}
impl<T:GetItemAt<U8>>  GetItemAt8  for T {}
impl<T:GetItemAt<U9>>  GetItemAt9  for T {}
impl<T:GetItemAt<U10>> GetItemAt10 for T {}
impl<T:GetItemAt<U11>> GetItemAt11 for T {}
impl<T:GetItemAt<U12>> GetItemAt12 for T {}
impl<T:GetItemAt<U13>> GetItemAt13 for T {}
impl<T:GetItemAt<U14>> GetItemAt14 for T {}
impl<T:GetItemAt<U15>> GetItemAt15 for T {}
