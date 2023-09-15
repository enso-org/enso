//! [`HList`] provides many operations to create and manipulate heterogenous lists whose length
//! and element types are known at compile-time. [`HLists`] can be used to implement records,
//! variants, type-indexed products (TIP), type-indexed co-products (TIC), or keyword arguments.
//!
//! Each [`HList`] is encoded using [`Cons`] and [`Nil`]. For example, a two-element [`HList`] can
//! be encoded as `Cons(A,Cons(B,Nil))`. You can use the provided [`new`], [`pat`] and [`ty`] macros
//! to easily work with HLists, for example:  
//! ```text
//! let HList::pat![t1, t2] : HList::ty![&str, usize] = HList::new!["hello", 7];
//! ```



/// Common traits for [`HList`] operations.
pub mod traits {
    pub use super::AsHList as _TRAIT_AsHList;
    pub use super::AsHListMut as _TRAIT_AsHListMut;
    pub use super::HasHListRepr as _TRAIT_HasRepr;
    pub use super::IntoHList as _TRAIT_IntoHList;
}



// =============
// === HList ===
// =============

/// Type of every `HList`.
pub trait HList = crate::HasFieldCount;

/// Empty `HList` value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Nil;

/// Non-empty `HList` with head and tail.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Cons<Head, Tail>(pub Head, pub Tail);



// === Smart Constructors ===

/// Creates new `HList` from the provided elements, similar to `vec!`. In order to provide type for
/// the list, use the `ty` macro. In order to pattern match on it, use the `pat` macro.
///
/// ```text
/// let HList::pat![t1,t2] : HList::ty![&str,usize] = HList::new!["hello",7];
/// ```
#[macro_export]
macro_rules! new {
    ($(,)*) => { $crate::Nil };
    ($t:expr $(,$ts:expr)* $(,)?) => {
        $crate::Cons($t, $crate::new!{ $($ts),* })
    }
}

/// Pattern matches on a `HList`. See docs of `new` to learn more.
#[macro_export]
macro_rules! pat {
    ($(,)*) => { $crate::Nil };
    ($t:pat $(,$ts:pat)* $(,)?) => {
        $crate::Cons($t, $crate::pat!{ $($ts),* })
    }
}

/// Smart `HList` type constructor. See docs of `new` to learn more.
#[macro_export]
macro_rules! ty {
    ($(,)*) => { $crate::Nil };
    ($t:ty $(,$ts:ty)* $(,)?) => {
        $crate::Cons<$t, $crate::ty!{ $($ts),* }>
    }
}



// ====================
// === HasHListRepr ===
// ====================

/// A generic representation of the given type. This is a [`HList`] representation of all struct
/// fields.
#[allow(missing_docs)]
pub trait HasHListRepr {
    type HListRepr: HList;
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
#[allow(missing_docs)]
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

/// Converts the struct to a [`HList`] containing references to all struct fields.
#[allow(missing_docs)]
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

/// Converts the struct to a [`HList`] containing mutable references to all struct fields.
#[allow(missing_docs)]
pub trait AsHListMut
where for<'t> &'t mut Self: HasHListRepr + Into<HListRepr<&'t mut Self>> {
    #[inline(always)]
    fn as_hlist_mut(&mut self) -> HListRepr<&mut Self> {
        self.into()
    }
}
impl<T> AsHListMut for T where for<'t> &'t mut T: HasHListRepr + Into<HListRepr<&'t mut T>> {}
