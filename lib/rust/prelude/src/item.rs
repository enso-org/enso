//! This module contains traits allowing working with "items" in a very general meaning. It
//! generalizes a lot of structures, such as [`Option`], [`RefCell`], [`Rc`], [`Box`], etc.,
//! allowing to work with them in a unified way.

use crate::std_reexports::*;

use crate::ZeroableOption;
use crate::ZeroableRefCell;

use std::cell::UnsafeCell;



// ===============
// === HasItem ===
// ===============

/// Trait for any type which wraps other type. See docs of `Wrapper` to learn more.
pub trait HasItem {
    type Item: ?Sized;
}

/// Accessor for the wrapped value.
pub type Item<T> = <T as HasItem>::Item;

/// Trait which enables `Sized` super-bound on the `Item` type.
pub trait HasSizedItem = HasItem where Item<Self>: Sized;


// === Impls ===

#[rustfmt::skip]
mod has_item_impls {
    use super::*;
    impl<T>         HasItem for Option<T>          { type Item = T; }
    impl<T>         HasItem for ZeroableOption<T>  { type Item = T; }
    impl<T, E>      HasItem for Result<T, E>       { type Item = T; }
    impl<T: ?Sized> HasItem for Box<T>             { type Item = T; }
    impl<T: ?Sized> HasItem for Rc<T>              { type Item = T; }
    impl<T>         HasItem for RefCell<T>         { type Item = T; }
    impl<T>         HasItem for ZeroableRefCell<T> { type Item = T; }
    impl<T>         HasItem for Cell<T>            { type Item = T; }
    impl<T>         HasItem for UnsafeCell<T>      { type Item = T; }
    impl            HasItem for String             { type Item = char; }
    impl<T>         HasItem for Vec<T>             { type Item = T; }
}


// =================
// === ItemClone ===
// =================

pub trait ItemClone = HasItem where <Self as HasItem>::Item: Clone;



// ================
// === FromItem ===
// ================

pub trait FromItem: HasItem {
    fn from_item(item: Self::Item) -> Self;
}

#[rustfmt::skip]
mod from_item_impls {
    use super::*;
    impl<T>    FromItem for Option<T>          { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl<T>    FromItem for ZeroableOption<T>  { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl<T, E> FromItem for Result<T, E>       { fn from_item(t: Self::Item) -> Self {Ok(t)} }
    impl<T>    FromItem for Box<T>             { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl<T>    FromItem for Rc<T>              { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl<T>    FromItem for RefCell<T>         { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl<T>    FromItem for ZeroableRefCell<T> { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl<T>    FromItem for Cell<T>            { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl<T>    FromItem for UnsafeCell<T>      { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl       FromItem for String             { fn from_item(t: Self::Item) -> Self {t.into()} }
    impl<T>    FromItem for Vec<T>             { fn from_item(t: Self::Item) -> Self {vec![t]} }
}



// ===============
// === ItemRef ===
// ===============

/// Get reference to the contained item. This should be implemented only by structs which always
/// contain one item.
///
/// Please note that this trait is similar to the [`Deref`] trait, however, it does not implement
/// `impl<'a, T> ItemRef for &'a T`, which allows writing unambiguous code easier.
#[allow(missing_docs)]
pub trait ItemRef: HasItem {
    fn item(&self) -> &Self::Item;
}

pub trait SizedItemRef = ItemRef + HasSizedItem;

impl<T: ?Sized> ItemRef for Box<T> {
    fn item(&self) -> &Self::Item {
        self.deref()
    }
}

impl<T: ?Sized> ItemRef for Rc<T> {
    fn item(&self) -> &Self::Item {
        self.deref()
    }
}



// ==================
// === ItemRefMut ===
// ==================

/// Get mutable reference to the contained item. This should be implemented by structs which contain
/// unconditionally one item.
///
/// Please note that this trait is similar to the [`DerefMut`] trait, however, it does not implement
/// `impl<'a, T> ItemRefMut for &'a mut T`, which allows writing unambiguous code easier.
#[allow(missing_docs)]
pub trait ItemRefMut: ItemRef {
    fn item_mut(&mut self) -> &mut Self::Item;
}

pub trait SizedItemRefMut = ItemRefMut + HasSizedItem;

impl<T: ?Sized> ItemRefMut for Box<T> {
    #[inline(always)]
    fn item_mut(&mut self) -> &mut Self::Item {
        self.deref_mut()
    }
}



// ===================
// === WithItemRef ===
// ===================

/// Runs a function on the reference to the content.
pub trait WithItemRef: HasSizedItem {
    /// Runs a function on the reference to the content.
    fn with_item<T>(&self, f: impl FnOnce(&Self::Item) -> T) -> T;
}

impl<T: SizedItemRef> WithItemRef for T {
    #[inline(always)]
    fn with_item<U>(&self, f: impl FnOnce(&Self::Item) -> U) -> U {
        f(self.item())
    }
}

impl<T> WithItemRef for RefCell<T> {
    #[inline(always)]
    fn with_item<U>(&self, f: impl FnOnce(&Self::Item) -> U) -> U {
        f(&*self.borrow())
    }
}

impl<T> WithItemRef for ZeroableRefCell<T> {
    #[inline(always)]
    fn with_item<U>(&self, f: impl FnOnce(&Self::Item) -> U) -> U {
        f(&*self.borrow())
    }
}



// ======================
// === WithItemRefMut ===
// ======================

/// Runs a function on the reference to the content.
pub trait WithItemRefMut: HasSizedItem {
    /// Runs a function on the reference to the content.
    fn with_item_mut<T>(&mut self, f: impl FnOnce(&mut Self::Item) -> T) -> T;
}

impl<T: SizedItemRefMut> WithItemRefMut for T {
    #[inline(always)]
    fn with_item_mut<U>(&mut self, f: impl FnOnce(&mut Self::Item) -> U) -> U {
        f(self.item_mut())
    }
}

impl<T> WithItemRefMut for RefCell<T> {
    #[inline(always)]
    fn with_item_mut<U>(&mut self, f: impl FnOnce(&mut Self::Item) -> U) -> U {
        f(&mut *self.borrow_mut())
    }
}

impl<T> WithItemRefMut for ZeroableRefCell<T> {
    #[inline(always)]
    fn with_item_mut<U>(&mut self, f: impl FnOnce(&mut Self::Item) -> U) -> U {
        f(&mut *self.borrow_mut())
    }
}



// ==================
// === OptItemRef ===
// ==================

/// Get reference to the contained item in case it exists.
pub trait OptItemRef: HasItem {
    /// Get the item, if any.
    fn opt_item(&self) -> Option<&Self::Item>;

    /// Check whether the item exists.
    fn has_item(&self) -> bool {
        self.opt_item().is_some()
    }
}

impl<T: ItemRef> OptItemRef for T {
    #[inline(always)]
    fn opt_item(&self) -> Option<&Self::Item> {
        Some(self.item())
    }
}

impl<T> OptItemRef for Option<T> {
    #[inline(always)]
    fn opt_item(&self) -> Option<&Self::Item> {
        self.as_ref()
    }
}

impl<T> OptItemRef for ZeroableOption<T> {
    #[inline(always)]
    fn opt_item(&self) -> Option<&Self::Item> {
        self.as_ref()
    }
}

impl<T, E> OptItemRef for Result<T, E> {
    #[inline(always)]
    fn opt_item(&self) -> Option<&Self::Item> {
        self.as_ref().ok()
    }
}



// =====================
// === OptItemRefMut ===
// =====================

/// Get reference to the contained item in case it exists.
pub trait OptItemRefMut: OptItemRef {
    /// Unwraps this type to get the inner value.
    fn opt_item_mut(&mut self) -> Option<&mut Self::Item>;
}

impl<T: ItemRefMut> OptItemRefMut for T {
    fn opt_item_mut(&mut self) -> Option<&mut Self::Item> {
        Some(self.item_mut())
    }
}

impl<T> OptItemRefMut for Option<T> {
    fn opt_item_mut(&mut self) -> Option<&mut Self::Item> {
        self.as_mut()
    }
}

impl<T> OptItemRefMut for ZeroableOption<T> {
    fn opt_item_mut(&mut self) -> Option<&mut Self::Item> {
        self.as_mut()
    }
}

impl<T, E> OptItemRefMut for Result<T, E> {
    fn opt_item_mut(&mut self) -> Option<&mut Self::Item> {
        self.as_mut().ok()
    }
}
