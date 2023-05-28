//! This type defines Wrap / Unwrap utilities. Unwrap is like `Deref` but does not implement
//! `impl<'a, T> Unwrap for &'a T` in order to make it less error prone. `Wrap` is like `pure` in
//! applicative functors – if lifts a value to the specific type.

use crate::std_reexports::*;
use crate::ZeroableOption;
use crate::ZeroableRefCell;
use std::cell::UnsafeCell;


// ===============
// === Content ===
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



// ===============
// === ItemRef ===
// ===============

/// Get reference to the contained item. This should be implemented by structs which contain
/// unconditionally one item.
///
/// Please note that this trait is similar to the [`Deref`] trait, however, it does not implement
/// `impl<'a, T> ItemRef for &'a T`, which allows writing unambiguous code easier.
pub trait ItemRef: HasItem {
    /// Unwraps this type to get the inner value.
    fn item(&self) -> &Self::Item;
}

impl<T: ?Sized> ItemRef for Rc<T> {
    fn item(&self) -> &Self::Item {
        self.deref()
    }
}



// ==================
// === ItemOptRef ===
// ==================

/// Get reference to the contained item in case it exists.
pub trait ItemOptRef: HasItem {
    /// Unwraps this type to get the inner value.
    fn try_item(&self) -> Option<&Self::Item>;
}

impl<T: ItemRef> ItemOptRef for T {
    fn try_item(&self) -> Option<&Self::Item> {
        Some(self.item())
    }
}

impl<T> ItemOptRef for Option<T> {
    fn try_item(&self) -> Option<&Self::Item> {
        self.as_ref()
    }
}

impl<T> ItemOptRef for ZeroableOption<T> {
    fn try_item(&self) -> Option<&Self::Item> {
        self.as_ref()
    }
}

impl<T, E> ItemOptRef for Result<T, E> {
    fn try_item(&self) -> Option<&Self::Item> {
        self.as_ref().ok()
    }
}



// ============
// === Wrap ===
// ============

/// Trait for objects which wrap values. Please note that this implements safe wrappers, so the
/// object - value relation must be bijective.
pub trait Wrapper = Wrap + ItemRef;

/// Wrapping utility for values.
pub trait Wrap: HasSizedItem {
    /// Wraps the value and returns the wrapped type.
    fn wrap(t: Self::Item) -> Self;
}



/// Runs a function on the reference to the content.
pub trait WithContent: HasSizedItem {
    /// Runs a function on the reference to the content.
    fn with_content<F, T>(&self, f: F) -> T
    where F: FnOnce(&Item<Self>) -> T;
}

/// Unwraps the content by consuming this value.
pub trait Unwrap: HasSizedItem {
    /// Unwraps the content by consuming this value.
    fn unwrap(self) -> Self::Item;
}


// === Utils ===

/// Wraps the value and returns the wrapped type.
pub fn wrap<T: Wrap>(t: T::Item) -> T {
    T::wrap(t)
}

/// Provides reference to the content of this value.
pub fn item<T: ItemRef>(t: &T) -> &T::Item {
    T::item(t)
}

/// Unwrap the content by consuming this value.
pub fn unwrap<T: Unwrap>(t: T) -> T::Item {
    T::unwrap(t)
}


// === Default Impls ===

impl<T: ItemRef + HasSizedItem> WithContent for T {
    fn with_content<F, S>(&self, f: F) -> S
    where F: FnOnce(&Item<Self>) -> S {
        f(self.item())
    }
}

// TODO: This should be implemented with the marker trait overlapping rules magic.
// impl<T:Deref> Unwrap for T
// where <T as Deref>::Target: Unwrap {
//     default fn unwrap(&self) -> &Self::Item {
//         self.deref().unwrap()
//     }
// }


// === Impls ===



impl<T> Wrap for Rc<T> {
    fn wrap(t: T) -> Self {
        Rc::new(t)
    }
}



impl Wrap for String {
    fn wrap(t: char) -> Self {
        t.to_string()
    }
}


impl<T> Wrap for Vec<T> {
    fn wrap(t: T) -> Self {
        vec![t]
    }
}
