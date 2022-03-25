//! This module re-exports a lot of useful stuff. It is not meant to be used
//! by libraries, but it is definitely usefull for bigger projects. It also
//! defines several aliases and utils which may find their place in new
//! libraries in the future.

// === Features ===
#![feature(concat_idents)]
#![feature(specialization)]
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(incomplete_features)] // To be removed, see: https://github.com/enso-org/ide/issues/1559
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(unsafe_code)]

#[cfg(feature = "futures")]
pub mod channel;
mod clone;
mod collections;
mod data;
pub mod debug;
pub mod env;
mod fail;
#[cfg(feature = "futures")]
pub mod future;
mod leak;
mod macros;
mod option;
mod phantom;
mod rc;
mod reference;
mod result;
#[cfg(feature = "serde")]
mod serde;
mod smallvec;
mod std_reexports;
mod string;
mod switch;
mod test;
mod tp;
mod vec;
mod wrapper;

#[cfg(feature = "serde")]
pub use crate::serde::*;
pub use crate::smallvec::*;
pub use clone::*;
pub use collections::*;
pub use data::*;
pub use debug::*;
pub use fail::*;
pub use leak::Leak;
pub use leak::*;
pub use macros::*;
pub use option::*;
pub use phantom::*;
pub use rc::*;
pub use reference::*;
pub use result::*;
pub use std_reexports::*;
pub use string::*;
pub use switch::*;
pub use test::traits::*;
pub use tp::*;
pub use vec::*;
pub use wrapper::*;

pub use boolinator::Boolinator;
pub use derivative::Derivative;
pub use derive_more::*;
pub use enclose::enclose;
pub use failure::Fail;
pub use ifmt::*;
pub use itertools::Itertools;
pub use lazy_static::lazy_static;
pub use num::Num;
pub use paste;
pub use shrinkwraprs::Shrinkwrap;
pub use weak_table;
pub use weak_table::traits::WeakElement;
pub use weak_table::traits::WeakKey;
pub use weak_table::WeakKeyHashMap;
pub use weak_table::WeakValueHashMap;

pub use std::collections::hash_map::DefaultHasher;
pub use std::hash::Hash;
pub use std::hash::Hasher;

use std::cell::UnsafeCell;

/// Serde reexports for the code generated by declarative macros.
///
/// They cannot be directly reexported from prelude, as the methods `serialize` and `deserialize`
/// that would be brought into scope by this, would collide with the other IDE-defined traits.
#[cfg(feature = "serde")]
pub mod serde_reexports {
    pub use serde::Deserialize;
    pub use serde::Serialize;
}

// =================
// === Immutable ===
// =================

/// A zero-overhead newtype which provides immutable access to its content. Of course this does not
/// apply to internal mutability of the wrapped data. A good use case of this structure is when you
/// want to pass an ownership to a structure, allow access all its public fields, but do not allow
/// their modification.
#[derive(Clone, Copy, Default, Eq, PartialEq)]
pub struct Immutable<T> {
    data: T,
}

/// Constructor of the `Immutable` struct.
#[allow(non_snake_case)]
pub fn Immutable<T>(data: T) -> Immutable<T> {
    Immutable { data }
}

impl<T: Debug> Debug for Immutable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

impl<T: Display> Display for Immutable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

impl<T: Clone> CloneRef for Immutable<T> {
    fn clone_ref(&self) -> Self {
        Self { data: self.data.clone() }
    }
}

impl<T> AsRef<T> for Immutable<T> {
    fn as_ref(&self) -> &T {
        &self.data
    }
}

impl<T> std::borrow::Borrow<T> for Immutable<T> {
    fn borrow(&self) -> &T {
        &self.data
    }
}

impl<T> Deref for Immutable<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

// ==============
// === ToImpl ===
// ==============

/// Provides method `to`, which is just like `into` but allows fo superfish syntax.
pub trait ToImpl: Sized {
    fn to<P>(self) -> P
    where Self: Into<P> {
        self.into()
    }
}
impl<T> ToImpl for T {}

#[cfg(feature = "nalgebra")]
impl<T, R, C, S> TypeDisplay for nalgebra::Matrix<T, R, C, S>
where
    T: nalgebra::Scalar,
    R: nalgebra::DimName,
    C: nalgebra::DimName,
{
    fn type_display() -> String {
        let cols = <C as nalgebra::DimName>::dim();
        let rows = <R as nalgebra::DimName>::dim();
        let item = type_name::<T>();
        match cols {
            1 => format!("Vector{}<{}>", rows, item),
            _ => format!("Matrix{}x{}<{}>", rows, cols, item),
        }
    }
}

#[macro_export]
macro_rules! clone_boxed {
    ( $name:ident ) => {
        paste::item! {
            #[allow(missing_docs)]
            pub trait [<CloneBoxedFor $name>] {
                fn clone_boxed(&self) -> Box<dyn $name>;
            }

            impl<T:Clone+$name+'static> [<CloneBoxedFor $name>] for T {
                fn clone_boxed(&self) -> Box<dyn $name> {
                    Box::new(self.clone())
                }
            }

            impl Clone for Box<dyn $name> {
                fn clone(&self) -> Self {
                    self.clone_boxed()
                }
            }
        }
    };
}

/// Alias for `for<'t> &'t Self : Into<T>`.
pub trait RefInto<T> = where for<'t> &'t Self: Into<T>;

// =================
// === CloneCell ===
// =================

#[derive(Debug)]
pub struct CloneCell<T> {
    data: UnsafeCell<T>,
}

impl<T> CloneCell<T> {
    pub fn new(elem: T) -> CloneCell<T> {
        CloneCell { data: UnsafeCell::new(elem) }
    }

    #[allow(unsafe_code)]
    pub fn get(&self) -> T
    where T: Clone {
        unsafe { (*self.data.get()).clone() }
    }

    #[allow(unsafe_code)]
    pub fn set(&self, elem: T) {
        unsafe {
            *self.data.get() = elem;
        }
    }

    #[allow(unsafe_code)]
    pub fn take(&self) -> T
    where T: Default {
        let ptr: &mut T = unsafe { &mut *self.data.get() };
        std::mem::take(ptr)
    }
}

impl<T: Clone> Clone for CloneCell<T> {
    fn clone(&self) -> Self {
        Self::new(self.get())
    }
}

impl<T: Default> Default for CloneCell<T> {
    fn default() -> Self {
        Self::new(default())
    }
}

// ====================
// === CloneRefCell ===
// ====================

#[derive(Debug)]
pub struct CloneRefCell<T: ?Sized> {
    data: UnsafeCell<T>,
}

impl<T> CloneRefCell<T> {
    pub fn new(elem: T) -> CloneRefCell<T> {
        CloneRefCell { data: UnsafeCell::new(elem) }
    }

    #[allow(unsafe_code)]
    pub fn get(&self) -> T
    where T: CloneRef {
        unsafe { (*self.data.get()).clone_ref() }
    }

    #[allow(unsafe_code)]
    pub fn set(&self, elem: T) {
        unsafe {
            *self.data.get() = elem;
        }
    }

    #[allow(unsafe_code)]
    pub fn take(&self) -> T
    where T: Default {
        let ptr: &mut T = unsafe { &mut *self.data.get() };
        std::mem::take(ptr)
    }
}

impl<T: CloneRef> Clone for CloneRefCell<T> {
    fn clone(&self) -> Self {
        Self::new(self.get())
    }
}

impl<T: CloneRef> CloneRef for CloneRefCell<T> {
    fn clone_ref(&self) -> Self {
        Self::new(self.get())
    }
}

impl<T: Default> Default for CloneRefCell<T> {
    fn default() -> Self {
        Self::new(default())
    }
}

// ================================
// === RefCell<Option<T>> Utils ===
// ================================

pub trait RefCellOptionOps<T> {
    fn clear(&self);
    fn set(&self, val: T);
}

impl<T> RefCellOptionOps<T> for RefCell<Option<T>> {
    fn clear(&self) {
        *self.borrow_mut() = None;
    }

    fn set(&self, val: T) {
        *self.borrow_mut() = Some(val);
    }
}

// ===============
// === HasItem ===
// ===============

/// Type family for structures containing items.
pub trait HasItem {
    type Item;
}

pub trait ItemClone = HasItem where <Self as HasItem>::Item: Clone;

impl<T> HasItem for Option<T> {
    type Item = T;
}
impl<T> HasItem for Cell<T> {
    type Item = T;
}
impl<T> HasItem for RefCell<T> {
    type Item = T;
}

// ===============================
// === CellGetter / CellSetter ===
// ===============================

/// Generalization of the [`Cell::get`] mechanism. Can be used for anything similar to [`Cell`].
pub trait CellGetter: HasItem {
    fn get(&self) -> Self::Item;
}

/// Generalization of the [`Cell::set`] mechanism. Can be used for anything similar to [`Cell`].
pub trait CellSetter: HasItem {
    fn set(&self, value: Self::Item);
}

/// Generalization of modify utilities for structures similar to [`Cell`].
pub trait CellProperty: CellGetter + CellSetter + ItemClone {
    /// Updates the contained value using a function and returns the new value.
    fn update<F>(&self, f: F) -> Self::Item
    where F: FnOnce(Self::Item) -> Self::Item {
        let new_val = f(self.get());
        self.set(new_val.clone());
        new_val
    }

    /// Modifies the contained value using a function and returns the new value.
    fn modify<F>(&self, f: F) -> Self::Item
    where F: FnOnce(&mut Self::Item) {
        let mut new_val = self.get();
        f(&mut new_val);
        self.set(new_val.clone());
        new_val
    }
}

impl<T: CellGetter + CellSetter + ItemClone> CellProperty for T {}

// === Impls ===

impl<T: Copy> CellGetter for Cell<T> {
    fn get(&self) -> Self::Item {
        self.get()
    }
}
impl<T: Copy> CellSetter for Cell<T> {
    fn set(&self, value: Self::Item) {
        self.set(value)
    }
}

// ================================
// === Strong / Weak References ===
// ================================

/// Abstraction for a strong reference like `Rc` or newtypes over it.
pub trait StrongRef: CloneRef {
    /// Downgraded reference type.
    type WeakRef: WeakRef<StrongRef = Self>;
    /// Creates a new weak reference of this allocation.
    fn downgrade(&self) -> Self::WeakRef;
}

/// Abstraction for a weak reference like `Weak` or newtypes over it.
pub trait WeakRef: CloneRef {
    /// Upgraded reference type.
    type StrongRef: StrongRef<WeakRef = Self>;
    /// Attempts to upgrade the weak referenc to a strong one, delaying dropping of the inner value
    /// if successful.
    fn upgrade(&self) -> Option<Self::StrongRef>;
}

impl<T: ?Sized> StrongRef for Rc<T> {
    type WeakRef = Weak<T>;
    fn downgrade(&self) -> Self::WeakRef {
        Rc::downgrade(self)
    }
}

impl<T: ?Sized> WeakRef for Weak<T> {
    type StrongRef = Rc<T>;
    fn upgrade(&self) -> Option<Self::StrongRef> {
        Weak::upgrade(self)
    }
}



// ======================
// === ImplementsDrop ===
// ======================

/// Check whether the structure implements custom drop behavior. Used mainly by the
/// [`NoCloneBecauseOfCustomDrop`] macro.
pub trait ImplementsDrop: Drop {}
