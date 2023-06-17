use crate::Clearable;
use crate::Deref;
use crate::DerefMut;
use crate::ImClearable;
use crate::Zeroable;

use core::fmt::Debug;
use std::cell::RefCell;



// ==================
// === RefCellOps ===
// ==================

pub trait RefCellOps {
    type Borrowed;
    /// Perform the provided lambda with the borrowed value of this `RefCell`.
    fn with_borrowed<T>(&self, f: impl FnOnce(&Self::Borrowed) -> T) -> T;
    /// Perform the provided lambda with the mutably borrowed value of this `RefCell`.
    fn with_borrowed_mut<T>(&self, f: impl FnOnce(&mut Self::Borrowed) -> T) -> T;
}

impl<T> RefCellOps for RefCell<T> {
    type Borrowed = T;
    #[inline(always)]
    fn with_borrowed<U>(&self, f: impl FnOnce(&Self::Borrowed) -> U) -> U {
        f(&*self.borrow())
    }
    #[inline(always)]
    fn with_borrowed_mut<U>(&self, f: impl FnOnce(&mut Self::Borrowed) -> U) -> U {
        f(&mut *self.borrow_mut())
    }
}

auto trait NotRefCell {}
impl<T> !NotRefCell for RefCell<T> {}

impl<T> RefCellOps for T
where
    T: NotRefCell + Deref,
    <T as Deref>::Target: RefCellOps,
{
    type Borrowed = <<T as Deref>::Target as RefCellOps>::Borrowed;
    #[inline(always)]
    fn with_borrowed<U>(&self, f: impl FnOnce(&Self::Borrowed) -> U) -> U {
        self.deref().with_borrowed(f)
    }
    #[inline(always)]
    fn with_borrowed_mut<U>(&self, f: impl FnOnce(&mut Self::Borrowed) -> U) -> U {
        self.deref().with_borrowed_mut(f)
    }
}


// ================
// === Borrowed ===
// ================

/// A value borrowed from a [`RefCell`]-like struct. In debug mode, this is an alias for
/// [`std::cell::Ref`]. In a non-debug mode, this is a zero-overhead wrapper for the borrowed value,
/// allowing uniform typing of borrowed values from [`ZeroOverheadRefCell`].
#[cfg(debug_assertions)]
pub type Borrowed<'a, T> = std::cell::Ref<'a, T>;

/// A value borrowed from a [`RefCell`]-like struct. In debug mode, this is an alias for
/// [`std::cell::Ref`].
#[cfg(not(debug_assertions))]
#[derive(Clone, Debug)]
pub struct Borrowed<'a, T: ?Sized + 'a>(&'a T);

#[cfg(not(debug_assertions))]
impl<'a, T> Borrowed<'a, T> {
    #[allow(missing_docs)]
    #[inline(always)]
    pub fn map<U, F>(orig: Borrowed<'a, T>, f: F) -> Borrowed<'a, U>
    where
        F: FnOnce(&'a T) -> &'a U,
        U: ?Sized, {
        Borrowed(f(orig.0))
    }
}

#[cfg(not(debug_assertions))]
impl<'a, T: ?Sized> Deref for Borrowed<'a, T> {
    type Target = T;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// A value mutably borrowed from a [`RefCell`]-like struct. In debug mode, this is an alias for
/// [`std::cell::RefMut`].
#[cfg(debug_assertions)]
pub type BorrowedMut<'a, T> = std::cell::RefMut<'a, T>;

/// A value mutably borrowed from a [`RefCell`]-like struct. In debug mode, this is an alias for
/// [`std::cell::RefMut`].
#[cfg(not(debug_assertions))]
#[derive(Debug)]
pub struct BorrowedMut<'a, T: ?Sized + 'a>(&'a mut T);

#[cfg(not(debug_assertions))]
impl<'a, T: ?Sized> Deref for BorrowedMut<'a, T> {
    type Target = T;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[cfg(not(debug_assertions))]
impl<'a, T: ?Sized> DerefMut for BorrowedMut<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}



// ===========================
// === ZeroOverheadRefCell ===
// ===========================

#[cfg(debug_assertions)]
use crate::ZeroableRefCell;
#[cfg(not(debug_assertions))]
use std::cell::UnsafeCell;

/// Just like [`RefCell`], but compiled as [`UnsafeCell`] in release mode, which makes it zero-cost.
///
/// Please use this type in performance-critical code sections only. The runtime overhead of
/// [`RefCell`] is small and in most cases negligible. Multiple mutable borrows of [`UnsafeCell`]
/// cause immediate undefined behavior, so all code using it must be extensively tested.
#[derive(Default, Zeroable)]
#[repr(transparent)]
pub struct ZeroOverheadRefCell<T> {
    #[cfg(not(debug_assertions))]
    inner: UnsafeCell<T>,
    #[cfg(debug_assertions)]
    inner: ZeroableRefCell<T>,
}

#[cfg(not(debug_assertions))]
#[allow(missing_docs)] // The functions reflect the [`RefCell`] API.
impl<T> ZeroOverheadRefCell<T> {
    #[inline(always)]
    pub fn new(t: T) -> Self {
        Self { inner: UnsafeCell::new(t) }
    }

    #[inline(always)]
    pub fn borrow(&self) -> Borrowed<T> {
        #[allow(unsafe_code)]
        unsafe {
            Borrowed(&*self.inner.get())
        }
    }

    #[inline(always)]
    pub fn borrow_mut(&self) -> BorrowedMut<T> {
        #[allow(unsafe_code)]
        unsafe {
            BorrowedMut(&mut *self.inner.get())
        }
    }

    #[inline(always)]
    pub fn replace(&self, t: T) -> T {
        std::mem::replace(&mut *self.borrow_mut(), t)
    }
}

#[cfg(not(debug_assertions))]
impl<T> RefCellOps for ZeroOverheadRefCell<T> {
    type Borrowed = T;
    #[inline(always)]
    fn with_borrowed<U>(&self, f: impl FnOnce(&Self::Borrowed) -> U) -> U {
        f(&*self.borrow())
    }
    #[inline(always)]
    fn with_borrowed_mut<U>(&self, f: impl FnOnce(&mut Self::Borrowed) -> U) -> U {
        f(&mut *self.borrow_mut())
    }
}

#[cfg(debug_assertions)]
#[allow(missing_docs)] // The functions reflect the [`RefCell`] API.
impl<T> ZeroOverheadRefCell<T> {
    #[inline(always)]
    pub fn new(t: T) -> Self {
        Self { inner: ZeroableRefCell::new(t) }
    }
}

#[cfg(debug_assertions)]
impl<T> Deref for ZeroOverheadRefCell<T> {
    type Target = ZeroableRefCell<T>;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[cfg(debug_assertions)]
impl<T> DerefMut for ZeroOverheadRefCell<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: Debug> Debug for ZeroOverheadRefCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.borrow(), f)
    }
}

impl<T: Clearable> ImClearable for ZeroOverheadRefCell<T> {
    #[inline(always)]
    fn clear_im(&self) {
        self.borrow_mut().clear()
    }
}
