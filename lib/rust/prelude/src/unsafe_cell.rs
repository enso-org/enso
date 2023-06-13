//! Additional methods for [`UnsafeCell`].

use std::cell::UnsafeCell;



// =====================
// === UnsafeCellOps ===
// =====================

/// Additional methods for [`UnsafeCell`].
pub trait UnsafeCellOps<T> {
    /// Borrow the cell content.
    ///
    /// # Safety
    /// It is not checked whether the cell is already mutably borrowed.
    #[allow(unsafe_code)]
    unsafe fn unchecked_borrow(&self) -> &T;

    /// Mutably borrow the cell content.
    ///
    /// # Safety
    /// It is not checked whether the cell is already borrowed.
    #[allow(clippy::mut_from_ref)]
    #[allow(unsafe_code)]
    unsafe fn unchecked_borrow_mut(&self) -> &mut T;
}

impl<T> UnsafeCellOps<T> for UnsafeCell<T> {
    #[allow(unsafe_code)]
    unsafe fn unchecked_borrow(&self) -> &T {
        unsafe { &*self.get() }
    }

    #[allow(unsafe_code)]
    unsafe fn unchecked_borrow_mut(&self) -> &mut T {
        unsafe { &mut *self.get() }
    }
}
