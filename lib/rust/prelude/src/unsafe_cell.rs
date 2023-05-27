use std::cell::UnsafeCell;

pub trait UnsafeCellOps<T> {
    unsafe fn unchecked_borrow_mut(&self) -> &mut T;
    unsafe fn unchecked_borrow(&self) -> &T;
}

impl<T> UnsafeCellOps<T> for UnsafeCell<T> {
    unsafe fn unchecked_borrow_mut(&self) -> &mut T {
        unsafe { &mut *self.get() }
    }

    unsafe fn unchecked_borrow(&self) -> &T {
        unsafe { &*self.get() }
    }
}
