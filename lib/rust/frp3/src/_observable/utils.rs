use std::{any::Any, mem::ManuallyDrop};

pub struct OnDrop<F: FnOnce()> {
    callback: ManuallyDrop<F>,
}

impl<F: FnOnce()> OnDrop<F> {
    /// Returns an object that will invoke the specified callback when dropped.
    pub fn new(callback: F) -> Self {
        Self {
            callback: ManuallyDrop::new(callback),
        }
    }
}

impl<F: FnOnce()> Drop for OnDrop<F> {
    fn drop(&mut self) {
        // SAFETY: We may move out of `self`, since this instance can never be observed after it's dropped.
        let callback = unsafe { ManuallyDrop::take(&mut self.callback) };
        callback();
    }
}

/// Returns a reference to the inner value as type `dyn T`.
///
/// # Safety
///
/// The contained value must be of type `T`. Calling this method
/// with the incorrect type is *undefined behavior*.
pub unsafe fn downcast_ref_unchecked<T: Any>(any: &dyn Any) -> &T {
    debug_assert!(any.is::<T>());
    // SAFETY: caller guarantees that T is the correct type
    unsafe { &*(any as *const dyn Any as *const T) }
}

/// Returns a mutable reference to the inner value as type `dyn T`.
///
/// # Safety
///
/// The contained value must be of type `T`. Calling this method
/// with the incorrect type is *undefined behavior*.
pub unsafe fn downcast_mut_unchecked<T: Any>(any: &mut dyn Any) -> &mut T {
    debug_assert!(any.is::<T>());
    // SAFETY: caller guarantees that T is the correct type
    unsafe { &mut *(any as *mut dyn Any as *mut T) }
}
