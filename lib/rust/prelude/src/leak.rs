//! `Leak` is a utility trait that prevents the wrapped value from being dropped when `Leak` is
//! being dropped. This is achieved by wrapping it with [`ManuallyDrop`], which will prevent the
//! [`Drop`] handler from being called when local variable out of scope. Can be used for example
//! to keep components alive for the whole lifetime of the application.

use std::mem::ManuallyDrop;



// ============
// === Leak ===
// ============

/// Wrapper that will prevent the wrapped value from being dropped. Equivalent to calling
/// `mem::forget` on the wrapped value at the end of its scope.
pub trait Leak {
    fn leak(self) -> ManuallyDrop<Self>
    where Self: Sized {
        ManuallyDrop::new(self)
    }
}

impl<T> Leak for T {}
