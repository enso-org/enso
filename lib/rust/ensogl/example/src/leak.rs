//! `Leak` is a utility struct that prevents the wrapped value from being dropped when `Leak` is
//! being dropped. This is achieved by passing the contained value to `std::mem::forget` in the
//! drop implementation of `Leak`. Can bue used for examples to keep components alive for the whole
//! lifetime of the application.



// ============
// === Leak ===
// ============

/// Wrapper that will prevent the wrapped value from being dropped. Instead, the value will be
/// leaked when the `Leak` is dropped.
#[derive(Debug)]
pub struct Leak<T> {
    value: Option<T>,
}

impl<T> Leak<T> {
    /// Constructor. The passed value will be prevented from being dropped. This will cause memory
    /// leaks.
    pub fn new(value: T) -> Self {
        Self { value: Some(value) }
    }

    /// Return a reference to the wrapped value.
    pub fn inner(&self) -> &T {
        // Guaranteed to never panic as this is always initialised with `Some` in the constructor.
        self.value.as_ref().unwrap()
    }

    /// Return a mutable reference to the wrapped value.
    pub fn inner_mut(&mut self) -> &mut T {
        // Guaranteed to never panic as this is always initialised with `Some` in the constructor.
        self.value.as_mut().unwrap()
    }
}

impl<T> Drop for Leak<T> {
    fn drop(&mut self) {
        std::mem::forget(self.value.take());
    }
}
