//! # Counter
//!
//! Counter type. Uses an internal global value to ensure every instance created has a different
//! value.

use crate::prelude::*;



// ===============
// === Counter ===
// ===============

/// Implements a globally-unique counter.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct Counter {
    value: std::num::NonZeroU64,
}

impl Counter {
    /// Generate a unique value.
    #[allow(clippy::new_without_default)] // Every new instance must have a different value.
    #[allow(unsafe_code)] // See comment inside.
    pub fn new() -> Self {
        use std::sync::atomic;
        static NEXT: atomic::AtomicU64 = atomic::AtomicU64::new(1);
        let value = NEXT.fetch_add(1, atomic::Ordering::Relaxed);
        // The counter is 64-bit. If we were to increment it 100 billion times per second,
        // it would take 5,845 years to wrap.
        let value = if cfg!(debug_assertions) {
            std::num::NonZeroU64::new(value).unwrap()
        } else {
            unsafe { std::num::NonZeroU64::new_unchecked(value) }
        };
        Self { value }
    }
}

impl From<Counter> for u64 {
    fn from(Counter { value }: Counter) -> Self {
        value.into()
    }
}

impl CloneRef for Counter {
    fn clone_ref(&self) -> Self {
        Self { value: self.value }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::Counter;

    #[test]
    fn test_counter() {
        let a = Counter::new();
        let b = Counter::new();
        assert_ne!(a, b);
    }
}
