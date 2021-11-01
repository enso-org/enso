//! Text range implementation. Similar to `std::ops::Range` but with specialized implementations
//! for text manipulation.

use crate::prelude::*;

use super::rope;
use super::unit::*;



// =============
// === Range ===
// =============

/// A (half-open) range bounded inclusively below and exclusively above [start,end).
///
/// Unlike `std::ops::Range`, this type implements `Copy`, and contains text-related trait
/// implementations.
#[derive(Clone, Copy, Default, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Range<T> {
    pub start: T,
    pub end:   T,
}

impl<T> Range<T> {
    /// Constructor.
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }

    /// The size of the range.
    pub fn size(&self) -> T
    where T: Clone + Sub<T, Output = T> {
        self.end.clone() - self.start.clone()
    }

    /// Return new range with the provided start value.
    pub fn with_start(&self, start: T) -> Self
    where T: Clone {
        let end = self.end.clone();
        Self { start, end }
    }

    /// Return new range with the provided end value.
    pub fn with_end(&self, end: T) -> Self
    where T: Clone {
        let start = self.start.clone();
        Self { start, end }
    }

    /// Map both values with the provided function.
    pub fn map(&self, f: impl Fn(T) -> T) -> Self
    where T: Clone {
        self.with_start(f(self.start.clone())).with_end(f(self.end.clone()))
    }

    /// Map the start value with the provided function.
    pub fn map_start(&self, f: impl FnOnce(T) -> T) -> Self
    where T: Clone {
        self.with_start(f(self.start.clone()))
    }

    /// Map the end value with the provided function.
    pub fn map_end(&self, f: impl FnOnce(T) -> T) -> Self
    where T: Clone {
        self.with_end(f(self.end.clone()))
    }
}


// === Range<Bytes> methods ===

impl Range<Bytes> {
    /// Convert to `rope::Interval`.
    pub fn into_rope_interval(self) -> rope::Interval {
        self.into()
    }
}


// === Impls ===

impl<T: Display> Display for Range<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}, {})", self.start, self.end)
    }
}

impl<T: Debug> Debug for Range<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?}, {:?})", self.start, self.end)
    }
}

impl<T> From<std::ops::Range<T>> for Range<T> {
    fn from(range: std::ops::Range<T>) -> Range<T> {
        let std::ops::Range { start, end } = range;
        Range { start, end }
    }
}


// === Bytes Impls ===

impl From<RangeTo<Bytes>> for Range<Bytes> {
    fn from(range: RangeTo<Bytes>) -> Range<Bytes> {
        Range::new(0.bytes(), range.end)
    }
}

impl From<RangeInclusive<Bytes>> for Range<Bytes> {
    fn from(range: RangeInclusive<Bytes>) -> Range<Bytes> {
        Range::new(*range.start(), range.end().saturating_add(1.bytes()))
    }
}

impl From<RangeToInclusive<Bytes>> for Range<Bytes> {
    fn from(range: RangeToInclusive<Bytes>) -> Range<Bytes> {
        Range::new(0.bytes(), range.end.saturating_add(1.bytes()))
    }
}


// === Conversions ===

impl<T: Clone> From<&Range<T>> for Range<T> {
    fn from(t: &Range<T>) -> Self {
        t.clone()
    }
}

impl From<Range<Bytes>> for rope::Interval {
    fn from(t: Range<Bytes>) -> Self {
        let start = t.start.value as usize;
        let end = t.end.value as usize;
        Self { start, end }
    }
}



// ===================
// === RangeBounds ===
// ===================

/// RangeBounds allows converting all Rust ranges to the `Range` type, including open ranges, like
/// `..`, `a..`, `..b`, and `..=c`. When used for text manipulation, open ranges are clamped between
/// 0 bytes and the total bytes of the text.
pub trait RangeBounds {
    /// Clamp the range to the total bytes of the text/
    fn with_upper_bound(self, upper_bound: Bytes) -> Range<Bytes>;
}

impl<T: Into<Range<Bytes>>> RangeBounds for T {
    fn with_upper_bound(self, _upper_bound: Bytes) -> Range<Bytes> {
        self.into()
    }
}

impl RangeBounds for RangeFrom<Bytes> {
    fn with_upper_bound(self, upper_bound: Bytes) -> Range<Bytes> {
        Range::new(self.start, upper_bound)
    }
}

impl RangeBounds for RangeFull {
    fn with_upper_bound(self, upper_bound: Bytes) -> Range<Bytes> {
        Range::new(0.bytes(), upper_bound)
    }
}
