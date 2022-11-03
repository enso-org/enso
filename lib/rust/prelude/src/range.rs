use std::ops::RangeInclusive;



pub mod traits {
    pub use super::RangeIntersect;
    // pub use super::RangeLen;
    pub use super::RangeOps;
    pub use super::RangeOverlap;
}



// ================
// === RangeOps ===
// ================

pub trait RangeOps {
    type Item;
    fn start(&self) -> &Self::Item;
    fn end(&self) -> &Self::Item;
    fn with_start(self, start: Self::Item) -> Self;
    fn with_end(self, end: Self::Item) -> Self;
}

impl<T> RangeOps for std::ops::Range<T> {
    type Item = T;

    fn start(&self) -> &Self::Item {
        &self.start
    }

    fn end(&self) -> &Self::Item {
        &self.end
    }

    fn with_start(self, start: Self::Item) -> Self {
        start..self.end
    }

    fn with_end(self, end: Self::Item) -> Self {
        self.start..end
    }
}

impl<T: Clone> RangeOps for std::ops::RangeInclusive<T> {
    type Item = T;

    fn start(&self) -> &Self::Item {
        self.start()
    }

    fn end(&self) -> &Self::Item {
        self.end()
    }

    fn with_start(self, start: Self::Item) -> Self {
        start..=self.end().clone()
    }

    fn with_end(self, end: Self::Item) -> Self {
        self.start().clone()..=end
    }
}



// ====================
// === RangeOverlap ===
// ====================

/// Functions for checking if two ranges overlap.
pub trait RangeOverlap<T = Self> {
    /// Check if the given range overlaps with this range. The argument range has to start after
    /// this range start.
    fn overlaps_sorted(&self, other: &T) -> bool;
    /// Check if the given range overlaps or touches this range. The argument range has to start
    /// after this range start.
    fn overlaps_or_touches_sorted(&self, other: &T) -> bool;
    /// Check if the given range overlaps with this range.
    fn overlaps(&self, other: &T) -> bool;
    /// Check if the given range overlaps or touches this range.
    fn overlaps_or_touches(&self, other: &T) -> bool;
}

impl<T: PartialOrd> RangeOverlap<std::ops::Range<T>> for std::ops::Range<T> {
    fn overlaps_sorted(&self, other: &Self) -> bool {
        other.start >= self.start && other.start < self.end
    }

    fn overlaps_or_touches_sorted(&self, other: &Self) -> bool {
        other.start >= self.start && other.start <= self.end
    }

    fn overlaps(&self, other: &Self) -> bool {
        self.overlaps_sorted(other) || other.overlaps_sorted(self)
    }

    fn overlaps_or_touches(&self, other: &Self) -> bool {
        self.overlaps_or_touches_sorted(other) || other.overlaps_or_touches_sorted(self)
    }
}

impl<T: PartialOrd> RangeOverlap<std::ops::RangeInclusive<T>> for std::ops::RangeInclusive<T> {
    fn overlaps_sorted(&self, other: &RangeInclusive<T>) -> bool {
        other.start() >= self.start() && other.start() <= self.end()
    }

    fn overlaps_or_touches_sorted(&self, other: &RangeInclusive<T>) -> bool {
        other.start() >= self.start() && other.start() <= self.end()
    }

    fn overlaps(&self, other: &Self) -> bool {
        self.overlaps_sorted(other) || other.overlaps_sorted(self)
    }

    fn overlaps_or_touches(&self, other: &Self) -> bool {
        self.overlaps_or_touches_sorted(other) || other.overlaps_or_touches_sorted(self)
    }
}



// ======================
// === RangeIntersect ===
// ======================

pub trait RangeIntersect<T = Self> {
    type Result;
    fn intersect(&self, other: &T) -> Self::Result;
}

impl<T> RangeIntersect<std::ops::RangeInclusive<T>> for std::ops::RangeInclusive<T>
where T: PartialOrd + Clone
{
    type Result = Option<std::ops::RangeInclusive<T>>;

    fn intersect(&self, other: &Self) -> Self::Result {
        if self.overlaps(other) {
            let self_start = self.start().clone();
            let self_end = self.end().clone();
            let other_start = other.start().clone();
            let other_end = other.end().clone();
            let start = if self_start > other_start { self_start } else { other_start };
            let end = if self_end < other_end { self_end } else { other_end };
            Some(start..=end)
        } else {
            None
        }
    }
}



// ================================
// === merge_overlapping_ranges ===
// ================================

pub fn merge_overlapping_ranges<R>(mut ranges: Vec<R>) -> impl Iterator<Item = R>
where
    R: Clone + RangeOverlap + RangeOps,
    <R as RangeOps>::Item: Clone + Ord, {
    crate::gen_iter!({
        ranges.sort_unstable_by(|a, b| a.start().cmp(b.start()));
        let mut iter = ranges.into_iter();
        let opt_current = iter.next();
        if let Some(mut current) = opt_current {
            let mut opt_next = iter.next();
            while let Some(next) = opt_next {
                if current.overlaps_or_touches_sorted(&next) {
                    if next.end() > current.end() {
                        current = current.with_end(next.end().clone());
                    }
                } else {
                    yield current;
                    current = next;
                }
                opt_next = iter.next();
            }
            yield current;
        }
    })
}


#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_merged {
        ([$($ts:tt)*], [$($ts2:tt)*]) => {
            let ranges = vec![$($ts)*];
            let expected = vec![$($ts2)*];
            let merged = merge_overlapping_ranges(ranges).collect::<Vec<_>>();
            assert_eq!(merged, expected);
        };
    }

    #[test]
    fn test_merge_overlapping_ranges_empty() {
        let empty: Vec<std::ops::Range<usize>> = vec![];
        assert_eq!(merge_overlapping_ranges(empty.clone()).collect::<Vec<_>>(), empty);
    }

    #[test]
    fn test_merge_overlapping_ranges() {
        test_merged!([0..1, 1..2], [0..2]);
        test_merged!([0..1, 1..2, 2..3], [0..3]);
        test_merged!([0..2, 3..5, 1..4], [0..5]);
        test_merged!([0..10, 5..15], [0..15]);

        test_merged!([0..=1, 1..=2], [0..=2]);
        test_merged!([0..=1, 1..=2, 2..=3], [0..=3]);
        test_merged!([0..=2, 3..=5, 1..=4], [0..=5]);
        test_merged!([0..=10, 5..=15], [0..=15]);

        test_merged!([0..2, 0..1], [0..2]);
        test_merged!([0..10, 4..5, 8..11], [0..11]);

        test_merged!([0..10, 20..30, 40..50], [0..10, 20..30, 40..50]);
        test_merged!([0..10, 5..20, 21..30, 25..40], [0..20, 21..40]);
        test_merged!([0..10, 10..20], [0..20]);
    }
}
