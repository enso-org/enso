//! This module defines utilities for working with the [`SmallVec`] type.

pub use smallvec::SmallVec;
use std::cmp::Ordering;


/// Adds methods to the `SmallVec` type.
pub trait SmallVecOps {
    type Item;

    /// Binary searches this sorted slice with a comparator function.
    ///
    /// The comparator function should implement an order consistent
    /// with the sort order of the underlying slice, returning an
    /// order code that indicates whether its argument is `Less`,
    /// `Equal` or `Greater` the desired target.
    ///
    /// If the value is found then [`Result::Ok`] is returned, containing the
    /// index of the matching element. If there are multiple matches, then any
    /// one of the matches could be returned. If the value is not found then
    /// [`Result::Err`] is returned, containing the index where a matching
    /// element could be inserted while maintaining sorted order.
    ///
    /// # Implementation Details
    /// Please note that the following implementation is a copy-paste from
    /// [`Vec::binary_search_by`].
    fn binary_search_by<F>(&self, f:F) -> Result<usize,usize>
    where F:FnMut(&Self::Item) -> Ordering;

    /// Binary searches this sorted slice for a given element.
    ///
    /// If the value is found then [`Result::Ok`] is returned, containing the
    /// index of the matching element. If there are multiple matches, then any
    /// one of the matches could be returned. If the value is not found then
    /// [`Result::Err`] is returned, containing the index where a matching
    /// element could be inserted while maintaining sorted order.
    ///
    /// # Implementation Details
    /// Please note that the following implementation is a copy-paste from
    /// [`Vec::binary_search`].
    fn binary_search(&self, t:&Self::Item) -> Result<usize, usize>
    where Self::Item:Ord;
}

impl<T:smallvec::Array> SmallVecOps for SmallVec<T> {
    type Item = <T as smallvec::Array>::Item;

    #[allow(unsafe_code)]
    fn binary_search_by<F>(&self, mut f:F) -> Result<usize, usize>
    where F:FnMut(&Self::Item) -> Ordering {
        let s = self;
        let mut size = s.len();
        if size == 0 {
            return Err(0);
        }
        let mut base = 0usize;
        while size > 1 {
            let half = size / 2;
            let mid = base + half;
            // SAFETY: the call is made safe by the following inconstants:
            // - `mid >= 0`: by definition
            // - `mid < size`: `mid = size / 2 + size / 4 + size / 8 ...`
            let cmp = f(unsafe { s.get_unchecked(mid) });
            base = if cmp == Ordering::Greater { base } else { mid };
            size -= half;
        }
        // SAFETY: base is always in [0, size) because base <= mid.
        let cmp = f(unsafe { s.get_unchecked(base) });
        if cmp == Ordering::Equal { Ok(base) } else { Err(base + (cmp == Ordering::Less) as usize) }
    }

    fn binary_search(&self, t:&Self::Item) -> Result<usize, usize>
    where Self::Item:Ord {
        self.binary_search_by(|p| p.cmp(t))
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::FromIterator;

    #[test]
    fn test_binary_search_by() {
        let v = SmallVec::<[usize;4]>::from_iter([5,10,20,40].iter().copied());
        assert_eq!(v.binary_search_by(|probe| probe.cmp(&0)), Err(0));
        assert_eq!(v.binary_search_by(|probe| probe.cmp(&5)), Ok(0));
        assert_eq!(v.binary_search_by(|probe| probe.cmp(&6)), Err(1));
        assert_eq!(v.binary_search_by(|probe| probe.cmp(&9)), Err(1));
        assert_eq!(v.binary_search_by(|probe| probe.cmp(&10)), Ok(1));
        assert_eq!(v.binary_search_by(|probe| probe.cmp(&11)), Err(2));
    }

    #[test]
    fn test_binary_search() {
        let v = SmallVec::<[usize;4]>::from_iter([5,10,20,40].iter().copied());
        assert_eq!(v.binary_search(&0), Err(0));
        assert_eq!(v.binary_search(&5), Ok(0));
        assert_eq!(v.binary_search(&6), Err(1));
        assert_eq!(v.binary_search(&9), Err(1));
        assert_eq!(v.binary_search(&10), Ok(1));
        assert_eq!(v.binary_search(&11), Err(2));
    }
}
