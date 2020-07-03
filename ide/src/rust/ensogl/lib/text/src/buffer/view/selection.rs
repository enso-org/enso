//! Text selection and carets implementation.

use crate::prelude::*;

use crate::buffer::data::unit::*;
use crate::buffer::data::Range;



// =================
// === Selection ===
// =================

/// Text selection. In case the `start` and `end` offsets are equal, the selection is interpreted as
/// a caret. The `column` field is a saved horizontal position used primarily for line up/down
/// movement. Please note that the start of the selection is not always smaller then its end.
/// If the selection was dragged from right to left, the start byte offset will be bigger than the
/// end. Use the `min` and `max` methods to discover the edges.
#[derive(Clone,Copy,PartialEq,Eq,Debug,Default)]
#[allow(missing_docs)]
pub struct Selection {
    pub start  : Bytes,
    pub end    : Bytes,
    // FIXME: Column is encoded as byte offset now which obviously do not work correctly in this
    //        use case (lines could have different byte offsets for the same amount of grapheme
    //        clustes. To be fixed during further implementation.
    pub column : Option<Column>,
}

impl Selection {
    /// Constructor.
    pub fn new(start:Bytes, end:Bytes) -> Self {
        let column = default();
        Self {start,end,column}
    }

    /// Cursor constructor (zero-length selection).
    pub fn new_cursor(offset:Bytes) -> Self {
        Self::new(offset,offset)
    }

    /// Range of this selection.
    pub fn range(&self) -> Range<Bytes> {
        (self.start .. self.end).into()
    }

    /// Size of this selection in bytes.
    pub fn size(&self) -> Bytes {
        self.end - self.start
    }

    /// Gets the earliest offset within the selection, ie the minimum of both edges.
    pub fn min(self) -> Bytes {
        std::cmp::min(self.start, self.end)
    }

    /// Gets the latest offset within the selection, ie the maximum of both edges.
    pub fn max(self) -> Bytes {
        std::cmp::max(self.start, self.end)
    }

    /// Determines whether the selection is a caret (ie has an empty interior).
    pub fn is_caret(self) -> bool {
        self.start == self.end
    }

    /// Returns a selection with the given horizontal position.
    pub fn with_column(self, column:Option<Column>) -> Self {
        Self {column,..self}
    }

    /// Indicate whether this region should merge with the next.
    /// Assumption: regions are sorted (self.min() <= other.min())
    pub fn should_merge_sorted(self, other:Selection) -> bool {
        let non_zero_overlap = other.min() < self.max();
        let zero_overlap     = (self.is_caret() || other.is_caret()) && other.min() == self.max();
        non_zero_overlap || zero_overlap
    }

    /// Merge self with an overlapping region. Retains direction of self.
    pub fn merge_with(self, other: Selection) -> Selection {
        let is_forward  = self.end >= self.start;
        let new_min     = std::cmp::min(self.min(), other.min());
        let new_max     = std::cmp::max(self.max(), other.max());
        let (start,end) = if is_forward { (new_min,new_max) } else { (new_max,new_min) };
        Selection::new(start,end)
    }
}



// =============
// === Group ===
// =============

/// A set of zero or more selection regions, representing a selection state.
#[derive(Clone,Debug,Default)]
pub struct Group {
    sorted_regions: Vec<Selection>,
}

impl Deref for Group {
    type Target = [Selection];
    fn deref(&self) -> &[Selection] {
        &self.sorted_regions
    }
}

impl Group {
    /// Constructor.
    pub fn new() -> Group {
        Group::default()
    }

    /// Add a region to the selection. This method implements merging logic.
    ///
    /// Two non-caret regions merge if their interiors intersect. Merely touching at the edges does
    /// not cause a merge. A caret merges with a non-caret if it is in the interior or on either
    /// edge. Two carets merge if they are the same offset.
    ///
    /// Performance note: should be O(1) if the new region strictly comes after all the others in
    /// the selection, otherwise O(n).
    pub fn add(&mut self, region:Selection) {
        let mut ix = self.selection_on_the_left_to(region.min());
        if ix == self.sorted_regions.len() {
            self.sorted_regions.push(region);
            return;
        }
        let mut region = region;
        let mut end_ix = ix;
        if self.sorted_regions[ix].min() <= region.min() {
            if self.sorted_regions[ix].should_merge_sorted(region) {
                region = region.merge_with(self.sorted_regions[ix]);
            } else {
                ix += 1;
            }
            end_ix += 1;
        }

        let max_ix = self.sorted_regions.len();
        while end_ix < max_ix && region.should_merge_sorted(self.sorted_regions[end_ix]) {
            region = region.merge_with(self.sorted_regions[end_ix]);
            end_ix += 1;
        }

        if ix == end_ix {
            self.sorted_regions.insert(ix,region);
        } else {
            let start = ix + 1;
            let len   = end_ix - ix - 1;
            self.sorted_regions[ix] = region;
            self.sorted_regions.drain(start..start+len);
        }
    }

    /// The smallest index so that offset > region.max() for all preceding
    /// regions.
    pub fn selection_on_the_left_to(&self, offset:Bytes) -> usize {
        if self.sorted_regions.is_empty() || offset > self.sorted_regions.last().unwrap().max() {
            self.sorted_regions.len()
        } else {
            self.sorted_regions.binary_search_by(|r| r.max().cmp(&offset)).unwrap_both()
        }
    }
}

impl From<Selection> for Group {
    fn from(t:Selection) -> Self {
        let sorted_regions = vec![t];
        Self {sorted_regions}
    }
}

impl<'t> IntoIterator for &'t Group {
    type Item     = &'t Selection;
    type IntoIter = slice::Iter<'t,Selection>;
    fn into_iter(self) -> Self::IntoIter {
        self.sorted_regions.iter()
    }
}
