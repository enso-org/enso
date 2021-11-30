//! Text selection and cursor implementation.

use crate::prelude::*;

use enso_text::unit::*;
use enso_text::Range;



// ================
// === Boundary ===
// ================

/// Selection boundary data type. In most cases it's either `Location` or `Bytes`.
pub trait Boundary = Copy + Ord + Eq;



// =============
// === Shape ===
// =============

/// Text selection shape. In case the `start` and `end` offsets are equal, the selection is
/// interpreted as a cursor. Please note that the start of the selection is not always smaller then
/// its end. If the selection was dragged from right to left, the start byte offset will be bigger
/// than the end. Use the `min` and `max` methods to discover the edges.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
#[allow(missing_docs)]
pub struct Shape<T = Location> {
    pub start: T,
    pub end:   T,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Shape<T: Boundary>(start: T, end: T) -> Shape<T> {
    Shape::new(start, end)
}

impl<T: Boundary> Shape<T> {
    /// Constructor.
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }

    /// Cursor constructor.
    pub fn new_cursor(start: T) -> Self {
        let end = start;
        Self { start, end }
    }

    /// Range of this selection.
    pub fn range(&self) -> Range<T> {
        (self.min()..self.max()).into()
    }

    /// Gets the earliest offset within the selection, ie the minimum of both edges.
    pub fn min(self) -> T {
        std::cmp::min(self.start, self.end)
    }

    /// Gets the latest offset within the selection, ie the maximum of both edges.
    pub fn max(self) -> T {
        std::cmp::max(self.start, self.end)
    }

    /// Replace start value.
    pub fn with_start(&self, start: T) -> Self {
        Self { start, ..*self }
    }

    /// Replace end value.
    pub fn with_end(&self, end: T) -> Self {
        Self { end, ..*self }
    }

    /// Map start value.
    pub fn map_start(&self, f: impl FnOnce(T) -> T) -> Self {
        self.with_start(f(self.start))
    }

    /// Map end value.
    pub fn map_end(&self, f: impl FnOnce(T) -> T) -> Self {
        self.with_end(f(self.end))
    }

    /// Map both start and end values.
    pub fn map(&self, f: impl Fn(T) -> T) -> Self {
        self.with_start(f(self.start)).with_end(f(self.end))
    }

    /// Produce cursor by snapping the end edge to the start one.
    pub fn snap_to_start(&self) -> Self {
        let start = self.start;
        let end = start;
        Self { start, end }
    }

    /// Produce cursor by snapping the end edge to the start one.
    pub fn snap_to_end(&self) -> Self {
        let end = self.end;
        let start = end;
        Self { start, end }
    }

    /// Determine whether the selection is a cursor.
    pub fn is_cursor(self) -> bool {
        self.start == self.end
    }
}



// =================
// === Selection ===
// =================

/// Text selection. It is a text selection `Shape` bundled with an `id` information, which is used
/// by graphical interface to track and animate the movement of the selections.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
#[allow(missing_docs)]
pub struct Selection<T = Location> {
    pub shape: Shape<T>,
    pub id:    usize,
}

impl<T> Deref for Selection<T> {
    type Target = Shape<T>;
    fn deref(&self) -> &Self::Target {
        &self.shape
    }
}

impl<T> DerefMut for Selection<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.shape
    }
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Selection<T: Boundary>(start: T, end: T, id: usize) -> Selection<T> {
    Selection::new(start, end, id)
}

impl<T: Boundary> Selection<T> {
    /// Constructor.
    pub fn new(start: T, end: T, id: usize) -> Self {
        let shape = Shape::new(start, end);
        Self { shape, id }
    }

    /// Cursor constructor.
    pub fn new_cursor(offset: T, id: usize) -> Self {
        let shape = Shape::new_cursor(offset);
        Self { shape, id }
    }

    /// Replace the shape value.
    pub fn with_shape(&self, shape: Shape<T>) -> Self {
        Self { shape, ..*self }
    }

    /// Map the shape value.
    pub fn map_shape(&self, f: impl FnOnce(Shape<T>) -> Shape<T>) -> Self {
        self.with_shape(f(self.shape))
    }

    /// Replace the start value.
    pub fn with_start(&self, start: T) -> Self {
        self.map_shape(|s| s.with_start(start))
    }

    /// Replace the end value.
    pub fn with_end(&self, end: T) -> Self {
        self.map_shape(|s| s.with_end(end))
    }

    /// Map the start value.
    pub fn map_start(&self, f: impl FnOnce(T) -> T) -> Self {
        self.map_shape(|s| s.map_start(f))
    }

    /// Map the end value.
    pub fn map_end(&self, f: impl FnOnce(T) -> T) -> Self {
        self.map_shape(|s| s.map_end(f))
    }

    /// Map both start and end values.
    pub fn map(&self, f: impl Fn(T) -> T) -> Self {
        self.map_shape(|s| s.map(f))
    }

    /// Convert selection to cursor by snapping the end edge to the start one.
    pub fn snap_to_start(&self) -> Self {
        self.map_shape(|s| s.snap_to_start())
    }

    /// Convert selection to cursor by snapping the start edge to the end one.
    pub fn snap_to_end(&self) -> Self {
        self.map_shape(|s| s.snap_to_end())
    }

    /// Indicate whether this region should merge with the next.
    /// Assumption: regions are sorted (self.min() <= other.min())
    #[allow(clippy::suspicious_operation_groupings)]
    pub fn should_merge_sorted(self, other: Selection<T>) -> bool {
        let non_zero_overlap = other.min() < self.max();
        let zero_overlap = (self.is_cursor() || other.is_cursor()) && other.min() == self.max();
        non_zero_overlap || zero_overlap
    }

    /// Merge self with an overlapping region. Retains direction of self.
    pub fn merge_with(self, other: Selection<T>) -> Selection<T> {
        let is_forward = self.end >= self.start;
        let new_min = std::cmp::min(self.min(), other.min());
        let new_max = std::cmp::max(self.max(), other.max());
        let (start, end) = if is_forward { (new_min, new_max) } else { (new_max, new_min) };
        Selection::new(start, end, self.id)
    }
}



// =============
// === Group ===
// =============

/// A set of zero or more selections.
///
/// The selections are kept in sorted order in order to maintain a good performance in algorithms.
/// It is used in many places, including selection merging process.
#[derive(Clone, Debug, Default)]
pub struct Group {
    sorted_selections: Vec<Selection>,
}

impl Deref for Group {
    type Target = [Selection];
    fn deref(&self) -> &[Selection] {
        &self.sorted_selections
    }
}

impl DerefMut for Group {
    fn deref_mut(&mut self) -> &mut [Selection] {
        &mut self.sorted_selections
    }
}

impl Group {
    /// Constructor.
    pub fn new() -> Group {
        Group::default()
    }

    /// Convert selections to cursors by snapping end edges to start ones.
    pub fn snap_selections_to_start(&self) -> Group {
        let sorted_selections = self.sorted_selections.iter().map(|t| t.snap_to_start()).collect();
        Self { sorted_selections }
    }

    /// Convert selections to cursors by snapping start edges to end ones.
    pub fn snap_selections_to_end(&self) -> Group {
        let sorted_selections = self.sorted_selections.iter().map(|t| t.snap_to_end()).collect();
        Self { sorted_selections }
    }

    /// Reference to the newest created selection if any.
    pub fn newest(&self) -> Option<&Selection> {
        self.sorted_selections.iter().max_by(|x, y| x.id.cmp(&y.id))
    }

    /// Reference to the oldest created selection if any.
    pub fn oldest(&self) -> Option<&Selection> {
        self.sorted_selections.iter().min_by(|x, y| x.id.cmp(&y.id))
    }

    /// Mutable reference to the newest created selection if any.
    pub fn newest_mut(&mut self) -> Option<&mut Selection> {
        self.sorted_selections.iter_mut().max_by(|x, y| x.id.cmp(&y.id))
    }

    /// Mutable reference to the oldest created selection if any.
    pub fn oldest_mut(&mut self) -> Option<&mut Selection> {
        self.sorted_selections.iter_mut().min_by(|x, y| x.id.cmp(&y.id))
    }

    /// Merge new selection with the group. This method implements merging logic.
    ///
    /// Two non-cursor regions merge if their interiors intersect. Merely touching at the edges does
    /// not cause a merge. A cursor merges with a non-cursor if it is in the interior or on either
    /// edge. Two cursors merge if they are the same offset.
    ///
    /// Performance note: should be O(1) if the new region strictly comes after all the others in
    /// the selection, otherwise O(n).
    pub fn merge(&mut self, region: Selection) {
        let mut ix = self.selection_index_on_the_left_to(region.min());
        if ix == self.sorted_selections.len() {
            self.sorted_selections.push(region);
        } else {
            let mut region = region;
            let mut end_ix = ix;
            if self.sorted_selections[ix].min() <= region.min() {
                if self.sorted_selections[ix].should_merge_sorted(region) {
                    region = region.merge_with(self.sorted_selections[ix]);
                } else {
                    ix += 1;
                }
                end_ix += 1;
            }

            let max_ix = self.sorted_selections.len();
            while end_ix < max_ix && region.should_merge_sorted(self.sorted_selections[end_ix]) {
                region = region.merge_with(self.sorted_selections[end_ix]);
                end_ix += 1;
            }

            if ix == end_ix {
                self.sorted_selections.insert(ix, region);
            } else {
                let start = ix + 1;
                let len = end_ix - ix - 1;
                self.sorted_selections[ix] = region;
                self.sorted_selections.drain(start..start + len);
            }
        }
    }

    /// The smallest index so that offset > region.max() for all preceding regions. Note that the
    /// index may be bigger than available indexes, which will mean that the new location should
    /// be inserted on the far right side.
    pub fn selection_index_on_the_left_to(&self, location: Location) -> usize {
        if self.sorted_selections.last().map(|t| location <= t.max()) == Some(true) {
            self.sorted_selections.binary_search_by(|r| r.max().cmp(&location)).unwrap_both()
        } else {
            self.sorted_selections.len()
        }
    }
}


// === Conversions ===

impl From<Selection> for Group {
    fn from(t: Selection) -> Self {
        let sorted_selections = vec![t];
        Self { sorted_selections }
    }
}

impl From<Option<Selection>> for Group {
    fn from(t: Option<Selection>) -> Self {
        t.map(|s| s.into()).unwrap_or_default()
    }
}


// === Iterators ===

impl<'t> IntoIterator for &'t Group {
    type Item = &'t Selection;
    type IntoIter = slice::Iter<'t, Selection>;
    fn into_iter(self) -> Self::IntoIter {
        self.sorted_selections.iter()
    }
}

impl IntoIterator for Group {
    type Item = Selection;
    type IntoIter = std::vec::IntoIter<Selection>;
    fn into_iter(self) -> Self::IntoIter {
        self.sorted_selections.into_iter()
    }
}

impl FromIterator<Selection> for Group {
    fn from_iter<T: IntoIterator<Item = Selection>>(iter: T) -> Self {
        let mut group = Group::new();
        for selection in iter {
            group.merge(selection);
        }
        group
    }
}
