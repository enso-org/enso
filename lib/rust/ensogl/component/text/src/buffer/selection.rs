//! Text selection and cursor implementation.

use crate::prelude::*;
use enso_text::unit::*;

use enso_text::Range;



// ================
// === Boundary ===
// ================

/// Selection boundary data type. In most cases it's either `Location` or `Byte`.
pub trait Boundary = Copy + Ord + Eq;



// ==========
// === Id ===
// ==========

/// Selection ID.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Display, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id {
    pub value: usize,
}



// =============
// === Shape ===
// =============

/// Text selection shape. In case the `start` and `end` offsets are equal, the selection is
/// interpreted as a cursor. Please note that the start of the selection is not always smaller than
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
pub fn Shape<T>(start: T, end: T) -> Shape<T> {
    Shape::new(start, end)
}

impl<T> Shape<T> {
    /// Constructor.
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }
}

impl<T: Boundary> Shape<T> {
    /// Cursor constructor.
    pub fn new_cursor(start: T) -> Self {
        let end = start;
        Self { start, end }
    }

    /// Range of this selection.
    pub fn range(&self) -> Range<T> {
        (self.min()..self.max()).into()
    }

    /// Normalized version of selection where the start is always smaller than the end.
    pub fn normalized(self) -> Self {
        if self.start <= self.end {
            self
        } else {
            self.reversed()
        }
    }

    /// Reversed version of selection.
    pub fn reversed(self) -> Self {
        let start = self.end;
        let end = self.start;
        Self { start, end }
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
    pub fn map<S>(self, f: impl Fn(T) -> S) -> Shape<S> {
        let start = f(self.start);
        let end = f(self.end);
        Shape { start, end }
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

/// Text selection. It is a text selection [`Shape`] bundled with an [`Id`] information, which is
/// used by graphical interface to track and animate the movement of the selections.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Deref, DerefMut)]
#[allow(missing_docs)]
pub struct Selection<T = Location> {
    #[deref]
    #[deref_mut]
    pub shape: Shape<T>,
    pub id:    Id,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn Selection<T>(start: T, end: T, id: Id) -> Selection<T> {
    Selection::new(start, end, id)
}

impl<T> Selection<T> {
    /// Constructor.
    pub fn new(start: T, end: T, id: Id) -> Self {
        let shape = Shape::new(start, end);
        Self { shape, id }
    }
}

impl<T: Boundary> Selection<T> {
    /// Cursor constructor.
    pub fn new_cursor(offset: T, id: Id) -> Self {
        let shape = Shape::new_cursor(offset);
        Self { shape, id }
    }

    /// Replace the shape value.
    pub fn with_shape<S>(self, shape: Shape<S>) -> Selection<S> {
        let id = self.id;
        Selection { shape, id }
    }

    /// Map the shape value.
    pub fn map_shape<S>(self, f: impl FnOnce(Shape<T>) -> Shape<S>) -> Selection<S> {
        self.with_shape(f(self.shape))
    }

    /// Map the id value.
    pub fn map_id(self, f: impl FnOnce(Id) -> Id) -> Self {
        let id = f(self.id);
        Self { id, ..self }
    }

    /// Replace the start value.
    pub fn with_start(&self, start: T) -> Self {
        self.map_shape(|s| s.with_start(start))
    }

    /// Replace the end value.
    pub fn with_end(&self, end: T) -> Self {
        self.map_shape(|s| s.with_end(end))
    }

    /// Replace the location value.
    pub fn with_location(self, location: T) -> Self {
        self.with_start(location).with_end(location)
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
    pub fn map<S>(&self, f: impl Fn(T) -> S) -> Selection<S> {
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
/// Some algorithms, such as selection merging, require the selections to be in sorted order. This
/// invariant is restored as needed, to support asymptotically-efficient addition of selections to
/// the collection.
#[derive(Clone, Debug, Default)]
pub struct Group {
    sorted_selections: LazyInvariantVec<Selection, SortAndMerge>,
}

impl Deref for Group {
    type Target = [Selection];
    fn deref(&self) -> &[Selection] {
        self.sorted_selections.as_slice()
    }
}

impl DerefMut for Group {
    fn deref_mut(&mut self) -> &mut [Selection] {
        self.sorted_selections.as_mut_slice()
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
        self.sorted_selections.iter().max_by_key(|x| x.id)
    }

    /// Reference to the oldest created selection if any.
    pub fn oldest(&self) -> Option<&Selection> {
        self.sorted_selections.iter().min_by_key(|x| x.id)
    }

    /// Mutable reference to the newest created selection if any.
    pub fn newest_mut(&mut self) -> Option<&mut Selection> {
        let i = self.sorted_selections.iter().enumerate().max_by_key(|(_, x)| x.id).map(|(i, _)| i);
        i.and_then(|i| self.sorted_selections.get_mut(i))
    }

    /// Mutable reference to the oldest created selection if any.
    pub fn oldest_mut(&mut self) -> Option<&mut Selection> {
        let i = self.sorted_selections.iter().enumerate().min_by_key(|(_, x)| x.id).map(|(i, _)| i);
        i.and_then(|i| self.sorted_selections.get_mut(i))
    }

    /// Merge new selection with the group. This method implements merging logic.
    ///
    /// Two non-cursor regions merge if their interiors intersect. Merely touching at the edges does
    /// not cause a merge. A cursor merges with a non-cursor if it is in the interior or on either
    /// edge. Two cursors merge if they are the same offset.
    ///
    /// Performance:
    /// This operation is O(1), but reading from the vector when `d` new selections have been added
    /// since the last read requires re-establishing the invariant, which involves a `O(d log d)`
    /// operation.
    pub fn merge(&mut self, region: Selection) {
        self.sorted_selections.push(region);
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
        let sorted_selections = vec![t].into();
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


// === Merging ===

#[derive(Copy, Clone, Debug, Default)]
struct SortAndMerge;

impl<T: Boundary> lazy_invariant_vec::RestoreInvariant<Selection<T>> for SortAndMerge {
    fn restore_invariant(&mut self, clean: usize, elements: &'_ mut Vec<Selection<T>>) {
        sort_and_merge(clean, elements)
    }
}

/// Given a collection `elements`, the first `clean` of which are sorted, update it so that it is
/// fully-sorted, and overlapping elements have been merged.
///
/// Time complexity: `O(n + m log m)`, where `n` is the number of old elements, and `m` is the
/// number of newly-added elements.
fn sort_and_merge<T: Boundary>(clean: usize, elements: &mut Vec<Selection<T>>) {
    let new = Vec::with_capacity(elements.len());
    let mut old = mem::replace(elements, new);
    // Sort the newly-added elements using a standard, fast sorting implementation. Some may
    // overlap; we'll merge them below.
    old[clean..].sort_unstable_by_key(|x| x.min());
    let mut ys = old.split_off(clean).into_iter().peekable();
    let mut xs = old.into_iter().peekable();
    // Buffer the next element to be emitted; this is so we can merge selections, while merging the
    // sorted lists of selections.
    let mut a: Option<Selection<_>> = None;
    loop {
        // Advance `xs` or `ys` (whichever has a lesser next element), putting the result in `b`.
        let b = match (xs.peek(), ys.peek()) {
            (Some(x), Some(y)) =>
                if x.min() <= y.min() {
                    xs.next()
                } else {
                    ys.next()
                },
            _ => xs.next().or_else(|| ys.next()),
        };
        // Move data along this path: `b --> a --> elements`.
        // While doing so, merge `(a,b) --> a` if appropriate.
        match (a, b) {
            (Some(a_), Some(next)) if a_.should_merge_sorted(next) => a = Some(a_.merge_with(next)),
            (Some(a_), _) => {
                elements.push(a_);
                a = b;
            }
            (None, Some(b)) => a = Some(b),
            (None, None) => break,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // Check that some specific cases are completely correct, including the results of merging
    // different selections.
    #[test]
    fn test_sort_and_merge_cases() {
        let mut selections: LazyInvariantVec<Selection<_>, SortAndMerge> = default();
        selections.push(Selection::new(1, 3, Id { value: 1 }));
        selections.push(Selection::new(0, 2, Id { value: 0 }));
        assert_eq!(selections.as_slice(), &[Selection::new(0, 3, Id { value: 0 })]);
        selections.push(Selection::new(4, 4, Id { value: 2 }));
        selections.push(Selection::new(0, 5, Id { value: 3 }));
        assert_eq!(selections.as_slice(), &[Selection::new(0, 5, Id { value: 0 })]);
        selections.push(Selection::new(7, 9, Id { value: 4 }));
        selections.push(Selection::new(8, 10, Id { value: 5 }));
        assert_eq!(selections.as_slice(), &[
            Selection::new(0, 5, Id { value: 0 }),
            Selection::new(7, 10, Id { value: 4 }),
        ]);
        selections.push(Selection::new(20, 20, Id { value: 20 }));
        selections.push(Selection::new(19, 21, Id { value: 19 }));
        assert_eq!(selections.as_slice(), &[
            Selection::new(0, 5, Id { value: 0 }),
            Selection::new(7, 10, Id { value: 4 }),
            Selection::new(19, 21, Id { value: 19 }),
        ]);
        selections.push(Selection::new(-1, 100, Id { value: 100 }));
        let _ = selections.as_slice();
        assert_eq!(selections.as_slice(), &[Selection::new(-1, 100, Id { value: 100 })]);
    }

    // Check that the outputs obey the sorted-and-merged invariant for random inputs. This property
    // doesn't guarantee that the results are correct, but it's a property we can easily verify for
    // arbitrarily many inputs.
    #[test]
    fn test_sort_and_merge_property() {
        use rand::Rng;
        use rand::SeedableRng;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(0);
        let mut selections: LazyInvariantVec<Selection<_>, SortAndMerge> = default();
        for i in 1..=100 {
            // Generate a batch of changes. Expand the range with each batch, so that new each new
            // batch will have a chance of containing values greater or lower than all previous
            // values.
            for _ in 0..10 {
                let start = rng.gen_range(-(i * 2)..i * 10);
                let end = rng.gen_range(-(i * 2)..i * 10);
                selections.push(Selection::new(start, end, Id { value: 0 }));
            }
            let selections_are_sorted = selections.iter().is_sorted_by_key(|x| x.min());
            assert!(selections_are_sorted);
            let no_unmerged_pairs =
                !selections.array_windows::<2>().any(|&[a, b]| a.should_merge_sorted(b));
            assert!(no_unmerged_pairs);
        }
    }
}
