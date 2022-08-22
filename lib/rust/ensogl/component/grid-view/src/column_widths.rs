//! Storage for column's widths in the GridView. See [`ColumnWidths`].

use crate::prelude::*;

use std::fmt::Formatter;



// ====================
// === ColumnWidths ===
// ====================

// === Helper definitions ===

type SegmentTree = segment_tree::PrefixPoint<f32, segment_tree::ops::Add>;

fn segment_tree(width_diffs: Vec<f32>) -> SegmentTree {
    segment_tree::PrefixPoint::build(width_diffs, segment_tree::ops::Add)
}


// === ColumnWidths ===

/// Storage for column widths.
///
/// It stores not the actual widths but rather the differences between the default size set by
/// [`Frp::set_entry_size`] endpoint and the width updated through [`Frp::set_column_width`] or
/// [`EntryFrp::override_column_width`].
///
/// It uses a segment tree data structure, allowing efficient access to cumulative differences
/// across column ranges. We use it in the [`ColumnWidths::pos_offset`] method to calculate the
/// position of the entry inside the grid view.
#[derive(Clone, CloneRef)]
pub struct ColumnWidths {
    width_diffs: Rc<RefCell<SegmentTree>>,
}

impl Debug for ColumnWidths {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("ColumnWidths")
            .field("width_diffs", &self.width_diffs.borrow().len())
            .finish()
    }
}

impl ColumnWidths {
    /// Constructor. Initializes the storage with every column having the default size inherited
    /// from [`Frp::set_entry_size`].
    pub fn new(number_of_columns: usize) -> Self {
        let init_vec = (0..number_of_columns).map(|_| 0.0).collect();
        let tree = segment_tree(init_vec);
        Self { width_diffs: Rc::new(RefCell::new(tree)) }
    }

    /// Set a width difference for the specified column.
    pub fn set_width_diff(&self, column: usize, width_diff: f32) {
        self.width_diffs.borrow_mut().set(column, width_diff);
    }

    /// Resize the storage to accommodate the new number of columns.
    pub fn resize(&self, number_of_columns: usize) {
        let mut values = self.width_diffs.take().unwrap();
        values.resize(number_of_columns, default());
        *self.width_diffs.borrow_mut() = segment_tree(values);
    }

    /// A cumulative sum of the differences for the columns to the left of the specified
    /// one. The result is a position offset from the normal position of the [`column`] inside
    /// the grid view.
    ///
    /// Works in O(log(N)) time.
    ///
    /// # Panics
    ///
    /// If the specified column is greater than the number of columns in the storage. The
    /// `colunm` can be equal to `number_of_columns` passed to the [`Self::new`] or
    /// [`Self::resize`].
    pub fn pos_offset(&self, column: usize) -> f32 {
        if column == 0 {
            0.0
        } else {
            self.width_diffs.borrow().query(column - 1)
        }
    }

    /// The difference between the original and overridden width of the column.
    ///
    /// Works in O(log(N)) time.
    pub fn width_diff(&self, column: usize) -> f32 {
        let borrowed = self.width_diffs.borrow();
        if column < borrowed.len() {
            borrowed.get(column)
        } else {
            0.0
        }
    }
}
