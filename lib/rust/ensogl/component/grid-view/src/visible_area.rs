//! Functions for evaluating which part of [`GridView`] is visible.

use crate::prelude::*;

use crate::Col;
use crate::ColumnWidths;
use crate::Row;
use crate::Viewport;



// ==========================================
// === Ranges of Rows and Columns Visible ===
// ==========================================

fn has_size(v: &Viewport) -> bool {
    v.right > v.left + f32::EPSILON && v.top > v.bottom + f32::EPSILON
}


/// Return range of visible rows.
pub fn visible_rows(v: &Viewport, entry_size: Vector2, row_count: usize) -> Range<Row> {
    let first_visible_unrestricted = (v.top / -entry_size.y).floor() as isize;
    let first_visible = first_visible_unrestricted.clamp(0, row_count as isize) as Row;
    let first_not_visible = if has_size(v) {
        let first_not_visible_unrestricted = (v.bottom / -entry_size.y).ceil() as isize;
        first_not_visible_unrestricted.clamp(0, row_count as isize) as Row
    } else {
        first_visible
    };
    first_visible..first_not_visible
}

/// Return range of visible columns.
pub fn visible_columns(
    v: &Viewport,
    entry_size: Vector2,
    col_count: usize,
    column_widths: &ColumnWidths,
) -> Range<Col> {
    let entry_width = entry_size.x;
    let column_left = |idx: usize| idx as f32 * entry_width + column_widths.pos_offset(idx);
    let column_right = |idx: usize| column_left(idx + 1);
    let index = |(idx, _): (usize, _)| idx;
    let clamp = |idx: isize| idx.clamp(0, col_count as isize) as Col;

    // We guess the first visible column by the position of the viewport. If there are no resized
    // columns (a common scenario), that would be the answer. If the guessed column is shifted by
    // resized columns (or is resized itself), then we iterate over all columns to find the
    // correct one. We repeat a similar process for the last visible column.
    let left_guess = clamp((v.left / entry_width).floor() as isize);
    let first_visible = {
        let pos_offset = column_widths.pos_offset(left_guess);
        let column_left_border = left_guess as f32 * entry_width + pos_offset;
        let column_width = entry_width + column_widths.width_diff(left_guess);
        let column_right_border = column_left_border + column_width;
        // The visibility of the first column is determined by its position and width. If the left
        // border of the column is not visible (not shifted), it does not mean the column itself is
        // invisible. It can be either visible or not, depending on its width.
        let is_partially_visible = column_left_border <= v.left && column_right_border >= v.left;
        let not_shifted = pos_offset == 0.0;

        if not_shifted && is_partially_visible {
            left_guess
        } else {
            let visible = |(_, x): &(_, f32)| v.left < *x;
            let mut right_borders = (0..col_count).map(|idx| (idx, column_right(idx)));
            right_borders.find(visible).map(index).unwrap_or(col_count)
        }
    };
    let first_not_visible = if has_size(v) {
        let right_guess = clamp((v.right / entry_width).ceil() as isize);
        let not_shifted = column_widths.pos_offset(right_guess) == 0.0;
        // The visibility of the last column does not depend on its width. If the left border of
        // the column is visible (not shifted) – it is always visible.
        if not_shifted {
            right_guess
        } else {
            let not_visible = |(_, x): &(_, f32)| v.right <= *x;
            let mut left_borders = (first_visible..col_count).map(|idx| (idx, column_left(idx)));
            left_borders.find(not_visible).map(index).unwrap_or(col_count)
        }
    } else {
        first_visible
    };
    first_visible..first_not_visible
}



// =============================
// === All Visible Locations ===
// =============================

/// Return iterator over all visible locations (row-column pairs).
pub fn all_visible_locations(
    v: &Viewport,
    entry_size: Vector2,
    row_count: usize,
    col_count: usize,
    column_widths: &ColumnWidths,
) -> impl Iterator<Item = (Row, Col)> {
    let visible_rows = visible_rows(v, entry_size, row_count);
    let visible_cols = visible_columns(v, entry_size, col_count, column_widths);
    itertools::iproduct!(visible_rows, visible_cols)
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    const ENTRY_SIZE: Vector2 = Vector2(20.0, 10.0);
    const ROW_COUNT: usize = 100;
    const COL_COUNT: usize = 100;

    #[test]
    fn visible_rows_and_columns() {
        #[derive(Clone, Debug)]
        struct Case {
            viewport:      Viewport,
            expected_rows: Range<Row>,
            expected_cols: Range<Col>,
            column_widths: ColumnWidths,
        }

        impl Case {
            fn new(
                (left, top): (f32, f32),
                (size_x, size_y): (f32, f32),
                expected_rows: Range<Row>,
                expected_cols: Range<Col>,
            ) -> Self {
                let right = left + size_x;
                let bottom = top - size_y;
                let viewport = Viewport { left, top, right, bottom };
                let column_widths = ColumnWidths::new(COL_COUNT);
                Self { viewport, expected_rows, expected_cols, column_widths }
            }

            fn run(&self) {
                assert_eq!(
                    visible_rows(&self.viewport, ENTRY_SIZE, ROW_COUNT),
                    self.expected_rows,
                    "Wrong visible rows in {self:?}"
                );
                assert_eq!(
                    visible_columns(&self.viewport, ENTRY_SIZE, COL_COUNT, &self.column_widths),
                    self.expected_cols,
                    "Wrong visible cols in {self:?}"
                );
            }
        }

        for case in [
            Case::new((0.0, 0.0), (40.0, 40.0), 0..4, 0..2),
            Case::new((1.0, -1.0), (40.0, 40.0), 0..5, 0..3),
            Case::new((-5.0, 5.0), (30.0, 30.0), 0..3, 0..2),
            Case::new((-20.0, 10.0), (30.0, 30.0), 0..2, 0..1),
            Case::new((-30.0, 30.0), (30.0, 30.0), 0..0, 0..0),
            Case::new((19.9, -9.9), (40.0, 40.0), 0..5, 0..3),
            Case::new((20.0, -10.0), (40.0, 40.0), 1..5, 1..3),
            Case::new((20.1, -10.1), (40.0, 40.0), 1..6, 1..4),
            Case::new((1960.0, -980.0), (40.0, 20.0), 98..100, 98..100),
            Case::new((1979.0, -989.0), (40.0, 20.0), 98..100, 98..100),
            Case::new((1980.0, -990.0), (40.0, 20.0), 99..100, 99..100),
            Case::new((1981.0, -991.0), (40.0, 20.0), 99..100, 99..100),
            Case::new((1999.0, -999.0), (40.0, 20.0), 99..100, 99..100),
            Case::new((2000.0, -1000.0), (40.0, 20.0), 100..100, 100..100),
            Case::new((2001.0, -1001.0), (40.0, 20.0), 100..100, 100..100),
        ] {
            case.run()
        }
    }

    #[test]
    fn visible_rows_and_columns_with_resizing_columns() {
        #[derive(Clone, Debug)]
        struct Case {
            viewport:      Viewport,
            expected_cols: Range<Col>,
            column_widths: ColumnWidths,
        }

        impl Case {
            fn new(
                left: f32,
                size_x: f32,
                expected_cols: Range<Col>,
                column_widths: &ColumnWidths,
            ) -> Self {
                let right = left + size_x;
                let viewport = Viewport { left, top: 0.0, right, bottom: -ENTRY_SIZE.y };
                let column_widths = column_widths.clone_ref();
                Self { viewport, expected_cols, column_widths }
            }

            fn run(self) {
                assert_eq!(
                    visible_columns(&self.viewport, ENTRY_SIZE, COL_COUNT, &self.column_widths),
                    self.expected_cols,
                    "Wrong visible cols in {self:?}"
                );
            }
        }

        let first_column_shrunk = adjusted_column_widths(0, -15.0);
        let second_column_shrunk = adjusted_column_widths(1, -10.0);
        let first_column_extended = adjusted_column_widths(0, 20.0);
        let second_column_extended = adjusted_column_widths(1, 20.0);
        let last_column_shrunk = adjusted_column_widths(COL_COUNT - 1, -10.0);
        let last_column_extended = adjusted_column_widths(COL_COUNT - 1, 20.0);

        for case in [
            Case::new(0.0, 40.0, 0..3, &first_column_shrunk),
            Case::new(4.9, 40.0, 0..3, &first_column_shrunk),
            Case::new(5.1, 40.0, 1..4, &first_column_shrunk),
            Case::new(0.0, 40.0, 0..3, &second_column_shrunk),
            Case::new(1.0, 40.0, 0..3, &second_column_shrunk),
            Case::new(19.9, 40.0, 0..4, &second_column_shrunk),
            Case::new(21.1, 40.0, 1..4, &second_column_shrunk),
            Case::new(0.0, 40.0, 0..1, &first_column_extended),
            Case::new(1.0, 40.0, 0..2, &first_column_extended),
            Case::new(39.9, 20.0, 0..2, &first_column_extended),
            Case::new(40.1, 20.0, 1..3, &first_column_extended),
            Case::new(0.0, 40.0, 0..2, &second_column_extended),
            Case::new(20.0, 40.0, 1..2, &second_column_extended),
            Case::new(59.9, 20.0, 1..3, &second_column_extended),
            Case::new(60.1, 20.0, 2..4, &second_column_extended),
            Case::new(2000.0, 20.0, 100..100, &last_column_shrunk),
            Case::new(1991.0, 20.0, 100..100, &last_column_shrunk),
            Case::new(1989.0, 20.0, 99..100, &last_column_shrunk),
            Case::new(1980.0, 40.0, 99..100, &last_column_extended),
            Case::new(1979.0, 40.0, 98..100, &last_column_extended),
        ] {
            case.run()
        }
    }

    fn adjusted_column_widths(adjusted_column: usize, adjusted_by: f32) -> ColumnWidths {
        let widths = ColumnWidths::new(COL_COUNT);
        widths.set_width_diff(adjusted_column, adjusted_by);
        widths
    }
}
