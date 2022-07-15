//! Functions for evaluating which part of [`GridView`] is visible.

use crate::prelude::*;

use crate::Col;
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
pub fn visible_columns(v: &Viewport, entry_size: Vector2, col_count: usize) -> Range<Col> {
    let first_visible_unrestricted = (v.left / entry_size.x).floor() as isize;
    let first_visible = first_visible_unrestricted.clamp(0, col_count as isize) as Col;
    let first_not_visible = if has_size(v) {
        let first_not_visible_unrestricted = (v.right / entry_size.x).ceil() as isize;
        first_not_visible_unrestricted.clamp(0, col_count as isize) as Col
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
) -> impl Iterator<Item = (Row, Col)> {
    let visible_rows = visible_rows(v, entry_size, row_count);
    let visible_cols = visible_columns(v, entry_size, col_count);
    itertools::iproduct!(visible_rows, visible_cols)
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn visible_rows_and_columns() {
        const ENTRY_SIZE: Vector2 = Vector2(20.0, 10.0);
        const ROW_COUNT: usize = 100;
        const COL_COUNT: usize = 100;

        #[derive(Clone, Debug)]
        struct Case {
            viewport:      Viewport,
            expected_rows: Range<Row>,
            expected_cols: Range<Col>,
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
                Self { viewport, expected_rows, expected_cols }
            }

            fn run(&self) {
                assert_eq!(
                    visible_rows(&self.viewport, ENTRY_SIZE, ROW_COUNT),
                    self.expected_rows,
                    "Wrong visible rows in {self:?}"
                );
                assert_eq!(
                    visible_columns(&self.viewport, ENTRY_SIZE, COL_COUNT),
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
}
