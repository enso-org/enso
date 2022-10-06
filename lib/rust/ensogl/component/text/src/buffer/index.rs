//! Indexing units definitions.

use crate::prelude::*;
use enso_text::unit::*;



enso_text::define_line_unit!(ViewLine);

impl Add<LineDiff> for ViewLine {
    type Output = ViewLine;
    fn add(self, line_diff: LineDiff) -> Self::Output {
        if -line_diff.value > self.value as i32 {
            error!("Adding of LineDiff to ViewLine resulted in negative value.");
            ViewLine(0)
        } else {
            ViewLine((self.value as i32 + line_diff.value) as usize)
        }
    }
}

impl Add<ViewLine> for LineDiff {
    type Output = ViewLine;
    fn add(self, line: ViewLine) -> Self::Output {
        line + self
    }
}

impl Add<&LineDiff> for ViewLine {
    type Output = ViewLine;
    fn add(self, line_diff: &LineDiff) -> Self::Output {
        self + *line_diff
    }
}

impl Add<LineDiff> for &ViewLine {
    type Output = ViewLine;
    fn add(self, line_diff: LineDiff) -> Self::Output {
        *self + line_diff
    }
}

impl Add<&LineDiff> for &ViewLine {
    type Output = ViewLine;
    fn add(self, line_diff: &LineDiff) -> Self::Output {
        *self + *line_diff
    }
}


// ====================
// === ViewLocation ===
// ====================

/// Alias for [`Location`] with [`ViewLine`] as line index.
pub type ViewLocation<Offset = Column> = Location<Offset, ViewLine>;
