//! Module with content regarding location in text (e.g. location of cursors or changes etc.)
use crate::display::shape::text::text_field::content::Change;

use enso_data::text::TextLocation;
use std::ops::Range;



// ==========================
// === TextLocationChange ===
// ==========================

/// A locations change after some edit.
///
/// After on or many changes to text content, the following locations are altered (e.g. when you
/// remove one line, all the next location are shifted one line up). This structure keeps history
/// of sequence of text changes and can tell how the further TextLocations should change to mark
/// the same place in the text.
#[derive(Copy,Clone,Debug,Default)]
pub struct TextLocationChange {
    /// A current line offset for all next locations;
    pub line_offset : isize,
    /// Last line altered by considered text changes (in the current numeration).
    pub last_changed_line : usize,
    /// The column offset for all next locations in `last_changed_line`.
    pub column_offset : isize,
}

impl TextLocationChange {
    /// Return the new location after considering text changes so far.
    pub fn apply_to(&self, location:TextLocation) -> TextLocation {
        let line   = (location.line as isize + self.line_offset) as usize;
        let column = if line == self.last_changed_line {
            (location.column as isize + self.column_offset) as usize
        } else {
            location.column
        };
        TextLocation{line,column}
    }

    /// Return the new location range after considering text changes so far.
    pub fn apply_to_range(&self, range:Range<TextLocation>) -> Range<TextLocation> {
        self.apply_to(range.start)..self.apply_to(range.end)
    }

    /// Add a new text change into consideration.
    pub fn add_change(&mut self, change:&Change) {
        let removed             = &change.replaced;
        let inserted            = change.inserted_text_range();
        let lines_removed       = (removed.end.line - removed.start.line) as isize;
        let lines_added         = (inserted.end.line - inserted.start.line) as isize;
        let change_in_last_line = removed.start.line == self.last_changed_line;
        let drop_column_offset  = !change_in_last_line || lines_removed > 0;
        if drop_column_offset {
            self.column_offset = 0;
        }
        self.line_offset      += lines_added - lines_removed;
        self.column_offset    += inserted.end.column as isize - removed.end.column as isize;
        self.last_changed_line = inserted.end.line;
    }
}



#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn applying_change_to_location() {
        // positive offsets
        let change = TextLocationChange {
            line_offset: 1,
            last_changed_line: 5,
            column_offset: 3
        };
        let in_last_changed_line     = TextLocation{line:4, column:2};
        let not_in_last_changed_line = TextLocation{line:5, column:2};
        assert_eq!(TextLocation{line:5, column:5}, change.apply_to(in_last_changed_line));
        assert_eq!(TextLocation{line:6, column:2}, change.apply_to(not_in_last_changed_line));

        // negative offsets
        let change = TextLocationChange {
            line_offset: -1,
            last_changed_line: 5,
            column_offset: -2
        };
        let in_last_changed_line     = TextLocation{line:6, column:2};
        let not_in_last_changed_line = TextLocation{line:5, column:2};
        assert_eq!(TextLocation{line:5, column:0}, change.apply_to(in_last_changed_line));
        assert_eq!(TextLocation{line:4, column:2}, change.apply_to(not_in_last_changed_line));
    }

    #[test]
    fn adding_changes() {
        let one_line  = "One line";
        let two_lines = "Two\nLines";
        let mut location_change = TextLocationChange::default();

        // initial change
        let replaced = TextLocation{line:2, column:2}..TextLocation{line:2, column: 2};
        location_change.add_change(&Change::replace(replaced,one_line));
        assert_eq!(0, location_change.line_offset);
        assert_eq!(2, location_change.last_changed_line);
        assert_eq!(8, location_change.column_offset);

        // single line in the same line as previous
        let replaced = TextLocation{line:2, column:2}..TextLocation{line:2, column: 13};
        location_change.add_change(&Change::replace(replaced,one_line));
        assert_eq!(0, location_change.line_offset);
        assert_eq!(2, location_change.last_changed_line);
        assert_eq!(5, location_change.column_offset);

        // single -> multiple lines in the same line as previous
        let replaced = TextLocation{line:2, column:2}..TextLocation{line:2, column: 8};
        location_change.add_change(&Change::replace(replaced,two_lines));
        assert_eq!(1, location_change.line_offset);
        assert_eq!(3, location_change.last_changed_line);
        assert_eq!(2, location_change.column_offset);

        // multiple -> multiple lines
        let replaced = TextLocation{line:3, column:2}..TextLocation{line:7, column: 3};
        location_change.add_change(&Change::replace(replaced,two_lines));
        assert_eq!(-2, location_change.line_offset);
        assert_eq!(4 , location_change.last_changed_line);
        assert_eq!(2 , location_change.column_offset);

        // multiple -> single lines in the same line as previous
        let replaced = TextLocation{line:4, column:2}..TextLocation{line:5, column: 11};
        location_change.add_change(&Change::replace(replaced,one_line));
        assert_eq!(-3, location_change.line_offset);
        assert_eq!(4 , location_change.last_changed_line);
        assert_eq!(-1, location_change.column_offset);

        // single line in other line than previous
        let replaced = TextLocation{line:5, column:2}..TextLocation{line:5, column: 12};
        location_change.add_change(&Change::replace(replaced,one_line));
        assert_eq!(-3, location_change.line_offset);
        assert_eq!(5 , location_change.last_changed_line);
        assert_eq!(-2, location_change.column_offset);
    }
}
