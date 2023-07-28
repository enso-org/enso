//! Text cursor transform implementation.

use crate::buffer::*;

use crate::buffer::rope::word::WordCursor;
use crate::buffer::selection;
use crate::buffer::selection::Selection;



// =================
// === Transform ===
// =================

/// Selection transformation patterns. Used for the needs of keyboard and mouse interaction.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Transform {
    /// Select all text.
    #[default]
    All,
    /// Move to the left by one grapheme cluster.
    Left,
    /// Move to the right by one grapheme cluster.
    Right,
    /// Move to the left selection border. Cursors will not be modified.
    LeftSelectionBorder,
    /// Move to the right selection border. Cursors will not be modified.
    RightSelectionBorder,
    /// Move to the left by one word.
    LeftWord,
    /// Move to the right by one word.
    RightWord,
    /// Select the word at every cursor.
    Word,
    /// Select the line at every cursor.
    Line,
    /// Move to left end of visible line.
    LeftOfLine,
    /// Move to right end of visible line.
    RightOfLine,
    /// Move up one visible line.
    Up,
    /// Move down one visible line.
    Down,
    /// Move to the start of the document.
    StartOfDocument,
    /// Move to the end of the document
    EndOfDocument,
}



// ==========================
// === Transform Handling ===
// ==========================

impl BufferModel {
    /// Convert selection to cursor location after a vertical movement.
    fn vertical_motion_selection_to_location(
        &self,
        selection: Selection,
        move_up: bool,
        modify: bool,
    ) -> Location {
        let end = selection.end;
        if modify {
            end
        } else if move_up {
            selection.min()
        } else {
            selection.max()
        }
    }

    /// Compute movement based on vertical motion by the given number of lines.
    fn vertical_motion(
        &self,
        selection: Selection,
        line_diff: LineDiff,
        modify: bool,
    ) -> selection::Shape {
        let move_up = line_diff < LineDiff(0);
        let location = self.vertical_motion_selection_to_location(selection, move_up, modify);
        let first_line = Line(0);
        let last_line = self.last_line_index();
        let desired_line = location.line.to_diff() + line_diff;
        let tgt_location = if desired_line < first_line.to_diff() {
            Location { line: first_line, offset: Column(0) }
        } else if desired_line > last_line.to_diff() {
            Location { line: last_line, offset: self.last_line_last_column() }
        } else {
            location.with_line(desired_line.to_line())
        };
        selection::Shape(selection.start, tgt_location)
    }

    /// Apply the movement to each region in the selection, and returns the union of the results.
    ///
    /// If `modify` is `true`, the selections are modified, otherwise the results of individual
    /// region movements become cursors. Modify is often mapped to the `shift` button in text
    /// editors.
    pub fn moved_selection(&self, transform: Transform, modify: bool) -> selection::Group {
        let mut result = selection::Group::new();
        let selections = self.selection.borrow().clone();
        for &selection in selections.iter() {
            let new_selection = self.moved_selection_region(transform, selection, modify);
            result.merge(new_selection);
        }
        result
    }

    /// Compute the result of movement on one selection region.
    ///
    /// If `modify` is `true`, the selections are modified, otherwise the results of individual
    /// region movements become cursors. Modify is often mapped to the `shift` button in text
    /// editors.
    pub fn moved_selection_region(
        &self,
        transform: Transform,
        selection: Selection,
        modify: bool,
    ) -> Selection {
        let text = &self.text();
        let shape = selection::Shape;
        let shape: selection::Shape = match transform {
            Transform::All => shape(default(), self.last_line_last_location()),
            Transform::Up => self.vertical_motion(selection, LineDiff(-1), modify),
            Transform::Down => self.vertical_motion(selection, LineDiff(1), modify),
            Transform::StartOfDocument => shape(selection.start, default()),
            Transform::EndOfDocument => {
                let end = Location::from_in_context_snapped(self, text.last_byte_index());
                shape(selection.start, end)
            }
            Transform::Left => {
                let do_move = selection.is_cursor() || modify;
                if do_move {
                    shape(selection.start, self.prev_column(selection.end))
                } else {
                    shape(selection.start, selection.min())
                }
            }
            Transform::Right => {
                let do_move = selection.is_cursor() || modify;
                if do_move {
                    shape(selection.start, self.next_column(selection.end))
                } else {
                    shape(selection.start, selection.max())
                }
            }

            Transform::LeftSelectionBorder => shape(selection.start, selection.min()),
            Transform::RightSelectionBorder => shape(selection.start, selection.max()),

            Transform::LeftOfLine => {
                let end = Location(selection.end.line, Column(0));
                shape(selection.start, end)
            }

            Transform::RightOfLine => {
                let line = selection.end.line;
                let text_byte_size = text.last_byte_index();
                let is_last_line = line == self.last_line_index();
                let next_line_offset_opt = self.line_offset(line + Line(1));
                let next_line_offset =
                    next_line_offset_opt.unwrap_or_else(|_| text.last_byte_index());
                let offset = if is_last_line {
                    text_byte_size
                } else {
                    text.prev_grapheme_offset(next_line_offset).unwrap_or(text_byte_size)
                };
                let end = Location::from_in_context_snapped(self, offset);
                shape(selection.start, end)
            }

            Transform::LeftWord => {
                let end_offset = Byte::from_in_context_snapped(self, selection.end);
                let mut word_cursor = WordCursor::new(text, end_offset);
                let offset = word_cursor.prev_boundary().unwrap_or_else(|| 0.byte());
                let end = Location::from_in_context_snapped(self, offset);
                shape(selection.start, end)
            }

            Transform::RightWord => {
                let end_offset = Byte::from_in_context_snapped(self, selection.end);
                let mut word_cursor = WordCursor::new(text, end_offset);
                let offset = word_cursor.next_boundary().unwrap_or_else(|| text.last_byte_index());
                let end = Location::from_in_context_snapped(self, offset);
                shape(selection.start, end)
            }

            Transform::Word => {
                let end_offset = Byte::from_in_context_snapped(self, selection.end);
                let mut word_cursor = WordCursor::new(text, end_offset);
                let offsets = word_cursor.select_word();
                let start = Location::from_in_context_snapped(self, offsets.0);
                let end = Location::from_in_context_snapped(self, offsets.1);
                shape(start, end)
            }

            Transform::Line => {
                let start_offset = self.line_offset_snapped(selection.start.line);
                let end_offset = self.line_end_offset_snapped(selection.end.line);
                let start = Location::from_in_context_snapped(self, start_offset);
                let end = Location::from_in_context_snapped(self, end_offset);
                shape(start, end)
            }
        };
        let start = if modify { shape.start } else { shape.end };
        let end = shape.end;
        Selection(start, end, selection.id)
    }
}
