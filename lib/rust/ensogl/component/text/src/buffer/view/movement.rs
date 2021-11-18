//! Text cursor transform implementation.

use super::*;
use crate::buffer::data::unit::*;
use crate::buffer::view::selection;
use crate::buffer::view::word::WordCursor;



// =================
// === Transform ===
// =================

/// Selection transformation patterns. Used for the needs of keyboard and mouse interaction.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Transform {
    /// Select all text.
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

impl ViewBuffer {
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
        line_delta: Line,
        modify: bool,
    ) -> selection::Shape {
        let move_up = line_delta < 0.line();
        let location = self.vertical_motion_selection_to_location(selection, move_up, modify);
        let min_line = 0.line();
        let max_line = self.last_line_index();
        let border_step = if move_up { (-1).line() } else { 1.line() };
        let snap_top = location.line < min_line;
        let snap_bottom = location.line > max_line;
        let next_line = max_line + border_step;
        let bottom = location.line + line_delta;
        let line = if snap_top {
            border_step
        } else if snap_bottom {
            next_line
        } else {
            bottom
        };
        let tgt_location = location.with_line(line);
        selection::Shape(selection.start, tgt_location)
    }

    /// Apply the movement to each region in the selection, and returns the union of the results.
    ///
    /// If `modify` is `true`, the selections are modified, otherwise the results of individual
    /// region movements become cursors. Modify is often mapped to the `shift` button in text
    /// editors.
    pub fn moved_selection(&self, transform: Transform, modify: bool) -> selection::Group {
        let mut result = selection::Group::new();
        for &selection in self.selection.borrow().iter() {
            let new_selection = self.moved_selection_region(transform, selection, modify);
            result.merge(new_selection);
        }
        result
    }

    /// Location of the previous grapheme cluster if any.
    pub fn prev_grapheme_location(&self, location: Location) -> Option<Location> {
        let offset = self.byte_offset_of_location_snapped(location);
        let prev_offset = self.prev_grapheme_offset(offset);
        prev_offset.map(|off| self.offset_to_location(off))
    }

    /// Location of the next grapheme cluster if any.
    pub fn next_grapheme_location(&self, location: Location) -> Option<Location> {
        let offset = self.byte_offset_of_location_snapped(location);
        let next_offset = self.next_grapheme_offset(offset);
        next_offset.map(|off| self.offset_to_location(off))
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
            Transform::All => shape(default(), self.offset_to_location(text.byte_size())),

            Transform::Up => self.vertical_motion(selection, (-1).line(), modify),

            Transform::Down => self.vertical_motion(selection, 1.line(), modify),

            Transform::StartOfDocument => shape(selection.start, default()),

            Transform::EndOfDocument =>
                shape(selection.start, self.offset_to_location(text.byte_size())),

            Transform::Left => {
                let def = shape(selection.start, default());
                let do_move = selection.is_cursor() || modify;
                if do_move {
                    self.prev_grapheme_location(selection.end)
                        .map(|t| shape(selection.start, t))
                        .unwrap_or(def)
                } else {
                    shape(selection.start, selection.min())
                }
            }

            Transform::Right => {
                let def = shape(selection.start, selection.end);
                let do_move = selection.is_cursor() || modify;
                if do_move {
                    self.next_grapheme_location(selection.end)
                        .map(|t| shape(selection.start, t))
                        .unwrap_or(def)
                } else {
                    shape(selection.start, selection.max())
                }
            }

            Transform::LeftSelectionBorder => shape(selection.start, selection.min()),

            Transform::RightSelectionBorder => shape(selection.start, selection.max()),

            Transform::LeftOfLine => {
                let end = Location(selection.end.line, 0.column());
                shape(selection.start, end)
            }

            Transform::RightOfLine => {
                let line = selection.end.line;
                let text_byte_size = text.byte_size();
                let is_last_line = line == self.last_line_index();
                let next_line_offset_opt = self.byte_offset_of_line_index(line + 1.line());
                let next_line_offset = next_line_offset_opt.unwrap_or_else(|_| text.byte_size());
                let offset = if is_last_line {
                    text_byte_size
                } else {
                    text.prev_grapheme_offset(next_line_offset).unwrap_or(text_byte_size)
                };
                let end = self.offset_to_location(offset);
                shape(selection.start, end)
            }

            Transform::LeftWord => {
                let end_offset = self.byte_offset_of_location_snapped(selection.end);
                let mut word_cursor = WordCursor::new(text, end_offset);
                let offset = word_cursor.prev_boundary().unwrap_or_else(|| 0.bytes());
                let end = self.offset_to_location(offset);
                shape(selection.start, end)
            }

            Transform::RightWord => {
                let end_offset = self.byte_offset_of_location_snapped(selection.end);
                let mut word_cursor = WordCursor::new(text, end_offset);
                let offset = word_cursor.next_boundary().unwrap_or_else(|| text.byte_size());
                let end = self.offset_to_location(offset);
                shape(selection.start, end)
            }

            Transform::Word => {
                let end_offset = self.byte_offset_of_location_snapped(selection.end);
                let mut word_cursor = WordCursor::new(text, end_offset);
                let offsets = word_cursor.select_word();
                let start = self.offset_to_location(offsets.0);
                let end = self.offset_to_location(offsets.1);
                shape(start, end)
            }

            Transform::Line => {
                let start_offset = self.byte_offset_of_line_index_snapped(selection.start.line);
                let end_offset = self.end_byte_offset_of_line_index_snapped(selection.end.line);
                let start = self.offset_to_location(start_offset);
                let end = self.offset_to_location(end_offset);
                shape(start, end)
            }
        };
        let start = if modify { shape.start } else { shape.end };
        let end = shape.end;
        Selection(start, end, selection.id)
    }
}
