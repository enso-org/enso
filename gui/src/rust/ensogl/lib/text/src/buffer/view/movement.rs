//! Text cursor movement implementation.

use super::*;
use crate::buffer::data;
use crate::buffer::data::unit::*;



// ================
// === Movement ===
// ================

/// Keyboard cursor navigation patterns.
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum Movement {
    /// Move to the left by one grapheme cluster.
    Left,
    /// Move to the right by one grapheme cluster.
    Right,
    /// Move to the left by one word.
    LeftWord,
    /// Move to the right by one word.
    RightWord,
    /// Move to left end of visible line.
    LeftOfLine,
    /// Move to right end of visible line.
    RightOfLine,
    /// Move up one visible line.
    Up,
    /// Move down one visible line.
    Down,
    /// Move up one viewport height.
    UpPage,
    /// Move down one viewport height.
    DownPage,
    /// Move up to the next line that can preserve the cursor position.
    UpExactPosition,
    /// Move down to the next line that can preserve the cursor position.
    DownExactPosition,
    /// Move to the start of the text line.
    StartOfParagraph,
    /// Move to the end of the text line.
    EndOfParagraph,
    /// Move to the end of the text line, or next line if already at end.
    EndOfParagraphKill,
    /// Move to the start of the document.
    StartOfDocument,
    /// Move to the end of the document
    EndOfDocument,
}



// =========================
// === Movement Handling ===
// =========================

impl ViewModel {
    /// Convert selection to caret location after a vertical movement.
    fn vertical_motion_selection_to_caret
    (&self, selection:Selection, move_up:bool, modify:bool) -> Location {
        let offset =
            if      modify  { selection.end }
            else if move_up { selection.min() }
            else            { selection.max() };
        let location = self.offset_to_line_col(offset);
        let column   = selection.column.unwrap_or(location.column);
        Location(location.line,column)
    }


    /// Compute movement based on vertical motion by the given number of lines.
    ///
    /// Note: in non-exceptional cases, this function preserves the `column` field of the selection
    /// region.
    ///
    /// Note: This code is quite careful to avoid integer overflow.
    // TODO: Write tests to verify that it's safe regarding integer ovewrflow.
    fn vertical_motion
    (&self, region:Selection, line_delta:isize, modify:bool) -> (Bytes,Option<Column>) {
        let move_up    = line_delta < 0;
        let line_delta = line_delta.saturating_abs() as usize;
        let location   = self.vertical_motion_selection_to_caret(region,move_up,modify);
        let n_lines    = self.line_of_offset(self.data().len());

        if move_up && line_delta > location.line.value {
            return (Bytes(0), Some(location.column));
        }

        let line = if move_up { location.line.value - line_delta }
                   else       { location.line.value.saturating_add(line_delta) };

        if line > n_lines.value {
            return (self.data().len(),Some(location.column));
        }

        let line       = Line(line);
        let new_offset = self.line_col_to_offset(line,location.column);
        (new_offset,Some(location.column))
    }

    /// Compute movement based on vertical motion by the given number of lines skipping
    /// any line that is shorter than the current cursor position.
    fn vertical_motion_exact_pos
    (&self, region:Selection, move_up:bool, modify:bool) -> (Bytes,Option<Column>) {
        let location         = self.vertical_motion_selection_to_caret(region, move_up, modify);
        let lines_count      = self.line_of_offset(self.data().len());
        let line_offset      = self.offset_of_line(location.line);
        let next_line_offset = self.offset_of_line(location.line.saturating_add(1.line()));
        let line_len         = next_line_offset - line_offset;
        if move_up && location.line == Line(0) {
            return (self.line_col_to_offset(location.line,location.column), Some(location.column));
        }
        let mut line = if move_up { location.line - 1.line() }
                       else       { location.line.saturating_add(1.line()) };

        // If the active columns is longer than the current line, use the current line length.
        let line_last_column = line_len.column();
        let col = if line_last_column < location.column { line_last_column - 1.bytes().column() } else { location.column };

        loop {
            let line_len = self.offset_of_line(line + 1.line()) - self.offset_of_line(line);

            // If the line is longer than the current cursor position, break.
            // We use > instead of >= because line_len includes newline.
            if line_len > col.value {
                break;
            }

            // If you are trying to add a selection past the end of the file or before the first line, return original selection
            if line >= lines_count || (line == Line(0) && move_up) {
                line = location.line;
                break;
            }

            line = if move_up { line - 1.line() } else { line.saturating_add(1.line()) };
        }

        (self.line_col_to_offset(line, col), Some(col))
    }

    /// Apply the movement to each region in the selection, and returns the union of the results.
    ///
    /// If `modify` is `true`, the selections are modified, otherwise the results of individual region
    /// movements become carets. Modify is often mapped to the `shift` button in text editors.
    pub fn moved_selection(&self, movement:Movement, modify:bool) -> selection::Group {
        let mut result = selection::Group::new();
        for &selection in self.selection.borrow().iter() {
            let new_selection = self.moved_selection_region(movement,selection,modify);
            result.add(new_selection);
        }
        result
    }

    /// Compute the result of movement on one selection region.
    pub fn moved_selection_region
    (&self, movement:Movement, region:Selection, modify:bool) -> Selection {
        let text        = self.data();
        let no_horiz    = |t|(t,None);
        let (end,horiz) : (Bytes,Option<Column>) = match movement {
            Movement::Up                => self.vertical_motion(region, -1, modify),
            Movement::Down              => self.vertical_motion(region,  1, modify),
            Movement::UpExactPosition   => self.vertical_motion_exact_pos(region, true, modify),
            Movement::DownExactPosition => self.vertical_motion_exact_pos(region, false, modify),
            Movement::UpPage            => self.vertical_motion(region, -self.page_scroll_height(), modify),
            Movement::DownPage          => self.vertical_motion(region,  self.page_scroll_height(), modify),
            Movement::StartOfDocument   => no_horiz(Bytes(0)),
            Movement::EndOfDocument     => no_horiz(text.len()),

            Movement::Left => {
                let def     = (Bytes(0),region.column);
                let do_move = region.is_caret() || modify;
                if  do_move { text.prev_grapheme_offset(region.end).map(no_horiz).unwrap_or(def) }
                else        { no_horiz(region.min()) }
            }

            Movement::Right => {
                let def     = (region.end,region.column);
                let do_move = region.is_caret() || modify;
                if  do_move { text.next_grapheme_offset(region.end).map(no_horiz).unwrap_or(def) }
                else        { no_horiz(region.max()) }
            }

            Movement::LeftOfLine => {
                let line   = self.line_of_offset(region.end);
                let offset = self.offset_of_line(line);
                no_horiz(offset)
            }

            Movement::RightOfLine => {
                let line             = self.line_of_offset(region.end);
                let text_len         = text.len();
                let last_line        = line == self.line_of_offset(text_len);
                let next_line_offset = self.offset_of_line(line+1.line());
                let offset           = if last_line { text_len } else {
                    text.prev_grapheme_offset(next_line_offset).unwrap_or(text_len)
                };
                no_horiz(offset)
            }

            Movement::StartOfParagraph => {
                // Note: TextEdit would start at modify ? region.end : region.min()
                let mut cursor = data::Cursor::new(&text, region.end.value);
                let offset     = Bytes(cursor.prev::<data::metric::Lines>().unwrap_or(0));
                no_horiz(offset)
            }

            Movement::EndOfParagraph => {
                // Note: TextEdit would start at modify ? region.end : region.max()
                let mut cursor = data::Cursor::new(&text, region.end.value);
                let     offset = match cursor.next::<data::metric::Lines>() {
                    None            => text.len(),
                    Some(next_line_offset) => {
                        let next_line_offset = Bytes(next_line_offset);
                        if cursor.is_boundary::<data::metric::Lines>() {
                            text.prev_grapheme_offset(next_line_offset).unwrap_or(region.end)
                        } else if Bytes(cursor.pos()) == text.len() {
                            text.len()
                        } else {
                            region.end
                        }
                    }
                };
                no_horiz(offset)
            }

            Movement::EndOfParagraphKill => {
                // Note: TextEdit would start at modify ? region.end : region.max()
                let mut cursor = data::Cursor::new(&text, region.end.value);
                let     offset = match cursor.next::<data::metric::Lines>() {
                    None            => region.end,
                    Some(next_line_offset) => {
                        let next_line_offset = Bytes(next_line_offset);
                        if cursor.is_boundary::<data::metric::Lines>() {
                            let eol = text.prev_grapheme_offset(next_line_offset);
                            let opt = eol.and_then(|t|(t!=region.end).as_some(t));
                            opt.unwrap_or(next_line_offset)
                        } else { next_line_offset }
                    }
                };
                no_horiz(offset)
            }

            Movement::LeftWord => todo!(),
            Movement::RightWord => todo!(),
        };
        let start = if modify { region.start } else { end };
        Selection::new(start,end).with_column(horiz)
    }
}
