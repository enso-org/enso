//! Module with structured describing cursors and their selectino in a TextField.

use crate::prelude::*;

use crate::display::shape::text::text_field::content::line::LineFullInfo;
use crate::display::shape::text::text_field::content::TextFieldContent;
use crate::display::shape::text::text_field::word_occurrence::IndexedWords;

use data::text::TextLocation;
use nalgebra::Vector2;
use nalgebra::min;
use nalgebra::max;
use std::cmp::Ordering;
use std::ops::Range;


// ==============
// === Cursor ===
// ==============

/// Cursor in TextComponent with its selection.
#[derive(Clone,Copy,Debug,Eq,PartialEq)]
pub struct Cursor {
    /// Cursor's position in text.
    pub position: TextLocation,
    /// A position when the selection of cursor ends. It may be before or after the cursor position.
    pub selected_to: TextLocation,
}

impl Cursor {
    /// Create a new cursor at given position and without any selection.
    pub fn new(position:TextLocation) -> Self {
        let selected_to = position;
        Cursor {position,selected_to}
    }

    /// Recalculate cursor position adjusting itself to new content.
    pub fn recalculate_position(&mut self, content:&TextFieldContent) {
        let lines               = content.lines();
        let max_line_index      = lines.len() - 1;
        self.position.line      = min(self.position.line,max_line_index);
        self.selected_to.line   = min(self.selected_to.line,max_line_index);
        let max_column_index    = lines[self.position.line].len();
        self.position.column    = min(self.position.column,max_column_index);
        let max_column_index    = lines[self.selected_to.line].len();
        self.selected_to.column = min(self.selected_to.column,max_column_index);
    }

    /// Returns true if some selection is bound to this cursor.
    pub fn has_selection(&self) -> bool {
        self.position != self.selected_to
    }

    /// Select line range.
    pub fn select_line_range(&mut self, line:&LineFullInfo, range:Range<usize>) {
        let start = TextLocation{line:line.line_id,column:min(range.start,line.len())};
        let end   = TextLocation{line:line.line_id,column:min(range.end,line.len())};
        let range = start..end;
        self.select_range(&range);
    }

    /// Select text range.
    pub fn select_range(&mut self, range:&Range<TextLocation>) {
        self.position    = range.end;
        self.selected_to = range.start;
    }

    /// Get range of selected text by this cursor.
    pub fn selection_range(&self) -> Range<TextLocation> {
        match self.position.cmp(&self.selected_to) {
            Ordering::Equal   => self.position..self.position,
            Ordering::Greater => self.selected_to..self.position,
            Ordering::Less    => self.position..self.selected_to
        }
    }

    /// Extend the selection to cover the given range. Cursor itself may be moved, and will be
    /// on the same side of selection as before.
    pub fn extend_selection(&mut self, range:&Range<TextLocation>) {
        let new_start = range.start.min(self.position).min(self.selected_to);
        let new_end   = range.end.max(self.position).max(self.selected_to);
        *self = match self.position.cmp(&self.selected_to) {
            Ordering::Less => Cursor{position:new_start, selected_to:new_end  },
            _              => Cursor{position:new_end  , selected_to:new_start},
        }
    }

    /// Check if char at given position is selected.
    pub fn is_char_selected(&self, position:TextLocation) -> bool {
        self.selection_range().contains(&position)
    }

    /// Get `LineFullInfo` object of this cursor's line.
    pub fn current_line<'a>(&self, content:&'a mut TextFieldContent)
    -> LineFullInfo<'a> {
        content.line(self.position.line)
    }

    /// Get the position where the cursor should be rendered. The returned point is on the
    /// middle of line's height, on the right side of character from the left side of the cursor
    /// (where usually the cursor is displayed by text editors).
    ///
    /// _Baseline_ is a font specific term, for details see [freetype documentation]
    ///  (https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1).
    pub fn render_position(position:&TextLocation, content:&mut TextFieldContent) -> Vector2<f32> {
        let line_height = content.line_height;
        let mut line    = content.line(position.line);
        // TODO[ao] this value should be read from font information, but msdf_sys library does
        // not provide it yet.
        let descender = line.baseline_start().y - 0.15 * line_height;
        let x         = Self::x_position_of_cursor_at(position.column,&mut line);
        let y         = descender + line_height / 2.0;
        Vector2::new(x,y)
    }

    fn x_position_of_cursor_at(column:usize, line:&mut LineFullInfo) -> f32 {
        if column > 0 {
            let char_index = column - 1;
            line.get_char_x_range(char_index).end
        } else {
            line.baseline_start().x
        }
    }
}



// ==================
// === Navigation ===
// ==================

/// An enum representing cursor moving step. The steps are based of possible keystrokes (arrows,
/// Home, End, Ctrl+Home, etc.)
#[derive(Copy,Clone,Debug,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub enum Step
{Left,LeftWord,Right,RightWord,PageUp,Up,PageDown,Down,LineBegin,LineEnd,DocBegin,DocEnd}

/// A struct for cursor navigation process.
#[derive(Debug)]
pub struct CursorNavigation<'a> {
    /// A snapshot of TextField's size.
    pub text_field_size: Vector2<f32>,
    /// A reference to text content. This is required to obtain the x positions of chars for proper
    /// moving cursors up and down.
    pub content: &'a mut TextFieldContent,
    /// Selecting navigation selects/unselects all text between current and new cursor position.
    pub selecting: bool,
}

impl<'a> CursorNavigation<'a> {
    /// Jump cursor directly to given position.
    pub fn move_cursor_to_position(&self, cursor:&mut Cursor, to:TextLocation) {
        cursor.position = to;
        if !self.selecting {
            cursor.selected_to = to;
        }
    }

    /// Get the nearest text location at the point on the screen.
    pub fn text_location_at_point(&mut self, point:Vector2<f32>) -> TextLocation {
        self.content.location_at_point(point)
    }

    /// Jump cursor to the nearest position from given point on the screen.
    pub fn move_cursor_to_point(&mut self, cursor:&mut Cursor, to:Vector2<f32>) {
        let position = self.content.location_at_point(to);
        self.move_cursor_to_position(cursor,position);
    }

    /// Move cursor by given step.
    pub fn move_cursor(&mut self, cursor:&mut Cursor, step:Step) {
        let new_position = self.new_position(cursor.position,step);
        self.move_cursor_to_position(cursor,new_position);
    }

    /// Get cursor position at end of given line.
    pub fn line_end_position(&self, line_index:usize) -> TextLocation {
        TextLocation {
            line   : line_index,
            column : self.content.lines()[line_index].len(),
        }
    }

    /// Get cursor position at end of whole content
    pub fn content_end_position(&self) -> TextLocation {
        TextLocation {
            column : self.content.lines().last().unwrap().len(),
            line   : self.content.lines().len() - 1,
        }
    }

    /// Get cursor position for the next char from given position. Returns none if at end of
    /// whole document.
    pub fn next_char_position(&self, position:&TextLocation) -> Option<TextLocation> {
        let current_line = &self.content.lines()[position.line];
        let next_column  = Some(position.column + 1).filter(|c| *c <= current_line.len());
        let next_line    = Some(position.line + 1)  .filter(|l| *l < self.content.lines().len());
        match (next_column,next_line) {
            (None         , None      ) => None,
            (None         , Some(line)) => Some(TextLocation::at_line_begin(line)),
            (Some(column) , _         ) => Some(TextLocation {column, ..*position})
        }
    }

    /// Get cursor position for the previous char from given position. Returns none if at begin of
    /// whole document.
    pub fn prev_char_position(&self, position:&TextLocation) -> Option<TextLocation> {
        let prev_column = position.column.checked_sub(1);
        let prev_line   = position.line.checked_sub(1);
        match (prev_column,prev_line) {
            (None         , None      ) => None,
            (None         , Some(line)) => Some(self.line_end_position(line)),
            (Some(column) , _         ) => Some(TextLocation {column, ..*position})
        }
    }

    /// Get cursor position one line above the given position, such the new x coordinate of
    /// displayed cursor on the screen will be nearest the current value.
    pub fn line_up_position(&mut self, position:&TextLocation, lines:usize) -> TextLocation {
        let prev_line = position.line.checked_sub(lines);
        let prev_line = prev_line.map(|line| self.near_same_x_in_another_line(position,line));
        prev_line.unwrap_or_else(TextLocation::at_document_begin)
    }

    /// Get cursor position one line behind the given position, such the new x coordinate of
    /// displayed cursor on the screen will be nearest the current value.
    pub fn line_down_position(&mut self, position:&TextLocation, lines:usize) -> TextLocation {
        let next_line = Some(position.line + lines).filter(|l| *l < self.content.lines().len());
        let next_line = next_line.map(|line| self.near_same_x_in_another_line(position,line));
        next_line.unwrap_or_else(|| self.content_end_position())
    }

    /// Returns the next column if it exists. If it doesn't exist it attempts to return the
    /// next line if it exists.
    fn next_valid_text_location<F:FnOnce(usize) -> usize>
    ( current_line : usize
    , next_line    : Option<usize>
    , next_column  : Option<usize>
    , f:F) -> Option<TextLocation> {
        match (next_column, next_line) {
            (Some(column), _)  => Some(TextLocation{line:current_line,column}),
            (None, Some(line)) => Some(TextLocation{line,column:f(line)}),
            (None, None)       => None
        }
    }

    /// If there is a word after `position`, returns its end index.
    fn next_word_position(&mut self, position:&TextLocation) -> Option<TextLocation> {
        let line        = position.line;
        let words       = IndexedWords::new(self.content.lines()[line].chars());
        let next_column = words.iter().find(|word| {
            let word_start = word.index;
            let word_end   = word_start + word.len();
            position.column < word_end
        }).map(|word| {
            let word_start = word.index;
            word_start + word.len()
        });
        let next_line  = Some(line + 1).filter(|l| *l < self.content.lines().len());
        let line_begin = |_| 0;
        Self::next_valid_text_location(line, next_line, next_column, line_begin)
    }

    /// If there is a word before `position`, returns its start index.
    fn prev_word_position(&mut self, position:&TextLocation) -> Option<TextLocation> {
        let line            = position.line;
        let words           = IndexedWords::new(self.content.lines()[line].chars());
        let previous_column = words.iter().rev().find(|word| {
            position.column > word.index
        }).map(|word| {
            word.index
        });
        let previous_line = Some(line).filter(|l| *l > 0).map(|l| l - 1);
        let line_end      = |line:usize| self.content.lines()[line].len();
        Self::next_valid_text_location(line, previous_line, previous_column, line_end)
    }

    fn get_lines_from_height(&self) -> usize {
        (self.text_field_size.y / self.content.line_height) as usize
    }

    /// New position of cursor at `position` after applying `step`.
    fn new_position(&mut self, position: TextLocation, step:Step) -> TextLocation {
        match step {
            Step::LeftWord  => self.prev_word_position(&position).unwrap_or(position),
            Step::RightWord => self.next_word_position(&position).unwrap_or(position),
            Step::Left      => self.prev_char_position(&position).unwrap_or(position),
            Step::Right     => self.next_char_position(&position).unwrap_or(position),
            Step::PageUp    => self.line_up_position(&position,self.get_lines_from_height()),
            Step::PageDown  => self.line_down_position(&position,self.get_lines_from_height()),
            Step::Up        => self.line_up_position(&position,1),
            Step::Down      => self.line_down_position(&position,1),
            Step::LineBegin => TextLocation::at_line_begin(position.line),
            Step::LineEnd   => self.line_end_position(position.line),
            Step::DocBegin  => TextLocation::at_document_begin(),
            Step::DocEnd    => self.content_end_position(),
        }
    }

    /// Get the cursor position on another line, such that the new x coordinate of
    /// displayed cursor on the screen will be nearest the current value.
    fn near_same_x_in_another_line(&mut self, position:&TextLocation, line_index:usize)
    -> TextLocation {
        let mut line   = self.content.line(position.line);
        let x_position = Cursor::x_position_of_cursor_at(position.column,&mut line);
        let column     = self.column_near_x(line_index,x_position);
        TextLocation {line:line_index, column}
    }

    /// Get the column number in given line, so the cursor will be as near as possible the
    /// `x_position` in _text space_. See `display::shape::text::content::line::Line`
    /// documentation for details about _text space_.
    fn column_near_x(&mut self, line_index:usize, x_position:f32) -> usize {
        let mut line                = self.content.line(line_index);
        let x                       = x_position;
        let char_at_x               = line.find_char_at_x_position(x);
        let nearer_to_end           = |range:Range<f32>| range.end - x < x - range.start;
        let mut nearer_to_chars_end = |index| nearer_to_end(line.get_char_x_range(index));
        match char_at_x {
            Some(index) if nearer_to_chars_end(index) => index + 1,
            Some(index)                               => index,
            None                                      => line.len()
        }
    }
}



// ===============
// === Cursors ===
// ===============



/// A newtype for cursor id.
#[derive(Clone,Copy,Debug,Default,PartialEq,Eq,PartialOrd,Ord)]
pub struct CursorId(pub usize);

/// Structure handling many cursors.
///
/// Usually there is only one cursor, but we have possibility of having many cursors in one text
/// component enabling editing in multiple lines/places at once.
#[derive(Debug)]
pub struct Cursors {
    /// All cursors' positions.
    pub cursors : Vec<Cursor>,
}

impl Default for Cursors {
    fn default() -> Self {
        Cursors {
            cursors : vec![Cursor::new(TextLocation::at_document_begin())],
        }
    }
}

impl Cursors {
    /// Removes all current cursors and replace them with single cursor without any selection.
    pub fn set_cursor(&mut self, position: TextLocation) {
        self.cursors = vec![Cursor::new(position)];
    }

    /// Get the selected text.
    pub fn get_selected_text(&self, content:&TextFieldContent) -> String {
        let cursor_select = |c:&Cursor| content.copy_fragment(c.selection_range());
        let mut cursors   = self.cursors.clone();
        cursors.sort_by(|a,b| a.position.line.cmp(&b.position.line));
        let mut selections = cursors.iter().map(cursor_select);
        selections.join("\n")
    }

    /// Recalculate cursors positions adjusting to new content.
    pub fn recalculate_positions(&mut self, content:&TextFieldContent) {
        for cursor in &mut self.cursors {
            cursor.recalculate_position(content);
        }
        self.merge_overlapping_cursors();
    }

    /// Finish multicursor mode, removing any additional cursors.
    pub fn finish_multicursor_mode(&mut self) {
        self.cursors.drain(0..self.cursors.len()-1);
    }

    /// Return the first cursor as mutable reference. Even on multiline edit some
    /// operations are applied to one, active cursor only (e.g. extending selection by mouse).
    pub fn first_cursor_mut(&mut self) -> &mut Cursor {
        self.cursors.first_mut().unwrap()
    }

    /// Return the last cursor. Even on multiline edit some operations are applied
    /// to one, active cursor only (e.g. extending selection by mouse).
    pub fn first_cursor(&self) -> &Cursor {
        self.cursors.first().unwrap()
    }

    /// Return the last cursor as mutable reference. Even on multiline edit some
    /// operations are applied to one, active cursor only (e.g. extending selection by mouse).
    pub fn last_cursor_mut(&mut self) -> &mut Cursor {
        self.cursors.last_mut().unwrap()
    }

    /// Return the last cursor. Even on multiline edit some operations are applied
    /// to one, active cursor only (e.g. extending selection by mouse).
    pub fn last_cursor(&self) -> &Cursor {
        self.cursors.last().unwrap()
    }

    fn block_selection_location
    ( &mut self
    , content       : &mut TextFieldContent
    , from_location : TextLocation
    , to_location   : TextLocation) {
        self.finish_multicursor_mode();
        let cursor = self.last_cursor_mut();

        let from_column = from_location.column;
        let to_column   = to_location.column;
        let from_line   = from_location.line;
        cursor.select_line_range(&content.line(from_line),from_column..to_column);

        let range = if from_location.line > to_location.line {
            let from_line = from_location.line - 1;
            to_location.line..=from_line
        } else {
            from_location.line+1..=to_location.line
        };
        for line in range {
            let line  = content.line(line);
            let range = from_location.column..to_location.column;
            let start = TextLocation{line:line.line_id,column:min(range.start,line.len())};
            let end   = TextLocation{line:line.line_id,column:min(range.end,line.len())};
            let range = start..end;

            self.add_cursor(range.end);
            let cursor = self.last_cursor_mut();
            cursor.selected_to = range.start
        }
    }

    /// Creates a multiline block selection from the first cursor position to the nearest
    /// location from the given point of the screen.
    pub fn block_selection(&mut self, content:&mut TextFieldContent, point:Vector2<f32>) {
        let from_location = self.first_cursor().selected_to;
        let from_line     = from_location.line;
        let to_line       = content.line_location_at_point(point);
        let start         = min(from_line,to_line);
        let end           = max(from_line,to_line);
        let range         = start..=end;
        let locations     = range.map(|line| {
            content.column_location_at_point(line,point)
        });
        let error_message = "Couldn't get maximum column of text lines.";
        let to_column     = locations.max().expect(error_message);
        let to_location   = TextLocation{line:to_line,column:to_column};
        self.block_selection_location(content, from_location, to_location);
    }

    /// Add new cursor without selection.
    pub fn add_cursor(&mut self, position: TextLocation) {
        self.cursors.push(Cursor::new(position));
        self.merge_overlapping_cursors();
    }

    /// Do the navigation step of all cursors.
    ///
    /// If after this operation some of the cursors occupies the same position, or their selected
    /// area overlap, they are irreversibly merged.
    pub fn navigate_all_cursors(&mut self, navigation:&mut CursorNavigation, step:Step) {
        self.navigate_cursors(navigation,step,|_| true)
    }

    /// Do the navigation step of all cursors satisfying given predicate.
    ///
    /// If after this operation some of the cursors occupies the same position, or their selected
    /// area overlap, they are irreversibly merged.
    pub fn navigate_cursors<Predicate>
    (&mut self, navigation:&mut CursorNavigation, step:Step, mut predicate:Predicate)
    where Predicate : FnMut(&Cursor) -> bool {
        let filtered = self.cursors.iter_mut().filter(|c| predicate(c));
        filtered.for_each(|cursor| navigation.move_cursor(cursor, step));
        self.merge_overlapping_cursors();
    }

    /// Jump the last cursor to the nearest location from given point of the screen.
    ///
    /// If after this operation some of the cursors occupies the same position, or their selected
    /// area overlap, they are irreversibly merged.
    pub fn jump_cursor(&mut self, navigation:&mut CursorNavigation, point:Vector2<f32>) {
        navigation.move_cursor_to_point(self.last_cursor_mut(),point);
        self.merge_overlapping_cursors();
    }

    /// Returns cursor indices sorted by cursors' position in text.
    pub fn sorted_cursor_indices(&self) -> Vec<CursorId> {
        let sorted_pairs = self.cursors.iter().enumerate().sorted_by_key(|(_,c)| c.position);
        sorted_pairs.map(|(i,_)| CursorId(i)).collect()
    }

    /// Merge overlapping cursors
    ///
    /// This function checks all cursors, and merge each pair where cursors are at the same position
    /// or their selection overlap.
    ///
    /// The merged pair will be replaced with one cursor with selection being a sum of selections of
    /// removed cursors.
    fn merge_overlapping_cursors(&mut self) {
        if !self.cursors.is_empty() {
            let sorted             = self.sorted_cursor_indices();
            let mut to_remove      = Vec::new();
            let mut last_cursor_id = sorted[0];
            for id in sorted.iter().skip(1) {
                let merged = self.merged_selection_range(last_cursor_id,*id);
                match merged {
                    Some(merged_range) => {
                        self.cursors[last_cursor_id.0].extend_selection(&merged_range);
                        to_remove.push(*id);
                    },
                    None => {
                        last_cursor_id = *id;
                    }
                };
            }
            for id in to_remove.iter().sorted().rev() {
                self.cursors.remove(id.0);
            }
        }
    }

    /// Checks if two cursors should be merged and returns new selection range after merging if they
    /// shoukd, and `None` otherwise.
    fn merged_selection_range(&self, left_cursor_index:CursorId, right_cursor_index:CursorId)
    -> Option<Range<TextLocation>> {
        let CursorId(left_id)           = left_cursor_index;
        let CursorId(right_id)          = right_cursor_index;
        let left_cursor_position        = self.cursors[left_id].position;
        let left_cursor_range           = self.cursors[left_id].selection_range();
        let right_cursor_position       = self.cursors[right_id].position;
        let right_cursor_range          = self.cursors[right_id].selection_range();
        let are_cursor_at_same_position = left_cursor_position == right_cursor_position;
        let are_ranges_overlapping      = right_cursor_range.start < left_cursor_range.end;
        let are_cursors_merged          = are_cursor_at_same_position || are_ranges_overlapping;
        are_cursors_merged.and_option_from(|| {
            let new_start = left_cursor_range.start.min(right_cursor_range.start);
            let new_end   = left_cursor_range.end  .max(right_cursor_range.end  );
            Some(new_start..new_end)
        })
    }

    #[cfg(test)]
    fn mock(cursors:Vec<Cursor>) -> Self {
        Cursors{cursors}
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use Step::*;

    use ensogl_text_msdf_sys as msdf_sys;

    use crate::display::shape::text::glyph::font;
    use crate::display::shape::text::text_field::content::TextFieldContent;
    use crate::display::shape::text::text_field::content::test::mock_properties;
    use crate::display::shape::text::text_field::TextFieldProperties;

    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    async fn moving_cursors() {
        ensogl_text_msdf_sys::initialized().await;
        let text        = "FirstLine.\nSecondLine\nThirdLine";
        let initial_cursors = vec!
            [ Cursor::new(TextLocation {line:0, column:0 })
            , Cursor::new(TextLocation {line:1, column:0 })
            , Cursor::new(TextLocation {line:1, column:6 })
            , Cursor::new(TextLocation {line:1, column:10})
            , Cursor::new(TextLocation {line:2, column:9 })
            ];
        let mut expected_positions = HashMap::<Step,Vec<(usize,usize)>>::new();
        expected_positions.insert(Left,      vec![(0,0),(0,10),(1,5),(1,9),(2,8)]);
        expected_positions.insert(Right,     vec![(0,1),(1,1),(1,7),(2,0),(2,9)]);
        expected_positions.insert(Up,        vec![(0,0),(0,6),(0,10),(1,9)]);
        expected_positions.insert(Down,      vec![(1,0),(2,0),(2,6),(2,9)]);
        expected_positions.insert(LineBegin, vec![(0,0),(1,0),(2,0)]);
        expected_positions.insert(LineEnd,   vec![(0,10),(1,10),(2,9)]);
        expected_positions.insert(DocBegin,  vec![(0,0)]);
        expected_positions.insert(DocEnd,    vec![(2,9)]);
        expected_positions.insert(PageUp,    vec![(0,0),(0,9)]);
        expected_positions.insert(PageDown,  vec![(2,0),(2,9)]);

        let mut fonts       = font::Registry::new();
        let mut properties  = TextFieldProperties::default(&mut fonts);
        let two_lines_high  = properties.text_size * 2.0;
        properties.size     = Vector2::new(10.0, two_lines_high);
        let content         = &mut TextFieldContent::new(text,&properties);
        let text_field_size = properties.size;
        let selecting       = false;
        let mut navigation  = CursorNavigation{selecting,content,text_field_size};

        for step in &[Left,Right,Up,Down,LineBegin,LineEnd,DocBegin,DocEnd,PageUp,PageDown] {
            let mut cursors = Cursors::mock(initial_cursors.clone());
            cursors.navigate_all_cursors(&mut navigation,*step);
            let expected = expected_positions.get(step).unwrap();
            let current  = cursors.cursors.iter().map(|c| (c.position.line, c.position.column));
            assert_eq!(expected,&current.collect_vec(), "Error for step {:?}", step);
        }
    }

    #[wasm_bindgen_test(async)]
    async fn moving_without_select() {
        ensogl_text_msdf_sys::initialized().await;
        let text              = "FirstLine\nSecondLine";
        let initial_cursor   = Cursor {
            position    : TextLocation {line:1, column:0},
            selected_to : TextLocation {line:0, column:1}
        };
        let initial_cursors   = vec![initial_cursor];
        let new_position      = TextLocation {line:1,column:10};

        let mut fonts       = font::Registry::new();
        let properties      = TextFieldProperties::default(&mut fonts);
        let content         = &mut TextFieldContent::new(text,&properties);
        let selecting       = false;
        let text_field_size = properties.size;
        let mut navigation  = CursorNavigation{content,text_field_size,selecting};
        let mut cursors = Cursors::mock(initial_cursors.clone());
        cursors.navigate_all_cursors(&mut navigation,LineEnd);
        assert_eq!(new_position, cursors.first_cursor().position);
        assert_eq!(new_position, cursors.first_cursor().selected_to);
    }

    #[wasm_bindgen_test(async)]
    async fn page_scrolling() {
        ensogl_text_msdf_sys::initialized().await;
        let text              = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19";
        let initial_cursor    = Cursor::new(TextLocation::at_document_begin());
        let initial_cursors   = vec![initial_cursor];
        let expected_position = TextLocation {line:6,column:0};

        let mut fonts  = font::Registry::new();
        let mut properties = TextFieldProperties::default(&mut fonts);
        properties.size = Vector2::new(100.0,100.0);
        let content         = &mut TextFieldContent::new(text,&properties);
        let selecting       = false;
        let text_field_size = properties.size;
        let mut navigation = CursorNavigation{selecting,content,text_field_size};
        let mut cursors = Cursors::mock(initial_cursors.clone());
        cursors.navigate_all_cursors(&mut navigation,PageDown);
        assert_eq!(expected_position, cursors.first_cursor().position);
        assert_eq!(expected_position, cursors.first_cursor().selected_to);
    }

    #[wasm_bindgen_test(async)]
    async fn moving_with_select() {
        ensogl_text_msdf_sys::initialized().await;
        let text              = "FirstLine\nSecondLine";
        let initial_loc     = TextLocation {line:0,column:1};
        let initial_cursors = vec![Cursor::new(initial_loc)];
        let new_loc         = TextLocation {line:0,column:9};

        let mut fonts       = font::Registry::new();
        let properties      = TextFieldProperties::default(&mut fonts);
        let content         = &mut TextFieldContent::new(text,&properties);
        let selecting       = true;
        let text_field_size = properties.size;
        let mut navigation = CursorNavigation{selecting,content,text_field_size};
        let mut cursors = Cursors::mock(initial_cursors.clone());
        cursors.navigate_all_cursors(&mut navigation,LineEnd);
        assert_eq!(new_loc    , cursors.first_cursor().position);
        assert_eq!(initial_loc, cursors.first_cursor().selected_to);
    }

    #[wasm_bindgen_test(async)]
    async fn merging_selection_after_moving() {
        ensogl_text_msdf_sys::initialized().await;
        let make_char_loc  = |(line,column):(usize,usize)| TextLocation {line,column};
        let cursor_on_left = |range:&Range<(usize,usize)>| Cursor {
            position    : make_char_loc(range.start),
            selected_to : make_char_loc(range.end)
        };
        let cursor_on_right = |range:&Range<(usize,usize)>| Cursor {
            position    : make_char_loc(range.end),
            selected_to : make_char_loc(range.start)
        };
        merging_selection_after_moving_case(cursor_on_left);
        merging_selection_after_moving_case(cursor_on_right);
    }

    fn merging_selection_after_moving_case<F>(convert:F)
    where F : FnMut(&Range<(usize,usize)>) -> Cursor + Clone {
        let ranges           = vec![(1,4)..(1,5), (0,0)..(0,5), (0,2)..(1,0), (1,5)..(2,0)];
        let expected_ranges  = vec![(1,4)..(1,5), (0,0)..(1,0), (1,5)..(2,0)];
        let initial_cursors  = ranges.iter().map(convert.clone()).collect_vec();
        let expected_cursors = expected_ranges.iter().map(convert).collect_vec();
        let mut cursors      = Cursors::mock(initial_cursors);

        cursors.merge_overlapping_cursors();

        assert_eq!(expected_cursors, cursors.cursors);
    }

    #[wasm_bindgen_test(async)]
    async fn recalculate_positions() {
        msdf_sys::initialized().await;
        let content     = "first sentence\r\nthis is a second sentence\r\nlast sentence\n";
        let new_content = "first sentence\r\nsecond one";
        let mut content = TextFieldContent::new(content,&mock_properties());
        let mut cursors = Cursors::default();
        cursors.cursors[0].position    = TextLocation{line:0,column:6};
        cursors.cursors[0].selected_to = TextLocation{line:0,column:14};
        cursors.add_cursor(TextLocation{line:1,column:17});
        cursors.cursors[1].selected_to = TextLocation{line:1,column:25};
        cursors.add_cursor(TextLocation{line:2,column:1});
        cursors.cursors[2].selected_to = TextLocation{line:2,column:2};
        content.set_content(new_content);
        cursors.recalculate_positions(&content);
        assert_eq!(cursors.cursors[0].position   , TextLocation{line:0,column:6});
        assert_eq!(cursors.cursors[0].selected_to, TextLocation{line:0,column:14});
        assert_eq!(cursors.cursors[1].position   , TextLocation{line:1,column:10});
        assert_eq!(cursors.cursors[1].selected_to, TextLocation{line:1,column:10});
        assert_eq!(cursors.cursors[2].position   , TextLocation{line:1,column:1});
        assert_eq!(cursors.cursors[2].selected_to, TextLocation{line:1,column:2});
    }

    #[wasm_bindgen_test(async)]
    async fn step_into_word() {
        msdf_sys::initialized().await;
        let content         = "first sentence\r\nthis is a second sentence\r\nlast sentence\n";
        let properties      = mock_properties();
        let content         = &mut TextFieldContent::new(content,&properties);
        let selecting       = false;
        let text_field_size = properties.size;
        let mut navigation  = CursorNavigation{content,selecting,text_field_size};
        let mut location    = TextLocation::at_document_begin();
        location            = navigation.next_word_position(&location).unwrap();
        assert_eq!(location, TextLocation{line:0, column:5});
        location = navigation.next_word_position(&location).unwrap();
        assert_eq!(location, TextLocation{line:0, column:14});
        location = navigation.next_word_position(&location).unwrap();
        assert_eq!(location, TextLocation{line:1, column:0});
        location = navigation.next_word_position(&location).unwrap();
        assert_eq!(location, TextLocation{line:1, column:4});
        location = navigation.prev_word_position(&location).unwrap();
        assert_eq!(location, TextLocation{line:1, column:0});
        location = navigation.prev_word_position(&location).unwrap();
        assert_eq!(location, TextLocation{line:0, column:14});
        location = navigation.prev_word_position(&location).unwrap();
        assert_eq!(location, TextLocation{line:0, column:6});
        location = navigation.prev_word_position(&location).unwrap();
        assert_eq!(location, TextLocation::at_document_begin());
    }

    #[wasm_bindgen_test(async)]
    async fn block_selection() {
        msdf_sys::initialized().await;
        let content     = "--this\r\n--is\r\n--a\r\n--multiline\r\n--sentence\n";
        let mut content = TextFieldContent::new(content,&mock_properties());
        let mut cursors = Cursors::default();

        let begin = TextLocation{line:0,column:2};
        let end   = TextLocation{line:0,column:6};
        cursors.block_selection_location(&mut content, begin, end);
        let text = cursors.get_selected_text(&content);
        assert_eq!(text, "this");

        cursors.block_selection_location(&mut content, end, begin);
        let text = cursors.get_selected_text(&content);
        assert_eq!(text, "this");

        let end = TextLocation{line:4,column:11};
        cursors.block_selection_location(&mut content, begin, end);
        let text = cursors.get_selected_text(&content);
        assert_eq!(text, "this\nis\na\nmultiline\nsentence");

        cursors.block_selection_location(&mut content, end, begin);
        let text = cursors.get_selected_text(&content);
        assert_eq!(text, "this\nis\na\nmultiline\nsentence");

        let end = TextLocation{line:4,column:4};
        cursors.block_selection_location(&mut content, begin, end);
        let text = cursors.get_selected_text(&content);
        assert_eq!(text, "th\nis\na\nmu\nse");

        cursors.block_selection_location(&mut content, end, begin);
        let text = cursors.get_selected_text(&content);
        assert_eq!(text, "th\nis\na\nmu\nse");
    }
}
