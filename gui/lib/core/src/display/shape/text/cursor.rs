#![allow(missing_docs)]

use crate::prelude::*;

use crate::display::render::webgl::Context;
use crate::display::render::webgl::set_buffer_data;
use crate::display::shape::text::content::TextLocation;
use crate::display::shape::text::content::TextComponentContent;
use crate::display::shape::text::content::line::LineRef;
use crate::display::shape::text::buffer::glyph_square::point_to_iterable;
use crate::display::shape::text::font::Fonts;

use nalgebra::Point2;
use nalgebra::Translation2;
use std::cmp::Ordering;
use std::ops::Range;
use web_sys::WebGlBuffer;



// ==============
// === Cursor ===
// ==============

/// Cursor in TextComponent with its selection
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct Cursor {
    pub position    : TextLocation,
    pub selected_to : TextLocation,
}

impl Cursor {
    /// Create a new cursor at given position and without any selection.
    pub fn new(position:TextLocation) -> Self {
        Cursor {position,
            selected_to : position
        }
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

    /// Get `LineRef` object of this cursor's line.
    pub fn current_line<'a>(&self, content:&'a mut TextComponentContent) -> LineRef<'a> {
        content.line(self.position.line)
    }

    /// Get the position where the cursor should be rendered. The returned point is on the
    /// _baseline_ of cursor's line, on the right side of character from the left side of the cursor
    /// (where usually the cursor is displayed by text editors).
    ///
    /// _Baseline_ is a font specific term, for details see [freetype documentation]
    //  (https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html#section-1).
    pub fn render_position(&self, content:&mut TextComponentContent, fonts:&mut Fonts)
    -> Point2<f64>{
        let x = Self::x_position_of_cursor_at(&self.position,content,fonts);
        let y = self.current_line(content).start_point().y;
        Point2::new(x,y)
    }

    fn x_position_of_cursor_at
    (at:&TextLocation, content:&mut TextComponentContent, fonts:&mut Fonts)
    -> f64 {
        let font     = fonts.get_render_info(content.font);
        let mut line = content.line(at.line);
        if at.column > 0 {
            let char_index = at.column - 1;
            line.get_char_x_range(char_index,font).end.into()
        } else {
            line.start_point().x
        }
    }
}



// ==================
// === Navigation ===
// ==================

/// An enum representing cursor moving step. The steps are based of possible keystrokes (arrows,
/// Home, End, Ctrl+Home, etc.)
#[derive(Debug,Eq,Hash,PartialEq)]
pub enum Step {Left,Right,Up,Down,LineBegin,LineEnd,DocBegin,DocEnd}

/// A struct for cursor navigation process
pub struct CursorNavigation<'a,'b> {
    pub content   : &'a mut TextComponentContent,
    pub fonts     : &'b mut Fonts,
    pub selecting : bool
}

impl<'a,'b> CursorNavigation<'a,'b> {
    /// Jump cursor directly to given position.
    pub fn move_cursor_to_position(&self, cursor:&mut Cursor, to:TextLocation) {
        cursor.position = to;
        if !self.selecting {
            cursor.selected_to = to;
        }
    }

    /// Move cursor by given step.
    pub fn move_cursor(&mut self, cursor:&mut Cursor, step:&Step) {
        let new_position = self.new_position(cursor.position,&step);
        self.move_cursor_to_position(cursor,new_position);
    }

    /// Get cursor position at end of given line.
    pub fn line_end_position(&self, line_index:usize) -> TextLocation {
        TextLocation {
            line   : line_index,
            column : self.content.lines[line_index].len(),
        }
    }

    /// Get cursor position at end of whole content
    pub fn content_end_position(&self) -> TextLocation {
        TextLocation {
            column : self.content.lines.last().unwrap().len(),
            line   : self.content.lines.len() - 1,
        }
    }

    /// Get cursor position for the next char from given position. Returns none if at end of
    /// whole document.
    pub fn next_char_position(&self, position:&TextLocation) -> Option<TextLocation> {
        let current_line = &self.content.lines[position.line];
        let next_column  = Some(position.column + 1).filter(|c| *c <= current_line.len());
        let next_line    = Some(position.line + 1)  .filter(|l| *l < self.content.lines.len());
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
    pub fn line_up_position(&mut self, position:&TextLocation) -> Option<TextLocation> {
        let prev_line = position.line.checked_sub(1);
        prev_line.map(|line| self.near_same_x_in_another_line(position,line))
    }

    /// Get cursor position one line behind the given position, such the new x coordinate of
    /// displayed cursor on the screen will be nearest the current value.
    pub fn line_down_position(&mut self, position:&TextLocation) -> Option<TextLocation> {
        let next_line = Some(position.line + 1).filter(|l| *l < self.content.lines.len());
        next_line.map(|line| self.near_same_x_in_another_line(position,line))
    }

    /// New position of cursor at `position` after applying `step`.
    fn new_position(&mut self, position: TextLocation, step:&Step) -> TextLocation {
        match step {
            Step::Left      => self.prev_char_position(&position).unwrap_or(position),
            Step::Right     => self.next_char_position(&position).unwrap_or(position),
            Step::Up        => self.line_up_position(&position).unwrap_or(position),
            Step::Down      => self.line_down_position(&position).unwrap_or(position),
            Step::LineBegin => TextLocation::at_line_begin(position.line),
            Step::LineEnd   => self.line_end_position(position.line),
            Step::DocBegin  => TextLocation::at_document_begin(),
            Step::DocEnd    => self.content_end_position(),
        }
    }

    /// Get the cursor position on another line, such that the new x coordinate of
    /// displayed cursor on the screen will be nearest the current value.
    fn near_same_x_in_another_line(&mut self, position:&TextLocation, line:usize)
    -> TextLocation {
        let x_position = Cursor::x_position_of_cursor_at(position,self.content,self.fonts);
        let column     = self.column_near_x(line,x_position);
        TextLocation {line,column}
    }

    /// Get the column number in given line, so the cursor will be as near as possible the
    /// `x_position` in _text space_. See `display::shape::text::content::line::Line`
    /// documentation for details about _text space_.
    fn column_near_x(&mut self, line_index:usize, x_position:f64) -> usize {
        let font                    = self.fonts.get_render_info(self.content.font);
        let mut line                = self.content.line(line_index);
        let x                       = x_position as f32;
        let char_at_x               = line.find_char_at_x_position(x,font);
        let nearer_to_end           = |range:Range<f32>| range.end - x < x - range.start;
        let mut nearer_to_chars_end = |index| nearer_to_end(line.get_char_x_range(index,font));
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

/// The number of vertices of single cursor.
const CURSOR_BASE_LAYOUT_SIZE : usize = 2;

lazy_static! {
    /// The base vertices position of single cursor. This position is then translated to the
    /// actual cursor position.
    pub static ref CURSOR_VERTICES_BASE_LAYOUT : [Point2<f32>;CURSOR_BASE_LAYOUT_SIZE] =
        [ Point2::new(0.0, -0.2)
        , Point2::new(0.0,  0.8)
        ];
}

/// Structure handling many cursors.
///
/// Usually there is only one cursor, but we have possibility of having many cursors in one text
/// component enabling editing in multiple lines/places at once. This structure also owns
/// a WebGL buffer with vertex positions of all cursors.
#[derive(Debug)]
pub struct Cursors {
    pub cursors : Vec<Cursor>,
    pub dirty   : bool,
    pub buffer  : Option<WebGlBuffer>,
}

impl Cursors {

    /// Create empty `Cursors` structure.
    pub fn new(gl_context:&Context) -> Self {
        Cursors {
            cursors : Vec::new(),
            dirty   : false,
            buffer  : gl_context.create_buffer()
        }
    }

    /// Update the cursors' buffer data.
    pub fn update_buffer_data
    (&mut self, gl_context:&Context, content:&mut TextComponentContent, fonts:&mut Fonts) {
        let cursors          = self.cursors.iter();
        let cursors_vertices = cursors.map(|cursor| Self::cursor_vertices(cursor,content,fonts));
        let buffer_data      = cursors_vertices.flatten().collect_vec();
        set_buffer_data(gl_context,self.buffer.as_ref().unwrap(),buffer_data.as_slice());
        self.dirty = false;
    }

    /// Removes all current cursors and replace them with single cursor without any selection.
    pub fn set_cursor(&mut self, position: TextLocation) {
        self.cursors = vec![Cursor::new(position)];
        self.dirty   = true;
    }

    /// Add new cursor without selection.
    pub fn add_cursor(&mut self, position: TextLocation) {
        self.cursors.push(Cursor::new(position));
        self.merge_overlapping_cursors();
        self.dirty = true;
    }

    /// Do the navigation step of all cursors.
    ///
    /// If after this operation some of the cursors occupies the same position, or their selected
    /// area overlap, they are irreversibly merged.
    pub fn navigate_all_cursors(&mut self, navigaton:&mut CursorNavigation, step:&Step) {
        self.cursors.iter_mut().for_each(|cursor| navigaton.move_cursor(cursor,&step));
        self.merge_overlapping_cursors();
        self.dirty = true;
    }

    /// Number of vertices in cursors' buffer.
    pub fn vertices_count(&self) -> usize {
        self.cursors.len() * CURSOR_BASE_LAYOUT_SIZE
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
            self.cursors.sort_by_key(|c| c.position);
            let mut i          = 1;
            while i < self.cursors.len() {
                let merged = self.merged_selection_range(i - 1,i);
                match merged {
                    Some(merged_range) => {
                        self.cursors[i-1].extend_selection(&merged_range);
                        self.cursors.remove(i);
                    },
                    None => {
                        i += 1
                    }
                };
            }
        }
    }

    /// Checks if two cursors should be merged and returns new selection range after merging if they
    /// shoukd, and `None` otherwise.
    fn merged_selection_range(&self, left_cursor_index:usize, right_cursor_index:usize)
    -> Option<Range<TextLocation>> {
        let left_cursor_position        = self.cursors[left_cursor_index].position;
        let left_cursor_range           = self.cursors[left_cursor_index].selection_range();
        let right_cursor_position       = self.cursors[right_cursor_index].position;
        let right_cursor_range          = self.cursors[right_cursor_index].selection_range();
        let are_cursor_at_same_position = left_cursor_position == right_cursor_position;
        let are_ranges_overlapping      = right_cursor_range.start < left_cursor_range.end;
        let are_cursors_merged          = are_cursor_at_same_position || are_ranges_overlapping;
        are_cursors_merged.and_option_from(|| {
            let new_start = left_cursor_range.start.min(right_cursor_range.start);
            let new_end   = left_cursor_range.end  .max(right_cursor_range.end  );
            Some(new_start..new_end)
        })
    }

    fn cursor_vertices(cursor:&Cursor, content:&mut TextComponentContent, fonts:&mut Fonts)
    -> SmallVec<[f32;12]> {
        let position    = cursor.render_position(content,fonts);
        let to_position = Translation2::new(position.x as f32,position.y as f32);
        let base        = CURSOR_VERTICES_BASE_LAYOUT.iter();
        let on_position = base.map(|p| to_position * p);
        on_position.map(point_to_iterable).flatten().collect()
    }

    #[cfg(test)]
    fn mock(cursors:Vec<Cursor>) -> Self {
        Cursors{cursors,
            dirty  : false,
            buffer : None
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use Step::*;

    use basegl_core_msdf_sys::test_utils::TestAfterInit;
    use std::future::Future;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;
    use crate::display::shape::text::cursor::Step::{LineBegin, DocBegin, LineEnd};

    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test(async)]
    fn moving_cursors() -> impl Future<Output=()> {
        TestAfterInit::schedule(||{
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

            let mut fonts      = Fonts::new();
            let font           = fonts.load_embedded_font("DejaVuSansMono").unwrap();
            let mut content    = TextComponentContent::new(font,text);
            let mut navigation = CursorNavigation {
                content: &mut content,
                fonts: &mut fonts,
                selecting: false
            };

            for step in &[Left,Right,Up,Down,LineBegin,LineEnd,DocBegin,DocEnd] {
                let mut cursors = Cursors::mock(initial_cursors.clone());

                cursors.navigate_all_cursors(&mut navigation,step);
                let expected = expected_positions.get(step).unwrap();
                let current  = cursors.cursors.iter().map(|c| (c.position.line, c.position.column));
                assert_eq!(expected,&current.collect_vec(), "Error for step {:?}", step);
            }
        })
    }

    #[wasm_bindgen_test(async)]
    fn moving_without_select() -> impl Future<Output=()> {
        TestAfterInit::schedule(||{
            let text              = "FirstLine\nSecondLine";
            let initial_cursor   = Cursor {
                position    : TextLocation {line:1, column:0},
                selected_to : TextLocation {line:0, column:1}
            };
            let initial_cursors   = vec![initial_cursor];
            let new_position      = TextLocation {line:1,column:10};

            let mut fonts      = Fonts::new();
            let font           = fonts.load_embedded_font("DejaVuSansMono").unwrap();
            let mut content    = TextComponentContent::new(font,text);
            let mut navigation = CursorNavigation {
                content: &mut content,
                fonts: &mut fonts,
                selecting: false
            };
            let mut cursors    = Cursors::mock(initial_cursors.clone());
            cursors.navigate_all_cursors(&mut navigation,&LineEnd);
            assert_eq!(new_position, cursors.cursors.first().unwrap().position);
            assert_eq!(new_position, cursors.cursors.first().unwrap().selected_to);
        })
    }

    #[wasm_bindgen_test(async)]
    fn moving_with_select() -> impl Future<Output=()> {
        TestAfterInit::schedule(||{
            let text              = "FirstLine\nSecondLine";
            let initial_loc     = TextLocation {line:0,column:1};
            let initial_cursors = vec![Cursor::new(initial_loc)];
            let new_loc         = TextLocation {line:0,column:9};

            let mut fonts      = Fonts::new();
            let font           = fonts.load_embedded_font("DejaVuSansMono").unwrap();
            let mut content    = TextComponentContent::new(font,text);
            let mut navigation = CursorNavigation {
                content: &mut content,
                fonts: &mut fonts,
                selecting: true
            };
            let mut cursors = Cursors::mock(initial_cursors.clone());
            cursors.navigate_all_cursors(&mut navigation,&LineEnd);
            assert_eq!(new_loc    , cursors.cursors.first().unwrap().position);
            assert_eq!(initial_loc, cursors.cursors.first().unwrap().selected_to);
        })
    }

    #[wasm_bindgen_test(async)]
    fn merging_selection_after_moving() -> impl Future<Output=()> {
        TestAfterInit::schedule(||{
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
        })
    }

    fn merging_selection_after_moving_case<F>(convert:F)
    where F : FnMut(&Range<(usize,usize)>) -> Cursor + Clone {
        let ranges           = vec![(1,4)..(1,5), (0,0)..(0,5), (0,2)..(1,0), (1,5)..(2,0)];
        let expected_ranges  = vec![(0,0)..(1,0), (1,4)..(1,5), (1,5)..(2,0)];
        let initial_cursors  = ranges.iter().map(convert.clone()).collect_vec();
        let expected_cursors = expected_ranges.iter().map(convert).collect_vec();
        let mut cursors      = Cursors::mock(initial_cursors);

        cursors.merge_overlapping_cursors();

        assert_eq!(expected_cursors, cursors.cursors);
    }
}
