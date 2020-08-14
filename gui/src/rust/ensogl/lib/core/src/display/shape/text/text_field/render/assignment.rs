//! Information about assignment displayed glyph lines to actual fragments of text field content.
//!
//! The proper assignment requires strong unit-test coverage, therefore it was separated from
//! rendering.

use crate::prelude::*;

use crate::display::shape::text::text_field::content::TextFieldContent;

use nalgebra::Vector2;
use std::ops::Range;
use std::ops::RangeInclusive;



/// =====================
/// === LineFragment ===
/// =====================

/// Struct describing specific one line's fragment.
#[derive(Clone,Debug,Eq,PartialEq)]
#[allow(missing_docs)]
pub struct LineFragment {
    pub line_index  : usize,
    pub chars_range : Range<usize>,
}

impl LineFragment {
    /// Tells if rendering this line's fragment will cover the x range.
    pub fn covers_displayed_range
    (&self, displayed_range:&RangeInclusive<f32>, content:&mut TextFieldContent) -> bool {
        let mut line       = content.line(self.line_index);
        let front_rendered = self.chars_range.start == 0;
        let back_rendered  = self.chars_range.end == line.len();
        let x_range_start  = line.get_char_x_position(self.chars_range.start);
        let x_range_end    = line.get_char_x_range(self.chars_range.end.saturating_sub(1)).end;

        let from_left  = front_rendered || *displayed_range.start() >= x_range_start;
        let from_right = back_rendered  || *displayed_range.end()   <= x_range_end;
        from_left && from_right
    }
}



// ============================
// === GlyphLinesAssignment ===
// ============================

/// Information about assignment displayed glyph lines to actual fragments of text field content.
///
/// Here we make distinction between _lines_ which are the line of text in our field, and
/// _glyph lines_ which are sets of sprites designed to render text. So, each _glyph line_ has
/// assigned line of text which it should render. We don't render the whole text, additionally after
/// scroll we try to reassign only some of _glyph lines_ to make a minimum gpu data update.
#[derive(Debug)]
pub struct GlyphLinesAssignment {
    /// The assigned line fragments for specific _glyph line_.
    pub glyph_lines_fragments: Vec<Option<LineFragment>>,
    /// The range of currently assigned _lines_.
    pub assigned_lines: RangeInclusive<usize>,
    /// List of dirty _glyph lines_ after updating assignments. Once those will be refreshed, the
    /// this set should be cleared.
    pub dirty_glyph_lines: HashSet<usize>,
    /// Maximum displayed glyphs in _glyph line_.
    pub max_glyphs_in_line: usize,
    /// The x margin of rendered glyphs in pixels.
    ///
    /// To make a horizontal scrolling faster, each _glyph line_ renders not only the visible
    /// glyphs, but also some on left and right.
    pub x_margin: f32,
    /// During x scrolling we don't immediately refresh all the lines, but pick only some which are
    /// "centered" on current scroll - the rest should still have glyphs rendered in their margin.
    /// This field keeps the id of the next _glyph line_ to do such update.
    pub next_glyph_line_to_x_scroll_update: usize,
}

impl GlyphLinesAssignment {
    /// Constructor making struct without any assignment set.
    pub fn new(glyph_lines_count:usize, max_glyphs_in_line:usize, x_margin:f32) -> Self {
        GlyphLinesAssignment {max_glyphs_in_line,x_margin,
            glyph_lines_fragments              : (0..glyph_lines_count).map(|_| None).collect(),
            assigned_lines                     : 1..=0,
            dirty_glyph_lines                  : HashSet::new(),
            next_glyph_line_to_x_scroll_update : 0,
        }
    }

    /// A number of glyph lines.
    pub fn glyph_lines_count(&self) -> usize {
        self.glyph_lines_fragments.len()
    }
}

impl Default for GlyphLinesAssignment {
    fn default() -> Self {
        GlyphLinesAssignment {
            glyph_lines_fragments              : default(),
            assigned_lines                     : 1..=0,
            dirty_glyph_lines                  : default(),
            max_glyphs_in_line                 : default(),
            x_margin                           : default(),
            next_glyph_line_to_x_scroll_update : default(),
        }
    }
}


// === Assignment update ===

/// A helper structure for making assignment updates. It takes references to GlyphLinesAssignment
/// structure and all required data to make proper reassignments.
#[derive(Debug)]
pub struct GlyphLinesAssignmentUpdate<'a,'b> {
    /// A reference to assignment structure.
    pub assignment: &'a mut GlyphLinesAssignment,
    /// A reference to TextField content.
    pub content: &'b mut TextFieldContent,
    /// Current scroll offset in pixels.
    pub scroll_offset: Vector2<f32>,
    /// Current view size in pixels.
    pub view_size: Vector2<f32>,
}

impl<'a,'b> GlyphLinesAssignmentUpdate<'a,'b> {
    /// Reassign _glyph line_ to currently displayed fragment of line.
    pub fn reassign(&mut self, glyph_line_id:usize, line_id:usize) {
        let fragment = self.displayed_fragment(line_id);
        self.assignment.glyph_lines_fragments[glyph_line_id] = Some(fragment);
        self.assignment.dirty_glyph_lines.insert(glyph_line_id);
    }

    /// Remove _glyph line_ assignment.
    pub fn unassign(&mut self, glyph_line_id:usize) {
        self.assignment.glyph_lines_fragments[glyph_line_id] = None;
        self.assignment.dirty_glyph_lines.insert(glyph_line_id);
    }

    /// Make minimum line reassignment to cover the all displayed lines.
    pub fn update_line_assignment(&mut self) {
        let old_assignment  = self.assignment.assigned_lines.clone();
        let new_assignment  = self.new_assignment();
        let new_on_top      = *new_assignment.start().. *old_assignment.start();
        let new_on_bottom   = old_assignment.end()+1 ..=*new_assignment.end();
        let mut new_lines   = new_on_top.chain(new_on_bottom);

        for glyph_line_id in 0..self.assignment.glyph_lines_count() {
            if self.assignment.can_be_reassigned(glyph_line_id,&new_assignment) {
                if let Some(fragment) = new_lines.next() {
                    self.reassign(glyph_line_id,fragment)
                } else {
                    break;
                }
            }
        }
        self.assignment.assigned_lines = new_assignment;
    }

    /// Update some line's fragments assigned to glyph_lines after horizontal scrolling.
    pub fn update_after_x_scroll(&mut self, x_scroll:f32) {
        let updated_count = (x_scroll.abs() / self.content.line_height).ceil() as usize;
        let updated_count = updated_count.min(self.assignment.glyph_lines_count());
        for glyph_line_id in 0..self.assignment.glyph_lines_count() {
            if self.should_be_updated_after_x_scroll(glyph_line_id,updated_count) {
                let fragment   = self.assignment.glyph_lines_fragments[glyph_line_id].as_ref();
                let line_index = fragment.unwrap().line_index;
                self.reassign(glyph_line_id,line_index);
            }
        }
        self.assignment.increment_next_glyph_line_to_x_scroll_update(updated_count);
    }

    /// Update assigned fragments after text edit.
    ///
    /// Some new lines could be created after edit, and some lines can be longer, what should be
    /// reflected in assigned fragments.
    pub fn update_after_text_edit(&mut self) {
        if self.content.dirty_lines.range.is_some() {
            self.update_line_assignment();
        }
        for i in 0..self.assignment.glyph_lines_fragments.len() {
            let assigned_fragment = &self.assignment.glyph_lines_fragments[i];
            let assigned_line     = assigned_fragment.as_ref().map(|f| f.line_index);

            match assigned_line {
                Some(line) if line >= self.content.lines().len()      => self.unassign(i),
                Some(line) if self.content.dirty_lines.is_dirty(line) => self.reassign(i,line),
                _                                                     => {},
            }
        }
    }
}


// === Private functions ===

impl GlyphLinesAssignment {
    /// Check if given _glyph line_ could be reassigned to another line assuming some set
    /// of visible lines.
    fn can_be_reassigned(&self, glyph_line_id:usize, displayed_lines:&RangeInclusive<usize>)
    -> bool {
        match &self.glyph_lines_fragments[glyph_line_id] {
            Some(fragment) => !displayed_lines.contains(&fragment.line_index),
            None           => true
        }
    }

    /// Increment the `next_glyph_line_to_x_scroll_update` field after updating.
    fn increment_next_glyph_line_to_x_scroll_update(&mut self, updated_count:usize) {
        self.next_glyph_line_to_x_scroll_update += updated_count;
        while self.next_glyph_line_to_x_scroll_update >= self.glyph_lines_fragments.len() {
            self.next_glyph_line_to_x_scroll_update -= self.glyph_lines_fragments.len();
        }
    }
}

impl<'a,'b> GlyphLinesAssignmentUpdate<'a,'b> {
    /// Returns LineFragment of specific line which is currently visible.
    fn displayed_fragment(&mut self, line_id:usize) -> LineFragment {
        let mut line             = self.content.line(line_id);
        let max_index            = line.len().saturating_sub(self.assignment.max_glyphs_in_line);
        let displayed_from_x     = self.scroll_offset.x - self.assignment.x_margin;
        let first_displayed      = line.find_char_at_x_position(displayed_from_x);
        let line_front_displayed = self.scroll_offset.x <= 0.0;

        let start = match first_displayed {
            Some(index)                  => index.min(max_index),
            None if line_front_displayed => 0,
            None                         => max_index
        };
        let end = (start + self.assignment.max_glyphs_in_line).min(line.len());
        LineFragment {
            line_index: line_id,
            chars_range: start..end,
        }
    }

    /// Returns new required line assignment range, which makes minimal change from current
    /// assignment state.
    fn new_assignment(&mut self) -> RangeInclusive<usize> {
        let visible_lines         = self.visible_lines_range();
        let assigned_lines        = &self.assignment.assigned_lines;
        let max_line_id           = self.content.lines().len().saturating_sub(1);
        let lines_count           = |r:&RangeInclusive<usize>| r.end() + 1 - r.start();
        let assigned_lines_count  = lines_count(assigned_lines);
        let displayed_lines_count = lines_count(&visible_lines);
        let hidden_lines_to_keep  = assigned_lines_count.saturating_sub(displayed_lines_count);
        if assigned_lines.start() < visible_lines.start() {
            let new_start = visible_lines.start() - hidden_lines_to_keep;
            new_start..=*visible_lines.end()
        } else if assigned_lines.end() > visible_lines.end() {
            let new_end = (visible_lines.end() + hidden_lines_to_keep).min(max_line_id);
            *visible_lines.start()..=new_end
        } else {
            visible_lines
        }
    }

    /// Returns range of currently visible lines.
    fn visible_lines_range(&mut self) -> RangeInclusive<usize> {
        let lines_count              = self.content.lines().len();
        let top                      = self.scroll_offset.y;
        let bottom                   = self.scroll_offset.y - self.view_size.y;
        let top_line_clipped         = self.content.line_at_y_position(top);
        let first_line_index         = top_line_clipped.map_or(0, |l| l.line_id);
        let bottom_line_clipped      = self.content.line_at_y_position(bottom);
        let last_line_index          = bottom_line_clipped.map_or(lines_count-1,|l| l.line_id);
        first_line_index..=last_line_index
    }

    /// Check if given _glyph line_ should be updated after x scroll.
    fn should_be_updated_after_x_scroll(&mut self, glyph_line_id:usize, updated_count:usize)
    -> bool {
        if let Some(fragment) = &self.assignment.glyph_lines_fragments[glyph_line_id] {
            let content           = &mut self.content;
            let lines_count       = self.assignment.glyph_lines_count();
            let first             = self.assignment.next_glyph_line_to_x_scroll_update;
            let last              = first + updated_count - 1;
            let last_overlap      = last as isize - lines_count as isize;
            let displayed_start   = self.scroll_offset.x;
            let displayed_end     = self.scroll_offset.x + self.view_size.x;
            let displayed_range   = displayed_start..=displayed_end;

            let not_covering      = !fragment.covers_displayed_range(&displayed_range,content);
            let to_update         = (first..=last).contains(&glyph_line_id);
            let to_update_overlap = (glyph_line_id as isize) < last_overlap;
            not_covering || to_update || to_update_overlap
        } else {
            false
        }
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    use crate::data::color;
    use crate::display::shape::text::glyph::font;
    use crate::display::shape::text::text_field::content::TextFieldContent;
    use crate::display::shape::text::text_field::content::line::Line;
    use crate::display::shape::text::text_field::TextFieldProperties;

    use wasm_bindgen_test::wasm_bindgen_test;

    fn mock_properties() -> TextFieldProperties {
        TextFieldProperties {
            font       : mock_font(),
            text_size  : 10.0,
            base_color : color::Rgba::new(0.0,0.0,0.0,1.0),
            size       : Vector2::new(20.0,35.0),
        }
    }

    fn mock_font() -> font::Handle {
        let font   = font::RenderInfo::mock_font("Test".to_string());
        let scale  = Vector2::new(1.0, 1.0);
        let offset = Vector2::new(0.0, 0.0);
        font.mock_char_info('A',scale,offset,1.0);
        font.mock_char_info('B',scale,offset,1.5);
        font.mock_kerning_info('A', 'A', 0.0);
        font.mock_kerning_info('B', 'B', 0.0);
        font.mock_kerning_info('A', 'B', 0.0);
        font.mock_kerning_info('B', 'A', 0.0);
        font::Handle::new(font)
    }

    #[wasm_bindgen_test(async)]
    async fn initial_assignment() {
        ensogl_text_msdf_sys::initialized().await;
        let properties  = mock_properties();
        let mut content = TextFieldContent::new("AAABBB\nBAABAB\n\nA\nA",&properties);

        let mut assignment = GlyphLinesAssignment::new(4, 4, 10.0);

        let mut update     = GlyphLinesAssignmentUpdate {
            assignment    : &mut assignment,
            content       : &mut content,
            scroll_offset : Vector2::new(22.0,0.0),
            view_size     : properties.size
        };
        update.update_line_assignment();
        let expected_fragments = vec!
            [ Some(LineFragment{line_index:0, chars_range: 1..5})
            , Some(LineFragment{line_index:1, chars_range: 0..4})
            , Some(LineFragment{line_index:2, chars_range: 0..0})
            , Some(LineFragment{line_index:3, chars_range: 0..1})
            ];
        let expected_dirties : HashSet<usize> = [0,1,2,3].iter().cloned().collect();

        assert_eq!(expected_fragments, assignment.glyph_lines_fragments);
        assert_eq!(expected_dirties,   assignment.dirty_glyph_lines);
    }

    #[wasm_bindgen_test(async)]
    async fn lines_reassignment() {
        ensogl_text_msdf_sys::initialized().await;
        let properties  = mock_properties();
        let mut content = TextFieldContent::new("AAABBB\nBAABAB\n\nA\nA\nAB",&properties);

        let mut assignment = GlyphLinesAssignment::new(4, 4, 10.0);
        assignment.glyph_lines_fragments = vec!
            [ Some(LineFragment{line_index:0, chars_range: 1..5})
            , Some(LineFragment{line_index:1, chars_range: 0..4})
            , Some(LineFragment{line_index:2, chars_range: 0..0})
            , Some(LineFragment{line_index:3, chars_range: 0..1})
            ];
        let assigned_lines        = 0..=3;
        assignment.assigned_lines = assigned_lines;
        // This line casues false warning about unnecessary parentheses
        // assignment.assigned_lines = 0..3;

        // scrolling down
        let mut update     = GlyphLinesAssignmentUpdate {
            assignment    : &mut assignment,
            content       : &mut content,
            scroll_offset : Vector2::new(22.0,-21.0),
            view_size     : properties.size
        };
        update.update_line_assignment();
        let expected_fragments = vec!
            [ Some(LineFragment{line_index:4, chars_range: 0..1})
            , Some(LineFragment{line_index:5, chars_range: 0..2})
            , Some(LineFragment{line_index:2, chars_range: 0..0})
            , Some(LineFragment{line_index:3, chars_range: 0..1})
            ];
        let expected_dirties : HashSet<usize> = [0,1].iter().cloned().collect();
        assert_eq!(expected_fragments, update.assignment.glyph_lines_fragments);
        assert_eq!(expected_dirties  , update.assignment.dirty_glyph_lines);
        assert_eq!(2..=5             , update.assignment.assigned_lines);

        // scrolling up.
        update.assignment.dirty_glyph_lines.clear();
        update.scroll_offset = Vector2::new(22.0,-11.0);
        update.update_line_assignment();
        let expected_fragments = vec!
            [ Some(LineFragment{line_index:4, chars_range: 0..1})
            , Some(LineFragment{line_index:1, chars_range: 0..4})
            , Some(LineFragment{line_index:2, chars_range: 0..0})
            , Some(LineFragment{line_index:3, chars_range: 0..1})
            ];
        let expected_dirties : HashSet<usize> = [1].iter().cloned().collect();
        assert_eq!(expected_fragments, update.assignment.glyph_lines_fragments);
        assert_eq!(expected_dirties  , update.assignment.dirty_glyph_lines);
        assert_eq!(1..=4             , update.assignment.assigned_lines);
    }

    #[wasm_bindgen_test(async)]
    async fn marking_dirty_after_x_scrolling() {
        ensogl_text_msdf_sys::initialized().await;
        let properties  = mock_properties();
        let mut content = TextFieldContent::new("AAABBB\nBAABAB\nBBABAB\nA\nA",&properties);

        let mut assignment = GlyphLinesAssignment::new(4, 4, 10.0);
        assignment.glyph_lines_fragments = vec!
            [ Some(LineFragment{line_index:0, chars_range: 1..5})
            , Some(LineFragment{line_index:1, chars_range: 0..4})
            , Some(LineFragment{line_index:2, chars_range: 1..5})
            , Some(LineFragment{line_index:3, chars_range: 0..1})
            ];
        assignment.next_glyph_line_to_x_scroll_update = 3;
        let mut update     = GlyphLinesAssignmentUpdate {
            assignment    : &mut assignment,
            content       : &mut content,
            scroll_offset : Vector2::new(42.0,-21.0),
            view_size     : properties.size
        };
        update.update_after_x_scroll(15.0);
        let expected_fragments = vec!
            [ Some(LineFragment{line_index:0, chars_range: 2..6})
            , Some(LineFragment{line_index:1, chars_range: 2..6})
            , Some(LineFragment{line_index:2, chars_range: 1..5})
            , Some(LineFragment{line_index:3, chars_range: 0..1})
            ];
        assert_eq!(expected_fragments, update.assignment.glyph_lines_fragments);
        assert_eq!(1                 , update.assignment.next_glyph_line_to_x_scroll_update);
    }

    #[wasm_bindgen_test(async)]
    async fn update_after_text_edit() {
        ensogl_text_msdf_sys::initialized().await;
        let properties  = mock_properties();

        let mut assignment = GlyphLinesAssignment::new(3, 4, 10.0);
        assignment.glyph_lines_fragments = vec!
            [ Some(LineFragment{line_index:0, chars_range: 1..5})
            , Some(LineFragment{line_index:1, chars_range: 0..4})
            , None
            ];
        assignment.assigned_lines = 0..=1;
        let mut content = TextFieldContent::new("AAABBB\nBA",&properties);
        let mut update     = GlyphLinesAssignmentUpdate {
            assignment    : &mut assignment,
            content       : &mut content,
            scroll_offset : Vector2::new(22.0,0.0),
            view_size     : properties.size
        };

        // Editing line:
        update.content.dirty_lines.add_single_line(1);
        update.update_after_text_edit();
        let expected_fragments = vec!
            [ Some(LineFragment{line_index:0, chars_range: 1..5})
            , Some(LineFragment{line_index:1, chars_range: 0..2})
            , None
            ];
        let expected_dirties:HashSet<usize> = [1].iter().cloned().collect();
        assert_eq!(expected_fragments, update.assignment.glyph_lines_fragments);
        assert_eq!(expected_dirties  , update.assignment.dirty_glyph_lines);

        // Removing line:
        update.assignment.dirty_glyph_lines.clear();
        update.content.lines_mut().pop();
        update.content.dirty_lines.add_lines_range_from(1..);
        update.update_after_text_edit();
        let expected_fragments = vec!
            [ Some(LineFragment{line_index:0, chars_range: 1..5})
            , None
            , None
            ];
        let expected_dirties:HashSet<usize> = [1].iter().cloned().collect();
        assert_eq!(expected_fragments, update.assignment.glyph_lines_fragments);
        assert_eq!(expected_dirties  , update.assignment.dirty_glyph_lines);

        // Adding line:
        update.assignment.dirty_glyph_lines.clear();
        update.content.lines_mut().push(Line::new("AAAAAAAAAAAA".to_string()));
        update.content.dirty_lines.add_lines_range_from(1..);
        update.update_after_text_edit();
        let expected_fragments = vec!
            [ Some(LineFragment{line_index:0, chars_range: 1..5})
            , Some(LineFragment{line_index:1, chars_range: 1..5})
            , None
            ];
        let expected_dirties : HashSet<usize> = [1].iter().cloned().collect();
        assert_eq!(expected_fragments, update.assignment.glyph_lines_fragments);
        assert_eq!(expected_dirties  , update.assignment.dirty_glyph_lines);
    }
}
