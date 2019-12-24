use crate::prelude::*;

use crate::display::shape::text::buffer::glyph_square::Pen;
use crate::display::shape::text::buffer::glyph_square::GlyphVertexPositionBuilder;
use crate::display::shape::text::buffer::glyph_square::GlyphTextureCoordsBuilder;
use crate::display::shape::text::buffer::line::LineAttributeBuilder;
use crate::display::shape::text::content::DirtyLines;
use crate::display::shape::text::font::FontRenderInfo;

use nalgebra::geometry::Point2;
use std::ops::Range;
use std::ops::RangeInclusive;


// ======================
// === BufferFragment ===
// ======================

/// Buffer Fragment
///
/// The buffers of TextComponent are split to equally-sized fragments, and each fragment may be
/// assigned to some line. Thanks to that we can easily refresh only minimal subset of lines and
/// quickly replace line with another during scrolling.
#[derive(Debug)]
pub struct BufferFragment {
    pub assigned_line : Option<usize>,
    pub rendered      : Option<RenderedFragment>,
    pub dirty         : bool,
}

/// Information what is currently rendered on screen for some specific _buffer fragment_.
#[derive(Debug)]
#[derive(Clone)]
pub struct RenderedFragment {
    pub first_char          : RenderedChar,
    pub last_char           : RenderedChar,
}

/// The rendered char position in line and on screen.
#[derive(Debug)]
#[derive(Clone)]
pub struct RenderedChar {
    pub index       : usize,
    pub byte_offset : usize,
    pub pen         : Pen
}

impl BufferFragment {
    /// Creates new buffer fragment which is not assigned to any line.
    pub fn unassigned() -> BufferFragment {
        BufferFragment {
            assigned_line     : None,
            rendered          : None,
            dirty             : false,
        }
    }

    /// Basing of list of current displayed lines, this function tells if the fragment can be
    /// assigned to another line.
    pub fn can_be_reassigned(&self,displayed_lines:&RangeInclusive<usize>) -> bool {
        match self.assigned_line {
            Some(index) => !displayed_lines.contains(&index),
            None        => true
        }
    }

    /// Tells if fragment's data should be updated.
    pub fn should_be_dirty(&self, displayed_x:&RangeInclusive<f64>, lines:&[String]) -> bool {
        match (&self.assigned_line,&self.rendered) {
            (Some(line),Some(ren)) => ren.should_be_updated(&displayed_x,lines[*line].as_str()),
            (Some(_)   ,None     ) => true,
            (None      ,_        ) => false
        }
    }
}

impl RenderedFragment {
    /// Tells if fragment needs to be updated because currently rendered content does not covers
    /// all displayed part of line.
    pub fn should_be_updated(&self, displayed_range:&RangeInclusive<f64>, line:&str)
     -> bool {
        let front_rendered  = self.first_char.index == 0;
        let back_char_size  = self.last_char.pen.current_char.map(|ch| ch.len_utf8()).unwrap_or(0);
        let back_rendered   = self.last_char.byte_offset == line.len() - back_char_size;
        let range           = self.x_range();

        let has_on_left     = !front_rendered && displayed_range.start() < range.start();
        let has_on_right    = !back_rendered  && displayed_range.end()   > range.end();
        has_on_left || has_on_right
    }

    /// X range of rendered line's fragment
    pub fn x_range(&self) -> RangeInclusive<f64> {
        let begin = self.first_char.pen.position.x;
        let end   = self.last_char.pen.position.x + self.last_char.pen.next_advance;
        begin..=end
    }
}

// ===========================
// === FragmentDataBuilder ===
// ===========================

/// Builder of buffer data of some consecutive buffer fragments
///
/// The result is stored in `vertex_position_data` and `texture_coords_data` fields.
pub struct FragmentsDataBuilder<'a> {
    pub vertex_position_data  : Vec<f32>,
    pub texture_coords_data   : Vec<f32>,
    pub font                  : &'a mut FontRenderInfo,
    pub line_clip             : Range<f64>,
    pub max_chars_in_fragment : usize,
}

impl<'a> FragmentsDataBuilder<'a> {

    /// Append buffers' data for fragment assigned to `line`
    pub fn build_for_line(&mut self, line_index:usize, line:&str) -> Option<RenderedFragment> {
        let line_y         = -(line_index as f64) - 1.0;
        let mut pen        = Pen::new(Point2::new(0.0, line_y));
        let first_char     = self.first_rendered_char(&mut pen,&line);
        let first_char_ref = first_char.as_ref();
        let rendered_text  = first_char_ref.map_or(line, |rch| &line[rch.byte_offset..]);
        let last_char      = first_char_ref.map(|fc| self.last_rendered_char(&fc,rendered_text));

        self.build_vertex_positions(&pen,rendered_text);
        self.build_texture_coords(&rendered_text);
        match (first_char,last_char.flatten()) {
            (Some(fch),Some(lch)) => Some(RenderedFragment{first_char:fch, last_char:lch}),
            _                     => None
        }
    }

    /// Get information about first char which data will be actually stored in buffer.
    pub fn first_rendered_char(&mut self, pen:&mut Pen, line:&str) -> Option<RenderedChar> {
        let line_length         = line.chars().count();
        let always_render_index = line_length.saturating_sub(self.max_chars_in_fragment);
        let line_clip           = &self.line_clip;
        let font                = &mut self.font;
        let pen_per_char        = line.chars().map(|c| { pen.next_char(c,font).clone() });
        let chars_with_index    = line.char_indices().enumerate();
        let chars_with_pen      = chars_with_index.zip(pen_per_char);
        let mut chars           = chars_with_pen.map(|((ind,(offset,ch)),pen)| (ind,offset,ch,pen));

        chars.find_map(|(index,offset,_,pen)| {
            let byte_offset = offset;
            let visible     = pen.is_in_x_range(line_clip);
            let rendered    = visible || index >= always_render_index;
            rendered.and_option_from(|| Some(RenderedChar{index,byte_offset,pen}))
        })
    }

    /// Get information about last char which data will be actually stored in buffer.
    pub fn last_rendered_char(&mut self, first_char:&RenderedChar, rendered_text:&str)
    -> Option<RenderedChar> {
        let mut pen               = first_char.pen.clone();
        let rendered_chars_iter   = rendered_text.char_indices().take(self.max_chars_in_fragment);
        let (last_char_offset, _) = rendered_chars_iter.clone().last()?;
        let last_char_index       = self.max_chars_in_fragment.min(rendered_text.len())-1;
        let byte_offset           = first_char.byte_offset + last_char_offset;
        let index                 = first_char.index + last_char_index;
        for (_, ch) in rendered_chars_iter.skip(1) {
            pen.next_char(ch, &mut self.font);
        }
        Some(RenderedChar { index, byte_offset, pen })
    }

    /// Extend vertex position data with a new line's.
    pub fn build_vertex_positions(&mut self, pen:&Pen, text:&str) {
        let rendering_pen = Pen::new(pen.position);
        let glyph_builder = GlyphVertexPositionBuilder::new(self.font,rendering_pen);
        let max_line_size = self.max_chars_in_fragment;
        let builder       = LineAttributeBuilder::new(text,glyph_builder,max_line_size);
        self.vertex_position_data.extend(builder.flatten().map(|f| f as f32));
    }

    /// Extend texture coordinates data with a new line's.
    pub fn build_texture_coords(&mut self, text:&str) {
        let glyph_builder = GlyphTextureCoordsBuilder::new(self.font);
        let max_line_size = self.max_chars_in_fragment;
        let builder       = LineAttributeBuilder::new(text,glyph_builder,max_line_size);
        self.texture_coords_data.extend(builder.flatten().map(|f| f as f32));
    }
}


// =======================
// === BufferFragments ===
// =======================

/// The pointer to the fragment which will be refreshed after x scrolling.
///
/// During x scrolling we don't immediately refresh all the lines, but pick only one which is
/// "centered" on current scroll - the rest of lines should still have data in buffers for
/// shown glyphs.
#[derive(Debug)]
pub struct NextFragmentToRefreshAfterXScrolling {
    pub fragments_count : usize,
    pub next_fragment   : usize
}

impl NextFragmentToRefreshAfterXScrolling {
    /// New structure pointing to the first line.
    pub fn new(fragments_count:usize) -> Self {
        NextFragmentToRefreshAfterXScrolling {fragments_count,
            next_fragment:0
        }
    }

    /// Get range of lines to refresh after x scroll
    ///
    /// The bigger jumps should result in more lines to refresh, although there is no point to
    /// returning last and first line, because in such case we'll be refreshing all the data anyway.
    /// Better hope that lines won't scroll further in such pace.
    pub fn get_after_x_scroll(&mut self, x_scroll:f64) -> Range<usize> {
        let returned_size  = x_scroll.abs().ceil() as usize;
        let returned_start = self.next_fragment;
        if self.fragments_count - self.next_fragment <= returned_size {
            self.next_fragment = 0;
            returned_start..self.fragments_count
        } else {
            self.next_fragment += returned_size;
            returned_start..(returned_start + returned_size)
        }
    }
}

/// A structure managing many buffer fragments.
#[derive(Debug)]
pub struct BufferFragments {
    pub fragments   : Vec<BufferFragment>,
    assigned_lines  : RangeInclusive<usize>,
    next_to_refresh : NextFragmentToRefreshAfterXScrolling
}

impl BufferFragments {

    /// Create the unassigned fragments for each displayed line.
    pub fn new(displayed_lines:usize) -> BufferFragments {
        let indexes              = 0..displayed_lines;
        let unassigned_fragments = indexes.map(|_| BufferFragment::unassigned());
        BufferFragments {
            fragments       : unassigned_fragments.collect(),
            assigned_lines  : 1..=0,
            next_to_refresh : NextFragmentToRefreshAfterXScrolling::new(displayed_lines),
        }
    }

    /// Make minimum fragments reassignment to cover the all displayed lines.
    ///
    /// Reassigned buffers are marked as dirty.
    pub fn reassign_fragments(&mut self, displayed_lines:RangeInclusive<usize>) {
        let current_assignment  = &self.assigned_lines;
        let new_assignment      = self.new_assignment(displayed_lines);
        let new_on_top          = *new_assignment.start()  .. *current_assignment.start();
        let new_on_bottom       = current_assignment.end()+1..=*new_assignment.end();
        let new_lines           = new_on_top.chain(new_on_bottom);
        let fragments           = self.fragments.iter_mut();
        let fragments_to_assign = fragments.filter(|f| f.can_be_reassigned(&new_assignment));
        let reassignments       = new_lines.zip(fragments_to_assign);

        for (line_id,fragment) in reassignments {
            fragment.assigned_line = Some(line_id);
            fragment.dirty         = true;
        }
        self.assigned_lines = new_assignment;
    }

    /// Returns new assignment for displayed lines, which makes minimal required reassignments.
    pub fn new_assignment(&self, displayed_lines:RangeInclusive<usize>) -> RangeInclusive<usize> {
        let lines_count           = |r:&RangeInclusive<usize>| r.end() + 1 - r.start();
        let assigned_lines_count  = lines_count(&self.assigned_lines);
        let displayed_lines_count = lines_count(&displayed_lines);
        let hidden_lines_to_keep  = assigned_lines_count.saturating_sub(displayed_lines_count);
        if self.assigned_lines.start() < displayed_lines.start() {
            let new_start = displayed_lines.start() - hidden_lines_to_keep;
            new_start..=*displayed_lines.end()
        } else if self.assigned_lines.end() > displayed_lines.end() {
            let new_end = displayed_lines.end() + hidden_lines_to_keep;
            *displayed_lines.start()..=new_end
        } else {
            displayed_lines
        }
    }

    /// Mark as dirty all fragments which should be refreshed after x scrolling.
    ///
    /// We should refresh fragment when after scroll a not-yet-rendered fragment of line appears.
    pub fn mark_dirty_after_x_scrolling
    (&mut self, x_scroll:f64, displayed_x_range:RangeInclusive<f64>, lines:&[String]) {
        let not_yet_dirty   = self.fragments.iter_mut().enumerate().filter(|(_,f)| !f.dirty);
        let range_refreshed = self.next_to_refresh.get_after_x_scroll(x_scroll);
        for (index,fragment) in not_yet_dirty {
            let must_be_refreshed = fragment.should_be_dirty(&displayed_x_range,lines);
            fragment.dirty        = must_be_refreshed || range_refreshed.contains(&index);
        }
    }

    /// Mark as dirty all fragments with dirty assigned line.
    pub fn mark_lines_dirty(&mut self, lines:&DirtyLines) {
        let not_yet_dirty = self.fragments.iter_mut().filter(|f| !f.dirty);
        for fragment in not_yet_dirty {
            fragment.dirty = fragment.assigned_line.map_or(false, |l| lines.is_dirty(l));
        }
    }

    /// Get the minimum fragment id range covering all dirties.
    pub fn minimum_fragments_range_with_all_dirties(&self) -> Option<RangeInclusive<usize>> {
        let fragments     = self.fragments.iter().enumerate();
        let dirty_indices = fragments.filter_map(|(i,f)| f.dirty.and_option_from(|| Some(i)));
        let first_dirty   = dirty_indices.clone().min();
        let last_dirty    = dirty_indices.clone().max();
        match (first_dirty,last_dirty) {
            (Some(first),Some(last)) => Some(first..=last),
            _                        => None,
        }
    }

    /// Use builder to build data for fragments.
    pub fn build_buffer_data_for_fragments<Indexes>
    (&mut self, fragments:Indexes, builder:&mut FragmentsDataBuilder, lines:&[String])
    where Indexes : Iterator<Item=usize> {
        for fragment_id in fragments {
            let fragment      = &mut self.fragments[fragment_id];
            let index         = fragment.assigned_line.unwrap_or(0);
            let line          = fragment.assigned_line.map_or("", |i| lines[i].as_str());
            fragment.rendered = builder.build_for_line(index, line);
            fragment.dirty    = false;
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use crate::display::shape::text::buffer::glyph_square::{GlyphAttributeBuilder,GlyphVertexPositionBuilder};

    use basegl_core_msdf_sys::test_utils::TestAfterInit;
    use nalgebra::Point2;
    use std::future::Future;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[test]
    fn fragment_reassignments() {
        let lines_range = 4..=6;

        assert!( make_assigned_fragment(2)     .can_be_reassigned(&lines_range));
        assert!(!make_assigned_fragment(4)     .can_be_reassigned(&lines_range));
        assert!(!make_assigned_fragment(6)     .can_be_reassigned(&lines_range));
        assert!( make_assigned_fragment(7)     .can_be_reassigned(&lines_range));
        assert!( BufferFragment::unassigned().can_be_reassigned(&lines_range));
    }

    #[test]
    fn rendered_fragment_updating() {
        let line     = "AAAĘĘĘ";
        let left_pen = Pen {
            position     : Point2::new(1.0, -1.0),
            current_char : Some('A'),
            next_advance : 0.6,
        };
        let right_pen = Pen {
            position     : Point2::new(12.0, -1.0),
            current_char : Some('Ę'),
            next_advance : 0.8,
        };
        let left_char  = |index,byte_offset| RenderedChar{index,byte_offset,pen:left_pen.clone()};
        let right_char = |index,byte_offset| RenderedChar{index,byte_offset,pen:right_pen.clone()};
        let front      = left_char(0,0);
        let back       = right_char(5,7);
        let some_left  = left_char(2,2);
        let some_right = right_char(4,5);

        let rendered_front  = RenderedFragment{first_char:front, last_char:some_right.clone()};
        let rendered_middle = RenderedFragment{first_char:some_left.clone(), last_char:some_right};
        let rendered_back   = RenderedFragment {first_char:some_left, last_char:back};

        let not_scrolled   = 1.1..=12.7;
        let scrolled_left  = 0.9..=12.0;
        let scrolled_right = 2.0..=13.0;

        assert!(!rendered_middle.should_be_updated(&not_scrolled  ,line));
        assert!( rendered_middle.should_be_updated(&scrolled_left ,line));
        assert!( rendered_middle.should_be_updated(&scrolled_right,line));
        assert!(!rendered_front .should_be_updated(&not_scrolled  ,line));
        assert!(!rendered_front .should_be_updated(&scrolled_left ,line));
        assert!( rendered_front .should_be_updated(&scrolled_right,line));
        assert!(!rendered_back  .should_be_updated(&not_scrolled  ,line));
        assert!( rendered_back  .should_be_updated(&scrolled_left ,line));
        assert!(!rendered_back  .should_be_updated(&scrolled_right,line));
    }

    #[wasm_bindgen_test(async)]
    fn build_data_for_empty_line() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font = FontRenderInfo::mock_font("Test font".to_string());

            let mut builder = FragmentsDataBuilder {
                vertex_position_data : Vec::new(),
                texture_coords_data  : Vec::new(),
                font                 : &mut font,
                line_clip            : 10.0..80.0,
                max_chars_in_fragment: 100
            };

            let result = builder.build_for_line(0, "");

            let expected_data = vec![0.0; 12 * 100];
            assert!(result.is_none());
            assert_eq!(expected_data, builder.vertex_position_data);
            assert_eq!(expected_data, builder.texture_coords_data);
        })
    }

    #[wasm_bindgen_test(async)]
    fn build_data_various_lines() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font       = FontRenderInfo::mock_font("Test font".to_string());
            let mut a_info     = font.mock_char_info('A');
            a_info.advance     = 1.0;
            let mut b_info     = font.mock_char_info('B');
            b_info.advance     = 1.5;
            font.mock_kerning_info('A', 'A', 0.0);
            font.mock_kerning_info('B', 'B', 0.0);
            font.mock_kerning_info('A', 'B', 0.0);
            font.mock_kerning_info('B', 'A', 0.0);
            let shortest_line  = "AB";
            let short_line     = "ABBA";
            let medium_line    = "ABBAAB";
            let long_line      = "ABBAABBABBA";

            let mut builder = FragmentsDataBuilder {
                vertex_position_data : Vec::new(),
                texture_coords_data  : Vec::new(),
                font                 : &mut font,
                line_clip            : 5.5..8.0,
                max_chars_in_fragment: 3
            };
            let shortest_result = builder.build_for_line(1,shortest_line).unwrap();
            let short_result    = builder.build_for_line(2,short_line).unwrap();
            let medium_result   = builder.build_for_line(3,medium_line).unwrap();
            let long_result     = builder.build_for_line(4,long_line).unwrap();

            assert_eq!(0, shortest_result.first_char.byte_offset);
            assert_eq!(1, shortest_result.last_char .byte_offset);
            assert_eq!(1, short_result   .first_char.byte_offset);
            assert_eq!(3, short_result   .last_char .byte_offset);
            assert_eq!(3, medium_result  .first_char.byte_offset);
            assert_eq!(5, medium_result  .last_char .byte_offset);
            assert_eq!(4, long_result    .first_char.byte_offset);
            assert_eq!(6, long_result    .last_char .byte_offset);

            assert_eq!(Point2::new(0.0, -2.0), shortest_result.first_char.pen.position);
            assert_eq!(Point2::new(1.0, -2.0), shortest_result.last_char .pen.position);
            assert_eq!(Point2::new(1.0, -3.0), short_result   .first_char.pen.position);
            assert_eq!(Point2::new(4.0, -3.0), short_result   .last_char .pen.position);
            assert_eq!(Point2::new(4.0, -4.0), medium_result  .first_char.pen.position);
            assert_eq!(Point2::new(6.0, -4.0), medium_result  .last_char .pen.position);
            assert_eq!(Point2::new(5.0, -5.0), long_result    .first_char.pen.position);
            assert_eq!(Point2::new(7.5, -5.0), long_result    .last_char .pen.position);

            let vertex_glyph_data_size    = GlyphVertexPositionBuilder::OUTPUT_SIZE;
            let tex_coord_glyph_data_size = GlyphTextureCoordsBuilder::OUTPUT_SIZE;
            let glyphs_count              = builder.max_chars_in_fragment * 4;
            let vertex_data_size          = vertex_glyph_data_size * glyphs_count;
            let tex_coord_data_size       = tex_coord_glyph_data_size * glyphs_count;
            assert_eq!(vertex_data_size   , builder.vertex_position_data.len());
            assert_eq!([0.0, -2.0], builder.vertex_position_data[0..2]);
            assert_eq!(tex_coord_data_size, builder.texture_coords_data.len());
        })
    }

    #[wasm_bindgen_test(async)]
    fn build_data_with_non_ascii() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font   = FontRenderInfo::mock_font("Test font".to_string());
            let mut a_info = font.mock_char_info('Ą');
            a_info.advance = 1.0;
            let mut b_info = font.mock_char_info('B');
            b_info.advance = 1.5;
            font.mock_kerning_info('Ą', 'B', 0.0);
            let line       = "ĄB";

            let mut builder = FragmentsDataBuilder {
                vertex_position_data : Vec::new(),
                texture_coords_data  : Vec::new(),
                font                 : &mut font,
                line_clip            : 0.0..10.0,
                max_chars_in_fragment: 3
            };
            let result = builder.build_for_line(1,line).unwrap();

            assert_eq!(0, result.first_char.byte_offset);
            assert_eq!(2, result.last_char.byte_offset);
        })
    }

    #[test]
    fn fragments_reassign() {
        let assigned_lines   = 4..=6;
        let new_assignment_1 = 2..=5;
        let new_assignment_2 = 5..=8;
        let mut fragments = BufferFragments {
            assigned_lines  : assigned_lines.clone(),
            fragments       : assigned_lines.map(make_assigned_fragment).collect(),
            next_to_refresh : NextFragmentToRefreshAfterXScrolling::new(4)
        };
        fragments.fragments.push(BufferFragment::unassigned());

        fragments.reassign_fragments(new_assignment_1.clone());
        let expected_assignments_1 = vec![4,5,2,3];
        let assignments_1_iter     = fragments.fragments.iter().map(|f| f.assigned_line.unwrap());
        let assignments_1          = assignments_1_iter.collect::<Vec<usize>>();
        assert_eq!(new_assignment_1      , fragments.assigned_lines);
        assert_eq!(expected_assignments_1, assignments_1);

        fragments.reassign_fragments(new_assignment_2.clone());
        let expected_assignments_2 = vec![6,5,7,8];
        let assignments_2_iter     = fragments.fragments.iter().map(|f| f.assigned_line.unwrap());
        let assignments_2          = assignments_2_iter.collect::<Vec<usize>>();
        assert_eq!(new_assignment_2      , fragments.assigned_lines);
        assert_eq!(expected_assignments_2, assignments_2);
    }

    #[test]
    fn marking_dirty_after_x_scrolling() {
        let make_pen   = |position| Pen{position, current_char:Some('A'), next_advance:0.5};
        let left_pen   = make_pen(Point2::new(3.0, -1.0));
        let middle_pen = make_pen(Point2::new(5.0, -1.0));
        let right_pen  = make_pen(Point2::new(7.0, -1.0));

        let make_char          = |pen| RenderedChar{index:1, byte_offset:1, pen};
        let make_rendered      = |l_pen,r_pen| RenderedFragment{
            first_char : make_char(l_pen),
            last_char  : make_char(r_pen)
        };
        let displayed_range    = 4.0..=6.0;
        let rendered_not_dirty = make_rendered(left_pen  ,right_pen.clone());
        let rendered_dirty     = make_rendered(middle_pen,right_pen);
        let displayed_lines    = 5;
        let lines_iter         = (0..displayed_lines).map(|_| "AA".to_string());
        let lines              = lines_iter.collect::<Vec<String>>();

        let mut fragments      = BufferFragments::new(displayed_lines);
        for (i,fragment) in fragments.fragments.iter_mut().take(4).enumerate() {
            fragment.assigned_line = Some(i);
        }
        fragments.fragments[0].rendered = Some(rendered_dirty);
        fragments.fragments[2].rendered = Some(rendered_not_dirty.clone());
        fragments.fragments[3].rendered = Some(rendered_not_dirty);
        fragments.fragments[3].dirty    = true;

        fragments.mark_dirty_after_x_scrolling(0.0,displayed_range,lines.as_ref());

        let dirties          = fragments.fragments.iter().map(|f| f.dirty).collect::<Vec<bool>>();
        let expected_dirties = vec![true, true, false, true, false];
        assert_eq!(expected_dirties, dirties);
    }

    fn make_assigned_fragment(line:usize) -> BufferFragment {
        BufferFragment {
            assigned_line : Some(line),
            rendered      : None,
            dirty         : true,
        }
    }
}
