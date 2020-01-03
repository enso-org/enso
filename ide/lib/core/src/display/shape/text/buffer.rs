pub mod fragment;
pub mod glyph_square;
pub mod line;

use crate::prelude::*;

use crate::display::render::webgl::Context;
use crate::display::render::webgl::set_buffer_data;
use crate::display::render::webgl::set_buffer_subdata;
use crate::display::shape::text::buffer::glyph_square::BASE_LAYOUT_SIZE;
use crate::display::shape::text::buffer::glyph_square::GlyphAttributeBuilder;
use crate::display::shape::text::buffer::glyph_square::GlyphVertexPositionBuilder;
use crate::display::shape::text::buffer::glyph_square::GlyphTextureCoordsBuilder;
use crate::display::shape::text::buffer::fragment::BufferFragments;
use crate::display::shape::text::buffer::fragment::FragmentsDataBuilder;
use crate::display::shape::text::content::RefreshInfo;
use crate::display::shape::text::font::FontRenderInfo;

use nalgebra::Vector2;
use web_sys::WebGlBuffer;
use std::ops::RangeInclusive;



// =============================
// === TextComponentsBuffers ===
// =============================

/// A structure managing all WebGl buffers used by TextComponent
///
/// Each attribute buffer is split to equal-sized fragments, and each fragment may is assigned to
/// displayed line The fragment keeps the data for this line.
#[derive(Debug)]
pub struct TextComponentBuffers {
    pub vertex_position     : WebGlBuffer,
    pub texture_coords      : WebGlBuffer,
    pub display_size        : Vector2<f64>,
    pub scroll_offset       : Vector2<f64>,
    pub fragments           : BufferFragments,
    max_chars_in_fragment   : usize,
    scroll_since_last_frame : Vector2<f64>
}

impl TextComponentBuffers {
    /// Create and initialize buffers.
    pub fn new(gl_context:&Context, display_size:Vector2<f64>, refresh:RefreshInfo)
    -> TextComponentBuffers {
        let mut ref_mut = refresh;
        let mut buffers     = Self::create_uninitialized(gl_context,display_size,&mut ref_mut);
        buffers.setup_buffers(gl_context,ref_mut);
        buffers
    }

    /// Change scrolling by given offset and marks appropriate dirties.
    ///
    /// The value of 1.0 on both dimensions is equal to one line's height.
    pub fn scroll(&mut self, offset:Vector2<f64>) {
        self.scroll_since_last_frame += offset;
        self.scroll_offset           += offset;
    }

    /// Jump to scroll position.
    ///
    /// The `scroll_position` is a position of top-left corner of the first line.
    /// The offset of 1.0 on both dimensions is equal to one line's height.
    pub fn jump_to(&mut self, scroll_position:Vector2<f64>) {
        self.scroll(scroll_position - self.scroll_offset)
    }

    /// Refresh the whole buffers data.
    pub fn refresh(&mut self, gl_context:&Context, info:RefreshInfo) {
        let scrolled_x = self.scroll_since_last_frame.x != 0.0;
        let scrolled_y = self.scroll_since_last_frame.y != 0.0;
        if scrolled_y || info.dirty_lines.range.is_some() {
            let displayed_lines = self.displayed_lines(info.lines.len());
            self.fragments.reassign_fragments(displayed_lines);
        }
        if scrolled_x {
            let displayed_x = self.displayed_x_range();
            let x_scroll    = self.scroll_since_last_frame.x;
            let lines       = &info.lines;
            self.fragments.mark_dirty_after_x_scrolling(x_scroll,displayed_x,lines);
        }
        if scrolled_x || scrolled_y || info.dirty_lines.any_dirty() {
            self.fragments.mark_lines_dirty(&info.dirty_lines);
            let opt_dirty_range = self.fragments.minimum_fragments_range_with_all_dirties();
            if let Some(dirty_range) = opt_dirty_range {
                self.refresh_fragments(gl_context,dirty_range,info); // Note[refreshing buffers]
            }
            self.scroll_since_last_frame = Vector2::new(0.0,0.0);
        }
    }

    /* Note[refreshing buffer]
     *
     * The data exchange with GPU have so big overhead, that we usually replace buffer data with
     * one operation. That's why we gathering range with all dirties possibly catching many
     * not-dirty fragments.
     */

    fn create_uninitialized
    (gl_context:&Context, display_size:Vector2<f64>, refresh:&mut RefreshInfo)
    -> TextComponentBuffers {
        // Display_size.(x/y).floor() makes space for all lines/glyphs that fit in space in
        // their full size. But we have 2 more lines/glyphs: one clipped from top or left, and one
        // from bottom or right.
        const ADDITIONAL: usize   = 2;
        let displayed_lines       = display_size.y.floor() as usize + ADDITIONAL;
        let space_width           = refresh.font.get_glyph_info(' ').advance;
        let displayed_chars       = (display_size.x/space_width).floor();
        // This margin is to ensure, that after x scrolling we won't need to refresh all the lines
        // at once.
        let x_margin              = (displayed_lines as f64) / space_width;
        let max_chars_in_fragment = (displayed_chars + 2.0*x_margin).floor() as usize + ADDITIONAL;
        TextComponentBuffers {display_size,max_chars_in_fragment,
            vertex_position         : gl_context.create_buffer().unwrap(),
            texture_coords          : gl_context.create_buffer().unwrap(),
            fragments               : BufferFragments::new(displayed_lines),
            scroll_offset           : Vector2::new(0.0,0.0),
            scroll_since_last_frame : Vector2::new(0.0,0.0)
        }
    }

    fn displayed_x_range(&self) -> RangeInclusive<f64> {
        let begin = self.scroll_offset.x;
        let end   = begin + self.display_size.x;
        begin..=end
    }

    fn displayed_lines(&self, lines_count:usize) -> RangeInclusive<usize> {
        let top                      = self.scroll_offset.y;
        let bottom                   = self.scroll_offset.y - self.display_size.y;
        let top_line_clipped         = Self::line_at_y_position(top,lines_count);
        let bottom_line_clipped      = Self::line_at_y_position(bottom,lines_count);
        let first_line_index         = top_line_clipped.unwrap_or(0);
        let last_line_index          = bottom_line_clipped.unwrap_or(lines_count-1);
        first_line_index..=last_line_index
    }

    fn line_at_y_position(y:f64, lines_count:usize) -> Option<usize> {
        let index    = -y.ceil();
        let is_valid = index >= 0.0 && index < lines_count as f64;
        is_valid.and_option_from(|| Some(index as usize))
    }

    fn setup_buffers(&mut self, gl_context:&Context, refresh:RefreshInfo) {
        let displayed_lines = self.displayed_lines(refresh.lines.len());
        let lines           = refresh.lines;
        let all_fragments   = 0..self.fragments.fragments.len();
        let mut builder     = self.create_fragments_data_builder(refresh.font);

        self.fragments.reassign_fragments(displayed_lines);
        self.fragments.build_buffer_data_for_fragments(all_fragments,&mut builder,lines);
        let vertex_position_data = builder.vertex_position_data.as_ref();
        let texture_coords_data  = builder.texture_coords_data.as_ref();
        set_buffer_data(gl_context,&self.vertex_position, vertex_position_data);
        set_buffer_data(gl_context,&self.texture_coords , texture_coords_data);
    }

    fn refresh_fragments
    (&mut self, gl_context:&Context, indexes:RangeInclusive<usize>, mut refresh:RefreshInfo) {
        let offset      = *indexes.start();
        let mut builder = self.create_fragments_data_builder(refresh.font);

        self.fragments.build_buffer_data_for_fragments(indexes,&mut builder,&mut refresh.lines);
        self.set_vertex_position_buffer_subdata(gl_context,offset,&builder);
        self.set_texture_coords_buffer_subdata (gl_context,offset,&builder);
    }

    fn create_fragments_data_builder<'a>(&self, font:&'a mut FontRenderInfo)
    -> FragmentsDataBuilder<'a> {
        let line_clip_left  = self.scroll_offset.x;
        let line_clip_right = line_clip_left + self.display_size.x;
        FragmentsDataBuilder {font,
            vertex_position_data  : Vec::new(),
            texture_coords_data   : Vec::new(),
            line_clip             : line_clip_left..line_clip_right,
            max_chars_in_fragment : self.max_chars_in_fragment
        }
    }

    const GL_FLOAT_SIZE : usize = 4;

    fn set_vertex_position_buffer_subdata
    (&self, gl_context:&Context, fragment_offset:usize, builder:&FragmentsDataBuilder) {
        let char_output_floats = GlyphVertexPositionBuilder::OUTPUT_SIZE;
        let line_output_floats = char_output_floats * self.max_chars_in_fragment;
        let fragment_size      = line_output_floats * Self::GL_FLOAT_SIZE;
        let offset             = fragment_size * fragment_offset;
        let data               = builder.vertex_position_data.as_ref();
        set_buffer_subdata(gl_context,&self.vertex_position,offset,data);
    }

    fn set_texture_coords_buffer_subdata
    (&self, gl_context:&Context, fragment_offset:usize, builder:&FragmentsDataBuilder) {
        let char_output_floats = GlyphTextureCoordsBuilder::OUTPUT_SIZE;
        let line_output_floats = char_output_floats * self.max_chars_in_fragment;
        let fragment_size      = line_output_floats * Self::GL_FLOAT_SIZE;
        let offset        = fragment_size * fragment_offset;
        let data          = builder.texture_coords_data.as_ref();
        set_buffer_subdata(gl_context,&self.texture_coords,offset,data);
    }

    pub fn vertices_count(&self) -> usize {
        BASE_LAYOUT_SIZE * self.fragments.fragments.len() * self.max_chars_in_fragment
    }
}
