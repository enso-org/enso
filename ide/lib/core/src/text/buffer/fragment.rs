use crate::prelude::*;

use crate::text::buffer::glyph_square::{Pen,GlyphVertexPositionBuilder,GlyphTextureCoordsBuilder};
use crate::text::buffer::line::LineAttributeBuilder;
use crate::text::font::FontRenderInfo;

use nalgebra::geometry::Point2;
use std::ops::Range;

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
    pub assigned_line     : Option<usize>,
    pub rendered_fragment : Option<RenderedFragment>
}

/// Information what is currently rendered on screen for some specific _buffer fragment_.
#[derive(Debug)]
pub struct RenderedFragment {
    pub first_char : RenderedChar,
    pub last_char  : RenderedChar,
}

/// The rendered char position in line and on screen.
#[derive(Debug)]
pub struct RenderedChar {
    pub byte_offset : usize,
    pub pen         : Pen
}

impl BufferFragment {
    /// Creates new buffer fragment which is not assigned to any line.
    pub fn unassigned() -> BufferFragment {
        BufferFragment {
            assigned_line     : None,
            rendered_fragment : None
        }
    }
}

// ===========================
// === FragmentDataBuilder ===
// ===========================

/// Builder of buffer data of some consecutive buffer fragments
///
/// The result is stored in `vertex_position_data` and `texture_coords_data` fields.
pub struct FragmentsDataBuilder<'a> {
    pub vertex_position_data : Vec<f32>,
    pub texture_coords_data  : Vec<f32>,
    pub font                 : &'a mut FontRenderInfo,
    pub line_clip            : Range<f64>,
    pub max_displayed_chars  : usize,
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
        let always_render_index = line_length.saturating_sub(self.max_displayed_chars);
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
            rendered.and_option_from(|| Some(RenderedChar{byte_offset,pen}))
        })
    }

    /// Get information about last char which data will be actually stored in buffer.
    pub fn last_rendered_char(&mut self, first_char:&RenderedChar, rendered_text:&str)
    -> Option<RenderedChar> {
        let mut pen               = first_char.pen.clone();
        let rendered_chars_iter   = rendered_text.char_indices().take(self.max_displayed_chars);
        let (last_char_offset, _) = rendered_chars_iter.clone().last()?;
        let byte_offset = first_char.byte_offset + last_char_offset;
        for (_, ch) in rendered_chars_iter.skip(1) {
            pen.next_char(ch, &mut self.font);
        }
        Some(RenderedChar { byte_offset, pen })
    }

    /// Extend vertex position data with a new line's.
    pub fn build_vertex_positions(&mut self, pen:&Pen, text:&str) {
        let rendering_pen = Pen::new(pen.position);
        let glyph_builder = GlyphVertexPositionBuilder::new(self.font,rendering_pen);
        let builder       = LineAttributeBuilder::new(text,glyph_builder,self.max_displayed_chars);
        self.vertex_position_data.extend(builder.flatten().map(|f| f as f32));
    }

    /// Extend texture coordinates data with a new line's.
    pub fn build_texture_coords(&mut self, text:&str) {
        let glyph_builder = GlyphTextureCoordsBuilder::new(self.font);
        let builder       = LineAttributeBuilder::new(text,glyph_builder,self.max_displayed_chars);
        self.texture_coords_data.extend(builder.flatten().map(|f| f as f32));
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    use crate::text::buffer::glyph_square::{GlyphAttributeBuilder,GlyphVertexPositionBuilder};

    use basegl_core_msdf_sys::test_utils::TestAfterInit;
    use nalgebra::Point2;
    use std::future::Future;
    use wasm_bindgen_test::wasm_bindgen_test;


    #[wasm_bindgen_test(async)]
    fn build_data_for_empty_line() -> impl Future<Output=()> {
        TestAfterInit::schedule(|| {
            let mut font = FontRenderInfo::mock_font("Test font".to_string());

            let mut builder = FragmentsDataBuilder {
                vertex_position_data : Vec::new(),
                texture_coords_data  : Vec::new(),
                font                 : &mut font,
                line_clip            : 10.0..80.0,
                max_displayed_chars  : 100
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
                max_displayed_chars  : 3
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
            let glyphs_count              = builder.max_displayed_chars * 4;
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
                max_displayed_chars  : 3
            };
            let result = builder.build_for_line(1,line).unwrap();

            assert_eq!(0, result.first_char.byte_offset);
            assert_eq!(2, result.last_char.byte_offset);
        })
    }
}
