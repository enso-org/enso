pub mod fragment;
pub mod glyph_square;
pub mod line;

use crate::prelude::*;

use crate::text::buffer::glyph_square::BASE_LAYOUT_SIZE;
use crate::text::buffer::fragment::{BufferFragment,FragmentsDataBuilder};
use crate::text::Line;
use crate::text::font::FontRenderInfo;

use basegl_backend_webgl::Context;
use js_sys::Float32Array;
use web_sys::WebGlBuffer;
use std::ops::RangeInclusive;
use crate::Area;


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
    fragments               : Vec<BufferFragment>,
    pub clip                : Area<f64>,
    pub displayed_lines     : usize,
    pub max_displayed_chars : usize,
}

impl TextComponentBuffers {
    /// Create and initialize buffers.
    pub fn new(gl_context:&Context, clip:Area<f64>, font:&mut FontRenderInfo)
    -> TextComponentBuffers {
        let displayed_lines     = clip.height().ceil() as usize;
        let space_width         = font.get_glyph_info(' ').advance;
        let max_displayed_chars = (clip.width().ceil() / space_width) as usize;
        TextComponentBuffers {
            vertex_position   : gl_context.create_buffer().unwrap(),
            texture_coords    : gl_context.create_buffer().unwrap(),
            fragments         : Self::build_fragments(displayed_lines),
            clip,
            displayed_lines,
            max_displayed_chars
        }
    }

    fn build_fragments(displayed_lines:usize) -> Vec<BufferFragment> {
        let indexes   = 0..displayed_lines;
        let fragments = indexes.map(|_| BufferFragment::unassigned());
        fragments.collect()
    }

    /// Refresh the whole buffers making them display the given lines.
    pub fn refresh_all<'a>
    (&mut self, gl_context:&Context, lines:&[Line], font:&'a mut FontRenderInfo) {
        let top_line_clipped     = Self::line_at_y_position(self.clip.top,lines.len());
        let bottom_line_clipped  = Self::line_at_y_position(self.clip.bottom,lines.len());
        let first_line_index     = top_line_clipped.unwrap_or(0);
        let last_line_index      = bottom_line_clipped.unwrap_or(lines.len()-1);
        let mut builder          = self.create_fragments_data_builder(font);

        self.assign_fragments(first_line_index..=last_line_index);
        for fragment in &mut self.fragments {
            let index   = fragment.assigned_line.unwrap_or(0);
            let content = fragment.assigned_line.map_or("", |i| lines[i].content.as_str());
            builder.build_for_line(index,content);
        }
        let vertex_position_data = builder.vertex_position_data;
        let texture_coords_data  = builder.texture_coords_data;
        self.set_buffer_data(gl_context,&self.vertex_position,vertex_position_data.as_ref());
        self.set_buffer_data(gl_context,&self.texture_coords ,texture_coords_data .as_ref());
    }

    fn line_at_y_position(y:f64, lines_count:usize) -> Option<usize> {
        let index    = -y.floor();
        let is_valid = index >= 0.0 && index < lines_count as f64;
        is_valid.and_option_from(|| Some(index as usize))
    }

    fn assign_fragments(&mut self, displayed_lines:RangeInclusive<usize>) {
        for (i, fragment) in self.fragments.iter_mut().enumerate() {
            let assigned_index     = displayed_lines.start() + i;
            let is_line_to_assign  = assigned_index <= *displayed_lines.end();
            fragment.assigned_line = is_line_to_assign.and_option(Some(i))
        }
    }

    fn create_fragments_data_builder<'a>(&self, font:&'a mut FontRenderInfo)
    -> FragmentsDataBuilder<'a> {
        FragmentsDataBuilder {
            vertex_position_data : Vec::new(),
            texture_coords_data  : Vec::new(),
            font,
            line_clip            : self.clip.left..self.clip.right,
            max_displayed_chars  : self.max_displayed_chars
        }
    }

    fn set_buffer_data(&self, gl_context:&Context, buffer:&WebGlBuffer, vertices:&[f32]) {
        let target = Context::ARRAY_BUFFER;
        gl_context.bind_buffer(target,Some(&buffer));
        Self::set_bound_buffer_data(gl_context,target,vertices);
    }

    fn set_bound_buffer_data(gl_context:&Context, target:u32, data:&[f32]) {
        let usage      = Context::STATIC_DRAW;
        unsafe { // Note [unsafe buffer_data]
            let float_32_array = Float32Array::view(&data);
            gl_context.buffer_data_with_array_buffer_view(target,&float_32_array,usage);
        }
    }

    /* Note [unsafe buffer_data]
     *
     * The Float32Array::view is safe as long there are no allocations done
     * until it is destroyed. This way of creating buffers were taken from
     * wasm-bindgen examples
     * (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html)
     */

    pub fn vertices_count(&self) -> usize {
        BASE_LAYOUT_SIZE * self.displayed_lines * self.max_displayed_chars
    }
}
