pub mod glyph_square;
pub mod line;

use crate::prelude::*;

use crate::text::buffer::glyph_square::
{GlyphVertexPositionBuilder, GlyphTextureCoordsBuilder, GlyphAttributeBuilder, BASE_LAYOUT_SIZE};
use crate::text::buffer::line::LineAttributeBuilder;
use crate::text::Line;
use crate::text::font::FontRenderInfo;

use basegl_backend_webgl::Context;
use js_sys::Float32Array;
use nalgebra::Transform2;
use web_sys::WebGlBuffer;


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
    pub texture_coords: WebGlBuffer,
    fragments               : Vec<BufferFragment>,
    pub displayed_lines     : usize,
    pub displayed_columns   : usize,
}

/// A buffer fragment which may be assigned to some line.
#[derive(Debug)]
struct BufferFragment {
    assigned_line              : Option<usize>
    // TODO [AO] maybe some new fields appear in future. if not, remove this and use Option<usize>
    // directly
}

impl TextComponentBuffers {
    /// Create and initialize buffers.
    pub fn new(gl_context:&Context, displayed_lines:usize, displayed_columns:usize)
    -> TextComponentBuffers {
        TextComponentBuffers {
            vertex_position   : gl_context.create_buffer().unwrap(),
            texture_coords    : gl_context.create_buffer().unwrap(),
            fragments         : Self::build_fragments(displayed_lines),
            displayed_lines,
            displayed_columns,
        }
    }

    fn build_fragments(displayed_lines:usize) -> Vec<BufferFragment> {
        let indexes   = 0..displayed_lines;
        let fragments = indexes.map(|_| BufferFragment { assigned_line : None });
        fragments.collect()
    }

    /// Refresh the whole buffers making them display the given lines.
    pub fn refresh_all<'a>
    ( &mut self
    , gl_context:&Context
    , lines:&[Line]
    , font:&'a mut FontRenderInfo
    , to_window:&Transform2<f64>
    ) {
        self.assign_fragments(lines.len());

        let content_from_index = |index : Option<usize>| index.map_or("", |i| lines[i].content.as_str());

        let assigned_lines     = self.fragments.iter().map(|fragment| fragment.assigned_line);
        let content            = assigned_lines.clone().map(content_from_index);
        let line_indexes       = assigned_lines.map(|index| index.unwrap_or(0));
        let content_with_index = content.clone().zip(line_indexes);

        self.refresh_vertex_position_buffer(gl_context,content_with_index,font,to_window);
        self.refresh_texture_coords_buffer (gl_context,content           ,font);
    }

    fn refresh_vertex_position_buffer<'a, Iter>
    ( &self
    , gl_context:&Context
    , content_with_index:Iter
    , font:&mut FontRenderInfo
    , to_window:&Transform2<f64>
    ) where Iter : ExactSizeIterator<Item=(&'a str,usize)> {
        let one_glyph_data_size = GlyphVertexPositionBuilder::OUTPUT_SIZE;
        let mut data            = Vec::new();
        let lines_count         = content_with_index.len();
        let line_length         = self.displayed_columns;
        let data_size           = one_glyph_data_size * line_length * lines_count;

        data.reserve(data_size);
        for (content, index) in content_with_index {
            let glyph_buider = GlyphVertexPositionBuilder::new(font,*to_window,index);
            let builder      = LineAttributeBuilder::new(content,glyph_buider,line_length);
            data.extend(builder.flatten().map(|f| f as f32));
        }

        self.set_buffer_data(gl_context,&self.vertex_position,data.as_ref());
    }

    fn refresh_texture_coords_buffer<'a, Iter>
    (&self, gl_context:&Context, content:Iter, font:&mut FontRenderInfo)
    where Iter : ExactSizeIterator<Item=&'a str>
    {
        let one_glyph_data_size = GlyphTextureCoordsBuilder::OUTPUT_SIZE;
        let mut data            = Vec::new();
        let lines_count         = content.len();
        let line_length         = self.displayed_columns;
        let data_size           = one_glyph_data_size * line_length * lines_count;

        data.reserve(data_size);
        for line in content {
            let glyph_buider = GlyphTextureCoordsBuilder::new(font);
            let builder      = LineAttributeBuilder::new(line,glyph_buider,line_length);
            data.extend(builder.flatten().map(|f| f as f32));
        }

        self.set_buffer_data(gl_context,&self.texture_coords,data.as_ref());
    }

    fn assign_fragments(&mut self, lines_count:usize) {
        for (i, fragment) in self.fragments.iter_mut().enumerate() {
            fragment.assigned_line = (i < lines_count).and_option(Some(i))
        }
    }

    fn set_buffer_data(&self, gl_context:&Context, buffer:&WebGlBuffer, vertices:&[f32]) {
        let target     = Context::ARRAY_BUFFER;

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
        BASE_LAYOUT_SIZE * self.displayed_lines * self.displayed_columns
    }
}
