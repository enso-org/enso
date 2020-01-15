#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod font;
#[warn(missing_docs)]
pub mod buffer;
#[warn(missing_docs)]
pub mod content;
#[warn(missing_docs)]
pub mod cursor;
#[warn(missing_docs)]
pub mod msdf;
#[warn(missing_docs)]
pub mod program;

use crate::prelude::*;

use crate::display::world::Workspace;
use crate::system::gpu::shader::Context;
use crate::display::shape::text::buffer::TextComponentBuffers;
use crate::display::shape::text::content::TextComponentContent;
use crate::display::shape::text::cursor::Cursors;
use crate::display::shape::text::cursor::Step;
use crate::display::shape::text::cursor::CursorNavigation;
use crate::display::shape::text::font::FontId;
use crate::display::shape::text::font::Fonts;
use crate::display::shape::text::msdf::MsdfTexture;
use crate::display::shape::text::program::MsdfProgram;
use crate::display::shape::text::program::BasicProgram;
use crate::display::shape::text::program::create_content_program;
use crate::display::shape::text::program::create_cursors_program;

use nalgebra::Vector2;
use nalgebra::Similarity2;
use nalgebra::Point2;
use nalgebra::Projective2;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlTexture;



// =================
// === Utilities ===
// =================

#[derive(Debug)]
pub struct Color<T> {
    pub r : T,
    pub g : T,
    pub b : T,
    pub a : T,
}

#[derive(Debug)]
pub struct Area<T> {
    pub left   : T,
    pub right  : T,
    pub top    : T,
    pub bottom : T,
}

impl<T:std::ops::Sub+Clone> Area<T> {
    pub fn width(&self) -> T::Output {
        self.right.clone() - self.left.clone()
    }

    pub fn height(&self) -> T::Output {
        self.top.clone() - self.bottom.clone()
    }
}



// =====================
// === TextComponent ===
// =====================

/// Properties of text component.
#[derive(Debug)]
pub struct TextComponentProperties {
    pub position  : Point2<f64>,
    pub size      : Vector2<f64>,
    pub text_size : f64,
    pub color     : Color<f32>,
}

/// Component rendering text
///
/// This component is under heavy construction, so the api may easily changed in few future
/// commits.
#[derive(Debug)]
pub struct TextComponent {
    pub content       : TextComponentContent,
    pub cursors       : Cursors,
    pub properties    : TextComponentProperties,
    gl_context        : WebGl2RenderingContext,
    content_program   : MsdfProgram,
    cursors_program   : BasicProgram,
    gl_msdf_texture   : WebGlTexture,
    msdf_texture_rows : usize,
    buffers           : TextComponentBuffers,
}

impl TextComponent {
    /// Scroll text by given offset.
    ///
    /// The value of 1.0 on both dimensions is equal to one line's height.
    pub fn scroll(&mut self, offset:Vector2<f64>) {
        self.buffers.scroll(offset);
    }

    /// Get current scroll position.
    ///
    /// The _scroll_position_ is a position of top-left corner of the first line.
    /// The offset of 1.0 on both dimensions is equal to one line's height.
    pub fn scroll_position(&self) -> &Vector2<f64> {
        &self.buffers.scroll_offset
    }

    /// Jump to scroll position.
    ///
    /// The `scroll_position` is a position of top-left corner of the first line.
    /// The offset of 1.0 on both dimensions is equal to one line's height.
    pub fn jump_to_position(&mut self, scroll_position:Vector2<f64>) {
        self.buffers.jump_to(scroll_position);
    }

    /// Render text component.
    pub fn display(&mut self, fonts:&mut Fonts) {
        self.refresh_content_buffers(fonts);
        self.refresh_msdf_texture(fonts);
        self.refresh_cursors(fonts);
        self.display_content(fonts);
        if !self.cursors.cursors.is_empty() {
            self.display_cursors();
        }
    }

    pub fn navigate_cursors(&mut self, step:Step, selecting:bool, fonts:&mut Fonts) {
        let content        = &mut self.content;
        let mut navigation = CursorNavigation {content,fonts,selecting};
        self.cursors.navigate_all_cursors(&mut navigation,&step);
    }

    fn refresh_content_buffers(&mut self, fonts:&mut Fonts) {
        let refresh_info = self.content.refresh_info(fonts);
        self.buffers.refresh(&self.gl_context,refresh_info);
    }

    fn refresh_msdf_texture(&mut self, fonts:&mut Fonts) {
        let font_msdf            = &fonts.get_render_info(self.content.font).msdf_texture;
        let current_msdf_rows    = font_msdf.rows();
        let msdf_texture_changed = self.msdf_texture_rows != current_msdf_rows;
        if msdf_texture_changed {
            let gl_ctx       = &self.gl_context;
            let target       = Context::TEXTURE_2D;
            let width        = MsdfTexture::WIDTH as i32;
            let height       = font_msdf.rows() as i32;
            let border       = 0;
            let tex_level    = 0;
            let format       = Context::RGB;
            let internal_fmt = Context::RGB as i32;
            let tex_type     = Context::UNSIGNED_BYTE;
            let data         = Some(font_msdf.data.as_slice());

            gl_ctx.bind_texture(target,Some(&self.gl_msdf_texture));
            let tex_image_result =
                gl_ctx.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array
                (target,tex_level,internal_fmt,width,height,border,format,tex_type,data);
            tex_image_result.unwrap();
            self.msdf_texture_rows = font_msdf.rows();
        }
    }

    fn refresh_cursors(&mut self, fonts:&mut Fonts) {
        if self.cursors.dirty {
            let gl_context = &self.gl_context;
            let content    = &mut self.content;
            self.cursors.update_buffer_data(gl_context,content,fonts);
        }
    }

    fn to_scene_matrix(&self) -> SmallVec<[f32;9]> {
        const ROTATION : f64            = 0.0;
        let position                    = &self.properties.position;
        let size                        = self.properties.size;
        let text_size                   = self.properties.text_size;
        let scroll_offset               = self.buffers.scroll_offset * text_size;
        let to_position                 = position.coords + Vector2::new(0.0, size.y);
        let translate                   = to_position - scroll_offset;
        let scale                       = text_size;
        let similarity                  = Similarity2::new(translate,ROTATION,scale);
        let to_scene : Projective2<f64> = nalgebra::convert(similarity);
        let matrix                      = to_scene.matrix();
        let view : &[[f64;3]]           = matrix.as_ref();
        let flatten_view                = view.iter().flatten();
        let converted                   = flatten_view.map(|f| *f as f32);
        converted.collect()
    }

    fn display_content(&self, fonts:&mut Fonts) {
        let gl_context       = &self.gl_context;
        let font             = fonts.get_render_info(self.content.font);
        let vertices_count   = self.buffers.vertices_count() as i32;
        let to_scene_matrix  = self.to_scene_matrix();
        let program          = &self.content_program;

        gl_context.use_program(Some(&program.gl_program));
        program.set_to_scene_transformation(&to_scene_matrix);
        program.set_msdf_size(font);
        program.bind_buffer_to_attribute("position",&self.buffers.vertex_position);
        program.bind_buffer_to_attribute("tex_coord",&self.buffers.texture_coords);
        self.setup_blending();
        gl_context.bind_texture(Context::TEXTURE_2D,Some(&self.gl_msdf_texture));
        gl_context.draw_arrays(WebGl2RenderingContext::TRIANGLES,0,vertices_count);
    }

    fn display_cursors(&self) {
        let buffer           = self.cursors.buffer.as_ref().unwrap();
        let gl_context       = &self.gl_context;
        let to_scene_matrix  = self.to_scene_matrix();
        let program          = &self.cursors_program;
        let vertices_count   = self.cursors.vertices_count() as i32;

        gl_context.use_program(Some(&program.gl_program));
        self.cursors_program.set_to_scene_transformation(&to_scene_matrix);
        self.cursors_program.bind_buffer_to_attribute("position",buffer);
        gl_context.line_width(2.0);
        gl_context.draw_arrays(WebGl2RenderingContext::LINES,0,vertices_count);
        gl_context.line_width(1.0);
    }

    fn setup_blending(&self) {
        let gl_context        = &self.gl_context;
        let rgb_source        = Context::SRC_ALPHA;
        let alpha_source      = Context::ZERO;
        let rgb_destination   = Context::ONE_MINUS_SRC_ALPHA;
        let alhpa_destination = Context::ONE;

        gl_context.enable(Context::BLEND);
        gl_context.blend_func_separate(rgb_source,rgb_destination,alpha_source,alhpa_destination);
    }
}


// ============================
// === TextComponentBuilder ===
// ============================

/// Text component builder
pub struct TextComponentBuilder<'a, 'b, Str:AsRef<str>> {
    pub workspace       : &'a Workspace,
    pub fonts           : &'b mut Fonts,
    pub text            : Str,
    pub font_id         : FontId,
    pub properties      : TextComponentProperties,
}

impl<'a,'b,Str:AsRef<str>> TextComponentBuilder<'a,'b,Str> {

    /// Build a new text component rendering on given workspace
    pub fn build(mut self) -> TextComponent {
        self.load_all_chars();
        let gl_context      = self.workspace.context.clone();
        let content_program = create_content_program(&gl_context);
        let cursors_program = create_cursors_program(&gl_context);
        let gl_msdf_texture = self.create_msdf_texture(&gl_context);
        let display_size    = self.properties.size / self.properties.text_size;
        let mut content     = TextComponentContent::new(self.font_id,self.text.as_ref());
        let initial_refresh = content.refresh_info(self.fonts);
        let buffers         = TextComponentBuffers::new(&gl_context,display_size,initial_refresh);
        let cursors         = Cursors::new(&gl_context);
        content_program.set_constant_uniforms(&self.properties);
        cursors_program.set_constant_uniforms(&self.properties);
        TextComponent
        {content,cursors,gl_context,content_program,cursors_program,gl_msdf_texture,buffers,
            properties        : self.properties,
            msdf_texture_rows : 0,
        }
    }

    fn load_all_chars(&mut self) {
        for ch in self.text.as_ref().chars() {
            self.fonts.get_render_info(self.font_id).get_glyph_info(ch);
        }
    }

    fn create_msdf_texture(&mut self, gl_ctx:&Context)
    -> WebGlTexture {
        let msdf_texture = gl_ctx.create_texture().unwrap();
        let target       = Context::TEXTURE_2D;
        let wrap         = Context::CLAMP_TO_EDGE as i32;
        let min_filter   = Context::LINEAR as i32;

        gl_ctx.bind_texture(target,Some(&msdf_texture));
        gl_ctx.tex_parameteri(target,Context::TEXTURE_WRAP_S,wrap);
        gl_ctx.tex_parameteri(target,Context::TEXTURE_WRAP_T,wrap);
        gl_ctx.tex_parameteri(target,Context::TEXTURE_MIN_FILTER,min_filter);
        msdf_texture
    }
}
