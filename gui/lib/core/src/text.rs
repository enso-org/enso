pub mod font;
pub mod buffer;
pub mod content;
pub mod msdf;

use crate::prelude::*;

use crate::Color;
use crate::display::world::Workspace;
use crate::text::buffer::TextComponentBuffers;
use crate::text::content::TextComponentContent;
use crate::text::font::FontId;
use crate::text::font::FontRenderInfo;
use crate::text::font::Fonts;
use crate::text::msdf::MsdfTexture;

use basegl_backend_webgl::Context;
use basegl_backend_webgl::compile_shader;
use basegl_backend_webgl::link_program;
use basegl_backend_webgl::Program;
use basegl_backend_webgl::Shader;
use nalgebra::Vector2;
use nalgebra::Similarity2;
use nalgebra::Point2;
use nalgebra::Projective2;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlBuffer;
use web_sys::WebGlTexture;


// =====================
// === TextComponent ===
// =====================

/// Component rendering text
///
/// This component is under heavy construction, so the api may easily changed in few future
/// commits
#[derive(Debug)]
pub struct TextComponent {
    pub content       : TextComponentContent,
    pub position      : Point2<f64>,
    pub size          : Vector2<f64>,
    pub text_size     : f64,
    gl_context        : WebGl2RenderingContext,
    gl_program        : Program,
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


    /// Render text
    pub fn display(&mut self, fonts:&mut Fonts) {
        self.buffers.refresh(&self.gl_context,self.content.refresh_info(fonts));

        if self.msdf_texture_rows != fonts.get_render_info(self.content.font).msdf_texture.rows() {
            self.update_msdf_texture(fonts);
        }

        let gl_context     = &self.gl_context;
        let vertices_count = self.buffers.vertices_count() as i32;

        gl_context.use_program(Some(&self.gl_program));
        self.update_uniforms(fonts);
        self.bind_buffer_to_attribute("position",&self.buffers.vertex_position);
        self.bind_buffer_to_attribute("tex_coord",&self.buffers.texture_coords);
        self.setup_blending();
        gl_context.bind_texture(Context::TEXTURE_2D, Some(&self.gl_msdf_texture));
        gl_context.draw_arrays(WebGl2RenderingContext::TRIANGLES,0,vertices_count);
    }

    fn update_uniforms(&self, fonts:&mut Fonts) {
        let gl_context    = &self.gl_context;
        let to_scene      = self.to_scene_matrix();
        let to_scene_ref  = to_scene.as_ref();
        let msdf_width    = MsdfTexture::WIDTH as f32;
        let msdf_height   = fonts.get_render_info(self.content.font).msdf_texture.rows() as f32;
        let to_scene_loc  = gl_context.get_uniform_location(&self.gl_program,"to_scene");
        let msdf_size_loc = gl_context.get_uniform_location(&self.gl_program,"msdf_size");
        let transpose     = false;
        gl_context.uniform_matrix3fv_with_f32_array(to_scene_loc.as_ref(),transpose,to_scene_ref);
        gl_context.uniform2f(msdf_size_loc.as_ref(),msdf_width,msdf_height);

    }

    fn update_msdf_texture(&mut self, fonts:&mut Fonts) {
        let gl_context   = &self.gl_context;
        let font_msdf    = &fonts.get_render_info(self.content.font).msdf_texture;
        let target       = Context::TEXTURE_2D;
        let width        = MsdfTexture::WIDTH as i32;
        let height       = font_msdf.rows() as i32;
        let border       = 0;
        let tex_level    = 0;
        let format       = Context::RGB;
        let internal_fmt = Context::RGB as i32;
        let tex_type     = Context::UNSIGNED_BYTE;
        let data         = Some(font_msdf.data.as_slice());

        gl_context.bind_texture(target,Some(&self.gl_msdf_texture));
        let tex_image_result =
            gl_context.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array
            (target,tex_level,internal_fmt,width,height,border,format,tex_type,data);
        tex_image_result.unwrap();
        self.msdf_texture_rows = font_msdf.rows();
    }

    fn to_scene_matrix(&self) -> SmallVec<[f32;9]> {
        const ROTATION : f64            = 0.0;
        let scroll_offset               = self.buffers.scroll_offset*self.text_size;
        let to_position                 = self.position.coords + Vector2::new(0.0, self.size.y);
        let translate                   = to_position - scroll_offset;
        let scale                       = self.text_size;
        let similarity                  = Similarity2::new(translate,ROTATION,scale);
        let to_scene : Projective2<f64> = nalgebra::convert(similarity);
        let matrix                      = to_scene.matrix();
        let view : &[[f64;3]]           = matrix.as_ref();
        let flatten_view                = view.iter().flatten();
        let converted                   = flatten_view.map(|f| *f as f32);
        converted.collect()
    }

    fn bind_buffer_to_attribute(&self, attribute_name:&str, buffer:&WebGlBuffer) {
        let gl_context = &self.gl_context;
        let gl_program = &self.gl_program;
        let location   = gl_context.get_attrib_location(gl_program,attribute_name) as u32;
        let target     = WebGl2RenderingContext::ARRAY_BUFFER;
        let item_size  = 2;
        let item_type  = WebGl2RenderingContext::FLOAT;
        let normalized = false;
        let stride     = 0;
        let offset     = 0;

        gl_context.enable_vertex_attrib_array(location);
        gl_context.bind_buffer(target,Some(buffer));
        gl_context.vertex_attrib_pointer_with_i32
        ( location
        , item_size
        , item_type
        , normalized
        , stride
        , offset
        );
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
    pub position        : Point2<f64>,
    pub size            : Vector2<f64>,
    pub text_size       : f64,
    pub color           : Color<f32>,
}

impl<'a,'b,Str:AsRef<str>> TextComponentBuilder<'a,'b,Str> {

    /// Build a new text component rendering on given workspace
    pub fn build(mut self) -> TextComponent {
        self.load_all_chars();
        let gl_context      = self.workspace.context.clone();
        let gl_program      = self.create_program(&gl_context);
        let gl_msdf_texture = self.create_msdf_texture(&gl_context);
        let display_size    = self.size / self.text_size;
        let mut content     = TextComponentContent::new(self.font_id,self.text.as_ref());
        let initial_refresh = content.refresh_info(self.fonts);
        let buffers         = TextComponentBuffers::new(&gl_context,display_size,initial_refresh);
        self.setup_constant_uniforms(&gl_context,&gl_program);
        TextComponent {content,gl_context,gl_program,gl_msdf_texture,buffers,
            position          : self.position,
            size              : self.size,
            text_size         : self.text_size,
            msdf_texture_rows : 0
        }
    }

    fn load_all_chars(&mut self) {
        for ch in self.text.as_ref().chars() {
            self.fonts.get_render_info(self.font_id).get_glyph_info(ch);
        }
    }

    fn create_program(&self, gl_context:&Context) -> Program {
        let vert_shader = self.create_vertex_shader(gl_context);
        let frag_shader = self.create_fragment_shader(gl_context);
        link_program(&gl_context, &vert_shader, &frag_shader).unwrap()
    }

    fn create_vertex_shader(&self, gl_context:&Context) -> Shader {
        let body        = include_str!("text/msdf_vert.glsl");
        let shader_type = WebGl2RenderingContext::VERTEX_SHADER;

        compile_shader(gl_context,shader_type,body).unwrap()
    }

    fn create_fragment_shader(&self, gl_context:&Context) -> Shader {
        let body        = include_str!("text/msdf_frag.glsl");
        let shader_type = WebGl2RenderingContext::FRAGMENT_SHADER;

        compile_shader(gl_context,shader_type,body).unwrap()
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

    fn setup_constant_uniforms(&mut self, gl_context:&Context, gl_program:&Program) {
        let left           = self.position.x                 as f32;
        let right          = (self.position.x + self.size.x) as f32;
        let top            = (self.position.y + self.size.y) as f32;
        let bottom         = self.position.y                 as f32;
        let color          = &self.color;
        let range          = FontRenderInfo::MSDF_PARAMS.range as f32;
        let clip_lower_loc = gl_context.get_uniform_location(gl_program,"clip_lower");
        let clip_upper_loc = gl_context.get_uniform_location(gl_program,"clip_upper");
        let color_loc      = gl_context.get_uniform_location(gl_program,"color");
        let range_loc      = gl_context.get_uniform_location(gl_program,"range");
        let msdf_loc       = gl_context.get_uniform_location(gl_program,"msdf");

        gl_context.use_program(Some(gl_program));
        gl_context.uniform2f(clip_lower_loc.as_ref(),left,bottom);
        gl_context.uniform2f(clip_upper_loc.as_ref(),right,top);
        gl_context.uniform4f(color_loc.as_ref(),color.r,color.g,color.b,color.a);
        gl_context.uniform1f(range_loc.as_ref(),range);
        gl_context.uniform1i(msdf_loc.as_ref(),0);
    }
}
