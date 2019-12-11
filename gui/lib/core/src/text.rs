pub mod font;
pub mod buffer;
pub mod msdf;

use crate::prelude::*;

use crate::{Area,Color};
use crate::display::world::Workspace;
use crate::text::buffer::TextComponentBuffers;
use crate::text::msdf::MsdfTexture;

use font::FontRenderInfo;
use basegl_backend_webgl::{Context,compile_shader,link_program,Program,Shader};
use nalgebra::{Vector2,Similarity2,Point2,Projective2};
use web_sys::{WebGlRenderingContext,WebGlBuffer,WebGlTexture};


// =====================
// === TextComponent ===
// =====================

/// Component rendering text
///
/// This component is under heavy construction, so the api may easily changed in few future
/// commits
#[derive(Debug)]
pub struct TextComponent {
    gl_context      : WebGlRenderingContext,
    gl_program      : Program,
    gl_msdf_texture : WebGlTexture,
    lines           : Vec<Line>,
    buffers         : TextComponentBuffers,
    to_window       : Projective2<f64>
}

#[derive(Debug)]
pub struct Line {
    pub content                  : String,
}

impl TextComponent {

    /// Render text
    pub fn display(&self) {
        let gl_context     = &self.gl_context;
        let vertices_count = self.buffers.vertices_count() as i32;

        gl_context.use_program(Some(&self.gl_program));
        self.bind_buffer_to_attribute("position",&self.buffers.vertex_position);
        self.bind_buffer_to_attribute("texCoord",&self.buffers.texture_coords);
        self.setup_blending();
        gl_context.bind_texture(Context::TEXTURE_2D, Some(&self.gl_msdf_texture));
        gl_context.draw_arrays(WebGlRenderingContext::TRIANGLES,0,vertices_count);
    }

    fn bind_buffer_to_attribute(&self, attribute_name:&str, buffer:&WebGlBuffer) {
        let gl_context = &self.gl_context;
        let gl_program = &self.gl_program;
        let location   = gl_context.get_attrib_location(gl_program,attribute_name) as u32;
        let target     = WebGlRenderingContext::ARRAY_BUFFER;
        let item_size  = 2;
        let item_type  = WebGlRenderingContext::FLOAT;
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
pub struct TextComponentBuilder<'a, Str:AsRef<str>> {
    pub text            : Str,
    pub font            : &'a mut FontRenderInfo,
    pub scroll_position : Vector2<f64>,
    pub size            : f64,
    pub color           : Color<f32>,
    pub area            : Area<f64>
}

impl<'a,Str:AsRef<str>> TextComponentBuilder<'a,Str> {

    /// Build a new text component rendering on given workspace
    pub fn build(mut self, workspace : &Workspace) -> TextComponent {
        self.load_all_chars();
        let gl_context        = workspace.context.clone();
        let gl_program        = self.create_program(&gl_context);
        let gl_msdf_texture   = self.create_msdf_texture(&gl_context);
        let lines             = self.split_lines();
        let to_window         = self.to_window_transform();
        let mut buffers       = self.create_buffers(&gl_context, &to_window);
        self.setup_uniforms(&gl_context,&gl_program,&to_window);
        buffers.refresh_all(&gl_context,&lines,self.font);
        TextComponent {
            gl_context,
            gl_program,
            gl_msdf_texture,
            buffers,
            lines,
            to_window,
        }
    }

    fn load_all_chars(&mut self) {
        for ch in self.text.as_ref().chars() {
            self.font.get_glyph_info(ch);
        }
    }

    fn split_lines(&self) -> Vec<Line> {
        let lines_text = self.text.as_ref().split('\n');
        let lines_iter = lines_text.map(Self::initialize_line);
        lines_iter.collect()
    }

    fn initialize_line(text:&str) -> Line {
        Line {
            content : text.to_string(),
        }
    }

    fn create_program(&self, gl_context:&Context) -> Program {
        gl_context.get_extension("OES_standard_derivatives").unwrap().unwrap();
        let vert_shader = self.create_vertex_shader(gl_context);
        let frag_shader = self.create_fragment_shader(gl_context);
        link_program(&gl_context, &vert_shader, &frag_shader).unwrap()
    }

    fn create_vertex_shader(&self, gl_context:&Context) -> Shader {
        let body        = include_str!("text/msdf_vert.glsl");
        let shader_type = WebGlRenderingContext::VERTEX_SHADER;

        compile_shader(gl_context,shader_type,body).unwrap()
    }

    fn create_fragment_shader(&self, gl_context:&Context) -> Shader {
        let body        = include_str!("text/msdf_frag.glsl");
        let shader_type = WebGlRenderingContext::FRAGMENT_SHADER;

        compile_shader(gl_context,shader_type,body).unwrap()
    }

    fn to_window_transform(&self) -> Projective2<f64> {
        const ROTATION : f64  = 0.0;
        let base_position     = Vector2::new(self.area.left,self.area.top);
        let scrolled_position = base_position - self.scroll_position;
        let similarity        = Similarity2::new(scrolled_position,ROTATION,self.size);
        nalgebra::convert(similarity)
    }

    fn create_msdf_texture(&self, gl_ctx:&Context)
    -> WebGlTexture {
        let msdf_texture = gl_ctx.create_texture().unwrap();
        let target       = Context::TEXTURE_2D;
        let wrap         = Context::CLAMP_TO_EDGE as i32;
        let min_filter   = Context::LINEAR as i32;
        let width        = MsdfTexture::WIDTH as i32;
        let height       = self.font.msdf_texture.rows() as i32;
        let border       = 0;
        let tex_level    = 0;
        let format       = Context::RGB;
        let internal_fmt = Context::RGB as i32;
        let tex_type     = Context::UNSIGNED_BYTE;
        let data         = Some(self.font.msdf_texture.data.as_slice());

        gl_ctx.bind_texture(target,Some(&msdf_texture));
        gl_ctx.tex_parameteri(target,Context::TEXTURE_WRAP_S,wrap);
        gl_ctx.tex_parameteri(target,Context::TEXTURE_WRAP_T,wrap);
        gl_ctx.tex_parameteri(target,Context::TEXTURE_MIN_FILTER,min_filter);
        let tex_image_result =
            gl_ctx.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_u8_array
            ( target
            , tex_level
            , internal_fmt
            , width
            , height
            , border
            , format
            , tex_type
            , data
            );
        tex_image_result.unwrap();
        msdf_texture
    }

    fn create_buffers(&mut self, gl_context:&Context, to_window:&Projective2<f64>)
    -> TextComponentBuffers {
        let from_window       = to_window.inverse();
        let component_lb      = Point2::new(self.area.left, self.area.bottom);
        let component_rt      = Point2::new(self.area.right, self.area.top);
        let displayed_text_lb = from_window * component_lb;
        let displayed_text_rt = from_window * component_rt;
        let displayed_text    = Area {
            left              : displayed_text_lb.x,
            right             : displayed_text_rt.x,
            top               : displayed_text_rt.y,
            bottom            : displayed_text_lb.y,
        };
        TextComponentBuffers::new(gl_context,displayed_text,self.font)
    }

    fn setup_uniforms(&self, gl_context:&Context, gl_program:&Program, to_window:&Projective2<f64>) {
        let to_window_converted = Self::convert_to_window(to_window);
        let to_window_ref       = to_window_converted.as_ref();
        let area                = &self.area;
        let color               = &self.color;
        let range               = FontRenderInfo::MSDF_PARAMS.range as f32;
        let msdf_width          = MsdfTexture::WIDTH as f32;
        let msdf_height         = self.font.msdf_texture.rows() as f32;
        let transpose           = false;
        let to_window_loc       = gl_context.get_uniform_location(gl_program,"toWindow");
        let clip_lower_loc      = gl_context.get_uniform_location(gl_program,"clipLower");
        let clip_upper_loc      = gl_context.get_uniform_location(gl_program,"clipUpper");
        let color_loc           = gl_context.get_uniform_location(gl_program,"color");
        let range_loc           = gl_context.get_uniform_location(gl_program,"range");
        let msdf_loc            = gl_context.get_uniform_location(gl_program,"msdf");
        let msdf_size_loc       = gl_context.get_uniform_location(gl_program,"msdfSize");

        gl_context.use_program(Some(gl_program));
        gl_context.uniform_matrix3fv_with_f32_array(to_window_loc.as_ref(),transpose,to_window_ref);
        gl_context.uniform2f(clip_lower_loc.as_ref(),area.left as f32,area.bottom as f32);
        gl_context.uniform2f(clip_upper_loc.as_ref(),area.right as f32,area.top as f32);
        gl_context.uniform4f(color_loc.as_ref(),color.r,color.g,color.b,color.a);
        gl_context.uniform1f(range_loc.as_ref(),range);
        gl_context.uniform1i(msdf_loc.as_ref(),0);
        gl_context.uniform2f(msdf_size_loc.as_ref(),msdf_width,msdf_height);
    }

    fn convert_to_window(to_window:&Projective2<f64>) -> SmallVec<[f32;9]> {
        let matrix            = to_window.matrix();
        let view : &[[f64;3]] = matrix.as_ref();
        let flatten_view      = view.iter().flatten();
        let converted         = flatten_view.map(|f| *f as f32);
        converted.collect()
    }
}
