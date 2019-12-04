pub mod font;
pub mod glyph_render;
pub mod msdf;

use crate::prelude::*;

use crate::Color;
use crate::display::world::Workspace;
use crate::text::glyph_render::{GylphSquareVerticesBuilder, GlyphSquareTextureCoordinatesBuilder};
use crate::text::msdf::MsdfTexture;

use font::FontRenderInfo;
use basegl_backend_webgl::{Context,compile_shader,link_program,Program,Shader};
use js_sys::Float32Array;
use nalgebra::{Vector2,Similarity2,Transform2};
use web_sys::{WebGlRenderingContext,WebGlBuffer,WebGlTexture};

pub struct TextComponentBuilder<'a> {
    pub text     : String,
    pub font     : &'a mut FontRenderInfo,
    pub position : Vector2<f64>,
    pub size     : f64,
    pub color    : Color<f32>,
}

#[derive(Debug)]
pub struct TextComponent {
    gl_context                    : WebGlRenderingContext,
    gl_program                    : Program,
    gl_vertex_buffer              : WebGlBuffer,
    gl_texture_coordinates_buffer : WebGlBuffer,
    gl_msdf_texture               : WebGlTexture,
    buffers_size                  : usize,
}

impl<'a> TextComponentBuilder<'a> {
    pub fn build(mut self, workspace  : &Workspace) -> TextComponent {
        self.load_all_chars();
        let gl_context           = workspace.context.clone();
        let gl_program           = self.create_program(&gl_context);
        let gl_vertex_buffer     = self.create_vertex_bufffer(&gl_context);
        let gl_tex_coord_buffer  = self.create_texture_coordinates_buffer(&gl_context);
        let gl_msdf_texture      = self.create_msdf_texture(&gl_context);
        let glyph_vertices_count = glyph_render::GLYPH_SQUARE_VERTICES_BASE_LAYOUT.len();
        let buffers_size         = self.text.len() * glyph_vertices_count;
        self.setup_uniforms(&gl_context, &gl_program);
        TextComponent {
            gl_context,
            gl_program,
            gl_vertex_buffer,
            gl_texture_coordinates_buffer : gl_tex_coord_buffer,
            gl_msdf_texture,
            buffers_size
        }
    }

    fn load_all_chars(&mut self) {
        for ch in self.text.chars() {
            self.font.get_glyph_info(ch);
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

    fn create_buffer(gl_context:&Context, vertices:&[f32]) -> WebGlBuffer {
        let target = WebGlRenderingContext::ARRAY_BUFFER;

        let buffer = gl_context.create_buffer().unwrap();
        gl_context.bind_buffer(target,Some(&buffer));
        Self::set_bound_buffer_data(gl_context,target,vertices);
        buffer
    }

    fn set_bound_buffer_data(gl_context:&Context, target:u32, data:&[f32]) {
        let usage  = WebGlRenderingContext::STATIC_DRAW;
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

    fn create_vertex_bufffer(&mut self, gl_context:&Context) -> WebGlBuffer {
        let to_window            = self.to_window_transform();
        let font                 = &mut self.font;

        let mut vertices_builder = GylphSquareVerticesBuilder::new(font,to_window);
        let char_to_vertices     = |ch| vertices_builder.build_for_next_glyph(ch);
        let grouped_vertices     = self.text.chars().map(char_to_vertices);
        let vertices             = grouped_vertices.flatten();
        let buffer_data          = vertices.map(|f| f as f32).collect::<SmallVec<[f32;32]>>();
        Self::create_buffer(gl_context,buffer_data.as_ref())
    }

    fn to_window_transform(&self) -> Transform2<f64> {
        const ROTATION : f64 = 0.0;
        let similarity       = Similarity2::new(self.position,ROTATION,self.size);
        nalgebra::convert(similarity)
    }

    fn create_texture_coordinates_buffer(&mut self, gl_context:&Context) -> WebGlBuffer {
        let font                            = &mut self.font;

        let mut texture_coordinates_builder = GlyphSquareTextureCoordinatesBuilder::new(font);
        let char_to_texture_coordinates     = |ch| texture_coordinates_builder.build_for_next_glyph(ch);
        let grouped_texture_coordinates     = self.text.chars().map(char_to_texture_coordinates);
        let texture_coordinates             = grouped_texture_coordinates.flatten();
        let converted_data                  = texture_coordinates.map(|f| f as f32);
        let buffer_data                     = converted_data.collect::<SmallVec<[f32;32]>>();
        Self::create_buffer(gl_context,buffer_data.as_ref())
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

    fn setup_uniforms(&self, gl_context:&Context, gl_program:&Program) {
        let color         = &self.color;
        let range         = FontRenderInfo::MSDF_PARAMS.range as f32;
        let msdf_width    = MsdfTexture::WIDTH as f32;
        let msdf_height   = self.font.msdf_texture.rows() as f32;
        let color_loc     = gl_context.get_uniform_location(gl_program,"color");
        let range_loc     = gl_context.get_uniform_location(gl_program,"range");
        let msdf_loc      = gl_context.get_uniform_location(gl_program,"msdf");
        let msdf_size_loc = gl_context.get_uniform_location(gl_program,"msdfSize");

        gl_context.use_program(Some(gl_program));
        gl_context.uniform4f(color_loc.as_ref(),color.r,color.g,color.b,color.a);
        gl_context.uniform1f(range_loc.as_ref(),range);
        gl_context.uniform1i(msdf_loc.as_ref(),0);
        gl_context.uniform2f(msdf_size_loc.as_ref(),msdf_width,msdf_height);
    }
}

impl TextComponent {

    pub fn display(&self) {
        let gl_context = &self.gl_context;

        gl_context.use_program(Some(&self.gl_program));
        self.bind_buffer_to_attribute("position",&self.gl_vertex_buffer);
        self.bind_buffer_to_attribute("texCoord",&self.gl_texture_coordinates_buffer);
        self.setup_blending();
        gl_context.bind_texture(Context::TEXTURE_2D, Some(&self.gl_msdf_texture));
        gl_context.draw_arrays(WebGlRenderingContext::TRIANGLES,0,self.buffers_size as i32);
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
