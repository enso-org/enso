#![allow(missing_docs)]

use crate::prelude::*;

use crate::display::render::webgl::Context;
use crate::display::render::webgl::compile_shader;
use crate::display::render::webgl::link_program;
use crate::display::render::webgl::Program;
use crate::display::shape::text::font::FontRenderInfo;
use crate::display::shape::text::msdf::MsdfTexture;
use crate::display::shape::text::TextComponentProperties;

use web_sys::WebGlBuffer;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlUniformLocation;


// ====================
// === BasicProgram ===
// ====================

/// A structure for a basic program used for rendering TextComponent. It handles the common uniform
/// values: a `to_scene` matrix, clipping and color.
#[derive(Debug)]
pub struct BasicProgram {
    pub gl_context        : Context,
    pub gl_program        : Program,
    pub to_scene_location : WebGlUniformLocation
}

impl BasicProgram {
    /// Create program having shaders compiled from the given code. The created program won't have
    /// any uniform set.
    pub fn new(gl_context:&Context, vertex_shader_body:&str, fragment_shader_body:&str) -> Self {
        let vert_type         = WebGl2RenderingContext::VERTEX_SHADER;
        let frag_type         = WebGl2RenderingContext::FRAGMENT_SHADER;
        let vert_shader       = compile_shader(gl_context,vert_type,vertex_shader_body).unwrap();
        let frag_shader       = compile_shader(gl_context,frag_type,fragment_shader_body).unwrap();
        let gl_program        = link_program(&gl_context,&vert_shader,&frag_shader).unwrap();
        let to_scene_location = gl_context.get_uniform_location(&gl_program,"to_scene").unwrap();
        BasicProgram {gl_program,to_scene_location,
            gl_context : gl_context.clone(),
        }
    }

    /// Set the uniforms values which don't change after component creation.
    pub fn set_constant_uniforms(&self, properties:&TextComponentProperties) {
        let position       = &properties.position;
        let size           = &properties.size;
        let color          = &properties.color;
        let left           = position.x            as f32;
        let right          = (position.x + size.x) as f32;
        let top            = (position.y + size.y) as f32;
        let bottom         = position.y            as f32;
        let clip_lower_loc = self.gl_context.get_uniform_location(&self.gl_program,"clip_lower");
        let clip_upper_loc = self.gl_context.get_uniform_location(&self.gl_program,"clip_upper");
        let color_loc      = self.gl_context.get_uniform_location(&self.gl_program,"color");

        self.gl_context.use_program(Some(&self.gl_program));
        self.gl_context.uniform2f(clip_lower_loc.as_ref(),left,bottom);
        self.gl_context.uniform2f(clip_upper_loc.as_ref(),right,top);
        self.gl_context.uniform4f(color_loc.as_ref(),color.r,color.g,color.b,color.a);
    }

    /// Set the uniform containing `to_scene` transformation.
    pub fn set_to_scene_transformation(&self, matrix:&SmallVec<[f32;9]>) {
        let to_scene_ref  = matrix.as_ref();
        let to_scene_loc  = Some(&self.to_scene_location);
        let transpose     = false;
        self.gl_context.use_program(Some(&self.gl_program));
        self.gl_context.uniform_matrix3fv_with_f32_array(to_scene_loc,transpose,to_scene_ref);
    }

    /// Bind buffer for this program's attribute.
    pub fn bind_buffer_to_attribute(&self, attribute_name:&str, buffer:&WebGlBuffer) {
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
            (location,item_size,item_type,normalized,stride,offset);
    }
}


// ===================
// === MsdfProgram ===
// ===================

/// An extension for `BasicProgram` structure, handling additional uniform needed for MSDF
/// implementation.
#[derive(Debug,Shrinkwrap)]
pub struct  MsdfProgram {
    #[shrinkwrap(main_field)]
    pub program            : BasicProgram,
    pub msdf_size_location : WebGlUniformLocation,
}

impl MsdfProgram {

    /// Create program having shaders compiled from the given code. The created program won't have
    /// any uniform set.
    pub fn new(gl_context:&Context, vertex_shader_body:&str, fragment_shader_body:&str) -> Self {
        let program       = BasicProgram::new(gl_context,vertex_shader_body,fragment_shader_body);
        let gl_program    = &program.gl_program;
        let msdf_size_loc = gl_context.get_uniform_location(gl_program,"msdf_size").unwrap();
        MsdfProgram{program,
            msdf_size_location: msdf_size_loc
        }
    }

    /// Set the uniforms values which don't change after component creation.
    pub fn set_constant_uniforms(&self, properties:&TextComponentProperties) {
        let range     = FontRenderInfo::MSDF_PARAMS.range as f32;
        let msdf_loc  = self.gl_context.get_uniform_location(&self.gl_program,"msdf");
        let range_loc = self.gl_context.get_uniform_location(&self.gl_program,"range");

        self.gl_context.use_program(Some(&self.gl_program));
        self.program.set_constant_uniforms(properties);
        self.gl_context.uniform1f(range_loc.as_ref(),range);
        self.gl_context.uniform1i(msdf_loc.as_ref(),0);
    }

    /// Update the uniform with msdf size.
    pub fn set_msdf_size(&self, font:&FontRenderInfo) {
        let msdf_width    = MsdfTexture::WIDTH as f32;
        let msdf_height   = font.msdf_texture.rows() as f32;

        self.gl_context.use_program(Some(&self.program.gl_program));
        self.gl_context.uniform2f(Some(&self.msdf_size_location),msdf_width,msdf_height);
    }
}


// =====================================
// === Specific program constructors ===
// =====================================

/// Create program for TextComponent content (glyphs) rendering.
pub fn create_content_program(gl_context:&Context) -> MsdfProgram {
    let vert_shader_body = include_str!("program/msdf_vert.glsl");
    let frag_shader_body = include_str!("program/msdf_frag.glsl");
    MsdfProgram::new(gl_context,vert_shader_body,frag_shader_body)
}

/// Create program for cursors rendering.
pub fn create_cursors_program(gl_context:&Context) -> BasicProgram {
    let vert_shader_body = include_str!("program/cursor_vert.glsl");
    let frag_shader_body = include_str!("program/cursor_frag.glsl");
    BasicProgram::new(gl_context,vert_shader_body,frag_shader_body)
}