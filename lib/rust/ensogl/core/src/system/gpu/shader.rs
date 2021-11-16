#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod glsl;

use enso_prelude::*;

use js_sys::Float32Array;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlBuffer;
use web_sys::WebGlProgram;
use web_sys::WebGlShader;



// ===============
// === Exports ===
// ===============

/// Common types.
pub mod types {
    pub use super::glsl;
    pub use glsl::traits::*;
    pub use glsl::Glsl;
}
pub use types::*;



// =============
// === Types ===
// =============

pub type Context = WebGl2RenderingContext;
pub type Shader = WebGlShader;
pub type Program = WebGlProgram;



// =============
// === Error ===
// =============

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Fail, From)]
pub enum Error {
    #[fail(display = "Unable to create {}.", target)]
    Create { target: ErrorTarget },
    #[fail(display = "Unable to compile {}.\n{}\n\n{}", target, message, code)]
    Compile { target: ErrorTarget, message: String, code: String },
}

#[derive(Copy, Clone, Debug, Fail)]
pub enum ErrorTarget {
    #[fail(display = "shader")]
    Shader,
    #[fail(display = "program")]
    Program,
}



// ==================
// === HasInfoLog ===
// ==================

pub trait CompilationTarget {
    fn check(&self, ctx: &Context) -> bool;
    fn logs(&self, ctx: &Context) -> String;
}

impl CompilationTarget for Shader {
    fn check(&self, ctx: &Context) -> bool {
        ctx.get_shader_parameter(self, Context::COMPILE_STATUS).as_bool().unwrap_or(false)
    }

    fn logs(&self, ctx: &Context) -> String {
        unwrap_error(ctx.get_shader_info_log(self))
    }
}

impl CompilationTarget for Program {
    fn check(&self, ctx: &Context) -> bool {
        ctx.get_program_parameter(self, Context::LINK_STATUS).as_bool().unwrap_or(false)
    }

    fn logs(&self, ctx: &Context) -> String {
        unwrap_error(ctx.get_program_info_log(self))
    }
}

fn unwrap_error(opt_err: Option<String>) -> String {
    opt_err.unwrap_or_else(|| "Unknown error.".to_string())
}



// ======================
// === Compile / Link ===
// ======================

pub fn compile_vertex_shader(ctx: &Context, src: &str) -> Result<Shader> {
    compile_shader(ctx, Context::VERTEX_SHADER, src)
}

pub fn compile_fragment_shader(ctx: &Context, src: &str) -> Result<Shader> {
    compile_shader(ctx, Context::FRAGMENT_SHADER, src)
}

// TODO: This is a very work-in-progress function. It should be refactored into helper functions.
pub fn compile_shader(ctx: &Context, tp: u32, src: &str) -> Result<Shader> {
    let target = ErrorTarget::Shader;
    let shader = ctx.create_shader(tp).ok_or(Error::Create { target })?;
    ctx.shader_source(&shader, src);
    ctx.compile_shader(&shader);
    if shader.check(ctx) {
        Ok(shader)
    } else {
        let code: String = src.into();
        let lines = code.split('\n').collect::<Vec<&str>>();
        let lines_num = lines.len();
        let lines_str_len = (lines_num as f32).log10().ceil() as usize;
        let lines_enum = lines.into_iter().enumerate();
        let lines_with_num =
            lines_enum.map(|(n, l)| format!("{1:0$} : {2}", lines_str_len, n + 1, l));
        let lines_with_num = lines_with_num.collect::<Vec<String>>();
        let code_with_num = lines_with_num.join("\n");
        let message = shader.logs(ctx);
        let error_loc_pfx = "ERROR: 0:";
        let out = if let Some(msg) = message.strip_prefix(error_loc_pfx) {
            let line_num: String = msg.chars().take_while(|c| c.is_digit(10)).collect();
            let line_num = line_num.parse::<usize>().unwrap() - 1;
            let preview_radius = 5;
            let preview_line_start = std::cmp::max(0, line_num - preview_radius);
            let preview_line_end = std::cmp::min(lines_num, line_num + preview_radius);
            lines_with_num[preview_line_start..preview_line_end].join("\n")
        } else {
            code_with_num
        };
        Err(Error::Compile { target, message, code: out })
    }
}

pub fn link_program(ctx: &Context, vert_shader: &Shader, frag_shader: &Shader) -> Result<Program> {
    let target = ErrorTarget::Program;
    let program = ctx.create_program().ok_or(Error::Create { target })?;
    ctx.attach_shader(&program, vert_shader);
    ctx.attach_shader(&program, frag_shader);
    ctx.link_program(&program);
    // TODO: handle errors
    Ok(program)
}



// ========================
// === Managing buffers ===
// ========================

// TODO: The functions below might be obsolete after text is fully integrated to buffer management.

/// Set the array buffer data with floats.
pub fn set_buffer_data(gl_context: &Context, buffer: &WebGlBuffer, data: &[f32]) {
    let target = Context::ARRAY_BUFFER;
    gl_context.bind_buffer(target, Some(buffer));
    set_bound_buffer_data(gl_context, target, data);
}

/// Set data in currently bound buffer
///
/// # Safety
/// The Float32Array::view is safe as long there are no allocations done
/// until it is destroyed. This way of creating buffers were taken from
/// wasm-bindgen examples
/// (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html)
#[allow(unsafe_code)]
fn set_bound_buffer_data(gl_context: &Context, target: u32, data: &[f32]) {
    let usage = Context::STATIC_DRAW;
    unsafe {
        let float_array = Float32Array::view(data);
        gl_context.buffer_data_with_array_buffer_view(target, &float_array, usage);
    }
}

/// Set the array buffer fragment with with floats.
pub fn set_buffer_subdata(gl_context: &Context, buffer: &WebGlBuffer, offset: usize, data: &[f32]) {
    let target = Context::ARRAY_BUFFER;
    gl_context.bind_buffer(target, Some(buffer));
    set_bound_buffer_subdata(gl_context, target, offset as i32, data);
}

/// Set subdata in currently bound buffer
///
/// # Safety
/// The Float32Array::view is safe as long there are no allocations done
/// until it is destroyed. This way of creating buffers were taken from
/// wasm-bindgen examples
/// (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html)
#[allow(unsafe_code)]
fn set_bound_buffer_subdata(gl_context: &Context, target: u32, offset: i32, data: &[f32]) {
    unsafe {
        let float_array = Float32Array::view(data);
        gl_context.buffer_sub_data_with_i32_and_array_buffer_view(target, offset, &float_array);
    }
}
