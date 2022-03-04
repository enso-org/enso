#![allow(missing_docs)]

#[warn(missing_docs)]
pub mod glsl;

use enso_prelude::*;

use crate::system::Context;
use js_sys::Float32Array;
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

pub type Shader = WebGlShader;
pub type Program = WebGlProgram;



// =============
// === Error ===
// =============

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



// ===================
// === Compilation ===
// ===================

/// Compilation error containing detailed error message.
#[derive(Debug)]
pub struct CompilationError(String);

impl std::error::Error for CompilationError {}

impl Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Abstraction for [`Shader`] and [`Program`] error handling.
pub trait CompilationTarget {
    /// Check whether the target was assembled correctly. In the context of [`Shader`], it checks if
    /// the compilation was successful. In the context of [`Program`], it checks whether it was
    /// linked successfully. In case of lost context, this function will succeed. For more
    /// information of why, see: https://www.khronos.org/webgl/wiki/HandlingContextLost.
    fn check(&self, ctx: &Context) -> Result<(), CompilationError>;
}

impl CompilationTarget for Shader {
    fn check(&self, ctx: &Context) -> Result<(), CompilationError> {
        let status = Context::COMPILE_STATUS;
        let status_ok = ctx.get_shader_parameter(self, status).as_bool().unwrap_or(false);
        let context_lost = ctx.is_context_lost();
        let ok = (status_ok || context_lost).then_some(());
        ok.ok_or_else(|| CompilationError(unwrap_error(ctx.get_shader_info_log(self))))
    }
}

impl CompilationTarget for Program {
    fn check(&self, ctx: &Context) -> Result<(), CompilationError> {
        let status = Context::LINK_STATUS;
        let status_ok = ctx.get_program_parameter(self, status).as_bool().unwrap_or(false);
        let context_lost = ctx.is_context_lost();
        let ok = (status_ok || context_lost).then_some(());
        ok.ok_or_else(|| CompilationError(unwrap_error(ctx.get_program_info_log(self))))
    }
}

fn unwrap_error(opt_err: Option<String>) -> String {
    opt_err.unwrap_or_else(|| "Unknown error.".into())
}



// ======================
// === Compile / Link ===
// ======================

pub fn compile_vertex_shader(ctx: &Context, src: &str) -> Result<Shader, Error> {
    compile_shader(ctx, Context::VERTEX_SHADER, src)
}

pub fn compile_fragment_shader(ctx: &Context, src: &str) -> Result<Shader, Error> {
    compile_shader(ctx, Context::FRAGMENT_SHADER, src)
}

pub fn compile_shader(ctx: &Context, tp: u32, src: &str) -> Result<Shader, Error> {
    let target = ErrorTarget::Shader;
    let shader = ctx.create_shader(tp).ok_or(Error::Create { target })?;
    ctx.shader_source(&shader, src);
    ctx.compile_shader(&shader);
    match shader.check(ctx) {
        Ok(_) => Ok(shader),
        Err(CompilationError(message)) => {
            let code: String = src.into();
            let lines = code.split('\n').collect::<Vec<&str>>();
            let lines_num = lines.len();
            let lines_str_len = (lines_num as f32).log10().ceil() as usize;
            let lines_enum = lines.into_iter().enumerate();
            let lines_with_num =
                lines_enum.map(|(n, l)| format!("{1:0$} : {2}", lines_str_len, n + 1, l));
            let lines_with_num = lines_with_num.collect::<Vec<String>>();
            let code_with_num = lines_with_num.join("\n");
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
}

/// Structure representing one of two states – an error, or lack of values because of a lost
/// context.
#[derive(Debug)]
pub enum ContextLossOrError {
    ContextLoss,
    Error(Error),
}

/// Link the provided vertex and fragment shaders into a program.
pub fn link_program(
    ctx: &Context,
    vert_shader: &Shader,
    frag_shader: &Shader,
) -> Result<Program, ContextLossOrError> {
    let target = ErrorTarget::Program;
    match ctx.create_program() {
        None => Err(if ctx.is_context_lost() {
            ContextLossOrError::ContextLoss
        } else {
            ContextLossOrError::Error(Error::Create { target })
        }),
        Some(program) => {
            ctx.attach_shader(&program, vert_shader);
            ctx.attach_shader(&program, frag_shader);
            ctx.link_program(&program);
            Ok(program)
        }
    }
}

/// Compile the provided vertex and fragment shader sources and then link them into a program.
pub fn compile_program(
    ctx: &Context,
    vert_src: &str,
    frag_src: &str,
) -> Result<Program, ContextLossOrError> {
    let vert_shader = compile_vertex_shader(ctx, vert_src).map_err(ContextLossOrError::Error)?;
    let frag_shader = compile_fragment_shader(ctx, frag_src).map_err(ContextLossOrError::Error)?;
    link_program(ctx, &vert_shader, &frag_shader)
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

/// Set data in currently bound buffer.
///
/// # Safety
/// The Float32Array::view is safe as long there are no allocations done
/// until it is destroyed. This way of creating buffers were taken from
/// wasm-bindgen examples
/// (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html).
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

/// Set subdata in currently bound buffer.
///
/// # Safety
/// The Float32Array::view is safe as long there are no allocations done
/// until it is destroyed. This way of creating buffers were taken from
/// wasm-bindgen examples
/// (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html).
#[allow(unsafe_code)]
fn set_bound_buffer_subdata(gl_context: &Context, target: u32, offset: i32, data: &[f32]) {
    unsafe {
        let float_array = Float32Array::view(data);
        gl_context.buffer_sub_data_with_i32_and_array_buffer_view(target, offset, &float_array);
    }
}
