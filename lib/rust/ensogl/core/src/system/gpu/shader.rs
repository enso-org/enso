// === Non-Standard Linter Configuration ===
#![allow(missing_docs)]

use enso_prelude::*;
use enso_web::traits::*;

use crate::system::gpu::Context;

use enso_shapely::define_singleton_enum;
use enso_web as web;
use js_sys::Float32Array;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlBuffer;
use web_sys::WebGlProgram;
use web_sys::WebGlShader;

pub mod compiler;
pub use compiler::Compiler;


pub mod traits {
    pub use super::BlockingCheckStatus;
    pub use super::BlockingGetErrorLog;
}


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Sources<Vertex, Fragment> {
    pub vertex:   Vertex,
    pub fragment: Fragment,
}

pub type Code = Sources<String, String>;
pub type CompiledCode = Sources<Shader<Vertex>, Shader<Fragment>>;



define_singleton_enum! {
    Type {
        Vertex, Fragment
    }
}


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Shader<T> {
    tp:     PhantomData<T>,
    code:   String,
    native: WebGlShader,
}

impl<T> Shader<T> {
    pub fn new(code: String, native: WebGlShader) -> Self {
        let tp = default();
        Self { tp, code, native }
    }
}


impl<T> AsRef<WebGlShader> for Shader<T> {
    fn as_ref(&self) -> &WebGlShader {
        &self.native
    }
}

impl<T> Deref for Shader<T> {
    type Target = WebGlShader;
    fn deref(&self) -> &Self::Target {
        &self.native
    }
}

impl ToGlEnum for Type {
    fn to_gl_enum(&self) -> GlEnum {
        match self {
            Self::Vertex => GlEnum(WebGl2RenderingContext::VERTEX_SHADER),
            Self::Fragment => GlEnum(WebGl2RenderingContext::FRAGMENT_SHADER),
        }
    }
}

// ==============
// === Export ===
// ==============

#[warn(missing_docs)]
pub mod glsl;



// ===============
// === Exports ===
// ===============

/// Common types.
pub mod types {
    pub use super::glsl;
    pub use glsl::traits::*;
    pub use glsl::Glsl;
}
use crate::display::GlEnum;
use crate::display::ToGlEnum;
pub use types::*;


// =============
// === Types ===
// =============

// pub type Shader = WebGlShader;
// pub type Program = WebGlProgram;



// =============
// === Error ===
// =============

#[derive(Debug, Fail)]
pub enum SingleTargetError {
    #[fail(display = "Unable to create {}.", target)]
    Create { target: ErrorTarget },
    #[fail(display = "Unable to compile {}.\n{}\n\n{}", target, message, preview_code)]
    Compile { target: ErrorTarget, message: String, preview_code: String },
}

#[derive(Debug, Fail, From)]
pub enum Error {
    Create {
        target: ErrorTarget,
    },

    Compile {
        js_path:  String,
        vertex:   Option<SingleTargetError>,
        fragment: Option<SingleTargetError>,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Create { target } => write!(f, "Unable to create {}", target),
            Self::Compile { js_path, vertex, fragment } => {
                let vtx_msg = vertex.as_ref().map(|t| format!("\n\n{}", t)).unwrap_or_default();
                let frag_msg = fragment.as_ref().map(|t| format!("\n\n{}", t)).unwrap_or_default();
                let run_msg = |n| {
                    format!("Run `console.log({}.{})` to inspect the {} shader.", js_path, n, n)
                };
                let vtx_run_msg = run_msg("vertex");
                let frag_run_msg = run_msg("fragment");
                let err_msg = "Unable to create shader.";
                write!(f, "{}\n{}\n{}{}{}", err_msg, vtx_run_msg, frag_run_msg, vtx_msg, frag_msg)
            }
        }
    }
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



impl CompilationTarget for WebGlShader {
    fn check(&self, ctx: &Context) -> Result<(), CompilationError> {
        let status = Context::COMPILE_STATUS;
        let status_ok = ctx.get_shader_parameter(self, *status).as_bool().unwrap_or(false);
        let context_lost = ctx.is_context_lost();
        let ok = (status_ok || context_lost).then_some(());
        ok.ok_or_else(|| CompilationError(unwrap_error(ctx.get_shader_info_log(self))))
    }
}

impl CompilationTarget for WebGlProgram {
    fn check(&self, ctx: &Context) -> Result<(), CompilationError> {
        let status = Context::LINK_STATUS;
        let status_ok = ctx.get_program_parameter(self, *status).as_bool().unwrap_or(false);
        let context_lost = ctx.is_context_lost();
        let ok = (status_ok || context_lost).then_some(());
        ok.ok_or_else(|| CompilationError(unwrap_error(ctx.get_program_info_log(self))))
    }
}


// ===============================
// === WebGlContext Extensions ===
// ===============================

pub trait BlockingCheckStatus<T> {
    fn blocking_check_status(&self, target: &T) -> Option<String>;
}

impl<T> BlockingCheckStatus<Shader<T>> for WebGl2RenderingContext {
    fn blocking_check_status(&self, target: &Shader<T>) -> Option<String> {
        let status = Context::COMPILE_STATUS;
        let ok = self.get_shader_parameter(target, *status).as_bool().unwrap_or(false);
        (!ok).then_some(unwrap_error(self.get_shader_info_log(target)))
    }
}

impl BlockingCheckStatus<WebGlProgram> for WebGl2RenderingContext {
    fn blocking_check_status(&self, target: &WebGlProgram) -> Option<String> {
        let status = Context::LINK_STATUS;
        let ok = self.get_program_parameter(target, *status).as_bool().unwrap_or(false);
        (!ok).then_some(unwrap_error(self.get_program_info_log(target)))
    }
}

fn unwrap_error(opt_err: Option<String>) -> String {
    opt_err.unwrap_or_else(|| "Unknown error.".into())
}



pub trait BlockingGetErrorLog {
    fn blocking_format_error_log<T>(&self, shader: &Shader<T>) -> Option<String>;
}

impl BlockingGetErrorLog for WebGl2RenderingContext {
    fn blocking_format_error_log<T>(&self, shader: &Shader<T>) -> Option<String> {
        self.blocking_check_status(shader).map(|message| {
            let lines = shader.code.split('\n').collect::<Vec<&str>>();
            let lines_num = lines.len();
            let lines_str_len = (lines_num as f32).log10().ceil() as usize;
            let lines_enum = lines.into_iter().enumerate();
            let lines_with_num =
                lines_enum.map(|(n, l)| format!("{1:0$} : {2}", lines_str_len, n + 1, l));
            let lines_with_num = lines_with_num.collect::<Vec<String>>();
            let code_with_num = lines_with_num.join("\n");
            let error_loc_pfx = "ERROR: 0:";
            let preview_code = if let Some(msg) = message.strip_prefix(error_loc_pfx) {
                let line_num: String = msg.chars().take_while(|c| c.is_digit(10)).collect();
                let line_num = line_num.parse::<usize>().unwrap() - 1;
                let preview_radius = 5;
                let preview_line_start = std::cmp::max(0, line_num - preview_radius);
                let preview_line_end = std::cmp::min(lines_num, line_num + preview_radius);
                let preview = lines_with_num[preview_line_start..preview_line_end].join("\n");
                format!("...\n{}\n...", preview)
            } else {
                code_with_num
            };
            format!("{}\n{}", message, preview_code)
        })
    }
}



// ========================
// === Managing buffers ===
// ========================

// TODO: The functions below might be obsolete after text is fully integrated to buffer management.

/// Set the array buffer data with floats.
pub fn set_buffer_data(gl_context: &Context, buffer: &WebGlBuffer, data: &[f32]) {
    let target = Context::ARRAY_BUFFER;
    gl_context.bind_buffer(*target, Some(buffer));
    set_bound_buffer_data(gl_context, *target, data);
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
        gl_context.buffer_data_with_array_buffer_view(target, &float_array, *usage);
    }
}

/// Set the array buffer fragment with with floats.
pub fn set_buffer_subdata(gl_context: &Context, buffer: &WebGlBuffer, offset: usize, data: &[f32]) {
    let target = Context::ARRAY_BUFFER;
    gl_context.bind_buffer(*target, Some(buffer));
    set_bound_buffer_subdata(gl_context, *target, offset as i32, data);
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
