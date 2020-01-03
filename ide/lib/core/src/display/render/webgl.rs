pub mod glsl;

use basegl_prelude::*;

use js_sys::Float32Array;
use web_sys::WebGlBuffer;
use web_sys::WebGlProgram;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlShader;



// =============
// === Types ===
// =============

pub type Context = WebGl2RenderingContext;
pub type Shader  = WebGlShader;
pub type Program = WebGlProgram;



// =============
// === Error ===
// =============

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Fail, From)]
pub enum Error {
    #[fail(display = "Unable to create {}.", target)]
    Create { target: ErrorTarget },
    #[fail(display = "Unable to compile {}: {}", target, message)]
    Compile { target: ErrorTarget, message: String },
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
    fn check_ok(&self, ctx: &Context) -> bool;
    fn get_info_log(&self, ctx: &Context) -> String;
}

impl CompilationTarget for Shader {
    fn check_ok(&self, ctx: &Context) -> bool {
        ctx.get_shader_parameter(&self, Context::COMPILE_STATUS).as_bool().unwrap_or(false)
    }

    fn get_info_log(&self, ctx: &Context) -> String {
        unwrap_error(ctx.get_shader_info_log(&self))
    }
}

impl CompilationTarget for Program {
    fn check_ok(&self, ctx: &Context) -> bool {
        ctx.get_program_parameter(&self, Context::LINK_STATUS).as_bool().unwrap_or(false)
    }

    fn get_info_log(&self, ctx: &Context) -> String {
        unwrap_error(ctx.get_program_info_log(&self))
    }
}

fn unwrap_error(opt_err: Option<String>) -> String {
    opt_err.unwrap_or_else(|| "Unknown error.".to_string())
}



// ======================
// === Compile / Link ===
// ======================

pub fn compile_vertex_shader(ctx:&Context, src:&str) -> Result<Shader> {
    compile_shader(ctx,Context::VERTEX_SHADER,src)
}

pub fn compile_fragment_shader(ctx:&Context, src:&str) -> Result<Shader> {
    compile_shader(ctx,Context::FRAGMENT_SHADER,src)
}

pub fn compile_shader(ctx:&Context, tp:u32, src:&str) -> Result<Shader> {
    let target = ErrorTarget::Shader;
    let shader = ctx.create_shader(tp).ok_or(Error::Create {target})?;
    ctx.shader_source(&shader, src);
    ctx.compile_shader(&shader);
    handle_error(ctx, target, shader)
}

pub fn link_program(ctx:&Context, vert_shader:&Shader, frag_shader:&Shader) -> Result<Program> {
    let target = ErrorTarget::Program;
    let program = ctx.create_program().ok_or(Error::Create {target})?;
    ctx.attach_shader(&program, vert_shader);
    ctx.attach_shader(&program, frag_shader);
    ctx.link_program(&program);
    handle_error(ctx, target, program)
}

fn handle_error<T: CompilationTarget>(ctx:&Context, target:ErrorTarget, t:T) -> Result<T> {
    if t.check_ok(ctx) {
        Ok(t)
    } else {
        let message = t.get_info_log(ctx);
        let error   = Error::Compile {target,message};
        Err(error)
    }
}


// ========================
// === Managing buffers ===
// ========================

/// Set the array buffer data with floats.
pub fn set_buffer_data(gl_context:&Context, buffer:&WebGlBuffer, data:&[f32]) {
    let target = Context::ARRAY_BUFFER;
    gl_context.bind_buffer(target,Some(&buffer));
    set_bound_buffer_data(gl_context,target,data);
}

/// Set data in currently bound buffer
///
/// # Safety
/// The Float32Array::view is safe as long there are no allocations done
/// until it is destroyed. This way of creating buffers were taken from
/// wasm-bindgen examples
/// (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html)
fn set_bound_buffer_data(gl_context:&Context, target:u32, data:&[f32]) {
    let usage      = Context::STATIC_DRAW;
    unsafe { // Note [unsafe buffer_data]
        let float_array = Float32Array::view(&data);
        gl_context.buffer_data_with_array_buffer_view(target,&float_array,usage);
    }
}

/// Set the array buffer fragment with with floats.
pub fn set_buffer_subdata(gl_context:&Context, buffer:&WebGlBuffer, offset:usize, data:&[f32]) {
    let target = Context::ARRAY_BUFFER;
    gl_context.bind_buffer(target,Some(&buffer));
    set_bound_buffer_subdata(gl_context,target,offset as i32,data);
}

/// Set subdata in currently bound buffer
///
/// # Safety
/// The Float32Array::view is safe as long there are no allocations done
/// until it is destroyed. This way of creating buffers were taken from
/// wasm-bindgen examples
/// (https://rustwasm.github.io/wasm-bindgen/examples/webgl.html)
fn set_bound_buffer_subdata(gl_context:&Context, target:u32, offset:i32, data:&[f32]) {
    unsafe { // Note [unsafe buffer_data]
        let float_array = Float32Array::view(&data);
        gl_context.buffer_sub_data_with_i32_and_array_buffer_view(target,offset,&float_array);
    }
}
