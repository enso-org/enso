//! Asynchronous interface to WebGL GLSL shader program compilation.
//!
//! Follows
//! [best practices](https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices),
//! in particular:
//! - "Compile Shaders and Link Programs in parallel"
//! - "Prefer KHR_parallel_shader_compile"
//! - "Don't check shader compile status unless linking fails"

use crate::prelude::*;
use crate::system::gpu::shader::Context;
use crate::system::gpu::shader::ShaderCode;

use web_sys::WebGlProgram;
use web_sys::WebGlShader;



// ================
// === Compiler ===
// ================

/// Compiles and links GL shader programs, asynchronously.
#[derive(Debug, Default)]
pub struct Compiler {
    /// Programs waiting to be submitted for compilation.
    new_jobs:     Vec<Job>,
    /// Programs that are ready to be polled for completion.
    started_jobs: Vec<CompilingJob>,
}

impl Compiler {
    /// Compile a shader program; the provided callback will be called when it is ready.
    pub fn compile<F>(&mut self, code: ShaderCode, callback: Box<F>)
    where F: FnOnce(CompileResult) + 'static {
        self.new_jobs.push(Job { code, callback });
    }

    /// Submit all new jobs to the WebGL engine for compilation.
    #[profile(Debug)]
    pub fn start_jobs(&mut self, context: &Context) {
        let mut linkable = Vec::new();
        let new_jobs = mem::replace(&mut self.new_jobs, default());
        // Start all shaders compiling.
        for job in &new_jobs {
            let v = compile_shader(context, Context::VERTEX_SHADER, &job.code.vertex).unwrap();
            let f = compile_shader(context, Context::FRAGMENT_SHADER, &job.code.fragment).unwrap();
            linkable.push((v, f));
        }
        // Request that shader pairs be linked.
        for (job, (v, f)) in new_jobs.into_iter().zip(linkable.into_iter()) {
            let linking = link_program(context, &v, &f).unwrap();
            let job = CompilingJob { job, linking };
            self.started_jobs.push(job);
        }
    }

    /// Check for jobs that have finished compiling, and run their callbacks.
    #[profile(Debug)]
    pub fn handle_finished_jobs(&mut self, context: &Context) {
        let psc = context.get_extension("KHR_parallel_shader_compile").unwrap().unwrap();
        let psc = js_sys::Reflect::get(&psc, &"COMPLETION_STATUS_KHR".into()).unwrap().as_f64()
            .unwrap() as u32;
        let mut i = 0;
        while i < self.started_jobs.len() {
            let job = &self.started_jobs[i];
            let ready = context.get_program_parameter(&job.linking.0, psc).as_bool().unwrap();
            if !ready {
                i += 1;
                continue;
            }
            let job = self.started_jobs.swap_remove(i);
            let linking = job.linking;
            let status = Context::LINK_STATUS;
            let ok = context.get_program_parameter(&linking.0, status).as_bool().unwrap_or(false);
            assert!(ok, "ok");
            (job.job.callback)(Ok(linking.0));
        }
    }
}

fn compile_shader(ctx: &Context, tp: u32, src: &str) -> Option<WebGlShader> {
    let shader = ctx.create_shader(tp)?;
    ctx.shader_source(&shader, src);
    ctx.compile_shader(&shader);
    Some(shader)
}

fn link_program(
    ctx: &Context,
    vert_shader: &WebGlShader,
    frag_shader: &WebGlShader,
) -> Option<Linking> {
    let program = ctx.create_program()?;
    ctx.attach_shader(&program, vert_shader);
    ctx.attach_shader(&program, frag_shader);
    ctx.link_program(&program);
    Some(Linking(program))
}


// === Error ===

#[derive(Debug, Display)]
pub struct Error;

pub type CompileResult = Result<WebGlProgram, Error>;


// === Compiling ===

/// A shader that has been submitted for compiling, but is not known to be ready for linking yet.
#[derive(Debug)]
struct Compiling(WebGlShader);

// === Linking ===

/// A program that has been submitted for linking, but is not known to be ready for use yet.
#[derive(Debug)]
struct Linking(WebGlProgram);


// ===========
// === Job ===
// ===========

/// A submission to the compile queue.
#[derive(Derivative)]
#[derivative(Debug)]
struct Job {
    code:     ShaderCode,
    #[derivative(Debug = "ignore")]
    callback: Box<dyn FnOnce(CompileResult)>,
}

/// A job that has been started.
#[derive(Debug)]
struct CompilingJob {
    job:     Job,
    linking: Linking,
}
