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
    new_jobs:      Vec<Job<CompilableShaders>>,
    /// Programs that are ready to be polled for completion.
    started_jobs:  Vec<Job<LinkingProgram>>,
    /// Mechanism for checking compilation/linking status. Lazily-initialized.
    ready_checker: Option<ReadyChecker>,
}

impl Compiler {
    /// Compile a shader program. The provided callback will be called when it is ready, which will
    /// likely not occur until a later frame.
    pub fn compile<F>(&mut self, code: ShaderCode, callback: Box<F>)
    where F: FnOnce(Result) + 'static {
        let work = CompilableShaders { code };
        self.new_jobs.push(Job { work, callback });
    }

    /// Submit all new jobs to the WebGL engine for compilation.
    #[profile(Debug)]
    pub fn start_jobs(&mut self, context: &Context) {
        // First start all shaders compiling, then start programs linking.
        // This maximizes the browser's parallelism:
        // https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices#compile_shaders_and_link_programs_in_parallel
        let new_jobs = self.new_jobs.drain(..);
        let compiling = new_jobs.filter_map(|job| job.map_work(|work| work.compile(context)));
        let compiling = compiling.collect::<Vec<_>>().into_iter();
        let linking = compiling.filter_map(|job| job.map_work(|work| work.link(context)));
        self.started_jobs.extend(linking);
    }

    /// Check for jobs that have finished compiling, and run their callbacks.
    #[profile(Detail)]
    pub fn handle_finished_jobs(&mut self, context: &Context) {
        let ready_checker = self.ready_checker.get_or_insert_with(|| ReadyChecker::new(context));
        let is_ready = |program| ready_checker.is_ready(context, program).unwrap_or(true);
        let ready_jobs = self.started_jobs.drain_filter(|job| is_ready(&job.work));
        for Job { callback, work } in ready_jobs {
            work.block_until_finished(context).for_each(callback);
        }
    }
}



// =============
// === Error ===
// =============

#[derive(Debug)]
pub struct Error {
    program_info_log:         Option<String>,
    vertex_shader_info_log:   Option<String>,
    fragment_shader_info_log: Option<String>,
}

pub type Result = std::result::Result<WebGlProgram, Error>;



// ===========
// === Job ===
// ===========

/// Contains a callback that accepts a [`Result`], and some work in progress that will be passed to
/// the callback when it is completed.
#[derive(Derivative)]
#[derivative(Debug)]
struct Job<T> {
    #[derivative(Debug = "ignore")]
    callback: Box<dyn FnOnce(Result)>,
    work:     T,
}

impl<T> Job<T> {
    /// Return a new job with the work transformed by a function, if the function returns a value.
    fn map_work<U, F>(self, f: F) -> Option<Job<U>> where F: FnOnce(T) -> Option<U> {
        let Job { callback, work } = self;
        f(work).map(|work| Job { callback, work })
    }
}



// =========================
// === CompilableShaders ===
// =========================

/// Complete program code, ready to be compiled.
#[derive(Debug)]
struct CompilableShaders {
    code: ShaderCode,
}

impl CompilableShaders {
    /// Starts shaders compiling, unless context is lost.
    fn compile(self, context: &Context) -> Option<LinkableShaders> {
        let vertex = compile_shader(context, Context::VERTEX_SHADER, &self.code.vertex)?;
        let fragment = compile_shader(context, Context::FRAGMENT_SHADER, &self.code.fragment)?;
        Some(LinkableShaders { vertex, fragment })
    }
}

/// Returns a compiling shader, unless context is lost.
fn compile_shader(context: &Context, tp: u32, src: &str) -> Option<WebGlShader> {
    let shader = context.create_shader(tp)?;
    context.shader_source(&shader, src);
    context.compile_shader(&shader);
    Some(shader)
}



// =======================
// === LinkableShaders ===
// =======================

/// Shaders ready to be linked; they may still be compiling.
#[derive(Debug)]
struct LinkableShaders {
    vertex:   WebGlShader,
    fragment: WebGlShader,
}

impl LinkableShaders {
    /// Starts shaders linking, unless context is lost.
    ///
    /// The underlying API is implicitly async and pipelined. If the shaders are still compiling,
    /// this won't block; using the returned program in any way would block until compilation and
    /// linking are finished.
    fn link(self, context: &Context) -> Option<LinkingProgram> {
        let program = context.create_program()?;
        context.attach_shader(&program, &self.vertex);
        context.attach_shader(&program, &self.fragment);
        context.link_program(&program);
        let shaders = self;
        Some(LinkingProgram { program, shaders })
    }
}



// ======================
// === LinkingProgram ===
// ======================

/// A program that has been submitted for linking, but is not known to be ready for use yet.
#[derive(Debug)]
struct LinkingProgram {
    program: WebGlProgram,
    shaders: LinkableShaders,
}

impl LinkingProgram {
    /// Block until compiling and linking is complete, and return the result, unless context was
    /// lost.
    fn block_until_finished(self, context: &Context) -> Option<Result> {
        let LinkingProgram { program, shaders } = self;
        let status = Context::LINK_STATUS;
        let ok = context.get_program_parameter(&program, status).as_bool().unwrap_or(false);
        if !ok {
            if context.is_context_lost() {
                return None;
            }
            let program_info_log = context.get_program_info_log(&program);
            let vertex_shader_info_log = context.get_shader_info_log(&shaders.vertex);
            let fragment_shader_info_log = context.get_shader_info_log(&shaders.fragment);
            let error =
                Error { program_info_log, vertex_shader_info_log, fragment_shader_info_log };
            return Some(Err(error));
        }
        Some(Ok(program))
    }
}



// ====================
// === ReadyChecker ===
// ====================

/// Mechanism for checking compilation/linking status.
#[derive(Debug)]
struct ReadyChecker {
    khr: Option<KhrParallelShaderCompile>,
}

impl ReadyChecker {
    /// Select the best available ready-checker for the context.
    pub fn new(context: &Context) -> Self {
        let khr = KhrParallelShaderCompile::new(context);
        if khr.is_none() && !context.is_context_lost() {
            // TODO: log
        }
        Self { khr }
    }

    /// Check if the job is ready, or return None if unknown.
    pub fn is_ready(&self, context: &Context, job: &LinkingProgram) -> Option<bool> {
        self.khr.and_then(|khr| khr.is_ready(context, job))
    }
}


// === KhrParallelShaderCompile ===

/// Use the `KHR_parallel_shader_compile` extension to poll status without blocking.
///
/// See: [https://www.khronos.org/registry/webgl/extensions/KHR_parallel_shader_compile]
#[derive(Debug)]
struct KhrParallelShaderCompile {
    completion_status_khr: u32,
}

impl KhrParallelShaderCompile {
    /// Try to obtain the extension.
    pub fn new(context: &Context) -> Option<Self> {
        let ext = context.get_extension("KHR_parallel_shader_compile").ok()??;
        let completion_status_khr =
            js_sys::Reflect::get(&ext, &"COMPLETION_STATUS_KHR".into()).ok()?.as_f64()? as u32;
        Some(Self { completion_status_khr })
    }

    /// Asynchronously check if the job is ready.
    pub fn is_ready(&self, context: &Context, job: &LinkingProgram) -> Option<bool> {
        let param = self.completion_status_khr;
        context.get_program_parameter(&job.program, param).as_bool()
    }
}
