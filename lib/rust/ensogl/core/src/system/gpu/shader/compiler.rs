//! Asynchronous interface to WebGL GLSL shader program compilation.
//!
//! # Performance
//!
//! In order to maximize parallelism and avoid blocking, follows
//! [best practices](https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices).
//!
//! In particular, these items from that document are implemented here:
//! - "Compile Shaders and Link Programs in parallel"
//! - "Prefer KHR_parallel_shader_compile"
//! - "Don't check shader compile status unless linking fails"
//!
//! # Context loss
//!
//! Many functions in this module must handle
//! [context loss](https://www.khronos.org/webgl/wiki/HandlingContextLost).
//!
//! In the case of context loss, no result is available, whether successful or an error; this
//! possibility is represented here with the [`Option`] type.

use enso_web as web;
use enso_web::traits::*;

use crate::prelude::*;
use crate::system::gpu::shader::Context;
use crate::system::gpu::shader::ShaderCode;
use crate::system::gpu::shader::VfPair;

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
    /// Compile a shader program.
    ///
    /// The provided callback will be called when compilation completes, unless the context is
    /// lost.
    pub fn compile<F>(&mut self, code: ShaderCode, callback: Box<F>)
    where F: FnOnce(Result) + 'static {
        let work = CompilableShaders { code };
        self.new_jobs.push(Job { work, callback });
    }

    /// Submit all new jobs to the WebGL engine for compilation.
    #[profile(Debug)]
    pub fn start_jobs(&mut self, context: &Context) {
        // To maximize parallelism, start all compilation processes before starting linking.
        // See: https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices#compile_shaders_and_link_programs_in_parallel
        let new_jobs = self.new_jobs.drain(..);
        let compiling = new_jobs.filter_map(|job| job.maybe_map_work(|work| work.compile(context)));
        let compiling = compiling.collect::<Vec<_>>().into_iter();
        let linking = compiling.filter_map(|job| job.maybe_map_work(|work| work.link(context)));
        self.started_jobs.extend(linking);
    }

    /// Check for jobs that have finished compiling, and run their callbacks.
    #[profile(Detail)]
    pub fn handle_finished_jobs(&mut self, context: &Context) {
        let ready_checker = self.ready_checker.get_or_insert_with(|| ReadyChecker::new(context));
        let ready_jobs = self
            .started_jobs
            .drain_filter(|job| ready_checker.is_ready(context, &job.work).unwrap_or(true));
        for Job { callback, work } in ready_jobs {
            work.block_until_finished(context).for_each(callback);
        }
    }
}



// =============
// === Error ===
// =============

#[derive(Debug, Fail)]
pub struct Error {
    /// Linking log, if present.
    link_log: Option<String>,
    /// Information about compiling shaders.
    shaders:  VfPair<ShaderInfo>,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let js_path = write_debug_objects(&self.shaders);
        let run_msg = VF_DBG_KEYS.clone().map(|n|
            format!("Run `console.log({}.{})` to inspect the {} shader.", js_path, n, n));
        let err_msg = "Unable to compile shader.";
        write!(f, "{}\n{}\n{}", err_msg, run_msg.vertex, run_msg.fragment)?;
        // If a compile-time failure occurred, we only need to report on the affected shader(s);
        // if the error occurred when linking, report on all shaders.
        let fail_time = match self.shaders.vertex.ok && self.shaders.fragment.ok {
            true => FailTime::Link,
            false => FailTime::Compile,
        };
        match fail_time {
            FailTime::Compile => self.shaders.as_ref().try_for_each(|shader| {
                if !shader.ok {
                    write!(f, "\n\n{}", shader)?;
                }
                Ok(())
            })?,
            FailTime::Link => {
                self.shaders.as_ref().try_for_each(|shader| write!(f, "\n\n{}", shader))?;
                if let Some(link_log) = self.link_log.as_ref() {
                    write!(f, "\n\n{}", &link_log)?;
                }
            }
        }
        Ok(())
    }
}

pub type Result = std::result::Result<WebGlProgram, Error>;

#[derive(PartialEq, Eq)]
enum FailTime {
    Compile,
    Link,
}

// FIXME: this should be taken from config and refactored to a function
const VF_DBG_KEYS: VfPair<&str> = VfPair { vertex: "vertex", fragment: "fragment" };

fn write_debug_objects(shaders: &VfPair<ShaderInfo>) -> String {
    // FIXME: this should be taken from config and refactored to a function along with
    //  `VF_DBG_KEYS` above
    let path = &["enso", "debug", "shader"];
    let shader_dbg = web::Reflect::get_nested_object_or_create(&web::window, path);
    let shader_dbg = shader_dbg.unwrap();
    let len = web::Object::keys(&shader_dbg).length();
    let debug_var_name = format!("shader_{}", len);
    let shader_tgt_dbg = web::Object::new();
    web::Reflect::set(&shader_dbg, &(&debug_var_name).into(), &shader_tgt_dbg).unwrap();
    let keys = VF_DBG_KEYS.map(enso_web::JsValue::from);
    shaders.as_ref().zip(keys).for_each(|(shader, key)| {
        let code = shader.source_code.as_str().into();
        web::Reflect::set(&shader_tgt_dbg, &key, &code).unwrap();
    });

    format!("window.{}.{}", path.join("."), debug_var_name)
}



// ==================
// === ShaderInfo ===
// ==================

#[derive(Debug)]
pub struct ShaderInfo {
    ok:          bool,
    info_log:    Option<String>,
    source_code: String,
}

impl Display for ShaderInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Shader info:\n")?;
        let info_log = self.info_log.as_ref().map(String::as_str).unwrap_or(match self.ok {
            true => "No compile-time error.",
            false => "Unknown error.",
        });
        let preview_code = fmt_preview(self);
        write!(f, "{}\n\n{}", info_log, preview_code)
    }
}

fn fmt_preview(shader: &ShaderInfo) -> String {
    let lines = shader.source_code.split('\n').collect::<Vec<&str>>();
    let lines_num = lines.len();
    let lines_str_len = (lines_num as f32).log10().ceil() as usize;
    let lines_enum = lines.into_iter().enumerate();
    let lines_with_num = lines_enum.map(|(n, l)| format!("{1:0$} : {2}", lines_str_len, n + 1, l));
    let lines_with_num = lines_with_num.collect::<Vec<String>>();
    let code_with_num = lines_with_num.join("\n");
    let error_loc_pfx = "ERROR: 0:";
    if let Some(msg) = shader.info_log.as_ref().map(String::as_str).unwrap_or_default().strip_prefix(error_loc_pfx) {
        let line_num: String = msg.chars().take_while(|c| c.is_digit(10)).collect();
        let line_num = line_num.parse::<usize>().unwrap() - 1;
        let preview_radius = 5;
        let preview_line_start = std::cmp::max(0, line_num - preview_radius);
        let preview_line_end = std::cmp::min(lines_num, line_num + preview_radius);
        lines_with_num[preview_line_start..preview_line_end].join("\n")
    } else {
        code_with_num
    }
}



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
    fn maybe_map_work<U, F>(self, f: F) -> Option<Job<U>>
    where F: FnOnce(T) -> Option<U> {
        let Job { callback, work } = self;
        f(work).map(|work| Job { callback, work })
    }
}



// ==============
// === Shader ===
// ==============

#[derive(Debug)]
struct Shader {
    shader:      WebGlShader,
    source_code: String,
}



// =========================
// === CompilableShaders ===
// =========================

/// Complete program code, ready to be compiled.
#[derive(Debug)]
struct CompilableShaders {
    code: VfPair<String>,
}

impl CompilableShaders {
    /// Starts shaders compiling, unless context is lost.
    fn compile(self, context: &Context) -> Option<LinkableShaders> {
        let types = VfPair { vertex: Context::VERTEX_SHADER, fragment: Context::FRAGMENT_SHADER };
        let shaders =
            self.code.zip(types).maybe_map(|(code, tp)| compile_shader(context, tp, code))?;
        Some(LinkableShaders { shaders })
    }
}

/// Returns a compiling shader, unless context is lost.
fn compile_shader(context: &Context, tp: u32, source_code: String) -> Option<Shader> {
    let shader = context.create_shader(tp)?;
    context.shader_source(&shader, &source_code);
    context.compile_shader(&shader);
    Some(Shader { shader, source_code })
}



// =======================
// === LinkableShaders ===
// =======================

/// Shaders ready to be linked; they may still be compiling.
#[derive(Debug)]
struct LinkableShaders {
    shaders: VfPair<Shader>,
}

impl LinkableShaders {
    /// Starts shaders linking, unless context is lost.
    ///
    /// The underlying API is implicitly async and pipelined. If the shaders are still compiling,
    /// this won't block; using the returned program in any way would block until compilation and
    /// linking are finished.
    fn link(self, context: &Context) -> Option<LinkingProgram> {
        let program = context.create_program()?;
        let shaders = self.shaders;
        shaders.as_ref().for_each(|shader| context.attach_shader(&program, &shader.shader));
        context.link_program(&program);
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
    shaders: VfPair<Shader>,
}

impl LinkingProgram {
    /// Block until compiling and linking is complete, and return the result, unless context was
    /// lost.
    fn block_until_finished(self, context: &Context) -> Option<Result> {
        let LinkingProgram { program, shaders } = self;
        let program_ok = context.get_program_parameter(&program, Context::LINK_STATUS).as_bool()?;
        Some(if program_ok {
            Ok(program)
        } else {
            // If the context is lost we can't determine full error details, but in that case we
            // don't need to handle the error anyway because we'll get another attempt at compiling
            // (and analyzing any error) when we have a new context.
            let link_log = context.get_program_info_log(&program);
            let shaders = shaders.maybe_map(|shader| shader_info(context, shader))?;
            Err(Error { link_log, shaders })
        })
    }
}

/// Return information about shader compilation, unless context was lost.
fn shader_info(context: &Context, shader: Shader) -> Option<ShaderInfo> {
    let Shader { shader, source_code } = shader;
    let param = Context::COMPILE_STATUS;
    let ok = context.get_shader_parameter(&shader, param).as_bool()?;
    let info_log = context.get_shader_info_log(&shader);
    Some(ShaderInfo { ok, info_log, source_code })
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
    /// Select the best available ready-checking implementation for the context.
    fn new(context: &Context) -> Self {
        let khr = KhrParallelShaderCompile::new(context);
        if khr.is_none() && !context.is_context_lost() {
            // TODO: log
        }
        Self { khr }
    }

    /// Check if the job is ready, or return None if unknown.
    fn is_ready(&self, context: &Context, job: &LinkingProgram) -> Option<bool> {
        self.khr.as_ref().and_then(|khr| khr.is_ready(context, job))
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
    fn new(context: &Context) -> Option<Self> {
        let ext = context.get_extension("KHR_parallel_shader_compile").ok()??;
        let completion_status_khr =
            js_sys::Reflect::get(&ext, &"COMPLETION_STATUS_KHR".into()).ok()?.as_f64()? as u32;
        Some(Self { completion_status_khr })
    }

    /// Asynchronously check if the job is ready.
    fn is_ready(&self, context: &Context, job: &LinkingProgram) -> Option<bool> {
        let param = self.completion_status_khr;
        context.get_program_parameter(&job.program, param).as_bool()
    }
}
