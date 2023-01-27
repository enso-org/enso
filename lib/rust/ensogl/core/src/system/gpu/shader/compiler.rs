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
//! The compiler handles [context loss](https://www.khronos.org/webgl/wiki/HandlingContextLost) and
//! does not report compilation errors when the context is not available.
//!
//! # `Compiler` and `Controller`
//!
//! In order to handle WebGL context loss, we divide the responsibilities of compiler
//! management between two objects: a `Compiler`, and a `Controller`.
//!
//! The [`Compiler`] acts as an extension of the context; its state will be lost if the context
//! is lost. It is therefore responsible for keeping track of such information as the
//! currently-running jobs, which will no longer be relevant if context loss occurs.
//!
//! The [`Controller`] is not bound to a context; it holds state that is independent of any
//! particular context object, and uses this state to drive `Compiler` operation.

use crate::control::callback::traits::*;
use crate::prelude::*;
use crate::system::gpu::context::native::traits::*;
use crate::system::web::traits::*;

use crate::animation;
use crate::control::callback;
use crate::display::ToGlEnum;
use crate::system::gpu::context::extension::KhrParallelShaderCompile;
use crate::system::gpu::context::native;
use crate::system::gpu::shader;
use crate::system::gpu::shader::Fragment;
use crate::system::gpu::shader::Shader;
use crate::system::gpu::shader::Vertex;
use crate::system::gpu::Context;
use crate::system::web;
use crate::types::unit2::Duration;

use web_sys::WebGl2RenderingContext;



// =================
// === Constants ===
// =================

/// We do not want the framerate to drop below this value when compiling shaders. Whenever we
/// discover that it drops below this threshold, no more compiler jobs (compilation or linking) will
/// be executed this frame. It does not mean, however, that the framerate will not be lower than
/// this threshold. Every frame at least one job is scheduled, and the last scheduled job can take
/// significant amount of time, causing the FPS to drop below this threshold. To learn more about
/// how jobs are scheduled, read the docs of the [`Compiler`].
const FPS_THRESHOLD: f32 = 60.0;
const FRAME_TIME_THRESHOLD: Duration = (1000.0 / FPS_THRESHOLD).ms();

/// Maximum number of parallel shader compilation jobs. Chromium (and Electron) seem to have limits
/// on the number of compile/link jobs that can be in flight. If the limits are exceeded, it applies
/// a crude form of backpressure by blocking until all pending jobs complete. We are not sure if it
/// is a feature or a bug. To learn more about our findings so far, see:
/// https://github.com/enso-org/enso/pull/3378#issuecomment-1090958946
const MAX_PARALLEL_COMPILE_JOBS: usize = 2;



// ===========
// === Job ===
// ===========

/// Compiler job. After the job is created it can be either transformed to another job, or, in case
/// that was the final job, the [`handler`] callback will be called. See the documentation of the
/// [`Compiler`] to learn more.
#[derive(Derivative, Deref)]
#[derivative(Debug)]
pub struct Job<T> {
    #[derivative(Debug = "ignore")]
    on_ready:  Box<dyn FnOnce(shader::Program)>,
    #[deref]
    input:     T,
    handler:   WeakJobHandler,
    /// A key used to cache the compiled program that this job is making progress on. When the job
    /// is finished or failed, the program or failure status is stored in the cache under this key.
    cache_key: ShaderCacheKey,
    profiler:  profiler::Debug,
}

impl<T> Job<T> {
    fn map_input<S>(self, f: impl FnOnce(T) -> S) -> Job<S> {
        let on_ready = self.on_ready;
        let handler = self.handler;
        let input = f(self.input);
        let profiler = self.profiler;
        let cache_key = self.cache_key;
        Job { on_ready, input, handler, profiler, cache_key }
    }
}

/// A handler to a job. After the handler is dropped, the job is invalidated and will no longer be
/// scheduled for evaluation.
#[derive(Debug, Clone, CloneRef)]
pub struct JobHandler {
    rc: Rc<()>,
}

/// A weak version of [`JobHandler`].
#[derive(Debug, Clone, CloneRef)]
pub struct WeakJobHandler {
    weak: Weak<()>,
}

impl JobHandler {
    /// Constructor.
    fn new() -> Self {
        Self { rc: default() }
    }

    /// Get weak reference to this handler.
    pub fn downgrade(&self) -> WeakJobHandler {
        let weak = Rc::downgrade(&self.rc);
        WeakJobHandler { weak }
    }
}

impl WeakJobHandler {
    /// Check whether the handler was dropped.
    pub fn exists(&self) -> bool {
        self.weak.strong_count() > 0
    }
}



// ==================
// === KhrProgram ===
// ==================

/// A program together with the [`KhrParallelShaderCompile`] extension. Used as one of the
/// [`Compiler`] jobs to provide nice API.
#[derive(Debug)]
struct KhrProgram {
    khr:     KhrParallelShaderCompile,
    program: shader::Program,
}



// ================
// === Progress ===
// ================

/// Shader compiler progress status.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub enum Progress {
    StepProgress,
    NewShaderReady,
}



// ================
// === Compiler ===
// ================

/// Compiler job queues. See the documentation of [`Compiler`] to learn more.
#[derive(Debug, Default)]
pub struct Jobs {
    compile:              Vec<Job<shader::Code>>,
    link:                 Vec<Job<shader::CompiledCode>>,
    read_cache:           Vec<Job<()>>,
    khr_completion_check: Vec<Job<KhrProgram>>,
    link_check:           Vec<Job<shader::Program>>,
}

/// Compiles and links GL shader programs, asynchronously. The compiler works in the following way:
/// 1. A new shader code is submitted with the [`submit`] method. A new job is created in the
///    [`Jobs::compile`] queue and the job handler is returned to the user.
///
/// 2. The following pseudo-algorithm is performed:
/// ```text
/// on_every_frame if job queues are not empty {
///     Promote ready jobs from [`Jobs::khr_completion_check`] to [`Jobs::link_check`].
///     Loop while the current frame time is < FRAME_TIME_THRESHOLD, at least one loop this frame {
///         if [`Jobs::compile`] is not empty {
///             submit its first job to the GLSL compiler
///             move the job to the [`Jobs::link`] queue
///         } else if [`Jobs::link`] is not empty {
///             submit its first job to the GLSL linker
///             if parallel compilation is available move the job to [`Jobs::khr_completion_check`],
///                 or to [`Jobs::link_check`] otherwise.
///         } else if [`Jobs::link_check`] is not empty {
///             check its first job linking status, report warnings, and call the job callback.
///         }
///     }
/// }
/// ```
#[derive(Debug)]
pub struct Compiler {
    cell: RefCell<CompilerData>,
}

#[derive(Debug)]
struct CompilerData {
    dirty:       bool,
    context:     native::ContextWithExtensions,
    jobs:        Jobs,
    cache:       ShaderCache,
    performance: web::Performance,
}

impl Compiler {
    /// Constructor.
    pub fn new(context: &native::ContextWithExtensions) -> Self {
        Self { cell: RefCell::new(CompilerData::new(context)) }
    }

    /// Submit shader for compilation.
    pub fn submit<F: 'static + Fn(shader::Program)>(
        &self,
        input: shader::Code,
        profiler: profiler::Debug,
        on_ready: F,
    ) -> JobHandler {
        self.cell.borrow_mut().submit(input, profiler, on_ready)
    }

    /// Returns `true` if the compiler has no jobs in progress or queued.
    fn idle(&self) -> bool {
        !self.cell.borrow().dirty
    }

    /// Do a time-limited amount of work. Return true if new shaders are available.
    fn run(&self, time: animation::TimeInfo) -> bool {
        let mut compiler = self.cell.borrow_mut();
        let mut new_shaders_available = false;
        if compiler.dirty {
            new_shaders_available = compiler.run(time);
        }
        new_shaders_available
    }
}

impl CompilerData {
    fn new(context: &native::ContextWithExtensions) -> Self {
        let dirty = false;
        let context = context.clone();
        let jobs = default();
        let cache = default();
        let performance = web::window.performance_or_panic();
        Self { dirty, context, jobs, cache, performance }
    }

    fn submit<F: 'static + FnOnce(shader::Program)>(
        &mut self,
        input: shader::Code,
        profiler: profiler::Debug,
        on_ready: F,
    ) -> JobHandler {
        let strong_handler = JobHandler::new();
        let handler = strong_handler.downgrade();
        let on_ready = Box::new(on_ready);
        let cache_key = ShaderCache::code_key(&input);

        match self.cache.get_program(cache_key) {
            Some(ProgramCacheEntry::Submitted) => {
                debug!("Awaiting shader compilation {cache_key:?}.");
                // This shader is currently being processed by another job. Create a new job that
                // will wait for that job to finish.
                let job = Job { input: (), handler, on_ready, profiler, cache_key };
                self.cache.add_to_waiting_list(job);
                self.dirty = true;
            }
            Some(ProgramCacheEntry::Validated(_)) => {
                debug!("Reusing cached shader {cache_key:?}.");
                // This shader has been successfully compiled in the past. Spawn a cache lookup job,
                // so that `on_ready` callback is will be called from within the job runner during
                // next processing round.
                let job = Job { input: (), handler, on_ready, profiler, cache_key };
                self.jobs.read_cache.push(job);
                self.dirty = true;
            }
            Some(ProgramCacheEntry::Failed) => {
                debug!("Submitted a shader that previously failed to compile {cache_key:?}.");
                // This shader failed to compile in the past. Don't spawn any job, as the `on_ready`
                // callback is never supposed to be called anyway when the compilation fails.

                // Because no new job is spawned, the dirty flag does not need to be signalled.
            }
            None => {
                debug!("Submitting shader for compilation {cache_key:?}.");
                self.cache.program_submitted(cache_key);
                let job = Job { input, handler, on_ready, profiler, cache_key };
                self.jobs.compile.push(job);
                self.dirty = true;
            }
        };

        strong_handler
    }

    #[profile(Debug)]
    fn run(&mut self, time: animation::TimeInfo) -> bool {
        let mut any_new_shaders_ready = false;
        self.run_khr_completion_check_jobs();
        while self.dirty {
            match self.run_step() {
                Ok(progress) => {
                    match progress {
                        None => break,
                        Some(Progress::NewShaderReady) => any_new_shaders_ready = true,
                        Some(Progress::StepProgress) => {}
                    }
                    let now = (self.performance.now() as f32).ms();
                    let deadline = time.frame_start() + FRAME_TIME_THRESHOLD;
                    if now > deadline {
                        let msg1 = "Shaders compilation takes more than the available frame time.";
                        let msg2 = "To be continued in the next frame.";
                        trace!("{msg1} {msg2}");
                        break;
                    }
                }
                Err(err) => {
                    if self.context.is_context_lost() {
                        break;
                    }
                    let err_msg = err.blocking_report(&self.context);
                    error!("{err_msg}");
                }
            }
        }
        any_new_shaders_ready
    }

    /// Runs the next compiler job if there is any left and if it will not cause too many jobs being
    /// run in parallel. The result [`bool`] indicates if the call to this function did any
    /// progress.
    fn run_step(&mut self) -> Result<Option<Progress>, Error> {
        let ok_progress = |_| Ok(Some(Progress::StepProgress));
        let no_progress = Ok(None);
        let jobs = &self.jobs;
        let max_jobs = self.current_parallel_job_count() >= MAX_PARALLEL_COMPILE_JOBS;
        match () {
            _ if !max_jobs && !jobs.compile.is_empty() => ok_progress(self.run_next_compile_job()?),
            _ if !jobs.link.is_empty() => ok_progress(self.run_next_link_job()?),
            _ if !jobs.link_check.is_empty() => {
                self.run_next_link_check_job()?;
                Ok(Some(Progress::NewShaderReady))
            }
            _ if !jobs.read_cache.is_empty() => {
                self.run_next_read_cache_job()?;
                Ok(Some(Progress::NewShaderReady))
            }
            _ => {
                if max_jobs {
                    if !jobs.compile.is_empty() {
                        let msg1 = "Maximum number of parallel shader compiler jobs.";
                        let msg2 = "Skipping spawning new ones.";
                        trace!("{msg1} {msg2}");
                    }
                } else if jobs.khr_completion_check.is_empty() {
                    trace!("All shaders compiled.");
                    self.dirty = false;
                }
                no_progress
            }
        }
    }

    /// Get the number of shader compilation jobs run in parallel. Please note, that it is
    /// impossible to get separate values for compilation and linking jobs here, because checking
    /// it is costly and can prevent the parallelism altogether. To learn more, see:
    /// https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices#dont_check_shader_compile_status_unless_linking_fails
    fn current_parallel_job_count(&self) -> usize {
        self.jobs.link.len() + self.jobs.khr_completion_check.len() + self.jobs.link_check.len()
    }

    #[profile(Debug)]
    fn run_khr_completion_check_jobs(&mut self) {
        trace!("Running KHR parallel shader compilation check job.");
        let jobs = &mut self.jobs.khr_completion_check;
        let ready_jobs =
            jobs.drain_filter(|job| match job.khr.is_ready(&self.context, &job.program) {
                Some(val) => val,
                None => {
                    if !self.context.is_context_lost() {
                        reportable_warn!(
                            "context.getProgramParameter returned non-bool value for KHR Parallel \
                            Shader Compile status check. This should never happen, however, it \
                            should not cause visual artifacts. Reverting to non-parallel mode."
                        );
                    }
                    true
                }
            });
        self.jobs.link_check.extend(ready_jobs.map(|job| job.map_input(|t| t.program)));
    }

    #[allow(unused_parens)]
    fn run_next_compile_job(&mut self) -> Result<(), Error> {
        self.with_next_job("shader compilation", (|t| &mut t.jobs.compile), |this, job| {
            let profiler = job.profiler;
            profiler.resume();
            let vertex = this.cache.get_or_insert_vertex_shader(job.cache_key, || {
                CompilerData::compile_shader(&this.context, Vertex, job.input.vertex)
            })?;
            let fragment = this.cache.get_or_insert_fragment_shader(job.cache_key, || {
                CompilerData::compile_shader(&this.context, Fragment, job.input.fragment)
            })?;
            profiler.pause();
            let input = shader::Sources { vertex, fragment };
            let handler = job.handler;
            let on_ready = job.on_ready;
            let cache_key = job.cache_key;
            let link_job = Job { input, handler, on_ready, profiler, cache_key };
            this.jobs.link.push(link_job);
            Ok(())
        })
    }

    #[allow(unused_parens)]
    fn run_next_link_job(&mut self) -> Result<(), Error> {
        self.with_next_job("shader linking", (|t| &mut t.jobs.link), |this, job| {
            let shader = job.input;
            let program = this.context.create_program().ok_or(Error::ProgramCreationError)?;
            let profiler = job.profiler;
            profiler.resume();
            this.context.attach_shader(&program, &shader.vertex);
            this.context.attach_shader(&program, &shader.fragment);
            this.context.link_program(&program);
            profiler.pause();
            let input = shader::Program::new(shader, program);
            let handler = job.handler;
            let on_ready = job.on_ready;
            let cache_key = job.cache_key;
            match this.context.extensions.khr_parallel_shader_compile {
                Some(khr) => {
                    let input = KhrProgram { khr, program: input };
                    let job = Job { input, handler, cache_key, on_ready, profiler };
                    this.jobs.khr_completion_check.push(job)
                }
                None => {
                    let job = Job { input, handler, cache_key, on_ready, profiler };
                    this.jobs.link_check.push(job)
                }
            }
            Ok(())
        })
    }

    #[allow(unused_parens)]
    fn run_next_link_check_job(&mut self) -> Result<(), Error> {
        self.with_next_job("shader validation", (|t| &mut t.jobs.link_check), |this, job| {
            let program = job.input;
            let param = WebGl2RenderingContext::LINK_STATUS;
            job.profiler.resume();
            let status = this.context.get_program_parameter(&program, param);
            job.profiler.finish();
            if !status.as_bool().unwrap_or(false) {
                return Err(Error::ProgramLinkingError(program.shader));
            } else {
                this.cache.program_validated(job.cache_key, program, job.on_ready);
            }
            Ok(())
        })
    }

    #[allow(unused_parens)]
    fn run_next_read_cache_job(&mut self) -> Result<(), Error> {
        self.with_next_job("shader cache read", (|t| &mut t.jobs.read_cache), |this, job| {
            // This job is only scheduled after this cache entry was determined to be successfully
            // processed. `Validated` is the only expected cache entry state at this point.
            match this.cache.get_program(job.cache_key) {
                Some(ProgramCacheEntry::Validated(program)) => {
                    (job.on_ready)(program.clone());
                    Ok(())
                }
                _ => panic!("Shader cache read was submitted for non-completed entry."),
            }
        })?;
        Ok(())
    }

    fn with_next_job<T>(
        &mut self,
        label: &str,
        mut jobs: impl FnMut(&mut Self) -> &mut Vec<Job<T>>,
        f: impl FnOnce(&mut Self, Job<T>) -> Result<(), Error>,
    ) -> Result<(), Error> {
        while let Some(mut job) = jobs(self).pop() {
            let cache_key = job.cache_key;

            if !job.handler.exists() {
                // A compilation job has been cancelled, but there still might be other jobs waiting
                // for it to complete and write data to the cache. Take one of those waiting jobs
                // and resume the compilation process with its callback.
                if let Some(waiting_job) = self.cache.take_next_waiting_job(cache_key) {
                    job.handler = waiting_job.handler;
                    job.on_ready = waiting_job.on_ready;
                } else {
                    // No waiting jobs, cancel the compilation process.
                    self.cache.program_canceled(cache_key);
                    trace!("Job handler dropped, skipping.");
                    continue;
                }
            }

            trace!("Running {label} job.");
            let result = f(self, job);
            if result.is_err() {
                self.cache.program_failed(cache_key);
            }
            return result;
        }
        Ok(())
    }

    fn compile_shader<T: Into<shader::Type>>(
        context: &native::ContextWithExtensions,
        shader_type: T,
        code: String,
    ) -> Result<Shader<T>, Error> {
        let tp = shader_type.into().to_gl_enum();
        let shader = context.create_shader(*tp).ok_or(Error::ShaderCreationError)?;
        context.shader_source(&shader, &code);
        context.compile_shader(&shader);
        Ok(Shader::new(code.into(), shader))
    }
}



// ===================
// === ShaderCache ===
// ===================

/// A compiled shader cache. Stores compiled shader and program objects, hashed by their source
/// code. This allows for reuse compiled shader programs with the same source code, even if they
/// were submitted from different shape system or render layer.
#[derive(Debug, Default)]
struct ShaderCache {
    vertex_shaders:   HashMap<ShaderHash, Shader<Vertex>>,
    fragment_shaders: HashMap<ShaderHash, Shader<Fragment>>,
    programs:         HashMap<ShaderCacheKey, ProgramCacheEntry>,
    waiting_jobs:     HashMap<ShaderCacheKey, Vec<Job<()>>>,
}

#[derive(Debug, Clone)]
enum ProgramCacheEntry {
    /// Shader program is already submitted for compilation.
    Submitted,
    /// Shader program was already compiled and linked successfully.
    Validated(shader::Program),
    /// Compilation of this shader program already failed.
    Failed,
}

impl ShaderCache {
    /// Generate new shader cache key
    fn code_key(code: &shader::Code) -> ShaderCacheKey {
        (ShaderHash::new(&code.vertex), ShaderHash::new(&code.fragment))
    }

    fn get_program(&self, key: ShaderCacheKey) -> Option<&ProgramCacheEntry> {
        self.programs.get(&key)
    }

    fn add_to_waiting_list(&mut self, job: Job<()>) {
        self.waiting_jobs.entry(job.cache_key).or_default().push(job);
    }

    /// Get single job that is awaiting for the completed compilation under specific cache entry.
    /// Only returns jobs that were not cancelled yet. All jobs that were scanned and found to be
    /// cancelled will be removed from the waiting list to avoid processing them in the future.
    fn take_next_waiting_job(&mut self, key: ShaderCacheKey) -> Option<Job<()>> {
        let jobs = self.waiting_jobs.get_mut(&key)?;
        if let Some(first_active) = jobs.iter().position(|job| job.handler.exists()) {
            // Remove all leading cancelled jobs from the list and take the active in single pass.
            // This ensures the minimum number of necessary memory moves and preserves the order of
            // submitted jobs.
            jobs.drain(0..=first_active).next_back()
        } else {
            // All jobs in the waiting list were cancelled.
            self.waiting_jobs.clear();
            None
        }
    }

    fn program_submitted(&mut self, key: ShaderCacheKey) {
        self.programs.insert(key, ProgramCacheEntry::Submitted);
    }

    fn program_failed(&mut self, key: ShaderCacheKey) {
        self.waiting_jobs.remove(&key);
        self.programs.insert(key, ProgramCacheEntry::Failed);
    }

    fn program_canceled(&mut self, key: ShaderCacheKey) {
        self.waiting_jobs.remove(&key);
        self.programs.remove(&key);
    }

    fn program_validated(
        &mut self,
        key: ShaderCacheKey,
        program: shader::Program,
        on_ready: impl FnOnce(shader::Program),
    ) {
        self.programs.insert(key, ProgramCacheEntry::Validated(program.clone()));

        // Complete all jobs that were scheduled for this program. This must be done after the cache
        // entry is updated, in case the job callback schedules another compilation of the same
        // shader program. We want those submissions to recognize cache status as validated and
        // dispatch a read from cache.
        on_ready(program.clone());
        if let Some(jobs) = self.waiting_jobs.remove(&key) {
            for job in jobs {
                if job.handler.exists() {
                    (job.on_ready)(program.clone());
                }
            }
        }
    }

    fn get_or_insert_vertex_shader(
        &mut self,
        key: ShaderCacheKey,
        compile: impl FnOnce() -> Result<Shader<Vertex>, Error>,
    ) -> Result<Shader<Vertex>, Error> {
        Ok(self.vertex_shaders.get_or_insert_with_result(key.0, compile)?.clone())
    }

    fn get_or_insert_fragment_shader(
        &mut self,
        key: ShaderCacheKey,
        compile: impl FnOnce() -> Result<Shader<Fragment>, Error>,
    ) -> Result<Shader<Fragment>, Error> {
        Ok(self.fragment_shaders.get_or_insert_with_result(key.1, compile)?.clone())
    }
}

/// Shader cache entry key. Allows retrieving linked programs or individual compiled vertex and
/// fragment shaders from [`ShaderCache`].
type ShaderCacheKey = (ShaderHash, ShaderHash);

/// A shader source hash value used for cache storage.
/// We do not want to store and compare very long shader source strings in the hashmap. Instead, we
/// rely on the hasher to provide good collision resistance on its own and base the comparison on
/// that.
///
/// Using u64 hash value with default hasher should be resistant enough for the relatively small
/// amount of unique shaders that we expect to hit.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
struct ShaderHash(u64);


impl ShaderHash {
    fn new(code: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        code.hash(&mut hasher);
        Self(hasher.finish())
    }
}



// =============
// === Error ===
// =============

/// Compilation error types.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Error {
    ShaderCreationError,
    ProgramCreationError,
    ProgramLinkingError(shader::CompiledCode),
}

impl Error {
    /// Report the error. This function uses GPU blocking API and thus will cause a significant
    /// performance hit.  Use it only when necessary.
    pub fn blocking_report(self, context: &WebGl2RenderingContext) -> String {
        match self {
            Self::ShaderCreationError => "WebGL was unable to create a new shader.".into(),
            Self::ProgramCreationError => "WebGl was unable to create a new program shader.".into(),
            Self::ProgramLinkingError(shader) => {
                let unwrap_error = |name: &str, err: Option<String>| {
                    let header = format!("----- {} Shader -----", name);
                    err.map(|t| format!("\n\n{}\n\n{}", header, t)).unwrap_or_else(|| "".into())
                };

                let vertex = &shader.vertex;
                let fragment = &shader.fragment;
                let vertex_log = context.blocking_format_error_log(vertex);
                let fragment_log = context.blocking_format_error_log(fragment);
                let vertex_error = unwrap_error("Vertex", vertex_log);
                let fragment_error = unwrap_error("Fragment", fragment_log);

                // FIXME: this should be taken from config and refactored to a function
                let dbg_path = &["enso", "debug", "shader"];
                let dbg_object = web::Reflect::get_nested_object_or_create(&web::window, dbg_path);
                let dbg_object = dbg_object.unwrap();
                let dbg_shaders_count = web::Object::keys(&dbg_object).length();
                let dbg_var_name = format!("shader_{}", dbg_shaders_count);
                let dbg_shader_object = web::Object::new();
                let vertex_code = vertex.code.to_string().into();
                let fragment_code = fragment.code.to_string().into();
                web::Reflect::set(&dbg_object, &(&dbg_var_name).into(), &dbg_shader_object).ok();
                web::Reflect::set(&dbg_shader_object, &"vertex".into(), &vertex_code).ok();
                web::Reflect::set(&dbg_shader_object, &"fragment".into(), &fragment_code).ok();

                let dbg_js_path = format!("window.{}.{}", dbg_path.join("."), dbg_var_name);
                let run_msg = |n| {
                    format!("Run `console.log({}.{})` to inspect the {} shader.", dbg_js_path, n, n)
                };

                let dbg_msg = format!("{}\n{}", run_msg("vertex"), run_msg("fragment"));

                format!(
                    "Unable to compile shader.\n{}\n{}{}",
                    dbg_msg, vertex_error, fragment_error
                )
            }
        }
    }
}



// ==================
// === Controller ===
// ==================

/// Controls the shader compiler.
///
/// While the shader compiler will be thrown away if context is lost, the controller's state is
/// persistent.
///
/// See [`mod`] docs for an explanation of the division of responsibilities between this and
/// [`Compiler`].
#[derive(Debug, Clone, CloneRef, Default)]
pub struct Controller {
    rc: Rc<RefCell<ControllerData>>,
}

#[derive(Debug, Default)]
struct ControllerData {
    on_idle: callback::registry::NoArgs,
}

impl Controller {
    /// Run the compiler. This should be run on every frame. Returns [`true`] if any new shaders
    /// finished the compilation process during this call.
    pub fn run(&self, context: &Context, time: animation::TimeInfo) -> bool {
        self.rc.borrow_mut().run(context, time)
    }

    /// Add a callback to be run when the compiler goes from busy to idle. The callback will remain
    /// installed until the handle returned here is dropped.
    pub fn on_idle(&self, f: impl FnMut() + 'static) -> callback::Handle {
        self.rc.borrow_mut().on_idle(f)
    }
}

impl ControllerData {
    fn run(&mut self, context: &Context, time: animation::TimeInfo) -> bool {
        let was_busy = !context.shader_compiler.idle();
        let result = context.shader_compiler.run(time);
        let now_idle = context.shader_compiler.idle();
        if was_busy && now_idle {
            self.on_idle.run_all();
        }
        result
    }

    fn on_idle(&mut self, f: impl FnMut() + 'static) -> callback::Handle {
        self.on_idle.add(f)
    }
}
