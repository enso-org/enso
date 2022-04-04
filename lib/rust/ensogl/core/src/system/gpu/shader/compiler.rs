use crate::prelude::*;

use crate::display::GlEnum;
use crate::display::ToGlEnum;
use enso_shapely::define_singleton_enum;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlProgram;
use web_sys::WebGlShader;

use crate::animation;
use crate::system::gpu::context::NativeContextWithExtensions;
use crate::system::gpu::shader;
use crate::system::gpu::shader::traits::*;
use crate::system::gpu::shader::Fragment;
use crate::system::gpu::shader::Shader;
use crate::system::gpu::shader::Vertex;
use crate::system::web;
use crate::system::web::traits::*;

use crate::system::gpu::shader::CompilationTarget;
use enso_logger::DefaultDebugLogger as Logger;


#[derive(Debug, Clone, CloneRef, Default)]
pub struct ProgramSlot {
    program: Rc<RefCell<Option<WebGlProgram>>>,
}

impl ProgramSlot {
    pub fn get(&self) -> Option<WebGlProgram> {
        self.program.borrow().clone()
    }

    pub fn set(&self, program: WebGlProgram) {
        *self.program.borrow_mut() = Some(program);
    }
}

#[derive(Debug, Clone, CloneRef)]
pub struct JobHandler {
    rc: Rc<()>,
}

impl JobHandler {
    fn new() -> Self {
        Self { rc: default() }
    }

    pub fn downgrade(&self) -> WeakJobHandler {
        let weak = Rc::downgrade(&self.rc);
        WeakJobHandler { weak }
    }
}

#[derive(Debug, Clone, CloneRef)]
pub struct WeakJobHandler {
    weak: Weak<()>,
}

impl WeakJobHandler {
    pub fn exists(&self) -> bool {
        self.weak.upgrade().is_some()
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Job<T> {
    #[derivative(Debug = "ignore")]
    on_ready: Box<dyn Fn(WebGlProgram)>,
    input:    T,
    handler:  WeakJobHandler,
}

pub type CompileJob = Job<shader::Code>;
pub type LinkJob = Job<shader::CompiledCode>;
pub type CompletionCheckJob = Job<(WebGlProgram, shader::CompiledCode)>;
pub type LinkCheckJob = Job<(WebGlProgram, shader::CompiledCode)>;

#[derive(Clone, CloneRef, Debug)]
pub struct Compiler {
    rc: Rc<RefCell<CompilerData>>,
}

impl Compiler {
    pub fn new(context: &NativeContextWithExtensions) -> Self {
        Self { rc: Rc::new(RefCell::new(CompilerData::new(context))) }
    }

    pub fn submit<F: 'static + Fn(WebGlProgram)>(
        &self,
        input: shader::Code,
        on_ready: F,
    ) -> JobHandler {
        self.rc.borrow_mut().submit(input, on_ready)
    }

    pub fn run(&self, time: animation::TimeInfo) {
        self.rc.borrow_mut().run(time)
    }
}

#[derive(Debug)]
pub struct CompilerData {
    dirty:                     bool,
    context:                   NativeContextWithExtensions,
    compile_jobs:              Vec<CompileJob>,
    link_jobs:                 Vec<LinkJob>,
    khr_completion_check_jobs: Vec<CompletionCheckJob>,
    link_check_jobs:           Vec<LinkCheckJob>,
    performance:               web::Performance,
    logger:                    Logger,
}

impl CompilerData {
    fn new(context: &NativeContextWithExtensions) -> Self {
        let dirty = false;
        let context = context.clone();
        let compile_jobs = default();
        let link_jobs = default();
        let khr_completion_check_jobs = default();
        let link_check_jobs = default();
        let performance = web::window.performance_or_panic();
        let logger = Logger::new("Shader Compiler");
        Self {
            dirty,
            context,
            compile_jobs,
            link_jobs,
            khr_completion_check_jobs,
            link_check_jobs,
            performance,
            logger,
        }
    }

    fn submit<F: 'static + Fn(WebGlProgram)>(
        &mut self,
        input: shader::Code,
        on_ready: F,
    ) -> JobHandler {
        self.dirty = true;
        let strong_handler = JobHandler::new();
        let handler = strong_handler.downgrade();
        let on_ready = Box::new(on_ready);
        let job = Job { input, handler, on_ready };
        self.compile_jobs.push(job);
        strong_handler
    }

    fn run(&mut self, time: animation::TimeInfo) {
        // FIXME: hardcoded values - these should be discovered.
        let desired_fps = 60.0;
        let max_frame_time_ms = 1000.0 / desired_fps;
        if self.dirty {
            self.run_khr_completion_check_jobs();
            while self.dirty {
                match self.run_step() {
                    Ok(_) => {
                        if !self.dirty {
                            break;
                        }
                        let now = self.performance.now() as f32;
                        let current_frame_time =
                            now - time.animation_loop_start - time.since_animation_loop_started;
                        if current_frame_time > max_frame_time_ms {
                            let msg1 =
                                "Shaders compilation takes more than the available frame time.";
                            let msg2 = "To be continued in the next frame.";
                            debug!(self.logger, "{msg1} {msg2}");
                            break;
                        }
                    }
                    Err(err) => {
                        if self.context.is_context_lost() {
                            break;
                        }
                        let err_msg = err.blocking_report(&self.context);
                        error!(self.logger, "{err_msg}");
                    }
                }
            }
        }
    }

    fn run_step(&mut self) -> Result<(), Error> {
        if !self.compile_jobs.is_empty() {
            self.run_next_compile_job()?;
        } else if !self.link_jobs.is_empty() {
            self.run_next_link_job()?;
        } else if !self.link_check_jobs.is_empty() {
            self.run_next_link_check_job()?;
        } else {
            if self.khr_completion_check_jobs.is_empty() {
                debug!(self.logger, "All shaders compiled.");
                self.dirty = false;
            }
        }

        Ok(())
    }

    fn run_next_compile_job(&mut self) -> Result<(), Error> {
        match self.compile_jobs.pop() {
            None => {}
            Some(job) => {
                debug!(self.logger, "Running shader compilation job.");
                if job.handler.exists() {
                    let vertex = self.compile_shader(Vertex, job.input.vertex)?;
                    let fragment = self.compile_shader(Fragment, job.input.fragment)?;
                    let input = shader::Sources { vertex, fragment };
                    let handler = job.handler;
                    let on_ready = job.on_ready;
                    let link_job = Job { input, handler, on_ready };
                    self.link_jobs.push(link_job);
                } else {
                    debug!(self.logger, "Job handler dropped, skipping.");
                }
            }
        }
        Ok(())
    }

    fn run_next_link_job(&mut self) -> Result<(), Error> {
        match self.link_jobs.pop() {
            None => {}
            Some(job) => {
                debug!(self.logger, "Running shader linking job.");
                if job.handler.exists() {
                    let program =
                        self.context.create_program().ok_or(Error::ProgramCreationError)?;
                    self.context.attach_shader(&program, &*job.input.vertex);
                    self.context.attach_shader(&program, &*job.input.fragment);
                    self.context.link_program(&program);
                    let input = (program, job.input);
                    let handler = job.handler;
                    let on_ready = job.on_ready;
                    let validate_job = Job { input, handler, on_ready };
                    if self.context.extensions.khr_parallel_shader_compile.is_some() {
                        self.khr_completion_check_jobs.push(validate_job);
                    } else {
                        self.link_check_jobs.push(validate_job);
                    }
                } else {
                    debug!(self.logger, "Job handler dropped, skipping.");
                }
            }
        }
        Ok(())
    }

    fn run_khr_completion_check_jobs(&mut self) {
        debug!(self.logger, "Running KHR parallel shader compilation check job.");
        let khr = self.context.extensions.khr_parallel_shader_compile.as_ref().unwrap();
        let ready_jobs = self
            .khr_completion_check_jobs
            .drain_filter(|job| khr.is_ready(&*self.context, &job.input.0));
        self.link_check_jobs.extend(ready_jobs);
    }

    fn run_next_link_check_job(&mut self) -> Result<(), Error> {
        match self.link_check_jobs.pop() {
            None => {}
            Some(job) => {
                debug!(self.logger, "Running shader validation job.");
                if job.handler.exists() {
                    let param = WebGl2RenderingContext::LINK_STATUS;
                    let status = self.context.get_program_parameter(&job.input.0, param);
                    if !status.as_bool().unwrap_or(false) {
                        Err(Error::ProgramLinkingError(job.input.1))?
                    }
                    (job.on_ready)(job.input.0);
                } else {
                    debug!(self.logger, "Job handler dropped, skipping.");
                }
            }
        }
        Ok(())
    }

    fn compile_shader<T: Into<shader::Type>>(
        &self,
        shader_type: T,
        code: String,
    ) -> Result<Shader<T>, Error> {
        let tp = shader_type.into().to_gl_enum();
        let shader = self.context.create_shader(*tp).ok_or(Error::ShaderCreationError)?;
        self.context.shader_source(&shader, &code);
        self.context.compile_shader(&shader);
        Ok(Shader::new(code, shader))
    }
}

#[derive(Debug)]
pub enum Error {
    ShaderCreationError,
    ProgramCreationError,
    ProgramLinkingError(shader::CompiledCode),
}

impl Error {
    pub fn blocking_report(self, context: &WebGl2RenderingContext) -> String {
        let fatal = |msg| {
            format!(
                "This is a browser error. Please report it here ... and give us this ... {}",
                msg
            )
        };
        match self {
            Self::ShaderCreationError => fatal("WebGL was unable to create a new shader."),
            Self::ProgramCreationError => fatal("WebGl was unable to create a new program shader."),
            Self::ProgramLinkingError(shader) => {
                let unwrap_error = |name: &str, err: Option<String>| {
                    let header = format!("----- {} Shader -----", name);
                    err.map(|t| format!("\n\n{}\n\n{}", header, t)).unwrap_or("".into())
                };

                let vertex = &shader.vertex;
                let fragment = &shader.fragment;
                let vertex_log = context.blocking_format_error_log(&vertex);
                let fragment_log = context.blocking_format_error_log(&fragment);
                let vertex_error = unwrap_error("Vertex", vertex_log);
                let fragment_error = unwrap_error("Fragment", fragment_log);

                // FIXME: this should be taken from config and refactored to a function
                let dbg_path = &["enso", "debug", "shader"];
                let dbg_object = web::Reflect::get_nested_object_or_create(&web::window, dbg_path);
                let dbg_object = dbg_object.unwrap();
                let dbg_shaders_count = web::Object::keys(&dbg_object).length();
                let dbg_var_name = format!("shader_{}", dbg_shaders_count);
                let dbg_shader_object = web::Object::new();
                let vertex_code = vertex.code.clone().into();
                let fragment_code = fragment.code.clone().into();
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
