use crate::prelude::*;

use crate::display::GlEnum;
use crate::display::ToGlEnum;
use enso_shapely::define_singleton_enum;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlProgram;
use web_sys::WebGlShader;

use crate::animation;
use crate::system::gpu::shader;
use crate::system::gpu::shader::Fragment;
use crate::system::gpu::shader::Shader;
use crate::system::gpu::shader::Vertex;
use crate::system::web;
use crate::system::web::traits::*;

use enso_logger::DefaultDebugLogger as Logger;


#[derive(Debug, Clone, CloneRef)]
pub struct ProgramSlot {
    program: Rc<RefCell<Option<WebGlProgram>>>,
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

#[derive(Debug)]
pub struct Job<T> {
    input:        T,
    handler:      WeakJobHandler,
    program_slot: ProgramSlot,
}

impl<T> Job<T> {
    fn replace_input<S>(self, input: S) -> Job<S> {
        let handler = self.handler;
        let program_slot = self.program_slot;
        Job { input, handler, program_slot }
    }
}

pub type CompileJob = Job<shader::Code>;
pub type LinkJob = Job<shader::CompiledCode>;

#[derive(Clone, CloneRef, Debug)]
pub struct Compiler {
    rc: Rc<RefCell<CompilerData>>,
}

impl Compiler {
    pub fn new(context: &WebGl2RenderingContext) -> Self {
        Self { rc: Rc::new(RefCell::new(CompilerData::new(context))) }
    }

    pub fn run(&self, time: animation::TimeInfo) {
        self.rc.borrow_mut().run(time)
    }
}

#[derive(Debug)]
pub struct CompilerData {
    context:      WebGl2RenderingContext,
    compile_jobs: Vec<CompileJob>,
    link_jobs:    Vec<LinkJob>,
    performance:  web::Performance,
    logger:       Logger,
}

impl CompilerData {
    fn new(context: &WebGl2RenderingContext) -> Self {
        let context = context.clone();
        let compile_jobs = default();
        let link_jobs = default();
        let performance = web::window.performance_or_panic();
        let logger = Logger::new("Shader Compiler");
        Self { context, compile_jobs, link_jobs, performance, logger }
    }

    fn submit(&mut self, input: shader::Code, program_slot: &ProgramSlot) -> JobHandler {
        let strong_handler = JobHandler::new();
        let handler = strong_handler.downgrade();
        let program_slot = program_slot.clone_ref();
        let job = Job { input, handler, program_slot };
        self.compile_jobs.push(job);
        strong_handler
    }

    fn run(&mut self, time: animation::TimeInfo) {
        let desired_fps = 60.0;
        let max_frame_time_ms = 1000.0 / desired_fps;
        loop {
            match self.run_step() {
                Ok(did_progress) => {
                    if !did_progress {
                        break;
                    }
                    let now = self.performance.now() as f32;
                    let current_frame_time =
                        now - time.animation_loop_start - time.since_animation_loop_started;
                    if current_frame_time > max_frame_time_ms {
                        debug!(
                            self.logger,
                            "Not all shaders compiled. To be continued in the next frame."
                        );
                        break;
                    }
                }
                Err(err) => {
                    if self.context.is_context_lost() {
                        break;
                    }
                    let err_msg = err.blocking_display();
                    error!(self.logger, "{err_msg}");
                }
            }
        }
    }

    fn run_step(&mut self) -> Result<bool, Error> {
        if !self.compile_jobs.is_empty() {
            self.run_next_compile_job()?;
            Ok(true)
        } else if !self.link_jobs.is_empty() {
            self.run_next_link_job()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn run_next_compile_job(&mut self) -> Result<(), ShaderCreationError> {
        match self.compile_jobs.pop() {
            None => {}
            Some(job) => {
                debug!(self.logger, "Running shader compilation job.");
                if job.handler.exists() {
                    let vertex = self.compile_shader(Vertex, &job.input.vertex)?;
                    let fragment = self.compile_shader(Fragment, &job.input.fragment)?;
                    let input = shader::Sources { vertex, fragment };
                    let link_job = job.replace_input(input);
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
                    let program = self.context.create_program().ok_or(ProgramCreationError)?;
                    self.context.attach_shader(&program, &*job.input.vertex);
                    self.context.attach_shader(&program, &*job.input.fragment);
                    self.context.link_program(&program);
                    let param = WebGl2RenderingContext::LINK_STATUS;
                    let status = self.context.get_program_parameter(&program, param);
                    if !status.as_bool().unwrap_or(false) {
                        Err(ProgramLinkingError(job.input))?
                    }
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
        src: &str,
    ) -> Result<Shader<T>, ShaderCreationError> {
        let tp = shader_type.into().to_gl_enum();
        let shader = self.context.create_shader(*tp).ok_or(ShaderCreationError)?;
        self.context.shader_source(&shader, src);
        self.context.compile_shader(&shader);
        Ok(shader.into())
    }
}

define_singleton_enum! {
    Error {
        ShaderCreationError,
        ProgramCreationError,
        ProgramLinkingError(shader::CompiledCode),
    }
}

impl Error {
    pub fn blocking_display(&self) -> String {
        match self {
            Self::ShaderCreationError => "Unable to create a new shader.".into(),
            Self::ProgramLinkingError => "Unable to create a new program shader.".into(),
            _ => todo!(), /* Self::ProgramLinkingError(shaders) => {
                           *     panic!()
                           *     // let code: String = src.into();
                           *     // let lines = code.split('\n').collect::<Vec<&str>>();
                           *     // let lines_num = lines.len();
                           *     // let lines_str_len = (lines_num as f32).log10().ceil() as
                           * usize;
                           *     // let lines_enum = lines.into_iter().enumerate();
                           *     // let lines_with_num =
                           *     //     lines_enum.map(|(n, l)| format!("{1:0$} : {2}",
                           * lines_str_len, n + 1, l));
                           *     // let lines_with_num = lines_with_num.collect::<Vec<String>>();
                           *     // let code_with_num = lines_with_num.join("\n");
                           *     // let error_loc_pfx = "ERROR: 0:";
                           *     // let preview_code = if let Some(msg) =
                           * message.strip_prefix(error_loc_pfx) {
                           *     //     let line_num: String = msg.chars().take_while(|c|
                           * c.is_digit(10)).collect();
                           *     //     let line_num = line_num.parse::<usize>().unwrap() - 1;
                           *     //     let preview_radius = 5;
                           *     //     let preview_line_start = std::cmp::max(0, line_num -
                           * preview_radius);
                           *     //     let preview_line_end = std::cmp::min(lines_num, line_num
                           * + preview_radius);
                           *     //     lines_with_num[preview_line_start..preview_line_end].join("\n")
                           *     // } else {
                           *     //     code_with_num
                           *     // };
                           * } */
        }
    }
}



// shader::CompiledCode

// pub fn compile_vertex_shader(ctx: &Context, src: &str) -> Result<Shader, SingleTargetError> {
//     compile_shader(ctx, *Context::VERTEX_SHADER, src)
// }
//
// pub fn compile_fragment_shader(ctx: &Context, src: &str) -> Result<Shader, SingleTargetError> {
//     compile_shader(ctx, *Context::FRAGMENT_SHADER, src)
// }
//
