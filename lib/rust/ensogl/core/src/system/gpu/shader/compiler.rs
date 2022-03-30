use web_sys::WebGlProgram;



// ================
// === Compiler ===
// ================

/// Compiles and links GL shader programs, asynchronously.
#[derive(Default)]
pub struct Compiler {
    /// Programs waiting to be submitted for compilation.
    new_jobs:     Vec<NewJob>,
    /// Programs that are ready to be polled for completion.
    started_jobs: Vec<StartedJob>,
}

impl Compiler {
    /// Compile a shader program; the provided callback will be called when it is ready.
    pub fn compile(&mut self, code: ProgramCode, callback: Box<dyn F>)
    where F: FnOnce(Result<WebGlProgram, Error>) {
        self.jobs.push(NewJob { code, callback });
    }

    /// Submit all new jobs to the WebGL engine for compilation.
    pub fn start_jobs(&mut self) {}

    /// Poll the WebGL engine for compilation status of started jobs.
    pub fn handle_finished_jobs(&mut self) {}
}

/// GLSL source code of a shader program.
pub struct ProgramCode;

pub struct Error;

/// A program that has been submitted for compilation, but is not known to be ready for use yet.
struct Compilation(WebGlProgram);

struct NewJob {
    code:     ProgramCode,
    callback: Box<dyn FnOnce(WebGlProgram)>,
}

struct StartedJob {
    compilation: Compilation,
    callback:    Box<dyn FnOnce(WebGlProgram)>,
}
