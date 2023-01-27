//! Extensions for the native [`WebGl2RenderingContext`] implementation.

use crate::prelude::*;

use crate::system::gpu::context::extension::Extensions;
use crate::system::gpu::shader::Shader;

use web_sys::WebGl2RenderingContext;
use web_sys::WebGlProgram;



#[allow(missing_docs)]
pub mod traits {
    pub use super::BlockingCheckStatus;
    pub use super::BlockingGetErrorLog;
    pub use super::ContextOps;
}


// =============================
// === ContextWithExtensions ===
// =============================

/// Native WebGL context and supported [`Extensions`].
#[derive(Clone, Debug, Deref)]
#[allow(missing_docs)]
pub struct ContextWithExtensions {
    #[deref]
    native:         WebGl2RenderingContext,
    pub extensions: Extensions,
}

impl ContextWithExtensions {
    /// Constructor.
    pub fn from_native(native: WebGl2RenderingContext) -> Self {
        let extensions = Extensions::init(&native);
        Self { native, extensions }
    }
}



// ==================
// === ContextOps ===
// ==================

/// Extensions to [`WebGl2RenderingContext`].
pub trait ContextOps {
    /// Combination of `use_program(Some(program))` and `self.use_program(None)`.
    fn with_program<T>(&self, program: &WebGlProgram, f: impl FnOnce() -> T) -> T;
}

impl ContextOps for WebGl2RenderingContext {
    fn with_program<T>(&self, program: &WebGlProgram, f: impl FnOnce() -> T) -> T {
        self.use_program(Some(program));
        let out = f();
        self.use_program(None);
        out
    }
}



// ===========================
// === BlockingCheckStatus ===
// ===========================

/// Status check for the given WebGL entity. For shaders, it checks the compilation status, for
/// shader programs, the linking status, and then, returns the compilation/linking logs if any.
/// This operation is blocking and should be used only when necessary. To learn more, see:
/// [https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices#compile_shaders_and_link_programs_in_parallel]
#[allow(missing_docs)]
pub trait BlockingCheckStatus<T> {
    fn blocking_get_status_logs(&self, target: &T) -> Option<String>;
}

impl<T> BlockingCheckStatus<Shader<T>> for WebGl2RenderingContext {
    fn blocking_get_status_logs(&self, target: &Shader<T>) -> Option<String> {
        let status = WebGl2RenderingContext::COMPILE_STATUS;
        let ok = self.get_shader_parameter(target, status).as_bool().unwrap_or(false);
        (!ok).then_some(unwrap_error(self.get_shader_info_log(target)))
    }
}

impl BlockingCheckStatus<WebGlProgram> for WebGl2RenderingContext {
    fn blocking_get_status_logs(&self, target: &WebGlProgram) -> Option<String> {
        let status = WebGl2RenderingContext::LINK_STATUS;
        let ok = self.get_program_parameter(target, status).as_bool().unwrap_or(false);
        (!ok).then_some(unwrap_error(self.get_program_info_log(target)))
    }
}

fn unwrap_error(opt_err: Option<String>) -> String {
    opt_err.unwrap_or_else(|| "Unknown error.".into())
}



// ===========================
// === BlockingGetErrorLog ===
// ===========================

/// Get a nicely formatted compilation error log for the given shader. The log will contain a
/// preview of the code where the error occurred. This operation is blocking and should be used
/// only when necessary. To learn more, see:
/// [https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices#compile_shaders_and_link_programs_in_parallel]
#[allow(missing_docs)]
pub trait BlockingGetErrorLog {
    fn blocking_format_error_log<T>(&self, shader: &Shader<T>) -> Option<String>;
}

impl BlockingGetErrorLog for WebGl2RenderingContext {
    fn blocking_format_error_log<T>(&self, shader: &Shader<T>) -> Option<String> {
        self.blocking_get_status_logs(shader).map(|message| {
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
                let line_num: String = msg.chars().take_while(|c| c.is_ascii_digit()).collect();
                let line_num = line_num.parse::<usize>().unwrap().saturating_sub(1);
                let preview_radius = 5;
                let preview_line_start = std::cmp::max(0, line_num.saturating_sub(preview_radius));
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
