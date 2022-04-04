use crate::prelude::*;

use crate::system::gpu::data::GlEnum;
use crate::system::gpu::Context;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlProgram;



// ================================
// === KhrParallelShaderCompile ===
// ================================

/// Use the `KHR_parallel_shader_compile` extension to poll status without blocking.
///
/// See: [https://www.khronos.org/registry/webgl/extensions/KHR_parallel_shader_compile]
#[derive(Debug)]
pub struct KhrParallelShaderCompile {
    completion_status_khr: GlEnum,
}

impl KhrParallelShaderCompile {
    /// Try to obtain the extension.
    pub fn init(context: &WebGl2RenderingContext) -> Option<Self> {
        let ext = context.get_extension("KHR_parallel_shader_compile").ok()??;
        let completion_status_khr = GlEnum(
            js_sys::Reflect::get(&ext, &"COMPLETION_STATUS_KHR".into()).ok()?.as_f64()? as u32,
        );
        Some(Self { completion_status_khr })
    }

    /// Asynchronously check if the job is ready.
    pub fn is_ready(&self, context: &WebGl2RenderingContext, program: &WebGlProgram) -> bool {
        let param = self.completion_status_khr;
        context.get_program_parameter(program, *param).as_bool().unwrap_or_else(|| {
            // FIXME: log info about how to report it
            WARNING!("context.getProgramParameter returned non bool value for KHR Parallel Shader Compile status check. This should never happen, however, it should not cause visual artifacts. Reverting to non-parallel mode.");
            true
        })
    }
}
