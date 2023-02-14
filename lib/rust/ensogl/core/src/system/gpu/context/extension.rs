//! WebGL extensions management.

use crate::prelude::*;

use crate::system::gpu::data::GlEnum;

use web_sys::WebGl2RenderingContext;
use web_sys::WebGlProgram;
use web_sys::WebGlShader;



// ==================
// === Extensions ===
// ==================

/// Set of all extensions that we try to enable after acquiring the context.
#[derive(Debug, Clone, Deref)]
pub struct Extensions {
    rc: Rc<ExtensionsData>,
}

impl Extensions {
    /// Constructor.
    pub fn init(context: &WebGl2RenderingContext) -> Self {
        Self { rc: Rc::new(ExtensionsData::init(context)) }
    }
}

/// Internal representation of [`Extensions`].
#[derive(Copy, Clone, Debug)]
#[allow(missing_docs)]
pub struct ExtensionsData {
    pub khr_parallel_shader_compile: Option<KhrParallelShaderCompile>,
}

impl ExtensionsData {
    /// Constructor.
    fn init(context: &WebGl2RenderingContext) -> Self {
        let khr_parallel_shader_compile = KhrParallelShaderCompile::try_init(context);
        Self { khr_parallel_shader_compile }
    }
}



// ================================
// === KhrParallelShaderCompile ===
// ================================

/// The `KHR_parallel_shader_compile` extension is used to poll shader compilation status without
/// blocking. To learn more, see:
/// [https://www.khronos.org/registry/webgl/extensions/KHR_parallel_shader_compile]
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct KhrParallelShaderCompile {
    pub completion_status_khr: GlEnum,
}

impl KhrParallelShaderCompile {
    /// Try to obtain the extension.
    pub fn try_init(context: &WebGl2RenderingContext) -> Option<Self> {
        let ext = context.get_extension("KHR_parallel_shader_compile").ok()??;
        let completion_status_khr = GlEnum(
            js_sys::Reflect::get(&ext, &"COMPLETION_STATUS_KHR".into()).ok()?.as_f64()? as u32,
        );
        Some(Self { completion_status_khr })
    }

    /// Asynchronously check if the job is ready. Returns [`None`] if it was impossible to get this
    /// information. This can happen during context loss or driver failure.
    pub fn is_program_ready(
        &self,
        context: &WebGl2RenderingContext,
        program: &WebGlProgram,
    ) -> Option<bool> {
        context.get_program_parameter(program, *self.completion_status_khr).as_bool()
    }

    /// Asynchronously check if the job is ready. Returns [`None`] if it was impossible to get this
    /// information. This can happen during context loss or driver failure.
    pub fn is_shader_ready(
        &self,
        context: &WebGl2RenderingContext,
        program: &WebGlShader,
    ) -> Option<bool> {
        context.get_shader_parameter(program, *self.completion_status_khr).as_bool()
    }
}
