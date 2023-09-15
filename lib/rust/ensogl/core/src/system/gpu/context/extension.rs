//! WebGL extensions management.

use crate::prelude::*;

use crate::system::gpu::data::GlEnum;

use js_sys::Object;
use wasm_bindgen::JsCast;
use web_sys::WebGl2RenderingContext;
use web_sys::WebGlProgram;
use web_sys::WebGlQuery;
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
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct ExtensionsData {
    pub khr_parallel_shader_compile:     Option<KhrParallelShaderCompile>,
    pub ext_disjoint_timer_query_webgl2: Option<ExtDisjointTimerQueryWebgl2>,
    pub webgl_lose_context:              Option<WebglLoseContext>,
}

impl ExtensionsData {
    /// Constructor.
    fn init(context: &WebGl2RenderingContext) -> Self {
        let khr_parallel_shader_compile = KhrParallelShaderCompile::try_init(context);
        let ext_disjoint_timer_query_webgl2 = ExtDisjointTimerQueryWebgl2::try_init(context);
        let webgl_lose_context = WebglLoseContext::try_init(context);
        Self { khr_parallel_shader_compile, ext_disjoint_timer_query_webgl2, webgl_lose_context }
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



// ===================================
// === ExtDisjointTimerQueryWebgl2 ===
// ===================================

/// This extension provides a query mechanism that can be used to determine the amount of time it
/// takes to fully complete a set of GL commands, and without stalling the rendering pipeline.  It
/// uses the query object mechanisms first introduced in the occlusion query extension, which allow
/// time intervals to be polled asynchronously by the application.
///
/// # WARNING: WORKS IN CHROME DESKTOP ONLY
/// Currently (2023) this extension is only available in Chrome Desktop. It was removed from all
/// browsers (including Chrome) due to ["GLitch" exploit](https://www.vusec.net/projects/glitch).
/// However, as Site Isolation shipped in Chrome on Desktop, the extension was re-enabled there
/// because the only data that could be read was data that came from the same origin. To learn more,
/// see: https://bugs.chromium.org/p/chromium/issues/detail?id=1230926
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct ExtDisjointTimerQueryWebgl2 {
    ext: Object,
    pub time_elapsed_ext: GlEnum,
    /// # WARNING: DO NOT USE.
    /// Usage of timestamp queries is not supported. To learn more see:
    /// - https://bugs.chromium.org/p/chromium/issues/detail?id=1442398
    /// - https://bugs.chromium.org/p/chromium/issues/detail?id=595172
    /// - https://bugs.chromium.org/p/chromium/issues/detail?id=1316254&q=TIMESTAMP_EXT&can=2
    pub timestamp_ext: GlEnum,
    pub query_counter_bits_ext: GlEnum,
    pub gpu_disjoint_ext: GlEnum,
    query_counter_ext_fn: js_sys::Function,
}

impl ExtDisjointTimerQueryWebgl2 {
    /// Try to obtain the extension.
    pub fn try_init(context: &WebGl2RenderingContext) -> Option<Self> {
        let ext = context.get_extension("EXT_disjoint_timer_query_webgl2").ok()??;
        let time_elapsed_ext = js_sys::Reflect::get(&ext, &"TIME_ELAPSED_EXT".into()).ok()?;
        let time_elapsed_ext = GlEnum(time_elapsed_ext.as_f64()? as u32);
        let timestamp_ext = js_sys::Reflect::get(&ext, &"TIMESTAMP_EXT".into()).ok()?;
        let timestamp_ext = GlEnum(timestamp_ext.as_f64()? as u32);
        let query_counter_bits_ext =
            js_sys::Reflect::get(&ext, &"QUERY_COUNTER_BITS_EXT".into()).ok()?;
        let query_counter_bits_ext = GlEnum(query_counter_bits_ext.as_f64()? as u32);
        let gpu_disjoint_ext = js_sys::Reflect::get(&ext, &"GPU_DISJOINT_EXT".into()).ok()?;
        let gpu_disjoint_ext = GlEnum(gpu_disjoint_ext.as_f64()? as u32);
        let query_counter_ext_obj = js_sys::Reflect::get(&ext, &"queryCounterEXT".into()).ok()?;
        let query_counter_ext_fn = query_counter_ext_obj.dyn_into::<js_sys::Function>().ok()?;
        Some(Self {
            ext,
            time_elapsed_ext,
            timestamp_ext,
            query_counter_bits_ext,
            gpu_disjoint_ext,
            query_counter_ext_fn,
        })
    }

    /// Query for timestamp at the current GL command queue.
    ///
    /// # WARNING: DO NOT USE.
    /// Usage of timestamp queries is not supported. To learn more see the docs of
    /// [`ExtDisjointTimerQueryWebgl2`].
    pub fn query_timestamp(&self, query: &WebGlQuery) {
        if let Err(err) =
            self.query_counter_ext_fn.call2(&self.ext, query, &(*self.timestamp_ext).into())
        {
            warn!("Error while querying timestamp: {:?}", err);
        }
    }
}



// ========================
// === WebglLoseContext ===
// ========================

/// Supports losing the WebGL Context.
/// See: [https://registry.khronos.org/webgl/extensions/WEBGL_lose_context]
#[derive(Debug, Clone)]
pub struct WebglLoseContext {
    ext: web_sys::WebglLoseContext,
}

impl WebglLoseContext {
    /// Try to obtain the extension.
    pub fn try_init(context: &WebGl2RenderingContext) -> Option<Self> {
        let ext = context.get_extension("WEBGL_lose_context").ok()??;
        let ext = (*ext).clone().into();
        Some(Self { ext })
    }

    /// Lose the WebGL context. This can be useful for testing, or to eagerly release resources.
    pub fn lose_context(&self) {
        self.ext.lose_context();
    }

    /// Restore the WebGL context. This will only succeed if the context was lost by
    /// [`lose_context`].
    pub fn restore_context(&self) {
        self.ext.restore_context();
    }
}
