//! This module provides an abstraction for the rendering context, such as WebGL or OpenGL one.

use crate::prelude::*;
use web::traits::*;

use crate::system::gpu::data::GlEnum;
use crate::system::gpu::shader;
use crate::system::web;

use web::Closure;
use web_sys::WebGl2RenderingContext;


// ==============
// === Export ===
// ==============

pub mod extension;
pub mod native;
pub mod profiler;



// ===============
// === Context ===
// ===============

/// The rendering context. Currently, we support only the WebGL 2.0 context. In case we would like
/// to support other contexts, this is the type that should be changed to an enum of supported
/// contexts.
///
/// ## Context Loss
///
/// **You can lose the context AT ANY TIME! In other words, you can lose the context part way
/// through initialization. You can also lose the context immediately after calling
/// `canvas.getContext`. You can lose the context between any 2 WebGL function calls.**
///
/// The GPU is a shared resource and as such there are times when it might be taken away from your
/// program. Examples:
/// - Another web page does something that takes the GPU too long and the browser or the OS decides
///   to reset the GPU to get control back.
/// - Tow or more pages use too many resources and the browser decides to tell all the pages they
///   lost the context and then restore it only to the front page for now.
/// - The user switches graphics cards (Turns on/off one or more in the control panel) or updates
///   their driver (no reboot required on Windows7+).
/// - Too many web pages use the GPU context and the browser decides to tell some pages they lost
///   the context in order to allow the newly opened ones to get it.
///
/// In all these cases and more your program may lose its WebGL context. By default, when a WebGL
/// program loses the context it never gets it back. To recover from a lost context you must add
/// a lost context handler and tell it to prevent the default behavior, and then re-setup all your
/// WebGL state and re-create all your WebGL resources when the context is restored.
///
/// This process is pretty complex and touches many places of your program, including WebGL error
/// handling, shaders and programs compilation and linking, WebGL-related variables null-checkers,
/// and many others. To learn more, see https://www.khronos.org/webgl/wiki/HandlingContextLost.
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct Context {
    rc: Rc<ContextData>,
}

/// Internal data of [`Context`].
#[derive(Debug, Deref)]
#[allow(missing_docs)]
pub struct ContextData {
    #[deref]
    native:              native::ContextWithExtensions,
    pub profiler:        profiler::Profiler,
    pub shader_compiler: shader::Compiler,
}

impl Context {
    fn from_native(native: WebGl2RenderingContext) -> Self {
        Self { rc: Rc::new(ContextData::from_native(native)) }
    }
}

impl ContextData {
    fn from_native(native: WebGl2RenderingContext) -> Self {
        let native = native::ContextWithExtensions::from_native(native);
        let profiler = profiler::Profiler::new(&native);
        let shader_compiler = shader::Compiler::new(&native);
        Self { native, profiler, shader_compiler }
    }
}

/// Abstraction for Device Context Handler. This name is used in OpenGL / DirectX implementations.
/// For the web target, this is simply the canvas. As we currently support WebGL 2.0 only, this is
/// simply an alias to a canvas type. See the docs of [`Context`] to learn more about future
/// extension plans.
pub type DeviceContextHandler = web::HtmlCanvasElement;

/// Handler for closures taking care of context restoration. After dropping this handler and losing
/// the context, the context will not be restored automaticaly.
#[derive(Debug)]
pub struct ContextLostHandler {
    on_lost:     web::EventListenerHandle,
    on_restored: web::EventListenerHandle,
}



// ===============
// === Display ===
// ===============

/// Abstraction for entities which contain [`DeviceContextHandler`] and are able to handle context
/// loss. In most cases, these are top-level entities, such as a scene.
#[allow(missing_docs)]
pub trait Display: CloneRef {
    fn device_context_handler(&self) -> &DeviceContextHandler;
    /// Set the GPU context. In most cases, this happens during app initialization or during context
    /// restoration, after the context was lost. See the docs of [`Context`] to learn more.
    fn set_context(&self, context: Option<&Context>);
}



// ==============
// === Errors ===
// ==============

/// Error about unsupported standard implementation, like unsupported WebGL 2.0.
#[derive(Copy, Clone, Debug)]
pub struct UnsupportedStandard(&'static str);

impl std::error::Error for UnsupportedStandard {}

impl fmt::Display for UnsupportedStandard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} is not supported.", self.0)
    }
}



// ============================
// === Initialization Utils ===
// ============================

/// Initialize WebGL 2.0 context.
pub fn init_webgl_2_context<D: Display + 'static>(
    display: &D,
) -> Result<ContextLostHandler, UnsupportedStandard> {
    let hdc = display.device_context_handler();
    let opt_context = hdc.get_webgl2_context();
    match opt_context {
        None => Err(UnsupportedStandard("WebGL 2.0")),
        Some(native) => {
            let context = Context::from_native(native);
            type Handler = web::JsEventHandler;
            display.set_context(Some(&context));
            let lost: Handler = Closure::new(f_!([display]
                warn!("Lost the WebGL context.");
                display.set_context(None)
            ));
            let restored: Handler = Closure::new(f_!([display]
                warn!("Trying to restore the WebGL context.");
                display.set_context(Some(&context))
            ));
            let on_lost = web::add_event_listener(hdc, "webglcontextlost", lost);
            let on_restored = web::add_event_listener(hdc, "webglcontextrestored", restored);
            Ok(ContextLostHandler { on_lost, on_restored })
        }
    }
}



// =================
// === Constants ===
// =================

/// Type-safe WebGL context constants ([`u32`] values) wrapped in `GlEnum`. The list is copy-pasted
/// from [`WebGl2RenderingContext`] implementation. It should not change, as it is defined in the
/// WebGL 2.0 standard.
///
/// Please note that the [`TIMEOUT_IGNORED`] const is skipped, as it is the only [`f64`] const and
/// is not used anywhere.
macro_rules! define_constants {
    ($($name:ident),*$(,)?) => {
        #[allow(missing_docs)]
        impl Context {
            $(pub const $name: GlEnum = GlEnum(WebGl2RenderingContext::$name);)*
        }
    };
}

define_constants![
    READ_BUFFER,
    UNPACK_ROW_LENGTH,
    UNPACK_SKIP_ROWS,
    UNPACK_SKIP_PIXELS,
    PACK_ROW_LENGTH,
    PACK_SKIP_ROWS,
    PACK_SKIP_PIXELS,
    COLOR,
    DEPTH,
    STENCIL,
    RED,
    RGB8,
    RGBA8,
    RGB10_A2,
    TEXTURE_BINDING_3D,
    UNPACK_SKIP_IMAGES,
    UNPACK_IMAGE_HEIGHT,
    TEXTURE_3D,
    TEXTURE_WRAP_R,
    MAX_3D_TEXTURE_SIZE,
    UNSIGNED_INT_2_10_10_10_REV,
    MAX_ELEMENTS_VERTICES,
    MAX_ELEMENTS_INDICES,
    TEXTURE_MIN_LOD,
    TEXTURE_MAX_LOD,
    TEXTURE_BASE_LEVEL,
    TEXTURE_MAX_LEVEL,
    MIN,
    MAX,
    DEPTH_COMPONENT24,
    MAX_TEXTURE_LOD_BIAS,
    TEXTURE_COMPARE_MODE,
    TEXTURE_COMPARE_FUNC,
    CURRENT_QUERY,
    QUERY_RESULT,
    QUERY_RESULT_AVAILABLE,
    STREAM_READ,
    STREAM_COPY,
    STATIC_READ,
    STATIC_COPY,
    DYNAMIC_READ,
    DYNAMIC_COPY,
    MAX_DRAW_BUFFERS,
    DRAW_BUFFER0,
    DRAW_BUFFER1,
    DRAW_BUFFER2,
    DRAW_BUFFER3,
    DRAW_BUFFER4,
    DRAW_BUFFER5,
    DRAW_BUFFER6,
    DRAW_BUFFER7,
    DRAW_BUFFER8,
    DRAW_BUFFER9,
    DRAW_BUFFER10,
    DRAW_BUFFER11,
    DRAW_BUFFER12,
    DRAW_BUFFER13,
    DRAW_BUFFER14,
    DRAW_BUFFER15,
    MAX_FRAGMENT_UNIFORM_COMPONENTS,
    MAX_VERTEX_UNIFORM_COMPONENTS,
    SAMPLER_3D,
    SAMPLER_2D_SHADOW,
    FRAGMENT_SHADER_DERIVATIVE_HINT,
    PIXEL_PACK_BUFFER,
    PIXEL_UNPACK_BUFFER,
    PIXEL_PACK_BUFFER_BINDING,
    PIXEL_UNPACK_BUFFER_BINDING,
    FLOAT_MAT2X3,
    FLOAT_MAT2X4,
    FLOAT_MAT3X2,
    FLOAT_MAT3X4,
    FLOAT_MAT4X2,
    FLOAT_MAT4X3,
    SRGB,
    SRGB8,
    SRGB8_ALPHA8,
    COMPARE_REF_TO_TEXTURE,
    RGBA32F,
    RGB32F,
    RGBA16F,
    RGB16F,
    VERTEX_ATTRIB_ARRAY_INTEGER,
    MAX_ARRAY_TEXTURE_LAYERS,
    MIN_PROGRAM_TEXEL_OFFSET,
    MAX_PROGRAM_TEXEL_OFFSET,
    MAX_VARYING_COMPONENTS,
    TEXTURE_2D_ARRAY,
    TEXTURE_BINDING_2D_ARRAY,
    R11F_G11F_B10F,
    UNSIGNED_INT_10F_11F_11F_REV,
    RGB9_E5,
    UNSIGNED_INT_5_9_9_9_REV,
    TRANSFORM_FEEDBACK_BUFFER_MODE,
    MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS,
    TRANSFORM_FEEDBACK_VARYINGS,
    TRANSFORM_FEEDBACK_BUFFER_START,
    TRANSFORM_FEEDBACK_BUFFER_SIZE,
    TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN,
    RASTERIZER_DISCARD,
    MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS,
    MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS,
    INTERLEAVED_ATTRIBS,
    SEPARATE_ATTRIBS,
    TRANSFORM_FEEDBACK_BUFFER,
    TRANSFORM_FEEDBACK_BUFFER_BINDING,
    RGBA32UI,
    RGB32UI,
    RGBA16UI,
    RGB16UI,
    RGBA8UI,
    RGB8UI,
    RGBA32I,
    RGB32I,
    RGBA16I,
    RGB16I,
    RGBA8I,
    RGB8I,
    RED_INTEGER,
    RGB_INTEGER,
    RGBA_INTEGER,
    SAMPLER_2D_ARRAY,
    SAMPLER_2D_ARRAY_SHADOW,
    SAMPLER_CUBE_SHADOW,
    UNSIGNED_INT_VEC2,
    UNSIGNED_INT_VEC3,
    UNSIGNED_INT_VEC4,
    INT_SAMPLER_2D,
    INT_SAMPLER_3D,
    INT_SAMPLER_CUBE,
    INT_SAMPLER_2D_ARRAY,
    UNSIGNED_INT_SAMPLER_2D,
    UNSIGNED_INT_SAMPLER_3D,
    UNSIGNED_INT_SAMPLER_CUBE,
    UNSIGNED_INT_SAMPLER_2D_ARRAY,
    DEPTH_COMPONENT32F,
    DEPTH32F_STENCIL8,
    FLOAT_32_UNSIGNED_INT_24_8_REV,
    FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING,
    FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE,
    FRAMEBUFFER_ATTACHMENT_RED_SIZE,
    FRAMEBUFFER_ATTACHMENT_GREEN_SIZE,
    FRAMEBUFFER_ATTACHMENT_BLUE_SIZE,
    FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE,
    FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE,
    FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE,
    FRAMEBUFFER_DEFAULT,
    UNSIGNED_INT_24_8,
    DEPTH24_STENCIL8,
    UNSIGNED_NORMALIZED,
    DRAW_FRAMEBUFFER_BINDING,
    READ_FRAMEBUFFER,
    DRAW_FRAMEBUFFER,
    READ_FRAMEBUFFER_BINDING,
    RENDERBUFFER_SAMPLES,
    FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER,
    MAX_COLOR_ATTACHMENTS,
    COLOR_ATTACHMENT1,
    COLOR_ATTACHMENT2,
    COLOR_ATTACHMENT3,
    COLOR_ATTACHMENT4,
    COLOR_ATTACHMENT5,
    COLOR_ATTACHMENT6,
    COLOR_ATTACHMENT7,
    COLOR_ATTACHMENT8,
    COLOR_ATTACHMENT9,
    COLOR_ATTACHMENT10,
    COLOR_ATTACHMENT11,
    COLOR_ATTACHMENT12,
    COLOR_ATTACHMENT13,
    COLOR_ATTACHMENT14,
    COLOR_ATTACHMENT15,
    FRAMEBUFFER_INCOMPLETE_MULTISAMPLE,
    MAX_SAMPLES,
    HALF_FLOAT,
    RG,
    RG_INTEGER,
    R8,
    RG8,
    R16F,
    R32F,
    RG16F,
    RG32F,
    R8I,
    R8UI,
    R16I,
    R16UI,
    R32I,
    R32UI,
    RG8I,
    RG8UI,
    RG16I,
    RG16UI,
    RG32I,
    RG32UI,
    VERTEX_ARRAY_BINDING,
    R8_SNORM,
    RG8_SNORM,
    RGB8_SNORM,
    RGBA8_SNORM,
    SIGNED_NORMALIZED,
    COPY_READ_BUFFER,
    COPY_WRITE_BUFFER,
    COPY_READ_BUFFER_BINDING,
    COPY_WRITE_BUFFER_BINDING,
    UNIFORM_BUFFER,
    UNIFORM_BUFFER_BINDING,
    UNIFORM_BUFFER_START,
    UNIFORM_BUFFER_SIZE,
    MAX_VERTEX_UNIFORM_BLOCKS,
    MAX_FRAGMENT_UNIFORM_BLOCKS,
    MAX_COMBINED_UNIFORM_BLOCKS,
    MAX_UNIFORM_BUFFER_BINDINGS,
    MAX_UNIFORM_BLOCK_SIZE,
    MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS,
    MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS,
    UNIFORM_BUFFER_OFFSET_ALIGNMENT,
    ACTIVE_UNIFORM_BLOCKS,
    UNIFORM_TYPE,
    UNIFORM_SIZE,
    UNIFORM_BLOCK_INDEX,
    UNIFORM_OFFSET,
    UNIFORM_ARRAY_STRIDE,
    UNIFORM_MATRIX_STRIDE,
    UNIFORM_IS_ROW_MAJOR,
    UNIFORM_BLOCK_BINDING,
    UNIFORM_BLOCK_DATA_SIZE,
    UNIFORM_BLOCK_ACTIVE_UNIFORMS,
    UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES,
    UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER,
    UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER,
    INVALID_INDEX,
    MAX_VERTEX_OUTPUT_COMPONENTS,
    MAX_FRAGMENT_INPUT_COMPONENTS,
    MAX_SERVER_WAIT_TIMEOUT,
    OBJECT_TYPE,
    SYNC_CONDITION,
    SYNC_STATUS,
    SYNC_FLAGS,
    SYNC_FENCE,
    SYNC_GPU_COMMANDS_COMPLETE,
    UNSIGNALED,
    SIGNALED,
    ALREADY_SIGNALED,
    TIMEOUT_EXPIRED,
    CONDITION_SATISFIED,
    WAIT_FAILED,
    SYNC_FLUSH_COMMANDS_BIT,
    VERTEX_ATTRIB_ARRAY_DIVISOR,
    ANY_SAMPLES_PASSED,
    ANY_SAMPLES_PASSED_CONSERVATIVE,
    SAMPLER_BINDING,
    RGB10_A2UI,
    INT_2_10_10_10_REV,
    TRANSFORM_FEEDBACK,
    TRANSFORM_FEEDBACK_PAUSED,
    TRANSFORM_FEEDBACK_ACTIVE,
    TRANSFORM_FEEDBACK_BINDING,
    TEXTURE_IMMUTABLE_FORMAT,
    MAX_ELEMENT_INDEX,
    TEXTURE_IMMUTABLE_LEVELS,
    MAX_CLIENT_WAIT_TIMEOUT_WEBGL,
    DEPTH_BUFFER_BIT,
    STENCIL_BUFFER_BIT,
    COLOR_BUFFER_BIT,
    POINTS,
    LINES,
    LINE_LOOP,
    LINE_STRIP,
    TRIANGLES,
    TRIANGLE_STRIP,
    TRIANGLE_FAN,
    ZERO,
    ONE,
    SRC_COLOR,
    ONE_MINUS_SRC_COLOR,
    SRC_ALPHA,
    ONE_MINUS_SRC_ALPHA,
    DST_ALPHA,
    ONE_MINUS_DST_ALPHA,
    DST_COLOR,
    ONE_MINUS_DST_COLOR,
    SRC_ALPHA_SATURATE,
    FUNC_ADD,
    BLEND_EQUATION,
    BLEND_EQUATION_RGB,
    BLEND_EQUATION_ALPHA,
    FUNC_SUBTRACT,
    FUNC_REVERSE_SUBTRACT,
    BLEND_DST_RGB,
    BLEND_SRC_RGB,
    BLEND_DST_ALPHA,
    BLEND_SRC_ALPHA,
    CONSTANT_COLOR,
    ONE_MINUS_CONSTANT_COLOR,
    CONSTANT_ALPHA,
    ONE_MINUS_CONSTANT_ALPHA,
    BLEND_COLOR,
    ARRAY_BUFFER,
    ELEMENT_ARRAY_BUFFER,
    ARRAY_BUFFER_BINDING,
    ELEMENT_ARRAY_BUFFER_BINDING,
    STREAM_DRAW,
    STATIC_DRAW,
    DYNAMIC_DRAW,
    BUFFER_SIZE,
    BUFFER_USAGE,
    CURRENT_VERTEX_ATTRIB,
    FRONT,
    BACK,
    FRONT_AND_BACK,
    CULL_FACE,
    BLEND,
    DITHER,
    STENCIL_TEST,
    DEPTH_TEST,
    SCISSOR_TEST,
    POLYGON_OFFSET_FILL,
    SAMPLE_ALPHA_TO_COVERAGE,
    SAMPLE_COVERAGE,
    NO_ERROR,
    INVALID_ENUM,
    INVALID_VALUE,
    INVALID_OPERATION,
    OUT_OF_MEMORY,
    CW,
    CCW,
    LINE_WIDTH,
    ALIASED_POINT_SIZE_RANGE,
    ALIASED_LINE_WIDTH_RANGE,
    CULL_FACE_MODE,
    FRONT_FACE,
    DEPTH_RANGE,
    DEPTH_WRITEMASK,
    DEPTH_CLEAR_VALUE,
    DEPTH_FUNC,
    STENCIL_CLEAR_VALUE,
    STENCIL_FUNC,
    STENCIL_FAIL,
    STENCIL_PASS_DEPTH_FAIL,
    STENCIL_PASS_DEPTH_PASS,
    STENCIL_REF,
    STENCIL_VALUE_MASK,
    STENCIL_WRITEMASK,
    STENCIL_BACK_FUNC,
    STENCIL_BACK_FAIL,
    STENCIL_BACK_PASS_DEPTH_FAIL,
    STENCIL_BACK_PASS_DEPTH_PASS,
    STENCIL_BACK_REF,
    STENCIL_BACK_VALUE_MASK,
    STENCIL_BACK_WRITEMASK,
    VIEWPORT,
    SCISSOR_BOX,
    COLOR_CLEAR_VALUE,
    COLOR_WRITEMASK,
    UNPACK_ALIGNMENT,
    PACK_ALIGNMENT,
    MAX_TEXTURE_SIZE,
    MAX_VIEWPORT_DIMS,
    SUBPIXEL_BITS,
    RED_BITS,
    GREEN_BITS,
    BLUE_BITS,
    ALPHA_BITS,
    DEPTH_BITS,
    STENCIL_BITS,
    POLYGON_OFFSET_UNITS,
    POLYGON_OFFSET_FACTOR,
    TEXTURE_BINDING_2D,
    SAMPLE_BUFFERS,
    SAMPLES,
    SAMPLE_COVERAGE_VALUE,
    SAMPLE_COVERAGE_INVERT,
    COMPRESSED_TEXTURE_FORMATS,
    DONT_CARE,
    FASTEST,
    NICEST,
    GENERATE_MIPMAP_HINT,
    BYTE,
    UNSIGNED_BYTE,
    SHORT,
    UNSIGNED_SHORT,
    INT,
    UNSIGNED_INT,
    FLOAT,
    DEPTH_COMPONENT,
    ALPHA,
    RGB,
    RGBA,
    LUMINANCE,
    LUMINANCE_ALPHA,
    UNSIGNED_SHORT_4_4_4_4,
    UNSIGNED_SHORT_5_5_5_1,
    UNSIGNED_SHORT_5_6_5,
    FRAGMENT_SHADER,
    VERTEX_SHADER,
    MAX_VERTEX_ATTRIBS,
    MAX_VERTEX_UNIFORM_VECTORS,
    MAX_VARYING_VECTORS,
    MAX_COMBINED_TEXTURE_IMAGE_UNITS,
    MAX_VERTEX_TEXTURE_IMAGE_UNITS,
    MAX_TEXTURE_IMAGE_UNITS,
    MAX_FRAGMENT_UNIFORM_VECTORS,
    SHADER_TYPE,
    DELETE_STATUS,
    LINK_STATUS,
    VALIDATE_STATUS,
    ATTACHED_SHADERS,
    ACTIVE_UNIFORMS,
    ACTIVE_ATTRIBUTES,
    SHADING_LANGUAGE_VERSION,
    CURRENT_PROGRAM,
    NEVER,
    LESS,
    EQUAL,
    LEQUAL,
    GREATER,
    NOTEQUAL,
    GEQUAL,
    ALWAYS,
    KEEP,
    REPLACE,
    INCR,
    DECR,
    INVERT,
    INCR_WRAP,
    DECR_WRAP,
    VENDOR,
    RENDERER,
    VERSION,
    NEAREST,
    LINEAR,
    NEAREST_MIPMAP_NEAREST,
    LINEAR_MIPMAP_NEAREST,
    NEAREST_MIPMAP_LINEAR,
    LINEAR_MIPMAP_LINEAR,
    TEXTURE_MAG_FILTER,
    TEXTURE_MIN_FILTER,
    TEXTURE_WRAP_S,
    TEXTURE_WRAP_T,
    TEXTURE_2D,
    TEXTURE,
    TEXTURE_CUBE_MAP,
    TEXTURE_BINDING_CUBE_MAP,
    TEXTURE_CUBE_MAP_POSITIVE_X,
    TEXTURE_CUBE_MAP_NEGATIVE_X,
    TEXTURE_CUBE_MAP_POSITIVE_Y,
    TEXTURE_CUBE_MAP_NEGATIVE_Y,
    TEXTURE_CUBE_MAP_POSITIVE_Z,
    TEXTURE_CUBE_MAP_NEGATIVE_Z,
    MAX_CUBE_MAP_TEXTURE_SIZE,
    TEXTURE0,
    TEXTURE1,
    TEXTURE2,
    TEXTURE3,
    TEXTURE4,
    TEXTURE5,
    TEXTURE6,
    TEXTURE7,
    TEXTURE8,
    TEXTURE9,
    TEXTURE10,
    TEXTURE11,
    TEXTURE12,
    TEXTURE13,
    TEXTURE14,
    TEXTURE15,
    TEXTURE16,
    TEXTURE17,
    TEXTURE18,
    TEXTURE19,
    TEXTURE20,
    TEXTURE21,
    TEXTURE22,
    TEXTURE23,
    TEXTURE24,
    TEXTURE25,
    TEXTURE26,
    TEXTURE27,
    TEXTURE28,
    TEXTURE29,
    TEXTURE30,
    TEXTURE31,
    ACTIVE_TEXTURE,
    REPEAT,
    CLAMP_TO_EDGE,
    MIRRORED_REPEAT,
    FLOAT_VEC2,
    FLOAT_VEC3,
    FLOAT_VEC4,
    INT_VEC2,
    INT_VEC3,
    INT_VEC4,
    BOOL,
    BOOL_VEC2,
    BOOL_VEC3,
    BOOL_VEC4,
    FLOAT_MAT2,
    FLOAT_MAT3,
    FLOAT_MAT4,
    SAMPLER_2D,
    SAMPLER_CUBE,
    VERTEX_ATTRIB_ARRAY_ENABLED,
    VERTEX_ATTRIB_ARRAY_SIZE,
    VERTEX_ATTRIB_ARRAY_STRIDE,
    VERTEX_ATTRIB_ARRAY_TYPE,
    VERTEX_ATTRIB_ARRAY_NORMALIZED,
    VERTEX_ATTRIB_ARRAY_POINTER,
    VERTEX_ATTRIB_ARRAY_BUFFER_BINDING,
    IMPLEMENTATION_COLOR_READ_TYPE,
    IMPLEMENTATION_COLOR_READ_FORMAT,
    COMPILE_STATUS,
    LOW_FLOAT,
    MEDIUM_FLOAT,
    HIGH_FLOAT,
    LOW_INT,
    MEDIUM_INT,
    HIGH_INT,
    FRAMEBUFFER,
    RENDERBUFFER,
    RGBA4,
    RGB5_A1,
    RGB565,
    DEPTH_COMPONENT16,
    STENCIL_INDEX8,
    DEPTH_STENCIL,
    RENDERBUFFER_WIDTH,
    RENDERBUFFER_HEIGHT,
    RENDERBUFFER_INTERNAL_FORMAT,
    RENDERBUFFER_RED_SIZE,
    RENDERBUFFER_GREEN_SIZE,
    RENDERBUFFER_BLUE_SIZE,
    RENDERBUFFER_ALPHA_SIZE,
    RENDERBUFFER_DEPTH_SIZE,
    RENDERBUFFER_STENCIL_SIZE,
    FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE,
    FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
    FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL,
    FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE,
    COLOR_ATTACHMENT0,
    DEPTH_ATTACHMENT,
    STENCIL_ATTACHMENT,
    DEPTH_STENCIL_ATTACHMENT,
    NONE,
    FRAMEBUFFER_COMPLETE,
    FRAMEBUFFER_INCOMPLETE_ATTACHMENT,
    FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT,
    FRAMEBUFFER_INCOMPLETE_DIMENSIONS,
    FRAMEBUFFER_UNSUPPORTED,
    FRAMEBUFFER_BINDING,
    RENDERBUFFER_BINDING,
    MAX_RENDERBUFFER_SIZE,
    INVALID_FRAMEBUFFER_OPERATION,
    UNPACK_FLIP_Y_WEBGL,
    UNPACK_PREMULTIPLY_ALPHA_WEBGL,
    CONTEXT_LOST_WEBGL,
    UNPACK_COLORSPACE_CONVERSION_WEBGL,
    BROWSER_DEFAULT_WEBGL
];
