//! This module provides an abstraction for the rendering context, such as WebGL or OpenGL one.

use crate::prelude::*;
use web::traits::*;

use crate::system::web;

use web::Closure;
use web_sys::WebGl2RenderingContext;



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
/// - Too many web pages use the GPU context and the browser decides to tell some of the pages they
///   lost the context in order to allow the newly opened ones to get it.
///
/// In all these cases and more your program may lose its WebGL context. By default when a WebGL
/// program loses the context it never gets it back. To recover from a lost context you must to add
/// a lost context handler and tell it to prevent the default behavior, and then re-setup all your
/// WebGL state and re-create all your WebGL resources when the context is restored.
///
/// This process is pretty complex and touches many places of your program, including WebGL error
/// handling, shaders and programs compilation and linking, WebGL-related variables null-checkers,
/// and many others. To learn more, see https://www.khronos.org/webgl/wiki/HandlingContextLost.
pub type Context = WebGl2RenderingContext;

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



// ======================
// === ContextHandler ===
// ======================

/// Abstraction for entities which contain [`DeviceContextHandler`] and are able to handle context
/// loss. In most cases, these are top-level entities, such as a scene.
#[allow(missing_docs)]
pub trait Display: CloneRef {
    fn device_context_handler(&self) -> &DeviceContextHandler;
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
        Some(context) => {
            type Handler = web::JsEventHandler;
            display.set_context(Some(&context));
            let lost: Handler = Closure::new(f_!(display.set_context(None)));
            let restored: Handler = Closure::new(f_!(display.set_context(Some(&context))));
            let on_lost = web::add_event_listener(hdc, "webglcontextlost", lost);
            let on_restored = web::add_event_listener(hdc, "webglcontextrestored", restored);
            Ok(ContextLostHandler { on_lost, on_restored })
        }
    }
}
