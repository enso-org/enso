//! This module provides an abstraction for the rendering context, such as WebGL or OpenGL one.

use web_sys::WebGl2RenderingContext;



// =============
// === Types ===
// =============

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
