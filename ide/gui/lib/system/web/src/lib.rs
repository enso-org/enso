#![feature(trait_alias)]

pub mod resize_observer;

use basegl_prelude::*;

use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;
use web_sys::HtmlCanvasElement;
use web_sys::WebGlRenderingContext;

pub use web_sys::console;

// =============
// === Error ===
// =============

type Result<A> = std::result::Result<A, Error>;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "Missing `{}`.", name)]
    Missing { name: String },
    #[fail(display = "Type mismatch. Expected `{}`, got `{}`.", expected, got)]
    TypeMismatch { expected: String, got: String },
    #[fail(display = "WebGL {} is not available.", version)]
    NoWebGL { version: u32 },
}
impl Error {
    pub fn missing(name: &str) -> Error {
        let name = name.to_string();
        Error::Missing { name }
    }

    pub fn type_mismatch(expected: &str, got: &str) -> Error {
        let expected = expected.to_string();
        let got = got.to_string();
        Error::TypeMismatch { expected, got }
    }
}

// ==============
// === LogMsg ===
// ==============

pub trait LogMsg {
    fn with_log_msg<F: FnOnce(&str) -> T, T>(&self, f: F) -> T;
}

impl LogMsg for &str {
    fn with_log_msg<F: FnOnce(&str) -> T, T>(&self, f: F) -> T {
        f(self)
    }
}

impl<F: Fn() -> S, S: AsRef<str>> LogMsg for F {
    fn with_log_msg<G: FnOnce(&str) -> T, T>(&self, f: G) -> T {
        f(self().as_ref())
    }
}

// ==============
// === Logger ===
// ==============

#[derive(Clone, Debug)]
pub struct Logger {
    pub path: String,
}
impl Logger {
    pub fn new<T: AsRef<str>>(path: T) -> Self {
        let path = path.as_ref().to_string();
        Self { path }
    }

    // FIXME: Default
    pub fn new_() -> Self {
        Self::new("")
    }

    pub fn sub<T: AsRef<str>>(&self, path: T) -> Self {
        Self::new(format!("{}.{}", self.path, path.as_ref()))
    }

    pub fn trace<M: LogMsg>(&self, msg: M) {
        console::debug_1(&self.format(msg));
    }

    pub fn info<M: LogMsg>(&self, msg: M) {
        // console::info_1(&self.format(msg));
        console::group_1(&self.format(msg));
        console::group_end();
    }

    pub fn warning<M: LogMsg>(&self, msg: M) {
        console::warn_1(&self.format(msg));
    }

    pub fn error<M: LogMsg>(&self, msg: M) {
        console::error_1(&self.format(msg));
    }

    pub fn group_begin<M: LogMsg>(&self, msg: M) {
        console::group_1(&self.format(msg));
    }

    pub fn group_end(&self) {
        console::group_end();
    }

    pub fn group<M: LogMsg, T, F: FnOnce() -> T>(&self, msg: M, f: F) -> T {
        self.group_begin(msg);
        let out = f();
        self.group_end();
        out
    }

    fn format<M: LogMsg>(&self, msg: M) -> JsValue {
        msg.with_log_msg(|s| format!("[{}] {}", self.path, s)).into()
    }
}

impl Default for Logger {
    fn default() -> Self {
        Self::new("")
    }
}

// ====================
// === Logger Utils ===
// ====================

#[macro_export]
macro_rules! fmt {
    ($($arg:tt)*) => (||(format!($($arg)*)))
}

#[macro_export]
macro_rules! group {
    ($logger:expr, $message:expr, $body:tt) => {{
        $logger.group_begin(|| $message);
        let out = $body;
        $logger.group_end();
        out
    }};
}

// ===================
// === DOM Helpers ===
// ===================

pub fn window() -> Result<web_sys::Window> {
    web_sys::window().ok_or_else(|| Error::missing("window"))
}

pub fn document() -> Result<web_sys::Document> {
    window()?.document().ok_or_else(|| Error::missing("document"))
}

pub fn get_element_by_id(id: &str) -> Result<web_sys::Element> {
    document()?.get_element_by_id(id).ok_or_else(|| Error::missing(id))
}

pub fn get_element_by_id_as<T: wasm_bindgen::JsCast>(id: &str) -> Result<T> {
    let elem = get_element_by_id(id)?;
    let expected = type_name::<T>(); // fixme
    let got = format!("{:?}", elem); // fixme
    elem.dyn_into().map_err(|_| Error::type_mismatch(&expected, &got))
}

pub fn get_canvas(id: &str) -> Result<web_sys::HtmlCanvasElement> {
    get_element_by_id_as(id)
}

pub fn get_webgl_context(
    canvas: &HtmlCanvasElement,
    version: u32,
) -> Result<WebGlRenderingContext>
{
    let no_webgl = || Error::NoWebGL { version };
    let name_sfx = if version == 1 { "".to_string() } else { version.to_string() };
    let name = &format!("webgl{}", &name_sfx);
    let context = canvas.get_context(name).map_err(|_| no_webgl())?.ok_or_else(no_webgl)?;
    context.dyn_into().map_err(|_| no_webgl())
}

pub fn request_animation_frame(f: &Closure<dyn FnMut()>) -> Result<i32> {
    let req = window()?.request_animation_frame(f.as_ref().unchecked_ref());
    req.map_err(|_| Error::missing("requestAnimationFrame"))
}

pub fn cancel_animation_frame(id: i32) -> Result<()> {
    let req = window()?.cancel_animation_frame(id);
    req.map_err(|_| Error::missing("cancel_animation_frame"))
}
