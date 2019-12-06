#![feature(trait_alias)]

pub mod resize_observer;
mod animationframeloop;
pub use animationframeloop::AnimationFrameLoop;

use basegl_prelude::*;

use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;
use web_sys::HtmlCanvasElement;
use web_sys::WebGlRenderingContext;
use web_sys::Performance;
use web_sys::Node;
use std::fmt::Debug;

pub use web_sys::console;


// =============
// === Error ===
// =============

pub type Result<A> = std::result::Result<A, Error>;

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
    pub fn missing(name:&str) -> Error {
        let name = name.to_string();
        Error::Missing { name }
    }

    pub fn type_mismatch(expected:&str, got:&str) -> Error {
        let expected = expected.to_string();
        let got = got.to_string();
        Error::TypeMismatch { expected, got }
    }
}


// ===================
// === JS Bindings ===
// ===================

#[macro_export]
macro_rules! console_log {
    ($($t:tt)*) => ($crate::console::log_1(&format_args!($($t)*)
                                    .to_string().into()))
}


// ==============
// === LogMsg ===
// ==============

pub trait LogMsg {
    fn with_log_msg<F: FnOnce(&str) -> T, T>(&self, f:F) -> T;
}

impl LogMsg for &str {
    fn with_log_msg<F: FnOnce(&str) -> T, T>(&self, f:F) -> T {
        f(self)
    }
}

impl<F: Fn() -> S, S: AsRef<str>> LogMsg for F {
    fn with_log_msg<G: FnOnce(&str) -> T, T>(&self, f:G) -> T {
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
    pub fn new<T: AsRef<str>>(path:T) -> Self {
        let path = path.as_ref().to_string();
        Self { path }
    }

    // FIXME: Default
    pub fn new_() -> Self {
        Self::new("")
    }

    pub fn sub<T: AsRef<str>>(&self, path:T) -> Self {
        Self::new(format!("{}.{}", self.path, path.as_ref()))
    }

    pub fn trace<M: LogMsg>(&self, msg:M) {
        console::debug_1(&self.format(msg));
    }

    pub fn info<M: LogMsg>(&self, msg:M) {
        // console::info_1(&self.format(msg));
        console::group_1(&self.format(msg));
        console::group_end();
    }

    pub fn warning<M: LogMsg>(&self, msg:M) {
        console::warn_1(&self.format(msg));
    }

    pub fn error<M: LogMsg>(&self, msg:M) {
        console::error_1(&self.format(msg));
    }

    pub fn group_begin<M: LogMsg>(&self, msg:M) {
        console::group_1(&self.format(msg));
    }

    pub fn group_end(&self) {
        console::group_end();
    }

    pub fn group<M:LogMsg, T, F:FnOnce() -> T>(&self, msg:M, f:F) -> T {
        self.group_begin(msg);
        let out = f();
        self.group_end();
        out
    }

    fn format<M: LogMsg>(&self, msg:M) -> JsValue {
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
    ($logger:expr, $str:expr, $a1:expr, $body:tt) => {{
        group!($logger, format!($str,$a1), $body)
    }};
    ($logger:expr, $str:expr, $a1:expr, $a2:expr, $body:tt) => {{
        group!($logger, format!($str,$a1,$a2), $body)
    }};
    ($logger:expr, $str:expr, $a1:expr, $a2:expr, $a3:expr, $body:tt) => {{
        group!($logger, format!($str,$a1,$a2,$a3), $body)
    }};
}

// ===================
// === DOM Helpers ===
// ===================

pub fn dyn_into<T,U>(obj :T) -> Result<U>
where T : wasm_bindgen::JsCast + Debug,
      U : wasm_bindgen::JsCast
{
    let expected = type_name::<T>();
    let got = format!("{:?}", obj);
    obj.dyn_into().map_err(|_| Error::type_mismatch(&expected, &got))
}

pub fn window() -> Result<web_sys::Window> {
    web_sys::window().ok_or_else(|| Error::missing("window"))
}

pub fn device_pixel_ratio() -> Result<f64> {
    let win = window()?;
    Ok(win.device_pixel_ratio())
}

pub fn document() -> Result<web_sys::Document> {
    window()?.document().ok_or_else(|| Error::missing("document"))
}

pub fn get_element_by_id(id:&str) -> Result<web_sys::Element> {
    document()?.get_element_by_id(id).ok_or_else(|| Error::missing(id))
}

#[deprecated(note = "Use get_element_by_id with dyn_into instead")]
pub fn get_element_by_id_as<T:wasm_bindgen::JsCast>(id:&str) -> Result<T> {
    let elem = get_element_by_id(id)?;
    dyn_into(elem)
}
pub fn create_element(id:&str) -> Result<web_sys::Element> {
    match document()?.create_element(id) {
        Ok(element) => Ok(element),
        Err(_) => Err(Error::missing(id)),
    }
}

pub fn get_canvas(id:&str) -> Result<web_sys::HtmlCanvasElement> {
    dyn_into(get_element_by_id(id)?)
}

pub fn get_webgl_context(
    canvas: &HtmlCanvasElement,
    version: u32,
) -> Result<WebGlRenderingContext>
{
    let no_webgl = || Error::NoWebGL { version };
    let name_sfx = if version == 1 {
        "".to_string()
    } else {
        version.to_string()
    };
    let name = &format!("webgl{}", &name_sfx);
    let context = canvas.get_context(name)
                        .map_err(|_| no_webgl())?.ok_or_else(no_webgl)?;
    context.dyn_into().map_err(|_| no_webgl())
}

pub fn request_animation_frame(f:&Closure<dyn FnMut()>) -> Result<i32> {
    let req = window()?.request_animation_frame(f.as_ref().unchecked_ref());
    req.map_err(|_| Error::missing("requestAnimationFrame"))
}

pub fn cancel_animation_frame(id:i32) -> Result<()> {
    let req = window()?.cancel_animation_frame(id);
    req.map_err(|_| Error::missing("cancel_animation_frame"))
}

pub fn get_performance() -> Result<Performance> {
    window()?.performance().ok_or_else(|| Error::missing("performance"))
}


// =====================
// === Other Helpers ===
// =====================

pub trait AttributeSetter {
    fn set_attribute_or_panic<T, U>(&self, name:T, value:U)
            where T : AsRef<str>,
                  U : AsRef<str>;
}

impl AttributeSetter for web_sys::HtmlElement {
    fn set_attribute_or_panic<T,U>(&self, name:T, value:U)
            where T : AsRef<str>,
                  U : AsRef<str> {
        let name   = name.as_ref();
        let value  = value.as_ref();
        let values = format!("\"{}\" = \"{}\" on \"{:?}\"",name,value,self);
        self.set_attribute(name,value)
            .unwrap_or_else(|_| panic!("Failed to set attribute {}", values));
    }
}

pub trait StyleSetter {
    fn set_property_or_panic<T,U>(&self, name:T, value:U)
            where T : AsRef<str>,
                  U : AsRef<str>;
}

impl StyleSetter for web_sys::HtmlElement {
    fn set_property_or_panic<T,U>(&self, name:T, value:U)
            where T : AsRef<str>,
                  U : AsRef<str> {
        let name   = name.as_ref();
        let value  = value.as_ref();
        let values = format!("\"{}\" = \"{}\" on \"{:?}\"",name,value,self);
        let panic_msg = |_| panic!("Failed to set style {}",values);
        self.style().set_property(name, value).unwrap_or_else(panic_msg);
    }
}

pub trait NodeInserter {
    fn append_or_panic (&self, node:&Node);
    fn prepend_or_panic(&self, node:&Node);
    fn insert_before_or_panic(&self,node:&Node,reference_node:&Node);
}

impl NodeInserter for Node {
    fn append_or_panic(&self, node:&Node) {
        let panic_msg = |_|
            panic!("Failed to append child {:?} to {:?}",node,self);
        self.append_child(node).unwrap_or_else(panic_msg);
    }

    fn prepend_or_panic(&self, node : &Node) {
        let panic_msg = |_|
            panic!("Failed to prepend child \"{:?}\" to \"{:?}\"",node,self);

        let first_c = self.first_child();
        self.insert_before(node, first_c.as_ref()).unwrap_or_else(panic_msg);
    }
    fn insert_before_or_panic(&self, node:&Node, ref_node:&Node) {
        let panic_msg = |_|
            panic!("Failed to insert {:?} before {:?} in {:?}",
                   node,
                   ref_node,
                   self);
        self.insert_before(node, Some(ref_node)).unwrap_or_else(panic_msg);
    }
}

pub trait NodeRemover {
    fn remove_child_or_panic(&self, node:&Node);
}

impl NodeRemover for Node {
    fn remove_child_or_panic(&self, node:&Node) {
        let panic_msg = |_|
            panic!("Failed to remove child {:?} from {:?}",node,self);
        self.remove_child(node).unwrap_or_else(panic_msg);
    }
}
