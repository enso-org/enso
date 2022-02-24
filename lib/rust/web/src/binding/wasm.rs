use enso_prelude::*;

pub use js_sys::Function;
pub use js_sys::Object;
pub use wasm_bindgen::prelude::Closure;
pub use wasm_bindgen::prelude::*;
pub use wasm_bindgen::JsCast;
pub use wasm_bindgen::JsValue;
pub use web_sys::console;
pub use web_sys::AddEventListenerOptions;
pub use web_sys::CanvasRenderingContext2d;
pub use web_sys::Document;
pub use web_sys::Element;
pub use web_sys::Event;
pub use web_sys::EventTarget;
pub use web_sys::HtmlCanvasElement;
pub use web_sys::HtmlCollection;
pub use web_sys::HtmlDivElement;
pub use web_sys::HtmlElement;
pub use web_sys::KeyboardEvent;
pub use web_sys::MouseEvent;
pub use web_sys::Node;
pub use web_sys::WebGl2RenderingContext;
pub use web_sys::WheelEvent;
pub use web_sys::Window;


use crate::Logger;
use enso_logger::*;


pub use std::time::Duration;
pub use std::time::Instant;


// lazy_static! {
//     static ref window: Rc<Window> = Rc::new(window());
// }


// =============
// === Error ===
// =============

/// Generic error representation. We may want to support errors in form of structs and enums,
/// but it requires significant work, so a simpler solution was chosen for now.
#[derive(Debug, Fail)]
#[fail(display = "{}", message)]
pub struct Error {
    message: String,
}

#[allow(non_snake_case)]
pub fn Error<S: Into<String>>(message: S) -> Error {
    let message = message.into();
    Error { message }
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<JsValue> for Error {
    fn from(t: JsValue) -> Self {
        let message = format!("{:?}", t);
        Self { message }
    }
}


// ==============
// === String ===
// ==============

#[wasm_bindgen]
extern "C" {
    #[allow(unsafe_code)]
    #[wasm_bindgen(js_name = "String")]
    fn js_to_string_inner(s: &JsValue) -> String;
}

/// Converts given `JsValue` into a `String`. Uses JS's `String` function,
/// see: https://www.w3schools.com/jsref/jsref_string.asp
pub fn js_to_string(s: impl AsRef<JsValue>) -> String {
    js_to_string_inner(s.as_ref())
}


// ===================
// === DOM Helpers ===
// ===================


macro_rules! wasm_lazy_global {
    ($name:ident : $tp:ty = $expr:expr) => {
        pub mod $name {
            use super::*;
            pub static mut STORE: Option<$tp> = None;
            pub struct Ref {}
        }

        impl Deref for $name::Ref {
            type Target = $tp;
            fn deref(&self) -> &Self::Target {
                unsafe {
                    $name::STORE.as_ref().unwrap_or_else(|| {
                        let val = $expr;
                        $name::STORE = Some(val);
                        $name::STORE.as_ref().unwrap()
                    })
                }
            }
        }

        pub const $name: $name::Ref = $name::Ref {};
    };
}

wasm_lazy_global! { window : Window = web_sys::window().unwrap() }
wasm_lazy_global! { document : Document = window.document().unwrap() }



/// Gets `Element` by ID.
pub fn get_element_by_id2(id: &str) -> Result<Element> {
    document
        .get_element_by_id(id)
        .ok_or_else(|| Error(format!("Element with id '{}' not found.", id)))
}

/// Tries to get `Element` by ID, and runs function on it.
pub fn with_element_by_id_or_warn<F>(logger: &Logger, id: &str, f: F)
where F: FnOnce(Element) {
    let root_elem = get_element_by_id2(id);
    match root_elem {
        Ok(v) => f(v),
        Err(_) => warning!(logger, "Failed to get element by ID."),
    }
}

/// Gets `Element`s by class name.
pub fn get_elements_by_class_name(name: &str) -> Result<Vec<Element>> {
    let collection = document.get_elements_by_class_name(name);
    let indices = 0..collection.length();
    let elements = indices.flat_map(|index| collection.get_with_index(index)).collect();
    Ok(elements)
}

pub fn get_html_element_by_id(id: &str) -> Result<HtmlElement> {
    let elem = get_element_by_id2(id)?;
    elem.dyn_into().map_err(|_| Error("Type cast error."))
}

// pub fn try_create_element(name: &str) -> Result<Element> {
//     document.create_element(name).map_err(|_| Error(format!("Cannot create element '{}'", name)))
// }
//
// pub fn create_element(name: &str) -> Element {
//     try_create_element(name).unwrap()
// }

// pub fn try_create_div() -> Result<HtmlDivElement> {
//     try_create_element("div").map(|t| t.unchecked_into())
// }
//
// pub fn create_div() -> HtmlDivElement {
//     create_element("div").unchecked_into()
// }
//
// pub fn try_create_canvas() -> Result<HtmlCanvasElement> {
//     try_create_element("canvas").map(|t| t.unchecked_into())
// }
//
// pub fn create_canvas() -> HtmlCanvasElement {
//     create_element("canvas").unchecked_into()
// }

/// Get WebGL 2.0 context. Returns [`None`] if WebGL 2.0 is not supported.
pub fn get_webgl2_context(canvas: &HtmlCanvasElement) -> Option<WebGl2RenderingContext> {
    let options = js_sys::Object::new();
    js_sys::Reflect::set(&options, &"antialias".into(), &false.into()).unwrap();
    let context = canvas.get_context_with_context_options("webgl2", &options).ok().flatten();
    context.and_then(|obj| obj.dyn_into::<WebGl2RenderingContext>().ok())
}

pub fn try_request_animation_frame(f: &Closure<dyn FnMut(f64)>) -> Result<i32> {
    window
        .request_animation_frame(f.as_ref().unchecked_ref())
        .map_err(|_| Error("Cannot access 'requestAnimationFrame'."))
}



/////////////

#[wasm_bindgen(
    inline_js = "export function request_animation_frame2(f) { requestAnimationFrame(f) }"
)]
extern "C" {
    #[allow(unsafe_code)]
    pub fn request_animation_frame2(closure: &Closure<dyn FnMut()>) -> i32;
}



// ===============
// === Printer ===
// ===============

#[wasm_bindgen(inline_js = "
export function set_stack_trace_limit() {
    Error.stackTraceLimit = 100
}
")]
extern "C" {
    #[allow(unsafe_code)]
    pub fn set_stack_trace_limit();
}


/// Enables forwarding panic messages to `console.error`.
pub fn forward_panic_hook_to_console() {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    console_error_panic_hook::set_once();
}


/// Enables throwing a descriptive JavaScript error on panics.
pub fn forward_panic_hook_to_error() {
    std::panic::set_hook(Box::new(error_throwing_panic_hook));
}

#[wasm_bindgen(module = "/js/rust_panic.js")]
extern "C" {
    #[allow(unsafe_code)]
    fn new_panic_error(message: String) -> JsValue;
}

fn error_throwing_panic_hook(panic_info: &std::panic::PanicInfo) {
    wasm_bindgen::throw_val(new_panic_error(panic_info.to_string()));
}

#[wasm_bindgen]
pub fn entry_point_panic() {
    forward_panic_hook_to_error();
    panic!();
}



/// Sleeps for the specified amount of time.
///
/// This function might sleep for slightly longer than the specified duration but never less.
///
/// This function is an async version of std::thread::sleep, its timer starts just after the
/// function call.
#[cfg(target_arch = "wasm32")]
pub async fn sleep(duration: Duration) {
    use gloo_timers::future::TimeoutFuture;

    TimeoutFuture::new(duration.as_millis() as u32).await
}

#[cfg(not(target_arch = "wasm32"))]
pub use async_std::task::sleep;

/// Get the nested value of the provided object. This is similar to writing `foo.bar.baz` in
/// JavaScript, but in a safe manner, while checking if the value exists on each level.
pub fn reflect_get_nested(target: &JsValue, keys: &[&str]) -> Result<JsValue> {
    let mut tgt = target.clone();
    for key in keys {
        let obj = tgt.dyn_into::<js_sys::Object>()?;
        let key = (*key).into();
        tgt = js_sys::Reflect::get(&obj, &key)?;
    }
    Ok(tgt)
}

/// Get the nested value of the provided object and cast it to [`Object`]. See docs of
/// [`reflect_get_nested`] to learn more.
pub fn reflect_get_nested_object(target: &JsValue, keys: &[&str]) -> Result<js_sys::Object> {
    let tgt = reflect_get_nested(target, keys)?;
    Ok(tgt.dyn_into()?)
}

/// Get the nested value of the provided object and cast it to [`String`]. See docs of
/// [`reflect_get_nested`] to learn more.
pub fn reflect_get_nested_string(target: &JsValue, keys: &[&str]) -> Result<String> {
    let tgt = reflect_get_nested(target, keys)?;
    if tgt.is_undefined() {
        Err(Error("Key was not present in the target."))
    } else {
        Ok(js_to_string(&tgt))
    }
}

/// Get all the keys of the provided [`Object`].
pub fn object_keys(target: &JsValue) -> Vec<String> {
    target
        .clone()
        .dyn_into::<js_sys::Object>()
        .ok()
        .map(|obj| {
            js_sys::Object::keys(&obj)
                .iter()
                .map(|key| {
                    // The unwrap is safe, the `Object::keys` API guarantees it.
                    let js_str = key.dyn_into::<js_sys::JsString>().unwrap();
                    js_str.into()
                })
                .collect()
        })
        .unwrap_or_default()
}
