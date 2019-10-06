use crate::prelude::*;

use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use web_sys::HtmlCanvasElement;
use web_sys::WebGlRenderingContext;

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

// ===================
// === JS Bindings ===
// ===================

#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);

    pub fn setInterval(closure: &Closure<dyn FnMut()>, time: u32) -> i32;
    pub fn clearInterval2(id: i32);

    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);

    pub fn unsafe_sleep(ms: u32);

    pub fn observe_canvas(closure: &Closure<dyn FnMut(i32, i32)>);
}

#[macro_export]
macro_rules! console_log {
    ($($t:tt)*) => ($crate::console::log_1(&format_args!($($t)*).to_string().into()))
}

// ===================
// === DOM Helpers ===
// ===================

pub fn window() -> Result<web_sys::Window> {
    web_sys::window().ok_or(Error::missing("window"))
}

pub fn document() -> Result<web_sys::Document> {
    window()?.document().ok_or(Error::missing("document"))
}

pub fn get_element_by_id(id: &str) -> Result<web_sys::Element> {
    document()?.get_element_by_id(id).ok_or(Error::missing(id))
}

pub fn get_element_by_id_as<T: wasm_bindgen::JsCast>(id: &str) -> Result<T> {
    let elem = get_element_by_id(id)?;
    let expected = type_name::<T>();
    let got = format!("{:?}", elem);
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
    let context = canvas.get_context(name).map_err(|_| no_webgl())?.ok_or(no_webgl())?;
    context.dyn_into().map_err(|_| no_webgl())
}

pub fn request_animation_frame(f: &Closure<dyn FnMut()>) -> Result<i32> {
    let req = window()?.request_animation_frame(f.as_ref().unchecked_ref());
    req.map_err(|_| Error::missing("requestAnimationFrame"))
}
