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

pub type XResult<T> = std::result::Result<T, Error>;

impl From<JsValue> for Error {
    fn from(t: JsValue) -> Self {
        let message = format!("{:?}", t);
        Self { message }
    }
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



// /// Gets `Element` by ID.
// pub fn get_element_by_id2(id: &str) -> Result<Element> {
//     document
//         .get_element_by_id(id)
//         .ok_or_else(|| Error(format!("Element with id '{}' not found.", id)))
// }



// /// Gets `Element`s by class name.
// pub fn get_elements_by_class_name(name: &str) -> Result<Vec<Element>> {
//     let collection = document.get_elements_by_class_name(name);
//     let indices = 0..collection.length();
//     let elements = indices.flat_map(|index| collection.get_with_index(index)).collect();
//     Ok(elements)
// }

// pub fn get_html_element_by_id(id: &str) -> Result<HtmlElement> {
//     let elem = get_element_by_id2(id)?;
//     elem.dyn_into().map_err(|_| Error("Type cast error."))
// }

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


// pub fn try_request_animation_frame(f: &Closure<dyn FnMut(f64)>) -> Result<i32> {
//     window
//         .request_animation_frame(f.as_ref().unchecked_ref())
//         .map_err(|_| Error("Cannot access 'requestAnimationFrame'."))
// }



/////////////

// #[wasm_bindgen(
//     inline_js = "export function request_animation_frame2(f) { requestAnimationFrame(f) }"
// )]
// extern "C" {
//     #[allow(unsafe_code)]
//     pub fn request_animation_frame2(closure: &Closure<dyn FnMut()>) -> i32;
// }
