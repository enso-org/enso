#![feature(trait_alias)]
#![feature(set_stdio)]

pub mod resize_observer;
pub mod intersection_observer;

use basegl_prelude::*;

use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use web_sys::HtmlCanvasElement;
use web_sys::WebGl2RenderingContext;
use web_sys::Performance;
use web_sys::Node;
use web_sys::MouseEvent;
use web_sys::EventTarget;
use js_sys::Function;
use std::fmt::Debug;

pub use web_sys::console;
use wasm_bindgen::prelude::*;



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
    #[fail(display = "Failed to add event listener")]
    FailedToAddEventListener,
    #[fail(display = "Failed to remove event listener")]
    FailedToRemoveEventListener
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


// =============
// === Utils ===
// =============

/// Ignores context menu when clicking with the right mouse button.
pub fn ignore_context_menu(target:&EventTarget) -> Result<Closure<dyn FnMut(MouseEvent)>> {
    let closure = move |event:MouseEvent| {
        const RIGHT_MOUSE_BUTTON : i16 = 2;
        if  event.button() == RIGHT_MOUSE_BUTTON {
            event.prevent_default();
        }
    };
    let closure = Closure::wrap(Box::new(closure) as Box<dyn FnMut(MouseEvent)>);
    let callback : &Function = closure.as_ref().unchecked_ref();
    match target.add_event_listener_with_callback("contextmenu", callback) {
        Ok(_)  => Ok(closure),
        Err(_) => Err(Error::FailedToAddEventListener)
    }
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

pub fn window() -> web_sys::Window {
    web_sys::window().unwrap_or_else(|| panic!("Cannot access window object."))
}


pub fn try_window() -> Result<web_sys::Window> {
    web_sys::window().ok_or_else(|| Error::missing("window"))
}

pub fn device_pixel_ratio() -> Result<f64> {
    let win = try_window()?;
    Ok(win.device_pixel_ratio())
}

pub fn document() -> Result<web_sys::Document> {
    try_window()?.document().ok_or_else(|| Error::missing("document"))
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

pub fn get_webgl2_context
(canvas:&HtmlCanvasElement) -> Result<WebGl2RenderingContext> {
    let no_webgl = || Error::NoWebGL { version:2 };
    let context = canvas.get_context("webgl2").map_err(|_| no_webgl())?.ok_or_else(no_webgl)?;
    context.dyn_into().map_err(|_| no_webgl())
}

pub fn request_animation_frame(f:&Closure<dyn FnMut(f64)>) -> Result<i32> {
    let req = try_window()?.request_animation_frame(f.as_ref().unchecked_ref());
    req.map_err(|_| Error::missing("requestAnimationFrame"))
}

pub fn cancel_animation_frame(id:i32) -> Result<()> {
    let req = try_window()?.cancel_animation_frame(id);
    req.map_err(|_| Error::missing("cancel_animation_frame"))
}

pub fn get_performance() -> Result<Performance> {
    try_window()?.performance().ok_or_else(|| Error::missing("performance"))
}



// =====================
// === Other Helpers ===
// =====================

/// Trait used to set HtmlElement attributes.
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

/// Trait used to set css styles.
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

/// Trait used to insert `Node`s.
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

/// Trait used to remove `Node`s.
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

#[wasm_bindgen(inline_js = "export function request_animation_frame2(f) { requestAnimationFrame(f) }")]
extern "C" {
    pub fn request_animation_frame2(closure: &Closure<dyn FnMut()>) -> i32;
}



// ===============
// === Printer ===
// ===============

type PrintFn = fn(&str) -> std::io::Result<()>;

struct Printer {
    printfn: PrintFn,
    buffer: String,
    is_buffered: bool,
}

impl Printer {
    fn new(printfn: PrintFn, is_buffered: bool) -> Printer {
        Printer {
            buffer: String::new(),
            printfn,
            is_buffered,
        }
    }
}

impl std::io::Write for Printer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buffer.push_str(&String::from_utf8_lossy(buf));

        if !self.is_buffered {
            (self.printfn)(&self.buffer)?;
            self.buffer.clear();

            return Ok(buf.len());
        }

        if let Some(i) = self.buffer.rfind('\n') {
            let buffered = {
                let (first, last) = self.buffer.split_at(i);
                (self.printfn)(first)?;

                String::from(&last[1..])
            };

            self.buffer.clear();
            self.buffer.push_str(&buffered);
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        (self.printfn)(&self.buffer)?;
        self.buffer.clear();

        Ok(())
    }
}

fn _print(msg: &str) -> std::io::Result<()> {
    web_sys::console::info_1(&msg.to_string().into());
    Ok(())
}


pub fn set_stdout() {
    let printer = Printer::new(_print, true);
    std::io::set_print(Some(Box::new(printer)));
}

pub fn set_stdout_unbuffered() {
    let printer = Printer::new(_print, false);
    std::io::set_print(Some(Box::new(printer)));
}

#[wasm_bindgen(inline_js = "
export function set_stack_trace_limit() {
    Error.stackTraceLimit = 100
}
")]
extern "C" {
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
