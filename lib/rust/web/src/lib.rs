#![allow(incomplete_features)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![feature(trait_alias)]
#![feature(negative_impls)]
#![feature(specialization)]
#![feature(auto_traits)]
//
#![allow(unused_doc_comments)]
#![allow(clippy::boxed_local)]

pub mod binding;
pub mod clipboard;
pub mod closure;
pub mod event;
pub mod platform;
pub mod resize_observer;
pub mod stream;

/// Common types that should be visible across the whole crate.
pub mod prelude {
    pub use super::traits::*;

    pub use super::Closure;
    pub use super::EventTarget;
    pub use super::Function;
    pub use super::HtmlDivElement;
    pub use super::HtmlElement;
    pub use super::JsCast;
    pub use super::JsValue;
    pub use super::Object;
    pub use enso_logger::DefaultInfoLogger as Logger;
    pub use enso_logger::*;
    pub use enso_prelude::*;
}

pub mod traits {
    pub use super::AttributeSetter;
    pub use super::DocumentOps;
    pub use super::HtmlCanvasElementOps;
    pub use super::JsCast;
    pub use super::NodeInserter;
    pub use super::NodeRemover;
    pub use super::ObjectOps;
    pub use super::ReflectOps;
    pub use super::StyleSetter;
}

use crate::prelude::*;

use enso_logger::warning;
use enso_logger::WarningLogger as Logger;


pub use std::time::Duration;
pub use std::time::Instant;


#[cfg(target_arch = "wasm32")]
pub use binding::wasm::*;

#[cfg(not(target_arch = "wasm32"))]
pub use binding::mock::*;



// ==================
// === ReflectOps ===
// ==================

pub trait ReflectOps {
    /// Get the nested value of the provided object. This is similar to writing `foo.bar.baz` in
    /// JavaScript, but in a safe manner, while checking if the value exists on each level.
    fn get_nested(target: &JsValue, keys: &[&str]) -> Result<JsValue, JsValue>;

    /// Get the nested value of the provided object and cast it to [`Object`]. See docs of
    /// [`get_nested`] to learn more.
    fn get_nested_object(target: &JsValue, keys: &[&str]) -> Result<Object, JsValue>;

    /// Get the nested value of the provided object and cast it to [`String`]. See docs of
    /// [`get_nested`] to learn more.
    fn get_nested_string(target: &JsValue, keys: &[&str]) -> Result<String, JsValue>;
}

impl ReflectOps for Reflect {
    fn get_nested(target: &JsValue, keys: &[&str]) -> Result<JsValue, JsValue> {
        let mut tgt = target.clone();
        for key in keys {
            let obj = tgt.dyn_into::<Object>()?;
            let key = (*key).into();
            tgt = Reflect::get(&obj, &key)?;
        }
        Ok(tgt)
    }

    fn get_nested_object(target: &JsValue, keys: &[&str]) -> Result<Object, JsValue> {
        let tgt = Self::get_nested(target, keys)?;
        Ok(tgt.dyn_into()?)
    }

    fn get_nested_string(target: &JsValue, keys: &[&str]) -> Result<String, JsValue> {
        let tgt = Self::get_nested(target, keys)?;
        let str = tgt.dyn_into::<JsString>()?;
        // FIXME: this seems better:
        // Ok(String::from(str))
        Ok(js_to_string(&str))
    }
}


pub trait ObjectOps {
    /// Get all the keys of the provided [`Object`].
    fn keys_vec(obj: &Object) -> Vec<String>;
}

impl ObjectOps for Object {
    #[cfg(target_arch = "wasm32")]
    fn keys_vec(_obj: &Object) -> Vec<String> {
        // The [`unwrap`] is safe, the `Object::keys` API guarantees it.
        Object::keys(&obj)
            .iter()
            .map(|key| key.dyn_into::<js_sys::JsString>().unwrap().into())
            .collect()
    }
    #[cfg(not(target_arch = "wasm32"))]
    fn keys_vec(_obj: &Object) -> Vec<String> {
        default()
    }
}



// =================================
// === Generic String Conversion ===
// =================================

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
extern "C" {
    #[allow(unsafe_code)]
    #[wasm_bindgen(js_name = "String")]
    fn js_to_string_inner(s: &JsValue) -> String;
}

#[cfg(target_arch = "wasm32")]
/// Converts given `JsValue` into a `String`. Uses JS's `String` function,
/// see: https://www.w3schools.com/jsref/jsref_string.asp
pub fn js_to_string(s: impl AsRef<JsValue>) -> String {
    js_to_string_inner(s.as_ref())
}

#[cfg(not(target_arch = "wasm32"))]
pub fn js_to_string(_: impl AsRef<JsValue>) -> String {
    "JsValue".into()
}



// ===================
// === DocumentOps ===
// ===================

pub trait DocumentOps {
    fn body_or_panic(&self) -> HtmlElement;
    fn create_element_or_panic(&self, local_name: &str) -> Element;
    fn create_div_or_panic(&self) -> HtmlDivElement;
    fn create_canvas_or_panic(&self) -> HtmlCanvasElement;
    fn get_html_element_by_id(&self, id: &str) -> Option<HtmlElement>;
    fn get_webgl2_context(&self, _canvas: &HtmlCanvasElement) -> Option<WebGl2RenderingContext>;
}

impl DocumentOps for Document {
    fn body_or_panic(&self) -> HtmlElement {
        self.body().unwrap()
    }

    fn create_element_or_panic(&self, local_name: &str) -> Element {
        self.create_element(local_name).unwrap()
    }


    fn create_div_or_panic(&self) -> HtmlDivElement {
        self.create_element_or_panic("div").unchecked_into()
    }

    fn create_canvas_or_panic(&self) -> HtmlCanvasElement {
        self.create_element_or_panic("canvas").unchecked_into()
    }

    fn get_html_element_by_id(&self, id: &str) -> Option<HtmlElement> {
        self.get_element_by_id(id).and_then(|t| t.dyn_into().ok())
    }

    fn get_webgl2_context(&self, _canvas: &HtmlCanvasElement) -> Option<WebGl2RenderingContext> {
        todo!()
    }
}



// =========================
// === HtmlCanvasElement ===
// =========================

pub trait HtmlCanvasElementOps {
    fn get_webgl2_context(&self) -> Option<WebGl2RenderingContext>;
}

impl HtmlCanvasElementOps for HtmlCanvasElement {
    #[cfg(target_arch = "wasm32")]
    fn get_webgl2_context(&self) -> Option<WebGl2RenderingContext> {
        let options = Object::new();
        Reflect::set(&options, &"antialias".into(), &false.into()).unwrap();
        let context = self.get_context_with_context_options("webgl2", &options).ok().flatten();
        context.and_then(|obj| obj.dyn_into::<WebGl2RenderingContext>().ok())
    }
    #[cfg(not(target_arch = "wasm32"))]
    fn get_webgl2_context(&self) -> Option<WebGl2RenderingContext> {
        None
    }
}


// ===================
// === StyleSetter ===
// ===================

/// Trait used to set css styles.
pub trait StyleSetter {
    fn set_style_or_panic<T: Str, U: Str>(&self, name: T, value: U);
    fn set_style_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &Logger);
}

impl StyleSetter for binding::wasm::HtmlElement {
    fn set_style_or_panic<T: Str, U: Str>(&self, name: T, value: U) {
        let name = name.as_ref();
        let value = value.as_ref();
        let values = format!("\"{}\" = \"{}\" on \"{:?}\"", name, value, self);
        let panic_msg = |_| panic!("Failed to set style {}", values);
        self.style().set_property(name, value).unwrap_or_else(panic_msg);
    }

    fn set_style_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &Logger) {
        let name = name.as_ref();
        let value = value.as_ref();
        let values = format!("\"{}\" = \"{}\" on \"{:?}\"", name, value, self);
        let warn_msg: &str = &format!("Failed to set style {}", values);
        if self.style().set_property(name, value).is_err() {
            warning!(logger, warn_msg);
        }
    }
}

impl StyleSetter for binding::mock::HtmlElement {
    fn set_style_or_panic<T: Str, U: Str>(&self, _name: T, _value: U) {}
    fn set_style_or_warn<T: Str, U: Str>(&self, _name: T, _value: U, _logger: &Logger) {}
}


// =====================
// === Other Helpers ===
// =====================

/// Trait used to set HtmlElement attributes.
pub trait AttributeSetter {
    fn set_attribute_or_panic<T: Str, U: Str>(&self, name: T, value: U);

    fn set_attribute_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &Logger);
}

impl AttributeSetter for binding::mock::Element {
    fn set_attribute_or_panic<T: Str, U: Str>(&self, _name: T, _value: U) {}

    fn set_attribute_or_warn<T: Str, U: Str>(&self, _name: T, _value: U, _logger: &WarningLogger) {}
}


impl AttributeSetter for binding::wasm::Element {
    fn set_attribute_or_panic<T: Str, U: Str>(&self, name: T, value: U) {
        let name = name.as_ref();
        let value = value.as_ref();
        let values = format!("\"{}\" = \"{}\" on \"{:?}\"", name, value, self);
        self.set_attribute(name, value)
            .unwrap_or_else(|_| panic!("Failed to set attribute {}", values));
    }

    fn set_attribute_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &Logger) {
        let name = name.as_ref();
        let value = value.as_ref();
        let values = format!("\"{}\" = \"{}\" on \"{:?}\"", name, value, self);
        let warn_msg: &str = &format!("Failed to set attribute {}", values);
        if self.set_attribute(name, value).is_err() {
            warning!(logger, warn_msg)
        }
    }
}

/// Trait used to insert `Node`s.
pub trait NodeInserter {
    fn append_or_panic(&self, node: &Self);

    fn append_or_warn(&self, node: &Self, logger: &Logger);

    fn prepend_or_panic(&self, node: &Self);

    fn prepend_or_warn(&self, node: &Self, logger: &Logger);

    fn insert_before_or_panic(&self, node: &Self, reference_node: &Self);

    fn insert_before_or_warn(&self, node: &Self, reference_node: &Self, logger: &Logger);
}

impl NodeInserter for binding::mock::Node {
    fn append_or_panic(&self, _node: &Self) {}

    fn append_or_warn(&self, _node: &Self, _logger: &WarningLogger) {}

    fn prepend_or_panic(&self, _node: &Self) {}

    fn prepend_or_warn(&self, _node: &Self, _logger: &WarningLogger) {}

    fn insert_before_or_panic(&self, _node: &Self, _reference_node: &Self) {}

    fn insert_before_or_warn(&self, _node: &Self, _reference_node: &Self, _logger: &WarningLogger) {
    }
}

impl NodeInserter for binding::wasm::Node {
    fn append_or_panic(&self, node: &Self) {
        let panic_msg = |_| panic!("Failed to append child {:?} to {:?}", node, self);
        self.append_child(node).unwrap_or_else(panic_msg);
    }

    fn append_or_warn(&self, node: &Self, logger: &Logger) {
        let warn_msg: &str = &format!("Failed to append child {:?} to {:?}", node, self);
        if self.append_child(node).is_err() {
            warning!(logger, warn_msg)
        };
    }

    fn prepend_or_panic(&self, node: &Self) {
        let panic_msg = |_| panic!("Failed to prepend child \"{:?}\" to \"{:?}\"", node, self);
        let first_c = self.first_child();
        self.insert_before(node, first_c.as_ref()).unwrap_or_else(panic_msg);
    }

    fn prepend_or_warn(&self, node: &Self, logger: &Logger) {
        let warn_msg: &str = &format!("Failed to prepend child \"{:?}\" to \"{:?}\"", node, self);
        let first_c = self.first_child();
        if self.insert_before(node, first_c.as_ref()).is_err() {
            warning!(logger, warn_msg)
        }
    }

    fn insert_before_or_panic(&self, node: &Self, ref_node: &Self) {
        let panic_msg =
            |_| panic!("Failed to insert {:?} before {:?} in {:?}", node, ref_node, self);
        self.insert_before(node, Some(ref_node)).unwrap_or_else(panic_msg);
    }

    fn insert_before_or_warn(&self, node: &Self, ref_node: &Self, logger: &Logger) {
        let warn_msg: &str =
            &format!("Failed to insert {:?} before {:?} in {:?}", node, ref_node, self);
        if self.insert_before(node, Some(ref_node)).is_err() {
            warning!(logger, warn_msg)
        }
    }
}

/// Trait used to remove `Node`s.
pub trait NodeRemover {
    fn remove_from_parent_or_panic(&self);

    fn remove_from_parent_or_warn(&self, logger: &Logger);

    fn remove_child_or_panic(&self, node: &Self);

    fn remove_child_or_warn(&self, node: &Self, logger: &Logger);
}

impl NodeRemover for binding::mock::Node {
    fn remove_from_parent_or_panic(&self) {}

    fn remove_from_parent_or_warn(&self, _logger: &WarningLogger) {}

    fn remove_child_or_panic(&self, _node: &Self) {}

    fn remove_child_or_warn(&self, _node: &Self, _logger: &WarningLogger) {}
}

impl NodeRemover for binding::wasm::Node {
    fn remove_from_parent_or_panic(&self) {
        if let Some(parent) = self.parent_node() {
            let panic_msg = |_| panic!("Failed to remove {:?} from parent", self);
            parent.remove_child(self).unwrap_or_else(panic_msg);
        }
    }

    fn remove_from_parent_or_warn(&self, logger: &Logger) {
        if let Some(parent) = self.parent_node() {
            let warn_msg: &str = &format!("Failed to remove {:?} from parent", self);
            if parent.remove_child(self).is_err() {
                warning!(logger, warn_msg)
            }
        }
    }

    fn remove_child_or_panic(&self, node: &Self) {
        let panic_msg = |_| panic!("Failed to remove child {:?} from {:?}", node, self);
        self.remove_child(node).unwrap_or_else(panic_msg);
    }

    fn remove_child_or_warn(&self, node: &Self, logger: &Logger) {
        let warn_msg: &str = &format!("Failed to remove child {:?} from {:?}", node, self);
        if self.remove_child(node).is_err() {
            warning!(logger, warn_msg)
        }
    }
}

pub use request_animation_frame_impl::*;

#[cfg(not(target_arch = "wasm32"))]
mod request_animation_frame_impl {
    use super::*;
    pub fn request_animation_frame(_f: &Closure<dyn FnMut(f64)>) -> i32 {
        mock_default()
    }

    pub fn cancel_animation_frame(_id: i32) {}
}

#[cfg(target_arch = "wasm32")]
mod request_animation_frame_impl {
    use super::*;
    pub fn request_animation_frame(f: &Closure<dyn FnMut(f64)>) -> i32 {
        window.request_animation_frame(f.as_ref().unchecked_ref()).unwrap()
    }

    pub fn cancel_animation_frame(id: i32) {
        window.cancel_animation_frame(id).unwrap();
    }
}


#[cfg(target_arch = "wasm32")]
mod arch_dependent_impls {}



// =============
// === Utils ===
// =============



/// Ignores context menu when clicking with the right mouse button.
pub fn ignore_context_menu(target: &EventTarget) -> EventListenerHandle {
    let closure = move |event: MouseEvent| {
        const RIGHT_MOUSE_BUTTON: i16 = 2;
        if event.button() == RIGHT_MOUSE_BUTTON {
            event.prevent_default();
        }
    };
    let closure = Closure::wrap(Box::new(closure) as Box<dyn FnMut(MouseEvent)>);
    add_event_listener_with_bool(target, "contextmenu", closure, true)
}



// =======================
// === Event Listeners ===
// =======================


/// The type of closures used for 'add_event_listener_*' functions.
pub type JsEventHandler<T = JsValue> = Closure<dyn FnMut(T)>;

/// Handler for event listeners. Unregisters the listener when the last clone is dropped.
#[derive(Debug, Clone, CloneRef)]
pub struct EventListenerHandle {
    rc: Rc<EventListenerHandleData>,
}

impl EventListenerHandle {
    /// Constructor.
    pub fn new<T: ?Sized + 'static>(
        target: EventTarget,
        name: ImString,
        listener: Function,
        closure: Closure<T>,
    ) -> Self {
        let _closure = Box::new(closure);
        let data = EventListenerHandleData { target, name, listener, _closure };
        let rc = Rc::new(data);
        Self { rc }
    }
}

/// Internal structure for [`EventListenerHandle`].
///
/// # Implementation Notes
/// The [`_closure`] field contains a wasm_bindgen's [`Closure<T>`]. Dropping it causes the
/// associated function to be pruned from memory.
#[derive(Debug)]
struct EventListenerHandleData {
    target:   EventTarget,
    name:     ImString,
    listener: Function,
    _closure: Box<dyn Any>,
}

impl Drop for EventListenerHandleData {
    fn drop(&mut self) {
        self.target.remove_event_listener_with_callback(&self.name, &self.listener).ok();
    }
}

macro_rules! gen_add_event_listener {
    ($name:ident, $wbindgen_name:ident $(,$arg:ident : $tp:ty)*) => {
        /// Wrapper for the function defined in web_sys which allows passing wasm_bindgen
        /// [`Closure`] directly.
        pub fn $name<T: ?Sized + 'static>(
            target: &EventTarget,
            name: &str,
            closure: Closure<T>
            $(,$arg : $tp)*
        ) -> EventListenerHandle {
            let listener = closure.as_ref().unchecked_ref::<Function>().clone();
            // Please note that using [`ok`] is safe here, as according to MDN this function never
            // fails: https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener.
            target.$wbindgen_name(name, &listener $(,$arg)*).ok();
            let target = target.clone();
            let name = name.into();
            EventListenerHandle::new(target, name, listener, closure)
        }
    };
}

gen_add_event_listener!(add_event_listener, add_event_listener_with_callback);
gen_add_event_listener!(
    add_event_listener_with_bool,
    add_event_listener_with_callback_and_bool,
    options: bool
);
gen_add_event_listener!(
    add_event_listener_with_options,
    add_event_listener_with_callback_and_add_event_listener_options,
    options: &AddEventListenerOptions
);



// ===================
// === Performance ===
// ===================

#[cfg(target_arch = "wasm32")]
pub use web_sys::Performance;

#[cfg(target_arch = "wasm32")]
/// Access the `window.performance` or panics if it does not exist.
pub fn performance() -> Performance {
    window.performance().unwrap_or_else(|| panic!("Cannot access window.performance."))
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Clone, Copy, Default)]
pub struct Performance {}

#[cfg(not(target_arch = "wasm32"))]
impl Performance {
    pub fn now(&self) -> f64 {
        0.0
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn performance() -> Performance {
    default()
}



// =========================
// === Stack Trace Limit ===
// =========================

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(inline_js = "
    export function set_stack_trace_limit() {
        Error.stackTraceLimit = 100
    }
")]
extern "C" {
    #[allow(unsafe_code)]
    pub fn set_stack_trace_limit2();
}

#[cfg(not(target_arch = "wasm32"))]
pub fn set_stack_trace_limit2() {}



// ==========================
// === device_pixel_ratio ===
// ==========================

#[cfg(target_arch = "wasm32")]
/// Access the `window.devicePixelRatio` or panic if the window does not exist.
pub fn device_pixel_ratio() -> f64 {
    window.device_pixel_ratio()
}

#[cfg(not(target_arch = "wasm32"))]
pub fn device_pixel_ratio() -> f64 {
    1.0
}



// ============
// === Time ===
// ============

static mut START_TIME: Option<std::time::Instant> = None;
static mut TIME_OFFSET: f64 = 0.0;

/// Initializes global stats of the program, like its start time. This function should be called
/// exactly once, as the first operation of a program.
///
/// # Safety
/// This function modifies a global variable, however, it should be safe as it should be called
/// exactly once on program entry point.
#[allow(unsafe_code)]
pub fn init() -> std::time::Instant {
    unsafe {
        let now = std::time::Instant::now();
        START_TIME = Some(now);
        now
    }
}

/// Start time of the program. Please note that the program should call the `init` function as
/// its first operation.
///
/// # Safety
/// The following modifies a global variable, however, even in case of a race condition, nothing
/// bad should happen (the variable may be initialized several times). Moreover, the variable
/// should be initialized on program start, so this should be always safe.
#[allow(unsafe_code)]
pub fn start_time() -> std::time::Instant {
    unsafe {
        match START_TIME {
            Some(time) => time,
            None => init(),
        }
    }
}

/// Time difference between the start time and current point in time.
///
/// # Safety
/// The following code will always be safe if the program called the `init` function on entry.
/// Even if that did not happen, the worst thing that may happen is re-initialization of the
/// program start time variable.
#[allow(unsafe_code)]
#[cfg(target_arch = "wasm32")]
pub fn time_from_start() -> f64 {
    unsafe { performance().now() + TIME_OFFSET }
}

/// Time difference between the start time and current point in time.
///
/// # Safety
/// The following code will always be safe if the program called the `init` function on entry.
/// Even if that did not happen, the worst thing that may happen is re-initialization of the
/// program start time variable.
#[allow(unsafe_code)]
#[cfg(not(target_arch = "wasm32"))]
pub fn time_from_start() -> f64 {
    unsafe { start_time().elapsed().as_millis() as f64 + TIME_OFFSET }
}

/// Simulates a time interval. This function will exit immediately, but the next time you will
/// check the `time_from_start`, it will be increased.
///
/// # Safety
/// This function is safe only in single-threaded environments.
#[allow(unsafe_code)]
pub fn simulate_sleep(duration: f64) {
    unsafe { TIME_OFFSET += duration }
}



// ============
// === Test ===
// ============

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     use wasm_bindgen_test::wasm_bindgen_test;
//     use wasm_bindgen_test::wasm_bindgen_test_configure;
//
//     wasm_bindgen_test_configure!(run_in_browser);
//
//     #[cfg(target_arch = "wasm32")]
//     mod helpers {
//         type Instant = f64;
//
//         pub fn now() -> Instant {
//             super::performance().now()
//         }
//
//         pub fn elapsed(instant: Instant) -> f64 {
//             super::performance().now() - instant
//         }
//     }
//
//     #[cfg(not(target_arch = "wasm32"))]
//     mod helpers {
//         use std::time::Instant;
//
//         pub fn now() -> Instant {
//             Instant::now()
//         }
//
//         pub fn elapsed(instant: Instant) -> f64 {
//             instant.elapsed().as_secs_f64()
//         }
//     }
//
//     #[wasm_bindgen_test(async)]
//     async fn async_sleep() {
//         let instant = helpers::now();
//         sleep(Duration::new(1, 0)).await;
//         assert!(helpers::elapsed(instant) >= 1.0);
//         sleep(Duration::new(2, 0)).await;
//         assert!(helpers::elapsed(instant) >= 3.0);
//     }
//
//     #[test]
//     #[cfg(not(target_arch = "wasm32"))]
//     fn async_sleep_native() {
//         async_std::task::block_on(async_sleep())
//     }
// }



// =============
// === Panic ===
// =============

// TODO: 2 mechanisms here. What is the difference between them?

/// Enables forwarding panic messages to `console.error`.
pub fn forward_panic_hook_to_console2() {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    console_error_panic_hook::set_once();
}

#[cfg(not(target_arch = "wasm32"))]
pub fn forward_panic_hook_to_error() {}

#[cfg(target_arch = "wasm32")]
/// Enables throwing a descriptive JavaScript error on panics.
pub fn forward_panic_hook_to_error() {
    std::panic::set_hook(Box::new(error_throwing_panic_hook));
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(module = "/js/rust_panic.js")]
extern "C" {
    #[allow(unsafe_code)]
    fn new_panic_error(message: String) -> JsValue;
}

#[cfg(target_arch = "wasm32")]
fn error_throwing_panic_hook(panic_info: &std::panic::PanicInfo) {
    wasm_bindgen::throw_val(new_panic_error(panic_info.to_string()));
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn entry_point_panic() {
    forward_panic_hook_to_error();
    panic!();
}


// =============
// === Sleep ===
// =============

#[cfg(target_arch = "wasm32")]
/// Sleeps for the specified amount of time.
///
/// This function might sleep for slightly longer than the specified duration but never less. This
/// function is an async version of std::thread::sleep, its timer starts just after the function
/// call.
pub async fn sleep(duration: Duration) {
    use gloo_timers::future::TimeoutFuture;
    TimeoutFuture::new(duration.as_millis() as u32).await
}

#[cfg(not(target_arch = "wasm32"))]
pub use async_std::task::sleep;



// ========== TODO: TO BE DECIDED

/// Tries to get `Element` by ID, and runs function on it.
pub fn with_element_by_id_or_warn<F>(logger: &Logger, id: &str, f: F)
where F: FnOnce(Element) {
    let root_elem = document.get_element_by_id(id);
    match root_elem {
        Some(v) => f(v),
        None => warning!(logger, "Failed to get element by ID."),
    }
}
