#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![feature(trait_alias)]
#![feature(negative_impls)]
#![feature(specialization)]
#![feature(auto_traits)]


pub mod clipboard;
pub mod closure;
pub mod event;
pub mod platform;
pub mod resize_observer;
pub mod stream;

/// Common types that should be visible across the whole crate.
pub mod prelude {
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

use crate::prelude::*;

use enso_logger::warning;
use enso_logger::WarningLogger as Logger;


pub use std::time::Duration;
pub use std::time::Instant;



pub use arch_dependent_impls::JsValue;
pub use arch_dependent_impls::*;


#[cfg(not(target_arch = "wasm32"))]
#[allow(missing_docs)]
mod arch_dependent_impls {
    use super::*;


    // ===================
    // === MockDefault ===
    // ===================

    pub trait MockDefault {
        fn mock_default() -> Self;
    }

    pub fn mock_default<T: MockDefault>() -> T {
        T::mock_default()
    }

    impl MockDefault for i16 {
        fn mock_default() -> Self {
            0
        }
    }

    impl MockDefault for i32 {
        fn mock_default() -> Self {
            0
        }
    }

    impl MockDefault for u32 {
        fn mock_default() -> Self {
            0
        }
    }

    impl MockDefault for String {
        fn mock_default() -> Self {
            "mock default".into()
        }
    }

    impl<T> MockDefault for Option<T> {
        fn mock_default() -> Self {
            None
        }
    }

    impl<T: MockDefault, E> MockDefault for std::result::Result<T, E> {
        fn mock_default() -> Self {
            Ok(mock_default())
        }
    }



    // ================
    // === MockData ===
    // ================

    pub trait MockData {}


    macro_rules! new_mock_data {
        ($name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)?
            $(=> $deref:ident)?
        ) => {
            #[derive(Debug)]
            pub struct $name $(<$($param $(:?$param_tp)?),*>)? {
                $($( $param : PhantomData<$param> ),*)?
            }

            impl $(<$($param $(:?$param_tp)?),*>)?
            Default for $name $(<$($param),*>)? {
                fn default() -> Self {
                    unsafe { mem::transmute(()) }
                }
            }

            impl $(<$($param $(:?$param_tp)?),*>)?
            MockDefault for $name $(<$($param),*>)? {
                fn mock_default() -> Self {
                    default()
                }
            }

            impl $(<$($param $(:?$param_tp)?),*>)?
            Clone for $name $(<$($param),*>)? {
                fn clone(&self) -> Self {
                    default()
                }
            }

            impl $(<$($param $(:?$param_tp)?),*>)?
            CloneRef for $name $(<$($param),*>)? {
                fn clone_ref(&self) -> Self {
                    default()
                }
            }

            impl $(<$($param $(:?$param_tp)?),*>)?
            Copy for $name $(<$($param),*>)? {}

            impl $(<$($param $(:?$param_tp)?),*>)?
            MockData for $name $(<$($param),*>)? {}

            impl<__T__: MockData, $($($param $(:?$param_tp)? ),*)?>
            AsRef<__T__> for $name $(<$($param),*>)? {
                fn as_ref(&self) -> &__T__ {
                    unsafe { mem::transmute(self) }
                }
            }

            $(
                impl Deref for $name {
                    type Target = $deref;
                    fn deref(&self) -> &Self::Target {
                        self.as_ref()
                    }
                }

                impl From<$name> for $deref {
                    fn from(_:$name) -> Self {
                        default()
                    }
                }
            )?

            impl JsCast for $name {
                fn instanceof(_val: &JsValue) -> bool {
                    true
                }
                fn unchecked_from_js(val: JsValue) -> Self {
                    default()
                }
                fn unchecked_from_js_ref(val: &JsValue) -> &Self {
                    val.as_ref()
                }
            }
        };
    }

    // ===============
    // === JsValue ===
    // ===============

    new_mock_data!(JsValue);

    auto trait IsNotJsValue {}
    impl !IsNotJsValue for JsValue {}
    impl<A: IsNotJsValue> From<A> for JsValue {
        default fn from(_: A) -> Self {
            default()
        }
    }


    // ==============
    // === Object ===
    // ==============

    new_mock_data!(Object => JsValue);

    impl Object {
        pub fn value_of(&self) -> Object {
            default()
        }
    }


    // ===================
    // === EventTarget ===
    // ===================

    new_mock_data!(EventTarget => Object);

    macro_rules! new_event_listener_fn {
        ($name:ident ( $($opt:ident : $tp:ty),* )) => {
            pub fn $name(&self, _tp: &str, _listener: &Function, $($opt:$tp),*)
            -> std::result::Result<(), JsValue> {
                Ok(())
            }
        };
    }

    impl EventTarget {
        new_event_listener_fn!(remove_event_listener_with_callback());
        new_event_listener_fn!(add_event_listener_with_callback());
        new_event_listener_fn!(add_event_listener_with_callback_and_bool(_options: bool));
        new_event_listener_fn!(add_event_listener_with_callback_and_add_event_listener_options(
            _options: &AddEventListenerOptions
        ));
    }


    // ==============
    // === Window ===
    // ==============

    new_mock_data!(Window => EventTarget);

    pub fn window() -> Window {
        mock_default()
    }

    impl Window {
        pub fn open_with_url_and_target(
            &self,
            _url: &str,
            _target: &str,
        ) -> std::result::Result<Option<Window>, JsValue> {
            mock_default()
        }
    }



    // ===============
    // === Closure ===
    // ===============



    // new_mock_data!(Closurex<T: ?Sized>);

    pub struct Closure<T: ?Sized> {
        _t: PhantomData<T>,
    }

    impl<T: ?Sized> Debug for Closure<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "Closure")
        }
    }

    impl<T: ?Sized> Default for Closure<T> {
        fn default() -> Self {
            Self { _t: default() }
        }
    }

    impl<T: ?Sized> Clone for Closure<T> {
        fn clone(&self) -> Self {
            default()
        }
    }

    impl<T: ?Sized> Copy for Closure<T> {}

    impl<T: ?Sized> AsRef<JsValue> for Closure<T> {
        fn as_ref(&self) -> &JsValue {
            self.as_ref()
        }
    }

    impl<T: ?Sized> Closure<T> {
        pub fn new<F>(t: F) -> Closure<T> {
            default()
        }

        pub fn wrap(_data: Box<T>) -> Closure<T> {
            default()
        }

        pub fn once<F>(fn_once: F) -> Closure<F> {
            default()
        }
    }



    // ================
    // === Function ===
    // ================

    new_mock_data!(Function);



    // ===============================
    // === AddEventListenerOptions ===
    // ===============================

    new_mock_data!(AddEventListenerOptions);

    impl AddEventListenerOptions {
        pub fn new() -> Self {
            mock_default()
        }

        pub fn capture(&mut self, _val: bool) -> &mut Self {
            self
        }

        pub fn passive(&mut self, _val: bool) -> &mut Self {
            self
        }
    }


    // =============
    // === Event ===
    // =============

    new_mock_data!(Event => Object);

    impl Event {
        pub fn prevent_default(&self) {}
        pub fn stop_propagation(&self) {}
        pub fn current_target(&self) -> Option<EventTarget> {
            mock_default()
        }
    }


    // =====================
    // === KeyboardEvent ===
    // =====================

    new_mock_data!(KeyboardEvent => Event);

    macro_rules! mock_fn {
        ($name:ident ()) => {
            pub fn $name() {}
        };
    }

    impl KeyboardEvent {
        mock_fn!(key2());
        pub fn key(&self) -> String {
            mock_default()
        }

        pub fn code(&self) -> String {
            mock_default()
        }

        pub fn alt_key(&self) -> bool {
            false
        }

        pub fn ctrl_key(&self) -> bool {
            false
        }
    }


    // =====================
    // === KeyboardEvent ===
    // =====================

    new_mock_data!(MouseEvent => Event);

    impl MouseEvent {
        pub fn button(&self) -> i16 {
            mock_default()
        }

        pub fn alt_key(&self) -> bool {
            false
        }

        pub fn ctrl_key(&self) -> bool {
            false
        }

        pub fn client_x(&self) -> i32 {
            0
        }

        pub fn client_y(&self) -> i32 {
            0
        }

        pub fn offset_x(&self) -> i32 {
            0
        }

        pub fn offset_y(&self) -> i32 {
            0
        }

        pub fn screen_x(&self) -> i32 {
            0
        }

        pub fn screen_y(&self) -> i32 {
            0
        }
    }


    // =====================
    // === KeyboardEvent ===
    // =====================

    new_mock_data!(WheelEvent => MouseEvent);

    impl WheelEvent {
        pub fn delta_x(&self) -> f64 {
            0.0
        }

        pub fn delta_y(&self) -> f64 {
            0.0
        }
    }



    // ==============
    // === JsCast ===
    // ==============

    pub trait JsCast
    where Self: AsRef<JsValue> + Into<JsValue> {
        fn has_type<T>(&self) -> bool
        where T: JsCast {
            T::is_type_of(self.as_ref())
        }

        fn dyn_into<T>(self) -> std::result::Result<T, Self>
        where T: JsCast {
            if self.has_type::<T>() {
                Ok(self.unchecked_into())
            } else {
                Err(self)
            }
        }

        fn dyn_ref<T>(&self) -> Option<&T>
        where T: JsCast {
            if self.has_type::<T>() {
                Some(self.unchecked_ref())
            } else {
                None
            }
        }

        fn unchecked_into<T>(self) -> T
        where T: JsCast {
            T::unchecked_from_js(self.into())
        }

        fn unchecked_ref<T>(&self) -> &T
        where T: JsCast {
            T::unchecked_from_js_ref(self.as_ref())
        }

        fn is_instance_of<T>(&self) -> bool
        where T: JsCast {
            T::instanceof(self.as_ref())
        }
        fn instanceof(val: &JsValue) -> bool;
        fn is_type_of(val: &JsValue) -> bool {
            Self::instanceof(val)
        }
        fn unchecked_from_js(val: JsValue) -> Self;

        fn unchecked_from_js_ref(val: &JsValue) -> &Self;
    }



    // =====================
    // === HtmlCollection ===
    // ======================

    new_mock_data!(HtmlCollection);

    impl HtmlCollection {
        pub fn length(&self) -> u32 {
            mock_default()
        }
    }

    // ===============
    // === DomRect ===
    // ===============

    new_mock_data!(DomRect);

    impl DomRect {
        pub fn width(&self) -> f64 {
            default()
        }

        pub fn height(&self) -> f64 {
            default()
        }

        pub fn left(&self) -> f64 {
            default()
        }

        pub fn right(&self) -> f64 {
            default()
        }

        pub fn top(&self) -> f64 {
            default()
        }

        pub fn bottom(&self) -> f64 {
            default()
        }
    }


    // ===============
    // === Element ===
    // ===============

    new_mock_data!(Element => Node);


    impl Element {
        pub fn children(&self) -> HtmlCollection {
            mock_default()
        }

        pub fn remove(&self) {}

        pub fn get_bounding_client_rect(&self) -> DomRect {
            default()
        }
    }

    // ===================
    // === HtmlElement ===
    // ===================

    new_mock_data!(HtmlElement => Element);


    impl HtmlElement {
        // FIXME: move to trait
        pub fn set_class_name(&self, n: &str) {}
    }

    impl From<HtmlElement> for EventTarget {
        fn from(_: HtmlElement) -> Self {
            default()
        }
    }


    // ======================
    // === HtmlDivElement ===
    // ======================

    new_mock_data!(HtmlDivElement => HtmlElement);

    impl From<HtmlDivElement> for EventTarget {
        fn from(_: HtmlDivElement) -> Self {
            default()
        }
    }

    // =========================
    // === HtmlCanvasElement ===
    // =========================

    new_mock_data!(HtmlCanvasElement => HtmlElement);



    // ================================
    // === CanvasRenderingContext2d ===
    // ================================

    new_mock_data!(CanvasRenderingContext2d);



    // ==============================
    // === WebGl2RenderingContext ===
    // ==============================

    // new_mock_data!(WebGl2RenderingContext);
    pub use web_sys::WebGl2RenderingContext;



    // ============
    // === Node ===
    // ============

    new_mock_data!(Node => EventTarget);



    // =============
    // === Utils ===
    // =============

    /// Access the `window.document.body` object or panic if it does not exist.
    pub fn body() -> HtmlElement {
        mock_default()
    }

    pub fn create_div() -> HtmlDivElement {
        mock_default()
    }

    pub fn create_canvas() -> HtmlCanvasElement {
        mock_default()
    }

    pub fn get_html_element_by_id(id: &str) -> Result<HtmlElement> {
        mock_default()
    }

    pub fn get_webgl2_context(canvas: &HtmlCanvasElement) -> Option<WebGl2RenderingContext> {
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

impl StyleSetter for web_sys::HtmlElement {
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

impl StyleSetter for HtmlElement {
    fn set_style_or_panic<T: Str, U: Str>(&self, name: T, value: U) {}
    fn set_style_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &Logger) {}
}


// =====================
// === Other Helpers ===
// =====================

/// Trait used to set HtmlElement attributes.
pub trait AttributeSetter {
    fn set_attribute_or_panic<T: Str, U: Str>(&self, name: T, value: U);

    fn set_attribute_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &Logger);
}

impl AttributeSetter for Element {
    fn set_attribute_or_panic<T: Str, U: Str>(&self, name: T, value: U) {}

    fn set_attribute_or_warn<T: Str, U: Str>(&self, name: T, value: U, logger: &WarningLogger) {}
}


impl AttributeSetter for web_sys::Element {
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

impl NodeInserter for Node {
    fn append_or_panic(&self, node: &Self) {}

    fn append_or_warn(&self, node: &Self, logger: &WarningLogger) {}

    fn prepend_or_panic(&self, node: &Self) {}

    fn prepend_or_warn(&self, node: &Self, logger: &WarningLogger) {}

    fn insert_before_or_panic(&self, node: &Self, reference_node: &Self) {}

    fn insert_before_or_warn(&self, node: &Self, reference_node: &Self, logger: &WarningLogger) {}
}

impl NodeInserter for web_sys::Node {
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

impl NodeRemover for Node {
    fn remove_from_parent_or_panic(&self) {}

    fn remove_from_parent_or_warn(&self, logger: &WarningLogger) {}

    fn remove_child_or_panic(&self, node: &Self) {}

    fn remove_child_or_warn(&self, node: &Self, logger: &WarningLogger) {}
}

impl NodeRemover for web_sys::Node {
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

mod request_animation_frame_impl {
    use super::*;
    pub fn request_animation_frame(f: &Closure<dyn FnMut(f64)>) -> i32 {
        mock_default()
    }

    pub fn cancel_animation_frame(id: i32) {}
}

#[cfg(target_arch = "wasm32")]
mod request_animation_frame_impl {
    pub fn request_animation_frame(f: &Closure<dyn FnMut(f64)>) -> i32 {
        window().request_animation_frame(f.as_ref().unchecked_ref()).unwrap()
    }

    pub fn cancel_animation_frame(id: i32) {
        window().cancel_animation_frame(id).unwrap();
    }
}


#[cfg(target_arch = "wasm32")]
mod arch_dependent_impls {
    use super::*;
    use js_sys::Function;
    use wasm_bindgen::prelude::Closure;
    pub use wasm_bindgen::prelude::*;
    use wasm_bindgen::JsCast;
    pub use wasm_bindgen::JsValue;
    pub use web_sys::console;
    pub use web_sys::CanvasRenderingContext2d;
    pub use web_sys::Document;
    pub use web_sys::Element;
    pub use web_sys::EventTarget;
    pub use web_sys::HtmlCanvasElement;
    pub use web_sys::HtmlCollection;
    pub use web_sys::HtmlDivElement;
    pub use web_sys::HtmlElement;
    pub use web_sys::MouseEvent;
    pub use web_sys::Node;
    pub use web_sys::WebGl2RenderingContext;
    pub use web_sys::Window;

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

    /// Access the `window` object if exists.
    pub fn try_window() -> Result<Window> {
        web_sys::window().ok_or_else(|| Error("Cannot access 'window'."))
    }

    /// Access the `window` object or panic if it does not exist.
    pub fn window() -> Window {
        try_window().unwrap()
    }

    /// Access the `window.document` object if exists.
    pub fn try_document() -> Result<Document> {
        try_window()
            .and_then(|w| w.document().ok_or_else(|| Error("Cannot access 'window.document'.")))
    }

    /// Access the `window.document` object or panic if it does not exist.
    pub fn document() -> Document {
        try_document().unwrap()
    }

    /// Access the `window.document.body` object if exists.
    pub fn try_body() -> Result<HtmlElement> {
        try_document()
            .and_then(|d| d.body().ok_or_else(|| Error("Cannot access 'window.document.body'.")))
    }

    /// Access the `window.document.body` object or panic if it does not exist.
    pub fn body() -> HtmlElement {
        try_body().unwrap()
    }

    /// Access the `window.devicePixelRatio` value if the window exists.
    pub fn try_device_pixel_ratio() -> Result<f64> {
        try_window().map(|window| window.device_pixel_ratio())
    }



    /// Gets `Element` by ID.
    pub fn get_element_by_id(id: &str) -> Result<Element> {
        try_document()?
            .get_element_by_id(id)
            .ok_or_else(|| Error(format!("Element with id '{}' not found.", id)))
    }

    /// Tries to get `Element` by ID, and runs function on it.
    pub fn with_element_by_id_or_warn<F>(logger: &Logger, id: &str, f: F)
    where F: FnOnce(Element) {
        let root_elem = get_element_by_id(id);
        match root_elem {
            Ok(v) => f(v),
            Err(_) => warning!(logger, "Failed to get element by ID."),
        }
    }

    /// Gets `Element`s by class name.
    pub fn get_elements_by_class_name(name: &str) -> Result<Vec<Element>> {
        let collection = try_document()?.get_elements_by_class_name(name);
        let indices = 0..collection.length();
        let elements = indices.flat_map(|index| collection.get_with_index(index)).collect();
        Ok(elements)
    }

    pub fn get_html_element_by_id(id: &str) -> Result<HtmlElement> {
        let elem = get_element_by_id(id)?;
        elem.dyn_into().map_err(|_| Error("Type cast error."))
    }

    pub fn try_create_element(name: &str) -> Result<Element> {
        try_document()?
            .create_element(name)
            .map_err(|_| Error(format!("Cannot create element '{}'", name)))
    }

    pub fn create_element(name: &str) -> Element {
        try_create_element(name).unwrap()
    }

    pub fn try_create_div() -> Result<HtmlDivElement> {
        try_create_element("div").map(|t| t.unchecked_into())
    }

    pub fn create_div() -> HtmlDivElement {
        create_element("div").unchecked_into()
    }

    pub fn try_create_canvas() -> Result<HtmlCanvasElement> {
        try_create_element("canvas").map(|t| t.unchecked_into())
    }

    pub fn create_canvas() -> HtmlCanvasElement {
        create_element("canvas").unchecked_into()
    }

    /// Get WebGL 2.0 context. Returns [`None`] if WebGL 2.0 is not supported.
    pub fn get_webgl2_context(canvas: &HtmlCanvasElement) -> Option<WebGl2RenderingContext> {
        let options = js_sys::Object::new();
        js_sys::Reflect::set(&options, &"antialias".into(), &false.into()).unwrap();
        let context = canvas.get_context_with_context_options("webgl2", &options).ok().flatten();
        context.and_then(|obj| obj.dyn_into::<WebGl2RenderingContext>().ok())
    }

    pub fn try_request_animation_frame(f: &Closure<dyn FnMut(f64)>) -> Result<i32> {
        try_window()?
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


    /// Common traits.
    pub mod traits {
        pub use super::NodeInserter;
        pub use super::NodeRemover;
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
}



#[cfg(target_arch = "wasm32")]
/// Access the `window.performance` or panics if it does not exist.
pub fn performance() -> Performance {
    window().performance().unwrap_or_else(|| panic!("Cannot access window.performance."))
}


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
    window().performance().unwrap_or_else(|| panic!("Cannot access window.performance."))
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



// ==========================
// === device_pixel_ratio ===
// ==========================

#[cfg(target_arch = "wasm32")]
/// Access the `window.devicePixelRatio` or panic if the window does not exist.
pub fn device_pixel_ratio() -> f64 {
    window().device_pixel_ratio()
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
