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

    /// Default value provider. Similar to [`Default`] but with additional implementations.
    pub trait MockDefault {
        fn mock_default() -> Self;
    }

    /// [`MockDefault::mock_default`] accessor.
    pub fn mock_default<T: MockDefault>() -> T {
        T::mock_default()
    }

    impl MockDefault for () {
        fn mock_default() -> Self {}
    }

    impl<T: MockDefault, E> MockDefault for std::result::Result<T, E> {
        fn mock_default() -> Self {
            Ok(mock_default())
        }
    }

    /// Macro which generates [`MockDefault`] impls which redirect the call to [`Default::default`].
    macro_rules! auto_impl_mock_default {
        ( $($tp:ident $(< $($arg:ident),* >)? ),* ) => {
            $(
                impl $(<$($arg),*>)? MockDefault for $tp $(<$($arg),*>)? {
                    fn mock_default() -> Self {
                        default()
                    }
                }
            )*
        };
    }

    auto_impl_mock_default!(bool, i16, i32, u32, f64, String, Option<T>);



    // ================
    // === MockData ===
    // ================

    /// Every mock structure implements this trait.
    pub trait MockData {}

    /// Macro used to generate mock structures. See the expansion of generated structures to learn
    /// more.
    macro_rules! mock_struct {
        ( $([$opt:ident])?
            $name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)? $(=> $deref:ident)?
        ) => {
            #[allow(missing_copy_implementations)]
            #[allow(non_snake_case)]
            pub struct $name $(<$($param $(:?$param_tp)?),*>)? {
                $($( $param : PhantomData<$param> ),*)?
            }

            impl$(<$($param $(:?$param_tp)?),*>)?
            Debug for $name $(<$($param),*>)? {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, stringify!($name))
                }
            }

            /// # Safety
            /// The usage of [`mem::transmute`] is safe here as we transmute ZST types.
            #[allow(unsafe_code)]
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
            MockData for $name $(<$($param),*>)? {}

            mock_struct_deref! {[$($deref)?] $name $(<$( $param $(:?$param_tp)?),*>)?}
            mock_struct_as_ref! {[$($opt)?] $name $(<$( $param $(:?$param_tp)?),*>)? $(=> $deref)?}
        };
    }

    macro_rules! mock_struct_as_ref {
        ([NO_AS_REF] $($ts:tt)*) => {};
        ([] $name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)?
            $(=> $deref:ident)?
        ) => {
            /// # Safety
            /// The usage of [`mem::transmute`] is safe here as we transmute ZST types.
            #[allow(unsafe_code)]
            impl<__T__: MockData, $($($param $(:?$param_tp)? ),*)?>
            AsRef<__T__> for $name $(<$($param),*>)? {
                fn as_ref(&self) -> &__T__ {
                    unsafe { mem::transmute(self) }
                }
            }
        };
    }

    macro_rules! mock_struct_deref {
        ([] $($ts:tt)*) => {};
        ([$deref:ident] $name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)?) => {
            impl $(<$($param $(:?$param_tp)?),*>)?
            Deref for $name $(<$($param),*>)? {
                type Target = $deref;
                fn deref(&self) -> &Self::Target {
                    self.as_ref()
                }
            }

            impl $(<$($param $(:?$param_tp)?),*>)?
            From<$name $(<$($param),*>)?> for $deref {
                fn from(_: $name) -> Self {
                    default()
                }
            }
        };
    }



    // ===============
    // === mock_fn ===
    // ===============

    /// Macro used to generate mock methods. Methods look just like their provided signature with
    /// a body returning `mock_default()`. There are two special cases: for functions returning
    /// `&Self`, and `&mut Self`, which just pass `&self` and `&mut self` to the output,
    /// respectively.
    macro_rules! mock_fn {
        ($name:ident $(<$($fn_tp:ident),*>)?
        (&self $(,$arg:ident : $arg_tp:ty)* $(,)? ) -> &Self ) => {
            pub fn $name $(<$($fn_tp),*>)? (&self $(,$arg : $arg_tp)*) -> &Self {
                self
            }
        };

        ($name:ident $(<$($fn_tp:ident),*>)?
        (&mut self $(,$arg:ident : $arg_tp:ty)* $(,)? ) -> &mut Self ) => {
            pub fn $name $(<$($fn_tp),*>)? (&mut self $(,$arg : $arg_tp)*) -> &mut Self {
                self
            }
        };

        ($name:ident $(<$($fn_tp:ident),*>)?
        (&self $(,$arg:ident : $arg_tp:ty)* $(,)? ) $(-> $out:ty)? ) => {
            pub fn $name $(<$($fn_tp),*>)? (&self $(,$arg : $arg_tp)*) $(-> $out)? {
                mock_default()
            }
        };

        ($name:ident $(<$($fn_tp:ident),*>)?
        ($($arg:ident : $arg_tp:ty)* $(,)? ) $(-> $out:ty)? ) => {
            pub fn $name $(<$($fn_tp),*>)? ($($arg : $arg_tp)*) $(-> $out)? {
                mock_default()
            }
        };
    }

    /// Combination of [`mock_struct`] and [`mock_fn`].
    macro_rules! mock_data {
        ( $([$opt:ident])?
            $name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)? $(=> $deref:ident)?
            $(
                fn $fn_name:ident $(<$($fn_tp:ident),*>)? ($($args:tt)*) $(-> $out:ty)?;
            )*
        ) => {
            mock_struct!{$([$opt])? $name $(<$($param $(:?$param_tp)?),*>)? $(=> $deref)?}
            impl $(<$($param $(:?$param_tp)?),*>)? $name $(<$($param),*>)? {
                $(
                    mock_fn!{$fn_name $(<$($fn_tp),*>)? ($($args)*) $(-> $out)?}
                )*
            }
        };
    }



    // ==============
    // === JsCast ===
    // ==============

    /// Mock of [`JsCast`] is implemented for all mocked types.
    impl<T: MockData + MockDefault + AsRef<JsValue> + Into<JsValue>> JsCast for T {}

    /// Mock of [`wasm_bindgen::JsCast`].
    pub trait JsCast
    where Self: MockData + MockDefault + AsRef<JsValue> + Into<JsValue> {
        fn has_type<T>(&self) -> bool {
            true
        }

        fn dyn_into<T>(self) -> std::result::Result<T, Self>
        where T: JsCast {
            Ok(self.unchecked_into())
        }

        fn dyn_ref<T>(&self) -> Option<&T>
        where T: JsCast {
            Some(self.unchecked_ref())
        }

        fn unchecked_into<T>(self) -> T
        where T: JsCast {
            T::unchecked_from_js(self.into())
        }

        fn unchecked_ref<T>(&self) -> &T
        where T: JsCast {
            T::unchecked_from_js_ref(self.as_ref())
        }

        fn is_instance_of<T>(&self) -> bool {
            true
        }
        fn instanceof(_val: &JsValue) -> bool {
            true
        }
        fn is_type_of(_val: &JsValue) -> bool {
            true
        }
        fn unchecked_from_js(_val: JsValue) -> Self {
            mock_default()
        }

        fn unchecked_from_js_ref(val: &JsValue) -> &Self {
            val.as_ref()
        }
    }


    // ===============
    // === JsValue ===
    // ===============

    /// Mock of [`wasm_bindgen::JsValue`]. All JS types can be converted to `JsValue` and thus it
    /// implements a generic conversion trait.
    mock_data! { JsValue }

    auto trait IsNotJsValue {}
    impl !IsNotJsValue for JsValue {}
    impl<A: IsNotJsValue> From<A> for JsValue {
        default fn from(_: A) -> Self {
            default()
        }
    }



    // ===============
    // === Closure ===
    // ===============

    /// The generated structure does not implement a generic [`AsRef`] impl, as the usages base on
    /// the fact that there exist exactly one such an impl (provided below), so the type inferencer
    /// can monomoprhise more free variables.
    mock_data! { [NO_AS_REF] Closure<T: ?Sized>
        fn new<F>(_t: F) -> Closure<T>;
        fn wrap(_data: Box<T>) -> Closure<T>;
        fn once<F>(_fn_once: F) -> Closure<F>;
    }

    #[allow(unsafe_code)]
    impl<T: ?Sized> AsRef<JsValue> for Closure<T> {
        fn as_ref(&self) -> &JsValue {
            unsafe { mem::transmute(self) }
        }
    }



    // ====================
    // === DOM Elements ===
    // ====================

    // === WebGl2RenderingContext ===
    /// The [`WebGl2RenderingContext`] is not a mocked structure because it defines tons of
    /// constants that we use heavily. Instead, the rendering engine runs context-less when
    /// compiled to native tests.
    pub use web_sys::WebGl2RenderingContext;

    // === Object ===
    mock_data! { Object => JsValue
        fn value_of(&self) -> Object;
    }


    // === EventTarget ===
    mock_data! { EventTarget => Object
        fn remove_event_listener_with_callback
            (&self, _tp:&str, _f:&Function) -> std::result::Result<(), JsValue>;
        fn add_event_listener_with_callback
            (&self, _tp:&str, _f:&Function) -> std::result::Result<(), JsValue>;
        fn add_event_listener_with_callback_and_bool
            (&self, _tp:&str, _f:&Function, _opt:bool) -> std::result::Result<(), JsValue>;
        fn add_event_listener_with_callback_and_add_event_listener_options
            (&self, _tp:&str, _f:&Function, _opt:&AddEventListenerOptions)
            -> std::result::Result<(), JsValue>;
    }


    // === Window ===
    mock_data! { Window => EventTarget
        fn open_with_url_and_target(&self,_url: &str,_target: &str)
            -> std::result::Result<Option<Window>, JsValue>;
    }
    pub fn window() -> Window {
        mock_default()
    }


    // === Function ===
    mock_data! { Function }


    // === AddEventListenerOptions ===
    mock_data! { AddEventListenerOptions
        fn new() -> Self;
    }
    impl AddEventListenerOptions {
        mock_fn!(capture(&mut self, _val:bool) -> &mut Self);
        mock_fn!(passive(&mut self, _val:bool) -> &mut Self);
    }


    // === Event ===
    mock_data! { Event => Object
        fn prevent_default(&self);
        fn stop_propagation(&self);
        fn current_target(&self) -> Option<EventTarget>;
    }


    // === KeyboardEvent ===
    mock_data! { KeyboardEvent => Event
        fn key(&self) -> String;
        fn code(&self) -> String;
        fn alt_key(&self) -> bool;
        fn ctrl_key(&self) -> bool;
    }


    // === MouseEvent ===
    mock_data! { MouseEvent => Event
        fn button(&self) -> i16;
        fn alt_key(&self) -> bool;
        fn ctrl_key(&self) -> bool;
        fn client_x(&self) -> i32;
        fn client_y(&self) -> i32;
        fn offset_x(&self) -> i32;
        fn offset_y(&self) -> i32;
        fn screen_x(&self) -> i32;
        fn screen_y(&self) -> i32;
    }


    // === WheelEvent ===
    mock_data! { WheelEvent => MouseEvent
        fn delta_x(&self) -> f64;
        fn delta_y(&self) -> f64;
    }


    // === HtmlCollection ===
    mock_data! { HtmlCollection
        fn length(&self) -> u32;
    }


    // === DomRect ===
    mock_data! { DomRect
        fn width(&self) -> f64;
        fn height(&self) -> f64;
        fn left(&self) -> f64;
        fn right(&self) -> f64;
        fn top(&self) -> f64;
        fn bottom(&self) -> f64;
    }


    // === Element ===
    mock_data! { Element => Node
        fn remove(&self);
        fn children(&self) -> HtmlCollection;
        fn get_bounding_client_rect(&self) -> DomRect;
    }

    // === HtmlElement ===
    mock_data! { HtmlElement => Element
        fn set_class_name(&self, _n: &str);
    }
    impl From<HtmlElement> for EventTarget {
        fn from(_: HtmlElement) -> Self {
            default()
        }
    }


    // === HtmlDivElement ===
    mock_data! { HtmlDivElement => HtmlElement }
    impl From<HtmlDivElement> for EventTarget {
        fn from(_: HtmlDivElement) -> Self {
            default()
        }
    }


    // === HtmlCanvasElement ===
    mock_data! { HtmlCanvasElement => HtmlElement }


    // === CanvasRenderingContext2d ===
    mock_data! { CanvasRenderingContext2d }


    // === Node ===
    mock_data! { Node => EventTarget }



    // =============
    // === Utils ===
    // =============

    mock_fn! { body() -> HtmlElement }
    mock_fn! { create_div() -> HtmlDivElement }
    mock_fn! { create_canvas() -> HtmlCanvasElement }
    mock_fn! { get_html_element_by_id(_id: &str) -> Result<HtmlElement> }
    mock_fn! { get_webgl2_context(_canvas: &HtmlCanvasElement) -> Option<WebGl2RenderingContext> }
    mock_fn! { forward_panic_hook_to_console() }
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

impl AttributeSetter for Element {
    fn set_attribute_or_panic<T: Str, U: Str>(&self, _name: T, _value: U) {}

    fn set_attribute_or_warn<T: Str, U: Str>(&self, _name: T, _value: U, _logger: &WarningLogger) {}
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
    fn append_or_panic(&self, _node: &Self) {}

    fn append_or_warn(&self, _node: &Self, _logger: &WarningLogger) {}

    fn prepend_or_panic(&self, _node: &Self) {}

    fn prepend_or_warn(&self, _node: &Self, _logger: &WarningLogger) {}

    fn insert_before_or_panic(&self, _node: &Self, _reference_node: &Self) {}

    fn insert_before_or_warn(&self, _node: &Self, _reference_node: &Self, _logger: &WarningLogger) {
    }
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

    fn remove_from_parent_or_warn(&self, _logger: &WarningLogger) {}

    fn remove_child_or_panic(&self, _node: &Self) {}

    fn remove_child_or_warn(&self, _node: &Self, _logger: &WarningLogger) {}
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
    pub fn request_animation_frame(_f: &Closure<dyn FnMut(f64)>) -> i32 {
        mock_default()
    }

    pub fn cancel_animation_frame(_id: i32) {}
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
