//! Mocked bindings to the web-api allowing its compilation for the native target without throwing
//! panics.

// === Non-Standard Linter Configuration ===
#![allow(clippy::boxed_local)]

use crate::prelude::*;

use std::marker::Unsize;



// ===================
// === MockDefault ===
// ===================

/// Default value provider. Similar to [`Default`] but with additional implementations.
#[allow(missing_docs)]
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

impl<T: MockDefault> MockDefault for Option<T> {
    fn mock_default() -> Self {
        Some(mock_default())
    }
}

impl<T: MockDefault, E> MockDefault for Result<T, E> {
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

auto_impl_mock_default!(bool, i16, i32, u32, f64, String);



// ================
// === MockData ===
// ================

/// Every mock structure implements this trait.
pub trait MockData {}

/// Macro used to generate mock structures. See the expansion of generated structures to learn more.
#[macro_export]
macro_rules! mock_struct {
    ( $([$opt:ident])?
        $name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)? $(=> $deref:ident)?
    ) => {
        #[allow(missing_copy_implementations)]
        #[allow(non_snake_case)]
        #[allow(missing_docs)]
        pub struct $name $(<$($param $(:?$param_tp)?),*>)? {
            $($( $param : PhantomData<$param> ),*)?
        }

        /// # Safety
        /// The usage of [`std::mem::transmute`] is safe here as we transmute ZST types.
        #[allow(unsafe_code)]
        impl$(<$($param $(:?$param_tp)?),*>)?
        $name $(<$($param),*>)? {
            /// Const constructor.
            pub const fn const_new() -> Self {
                unsafe { std::mem::transmute(()) }
            }
        }

        impl$(<$($param $(:?$param_tp)?),*>)?
        Debug for $name $(<$($param),*>)? {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, stringify!($name))
            }
        }

        #[allow(unsafe_code)]
        impl $(<$($param $(:?$param_tp)?),*>)?
        Default for $name $(<$($param),*>)? {
            fn default() -> Self {
                Self::const_new()
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
        mock_struct_into_js_ref! {$name $(<$( $param $(:?$param_tp)?),*>)? $(=> $deref)?}
    };
}

/// Helper of [`mock_struct`].
#[macro_export]
macro_rules! mock_struct_as_ref {
    ([NO_AS_REF] $($ts:tt)*) => {};
    ([] $name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)?
        $(=> $deref:ident)?
    ) => {
        /// # Safety
        /// The usage of [`std::mem::transmute`] is safe here as we transmute ZST types.
        #[allow(unsafe_code)]
        impl<__T__: MockData, $($($param $(:?$param_tp)? ),*)?>
        AsRef<__T__> for $name $(<$($param),*>)? {
            fn as_ref(&self) -> &__T__ {
                unsafe { std::mem::transmute(self) }
            }
        }
    };
}

/// Helper of [`mock_struct`].
#[macro_export]
macro_rules! mock_struct_into_js_ref {
    (JsValue $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)? $(=> $deref:ident)?) => {};
    ($name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)? => JsValue) => {};
    ($name:ident $(<$( $param:ident $(: ?$param_tp:ident)? ),*>)? $(=> $deref:ident)?) => {
        impl $(<$($param $(:?$param_tp)?),*>)?
        From<$name $(<$($param),*>)?> for JsValue {
            fn from(_: $name $(<$($param),*>)?) -> Self {
                default()
            }
        }
    };
}

/// Helper of [`mock_struct`].
#[macro_export]
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

/// Create a mock implementation of a non-public function. Read the docs of [`mock_fn_gen`] to learn
/// more.
#[macro_export(local_inner_macros)]
macro_rules! mock_fn {
    ( $($ts:tt)* ) => {
        mock_fn_gen! {[] $($ts)*}
    };
}

/// Create a mock implementation of a public function. Read the docs of [`mock_fn_gen`] to learn
/// more.
#[macro_export(local_inner_macros)]
macro_rules! mock_pub_fn {
    ( $($ts:tt)* ) => {
        mock_fn_gen! {[pub] $($ts)*}
    };
}

/// Macro used to generate mock methods. Methods look just like their provided signature with a body
/// returning `mock_default()`. There are two special cases: for functions returning `&Self`, and
/// `&mut Self`, which just pass `&self` and `&mut self` to the output, respectively.
#[macro_export(local_inner_macros)]
macro_rules! mock_fn_gen {
    ($viz:tt $name:ident $(<$($fn_tp:ident),*>)? (&self $($args:tt)*) -> &Self ) => {
        $crate::mock_fn_gen_print! {
            $viz $name $(<$($fn_tp),*>)? (&self $($args)*) -> &Self {self}
        }
    };

    ($viz:tt $name:ident $(<$($fn_tp:ident),*>)? (&mut self $($args:tt)*) -> &mut Self ) => {
        $crate::mock_fn_gen_print! {
            $viz $name $(<$($fn_tp),*>)? (&mut self $($args)*) -> &mut Self {self}
        }
    };

    ($viz:tt $name:ident $(<$($fn_tp:ident),*>)? (&self $($args:tt)*) -> &$out:ty ) => {
        $crate::mock_fn_gen_print! {
            $viz $name $(<$($fn_tp),*>)? (&self $($args)*) -> &$out {self.as_ref()}
        }
    };

    ($viz:tt $name:ident $(<$($fn_tp:ident),*>)? ($($args:tt)*) $(-> $out:ty)? ) => {
        $crate::mock_fn_gen_print! {
            $viz $name $(<$($fn_tp),*>)? ($($args)*) $(-> $out)? {mock_default()}
        }
    };
}

/// Macro used to print the final version of the function.
#[macro_export(local_inner_macros)]
macro_rules! mock_fn_gen_print {
    ([$($viz:ident)?] $name:ident $(<$($fn_tp:ident),*>)?
    ( $($args:tt)* ) $(-> $out:ty)? {$($body:tt)*} ) => {
        #[allow(unused_variables)]
        #[allow(clippy::too_many_arguments)]
        #[allow(clippy::should_implement_trait)]
        #[allow(missing_docs)]
        $($viz)? fn $name $(<$($fn_tp),*>)? ( $($args)* ) $(-> $out)? {
            $($body)*
        }
    };
}

/// Combination of [`mock_struct`] and [`mock_pub_fn`].
#[macro_export(local_inner_macros)]
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
                mock_pub_fn!{$fn_name $(<$($fn_tp),*>)? ($($args)*) $(-> $out)?}
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
#[allow(missing_docs)]
pub trait JsCast
where Self: MockData + MockDefault + AsRef<JsValue> + Into<JsValue> {
    fn has_type<T>(&self) -> bool {
        true
    }

    fn dyn_into<T>(self) -> Result<T, Self>
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

mock_data! { JsValue
    fn is_undefined(&self) -> bool;
    fn is_null(&self) -> bool;
    fn from_str(s: &str) -> JsValue;
    fn from_f64(n: f64) -> JsValue;
    fn as_f64(&self) -> Option<f64>;
}

impl JsValue {
    /// NULL value mock.
    pub const NULL: JsValue = JsValue {};
}

impl AsRef<JsValue> for wasm_bindgen::JsValue {
    fn as_ref(&self) -> &JsValue {
        &JsValue::NULL
    }
}

impl From<wasm_bindgen::JsValue> for JsValue {
    fn from(_: wasm_bindgen::JsValue) -> Self {
        default()
    }
}

impl From<js_sys::Uint8Array> for JsValue {
    fn from(_: js_sys::Uint8Array) -> Self {
        default()
    }
}

impl From<f32> for JsValue {
    fn from(_: f32) -> Self {
        default()
    }
}

impl From<&str> for JsValue {
    fn from(_: &str) -> Self {
        default()
    }
}

impl From<&String> for JsValue {
    fn from(_: &String) -> Self {
        default()
    }
}

impl From<String> for JsValue {
    fn from(_: String) -> Self {
        default()
    }
}



// ===============
// === Closure ===
// ===============

mock_data! { [NO_AS_REF] Closure<T: ?Sized>
    fn wrap(_data: Box<T>) -> Closure<T>;
    fn once<F>(_fn_once: F) -> Closure<F>;
}

#[allow(missing_docs)]
impl Closure<dyn FnOnce()> {
    pub fn once_into_js<F>(_fn_once: F) -> JsValue {
        default()
    }
}

#[allow(missing_docs)]
impl<T: ?Sized> Closure<T> {
    pub fn new<F>(_t: F) -> Closure<T>
    where F: Unsize<T> + 'static {
        mock_default()
    }
}

/// The generated structure does not implement a generic [`AsRef`] impl, as the usages base on the
/// fact that there exist exactly one such an impl (provided below), so the type inferencer can
/// monomoprphise more free variables.
#[allow(unsafe_code)]
impl<T: ?Sized> AsRef<JsValue> for Closure<T> {
    fn as_ref(&self) -> &JsValue {
        unsafe { std::mem::transmute(self) }
    }
}

/// This impl is provided to mimic the behavior of the [`wasm_bindgen::Closure`] type. It silences
/// clippy warnings.
impl<T: ?Sized> Drop for Closure<T> {
    fn drop(&mut self) {}
}



// ================
// === Js Prims ===
// ================

// === String ===
mock_data! { JsString => Object
    fn to_string(&self) -> String;
}

impl From<JsString> for String {
    fn from(_: JsString) -> Self {
        "JsString".into()
    }
}

impl From<&JsString> for String {
    fn from(_: &JsString) -> Self {
        "JsString".into()
    }
}


// === Array ===
mock_data! { Array => Object
    fn length(&self) -> u32;
    fn get(&self, index: u32) -> JsValue;
    fn of2(a: &JsValue, b: &JsValue) -> Array;
    fn of3(a: &JsValue, b: &JsValue, c: &JsValue) -> Array;
    fn of4(a: &JsValue, b: &JsValue, c: &JsValue, d: &JsValue) -> Array;
    fn of5(a: &JsValue, b: &JsValue, c: &JsValue, d: &JsValue, e: &JsValue) -> Array;
}


// === Map ===
mock_data! { Map => Object
    fn new() -> Self;
    fn get(&self, key: &JsValue) -> JsValue;
    fn set(&self, key: &JsValue, value: &JsValue) -> Map;
    fn entries(&self) -> Iterator;
}


// === Error ===
mock_data! { Error => Object
    fn new(message: &str) -> Self;
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
    fn new() -> Self;
    fn value_of(&self) -> Object;
    fn keys(object: &Object) -> Array;
}


// === EventTarget ===
mock_data! { EventTarget => Object
    fn remove_event_listener_with_callback
        (&self, tp:&str, f:&Function) -> Result<(), JsValue>;
    fn add_event_listener_with_callback
        (&self, tp:&str, f:&Function) -> Result<(), JsValue>;
    fn add_event_listener_with_callback_and_bool
        (&self, tp:&str, f:&Function, opt:bool) -> Result<(), JsValue>;
    fn add_event_listener_with_callback_and_add_event_listener_options
        (&self, tp:&str, f:&Function, opt:&AddEventListenerOptions)
        -> Result<(), JsValue>;
}


// === Document ===
mock_data! { Document => EventTarget
    fn body(&self) -> Option<HtmlElement>;
    fn head(&self) -> Option<HtmlHeadElement>;
    fn fonts(&self) -> FontFaceSet;
    fn create_element(&self, local_name: &str) -> Result<Element, JsValue>;
    fn get_element_by_id(&self, element_id: &str) -> Option<Element>;
    fn create_text_node(&self, data: &str) -> Text;
}


// === Window ===
mock_data! { Window => EventTarget
    fn open_with_url_and_target(&self, url: &str, target: &str)
        -> Result<Option<Window>, JsValue>;
    fn request_animation_frame(&self, callback: &Function) -> Result<i32, JsValue>;
    fn cancel_animation_frame(&self, handle: i32) -> Result<(), JsValue>;
    fn performance(&self) -> Option<Performance>;
    fn device_pixel_ratio(&self) -> f64;
    fn set_timeout_with_callback_and_timeout_and_arguments_0
        (&self, handler: &Function, timeout: i32) -> Result<i32, JsValue>;
    fn set_interval_with_callback_and_timeout_and_arguments_0
        (&self, handler: &Function, timeout: i32) -> Result<i32, JsValue>;
    fn clear_timeout_with_handle(&self, handle: i32);
    fn clear_interval_with_handle(&self, handle: i32);
}


// === Function ===
mock_data! { Function
    fn call1(&self, context: &JsValue, arg1: &JsValue) -> Result<JsValue, JsValue>;
    fn call2(&self, context: &JsValue, arg1: &JsValue, arg2: &JsValue) -> Result<JsValue, JsValue>;
    fn call3(&self, context: &JsValue, arg1: &JsValue, arg2: &JsValue, arg3: &JsValue)
        -> Result<JsValue, JsValue>;
}


// === AddEventListenerOptions ===
mock_data! { AddEventListenerOptions
    fn new() -> Self;
}
impl AddEventListenerOptions {
    mock_pub_fn!(capture(&mut self, val:bool) -> &mut Self);
    mock_pub_fn!(passive(&mut self, val:bool) -> &mut Self);
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
    fn shift_key(&self) -> bool;
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
    fn get_with_index(&self, index: u32) -> Option<Element>;
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
    fn set_inner_html(&self, value: &str);
    fn set_class_name(&self, value: &str);
    fn set_id(&self, value: &str);
    fn set_attribute(&self, name: &str, value: &str) -> Result<(), JsValue>;
    fn set_scroll_top(&self, value: i32);
    fn prepend_with_node_0(&self) -> Result<(), JsValue>;
    fn prepend_with_node_1(&self, n1: &Node) -> Result<(), JsValue>;
    fn prepend_with_node_2(&self, n1: &Node, n2:&Node) -> Result<(), JsValue>;
    fn prepend_with_node_3(&self, n1: &Node, n2:&Node, n3:&Node) -> Result<(), JsValue>;
}

// === HtmlElement ===
mock_data! { HtmlElement => Element
    fn set_class_name(&self, n: &str);
    fn set_inner_text(&self, value: &str);
    fn inner_text(&self) -> String;
    fn get_elements_by_class_name(&self, class_names: &str) -> HtmlCollection;
    fn style(&self) -> CssStyleDeclaration;
    fn offset_top(&self) -> i32;
}
impl From<HtmlElement> for EventTarget {
    fn from(_: HtmlElement) -> Self {
        default()
    }
}


// === HtmlHeadElement ===
mock_data! { HtmlHeadElement => HtmlElement }


// === HtmlHeadElement ===
mock_data! { Promise
    fn then(&self, cb: &Closure<dyn FnMut(JsValue)>) -> Promise;
}


// === HtmlHeadElement ===
mock_data! { FontFaceSet
    fn ready(&self) -> Result<Promise, JsValue>;
}


// === HtmlDivElement ===
mock_data! { HtmlDivElement => HtmlElement }
impl From<HtmlDivElement> for EventTarget {
    fn from(_: HtmlDivElement) -> Self {
        default()
    }
}


// === HtmlDivElement ===
mock_data! { Text => CharacterData }



// === CharacterData ===
mock_data! { CharacterData => Node }



// === HtmlCanvasElement ===
mock_data! { HtmlCanvasElement => HtmlElement
    fn width(&self) -> u32;
    fn height(&self) -> u32;
    fn set_width(&self, value: u32);
    fn set_height(&self, value: u32);
    fn get_context(&self, context_id: &str) -> Result<Option<Object>, JsValue>;
    fn get_context_with_context_options(
        &self,
        context_id: &str,
        context_options: &JsValue
        ) -> Result<Option<Object>, JsValue>;
}

// === HtmlCanvasElement ===
mock_data! { TextMetrics
    fn actual_bounding_box_right(&self) -> u32;
    fn actual_bounding_box_left(&self) -> u32;
    fn width(&self) -> u32;
}


// === CanvasRenderingContext2d ===
mock_data! { CanvasRenderingContext2d
    fn save(&self);
    fn measure_text(&self, text: &str) -> Result<TextMetrics, JsValue>;
    fn restore(&self);
    fn begin_path(&self);
    fn stroke(&self);
    fn move_to(&self, x: f64, y: f64);
    fn line_to(&self, x: f64, y: f64);
    fn scale(&self, x: f64, y: f64) -> Result<(), JsValue>;
    fn set_fill_style(&self, value: &JsValue);
    fn set_stroke_style(&self, value: &JsValue);
    fn clear_rect(&self, x: f64, y: f64, w: f64, h: f64);
    fn set_line_width(&self, value: f64);
    fn translate(&self, x: f64, y: f64) -> Result<(), JsValue>;
    fn fill_rect(&self, x: f64, y: f64, w: f64, h: f64);
    fn set_font(&self, font: &str);
    fn set_text_align(&self, text_align: &str);
    fn fill_text(&self, text: &str, x: f64, y: f64) -> Result<(), JsValue>;
    fn draw_image_with_html_canvas_element_and_sw_and_sh_and_dx_and_dy_and_dw_and_dh(
        &self, image: &HtmlCanvasElement,
        sx: f64, sy: f64, sw: f64, sh: f64, dx: f64, dy: f64, dw: f64, dh: f64
        ) -> Result<(), JsValue>;
}


// === Node ===
mock_data! { Node => EventTarget
    fn parent_node(&self) -> Option<Node>;
    fn remove_child(&self, child: &Node) -> Result<Node, JsValue>;
    fn set_text_content(&self, value: Option<&str>);
    fn append_child(&self, node: &Node) -> Result<Node, JsValue>;
    fn first_child(&self) -> Option<Node>;
    fn last_child(&self) -> Option<Node>;
    fn insert_before(
        &self,
        node: &Node,
        child: Option<&Node>
    ) -> Result<Node, JsValue>;
}



// ===========
// === CSS ===
// ===========

// === CssStyleDeclaration ===
mock_data! { CssStyleDeclaration => Object
    fn set_property(&self, property: &str, value: &str) -> Result<(), JsValue>;
}



// =============
// === Other ===
// =============

// === Performance ===
mock_data! { Performance => EventTarget
    fn now(&self) -> f64;
    fn time_origin(&self) -> f64;
}



// ===============
// === Reflect ===
// ===============

mock_data! { Reflect
    fn get(target: &JsValue, key: &JsValue) -> Result<JsValue, JsValue>;
    fn set(
        target: &JsValue,
        property_key: &JsValue,
        value: &JsValue
    ) -> Result<bool, JsValue>;
}



// ===========================
// === Window and Document ===
// ===========================

#[allow(non_upper_case_globals)]
#[allow(missing_docs)]
pub static window: Window = Window {};

#[allow(non_upper_case_globals)]
#[allow(missing_docs)]
pub static document: Document = Document::const_new();



// ================
// === Iterator ===
// ================

#[derive(Default, Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct Iterator;
impl MockDefault for Iterator {
    fn mock_default() -> Self {
        default()
    }
}

#[derive(Default, Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct IntoIter;
impl MockDefault for IntoIter {
    fn mock_default() -> Self {
        default()
    }
}

impl IntoIterator for Iterator {
    type Item = Result<JsValue, JsValue>;
    type IntoIter = IntoIter;
    fn into_iter(self) -> IntoIter {
        default()
    }
}

impl std::iter::Iterator for IntoIter {
    type Item = Result<JsValue, JsValue>;
    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
