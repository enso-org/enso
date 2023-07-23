//! This is Rust wrapper for the [`react-toastify`](https://fkhadra.github.io/react-toastify) library.
//!
//! # Example
//!
//! ```no_run
//! use ide_view::notification;
//! # fn main() -> Result<(), wasm_bindgen::JsValue> {
//! use ide_view::notification::UpdateOptions;
//! let handle = notification::info(
//!     "Undo triggered in UI.",
//!     &Some(notification::Options {
//!         theme: Some(notification::Theme::Dark),
//!         auto_close: Some(notification::AutoClose::Never()),
//!         draggable: Some(false),
//!         close_on_click: Some(false),
//!         ..Default::default()
//!     }),
//! )?;
//! handle.update(&UpdateOptions::default().render_string("Undo done."))?;
//! # Ok(())
//! # }
//! ```

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use gloo_utils::format::JsValueSerdeExt;
use serde::Deserialize;
use serde::Serialize;


// ==============
// === Export ===
// ==============

pub mod js;

pub use js::Id;



// ===================
// === Primary API ===
// ===================

/// Send any kind of notification.
pub fn send_any(message: &str, r#type: Type, options: &Option<Options>) -> Result<Id, JsValue> {
    let options = match options {
        Some(options) => options.try_into()?,
        None => JsValue::UNDEFINED,
    };
    js::toast(message, r#type, &options)
}

/// Send an info notification.
pub fn info(message: &str, options: &Option<Options>) -> Result<Id, JsValue> {
    send_any(message, Type::Info, options)
}

/// Send a warning notification.
pub fn warning(message: &str, options: &Option<Options>) -> Result<Id, JsValue> {
    send_any(message, Type::Warning, options)
}

/// Send a error notification.
pub fn error(message: &str, options: &Option<Options>) -> Result<Id, JsValue> {
    send_any(message, Type::Error, options)
}

/// Send a success notification.
pub fn success(message: &str, options: &Option<Options>) -> Result<Id, JsValue> {
    send_any(message, Type::Success, options)
}



// =============================
// === JS-conversion helpers ===
// =============================


// === Rust->JS error ===

/// Convert arbitrary Rust error value into JsValue-based error.
pub fn to_js_error(error: impl std::error::Error) -> JsValue {
    js_sys::Error::new(&error.to_string()).into()
}


// === Rust->JS conversion ===

/// Macro that implements TryFrom for given `type` to/from JsValue using
/// [`gloo-utils::format::JsValueSerdeExt`].
///
/// Implements:
/// - `TryFrom<&type> for JsValue`
/// - `TryFrom<JsValue> for type`
macro_rules! impl_try_from_jsvalue {
    ($type:ty) => {
        impl TryFrom<&$type> for JsValue {
            type Error = JsValue;

            fn try_from(value: &$type) -> Result<Self, Self::Error> {
                JsValue::from_serde(value).map_err($crate::notification::to_js_error)
            }
        }

        impl TryFrom<JsValue> for $type {
            type Error = JsValue;

            fn try_from(js_value: JsValue) -> Result<Self, Self::Error> {
                js_value.into_serde().map_err($crate::notification::to_js_error)
            }
        }
    };
}


// === SerializableJsValue ===

/// A wrapper around a `JsValue` that implements the `Serialize` and `Deserialize` traits.
#[derive(Clone, Debug, Deref, DerefMut, AsRef, From)]
pub struct SerializableJsValue(pub JsValue);

impl Serialize for SerializableJsValue {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let value = js_sys::JSON::stringify(self).map(String::from).map_err(|e| {
            serde::ser::Error::custom(format!("Failed to stringify JsValue: {e:?}"))
        })?;
        let value = serde_json::from_str::<serde_json::Value>(&value)
            .map_err(|e| serde::ser::Error::custom(format!("Failed to parse JSON: {e:?}")))?;
        value.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SerializableJsValue {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = serde_json::Value::deserialize(deserializer)?;
        JsValue::from_serde(&value).map(SerializableJsValue).map_err(serde::de::Error::custom)
    }
}


// ===============
// === Options ===
// ===============

// === AutoClose ===

/// Helper structure for [`AutoClose`] so it serializes in a way compatible with the JS library.
///
/// Users of this module should not deal directly with this type. Instead, rely on
/// [`AutoClose::After`] or [`AutoClose::Never`]
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AutoCloseInner {
    /// Auto close after a delay expressed in milliseconds.
    Delay(u32),
    /// Do not auto close. The boolean value must be `false`.
    ShouldEver(bool),
}

/// Represents the auto-close delay of a notification.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Deref)]
#[serde(transparent)]
pub struct AutoClose(AutoCloseInner);

impl AutoClose {
    /// Auto close after a delay expressed in milliseconds.
    #[allow(non_snake_case)]
    pub fn After(delay_ms: u32) -> Self {
        Self(AutoCloseInner::Delay(delay_ms))
    }

    /// Do not auto close.
    #[allow(non_snake_case)]
    pub fn Never() -> Self {
        Self(AutoCloseInner::ShouldEver(false))
    }
}


// === Type ===

/// Represents the type of a notification.
///
/// Affects styling and icon.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, strum::AsRefStr)]
#[serde(rename_all = "kebab-case")]
#[strum(serialize_all = "kebab-case")]
#[allow(missing_docs)]
pub enum Type {
    Info,
    Success,
    Warning,
    Error,
    Default,
}


// === Position ===

/// Represents the position of a notification on the screen.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[allow(missing_docs)]
pub enum Position {
    TopRight,
    TopCenter,
    TopLeft,
    BottomRight,
    BottomCenter,
    BottomLeft,
}


// === DraggableDirection ===

/// Direction that the notification can be dragged (swiped) to dismiss it.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[allow(missing_docs)]
pub enum DraggableDirection {
    X,
    Y,
}



// === Theme ===

/// Themes supported by the library.
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[allow(missing_docs)]
pub enum Theme {
    Light,
    Dark,
    Colored,
}


// === Options ===

/// Customization options for the notification.
///
/// Note that it is not necessary to set any of these. All options marked as `None` will be
/// auto-filled with default values.
#[derive(Debug, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct Options {
    /// Unique notification identifier.
    pub toast_id:            Option<String>,
    /// Type of the notification, affecting styling and icon. Default: `Type::Default`.
    pub r#type:              Option<Type>,
    /// The position where the toast should appear. Default: `bottom-right`.
    pub position:            Option<Position>,
    /// Time in milliseconds after the toast is removed. Default: `5000`.
    pub auto_close:          Option<AutoClose>,
    /// Whether to show a close button. Default: `true`.
    pub close_button:        Option<bool>,
    // pub transition:          Option<ToastTransition>,
    /// Hide or show the progress bar. `Default: false`
    pub hide_progress_bar:   Option<bool>,
    /// Pause the timer when the mouse hover the toast. Default: `true`.
    pub pause_on_hover:      Option<bool>,
    /// Pause the timer when the window loses focus. Default: `true`.
    pub pause_on_focus_loss: Option<bool>,
    /// Remove the toast when clicked. Default: `true`.
    pub close_on_click:      Option<bool>,
    /// An optional css class to set.
    pub class_name:          Option<String>,
    /// An optional css class to set for the toast content.
    pub body_class_name:     Option<String>,
    /// An optional inline style to apply. This should be a JS object with CSS properties.
    pub style:               Option<SerializableJsValue>,
    /// An optional inline style to apply for the toast content. This should be a JS object with
    /// CSS properties.
    pub body_style:          Option<SerializableJsValue>,
    /// An optional css class to set for the progress bar.
    pub progress_class_name: Option<String>,
    /// An optional inline style to apply for the progress bar. This should be a JS object with
    /// CSS properties.
    pub progress_style:      Option<SerializableJsValue>,
    /// Allow toast to be draggable. `Default: true`.
    pub draggable:           Option<bool>,
    /// The percentage of the toast's width it takes for a drag to dismiss a toast. `Default: 80`.
    pub draggable_percent:   Option<f32>,
    /// Specify in which direction should you swipe to dismiss the toast. `Default: "x"`.
    pub draggable_direction: Option<DraggableDirection>,
    /// Set id to handle multiple `ToastContainer` instances.
    pub container_id:        Option<String>,
    /// Define the [ARIA role](https://www.w3.org/WAI/PF/aria/roles) for the notification. `Default: "alert"`.
    pub role:                Option<String>,
    /// Add a delay in ms before the toast appear.
    pub delay:               Option<u32>,
    /// Whether the ongoing action is still in progress.
    ///
    /// The auto-close timer will not start if this is set to `true`.
    pub is_loading:          Option<bool>,
    /// Support right to left display content. `Default: false`.
    pub rtl:                 Option<bool>,
    /// Theme to use.
    pub theme:               Option<Theme>,
    /// Used to display a custom icon. Set it to `false` to prevent the icons from being displayed
    pub icon:                Option<bool>,
    /// Any additional data to pass to the toast.
    pub data:                Option<SerializableJsValue>,
}

impl_try_from_jsvalue! {Options}


// === Update ===

/// Update options for a toast.
///
/// Just like [`Options`], but also includes a `render` field that allows to update the
/// notification message.
#[derive(Debug, Serialize, Deserialize, Default, Deref, DerefMut)]
#[serde(rename_all = "camelCase")]
pub struct UpdateOptions {
    /// New state of the toast.
    #[deref]
    #[deref_mut]
    #[serde(flatten)]
    pub options: Options,
    /// Used to update a toast. Pass any valid ReactNode(string, number, component).
    pub render:  Option<SerializableJsValue>,
}

impl_try_from_jsvalue! { UpdateOptions }

impl UpdateOptions {
    /// Allows set a new message in the notification.
    ///
    /// Useful only for update a toast.
    pub fn render_string(mut self, render: impl AsRef<str>) -> Self {
        let message = render.as_ref();
        let value = JsValue::from_str(message);
        self.render = Some(value.into());
        self
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;
    use wasm_bindgen_test::wasm_bindgen_test_configure;

    wasm_bindgen_test_configure!(run_in_browser);

    #[test]
    fn test_auto_close_delay() {
        let auto_close = AutoClose::After(5000);
        let serialized = serde_json::to_string(&auto_close).unwrap();
        assert_eq!(serialized, "5000");
    }

    #[test]
    fn test_auto_close_never() {
        let auto_close = AutoClose::Never();
        let serialized = serde_json::to_string(&auto_close).unwrap();
        assert_eq!(serialized, "false");
    }

    #[wasm_bindgen_test]
    fn test_options_marshalling() {
        let options = Options { theme: Some(Theme::Dark), ..default() };
        let js_value = JsValue::try_from(&options).unwrap();
        // Make sure that `js_value` is a valid JS object.
        assert!(js_value.is_object());
        let js_object = js_value.dyn_into::<js_sys::Object>().unwrap();
        // Make sure that `js_object` has a `theme` property.
        assert!(js_object.has_own_property(&"theme".into()));
        // Make sure that `js_object.theme` is a string.
        let theme = js_sys::Reflect::get(&js_object, &"theme".into()).unwrap();
        assert!(theme.is_string());
        // Make sure that `js_object.theme` is equal to "dark".
        let theme = theme.as_string().unwrap();
        assert_eq!(theme, "dark");
    }
}
