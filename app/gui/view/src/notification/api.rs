//! This is Rust wrapper for the [`react-toastify`](https://fkhadra.github.io/react-toastify) library.
//!
//! # Example
//!
//! ```no_run
//! use ide_view::notification::api::*;
//! # fn main() -> Result<(), wasm_bindgen::JsValue> {
//! let handle = info(
//!     "Undo triggered in UI.",
//!     &Some(Options {
//!         theme: Some(Theme::Dark),
//!         auto_close: Some(AutoClose::Never()),
//!         draggable: Some(false),
//!         close_on_click: Some(false),
//!         ..Default::default()
//!     }),
//! )?;
//! handle.update(&UpdateOptions::default().raw_text_content("Undo done."))?;
//! # Ok(())
//! # }
//! ```

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::notification::js;

use gloo_utils::format::JsValueSerdeExt;
use serde::Deserialize;
use serde::Serialize;


// ==============
// === Export ===
// ==============

pub use crate::notification::js::Id;



// ===================
// === Primary API ===
// ===================

/// Send any kind of notification.
pub fn send_any(message: &Content, r#type: Type, options: &Option<Options>) -> Result<Id, JsValue> {
    let options = match options {
        Some(options) => options.try_into()?,
        None => JsValue::UNDEFINED,
    };
    js::toast(message, r#type, &options)
}

/// Send an info notification.
pub fn info(message: impl Into<Content>, options: &Option<Options>) -> Result<Id, JsValue> {
    send_any(&message.into(), Type::Info, options)
}

/// Send a warning notification.
pub fn warning(message: impl Into<Content>, options: &Option<Options>) -> Result<Id, JsValue> {
    send_any(&message.into(), Type::Warning, options)
}

/// Send a error notification.
pub fn error(message: impl Into<Content>, options: &Option<Options>) -> Result<Id, JsValue> {
    send_any(&message.into(), Type::Error, options)
}

/// Send a success notification.
pub fn success(message: impl Into<Content>, options: &Option<Options>) -> Result<Id, JsValue> {
    send_any(&message.into(), Type::Success, options)
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
                JsValue::from_serde(value).map_err($crate::notification::api::to_js_error)
            }
        }

        impl TryFrom<JsValue> for $type {
            type Error = JsValue;

            fn try_from(js_value: JsValue) -> Result<Self, Self::Error> {
                js_value.into_serde().map_err($crate::notification::api::to_js_error)
            }
        }
    };
}

// === SerializableJsValue ===

/// A wrapper around a `JsValue` that implements the `Serialize` and `Deserialize` traits.
#[derive(Clone, Debug, Deref, DerefMut, AsRef, From, PartialEq)]
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
// === Content ===
// ===============

/// The notification content.
///
/// In future this will be extended to support more than just plain text (e.g. HTML).
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(untagged)]
pub enum Content {
    /// Plain text.
    Text(String),
}

impl From<&str> for Content {
    fn from(text: &str) -> Self {
        Content::Text(text.to_owned())
    }
}

impl From<String> for Content {
    fn from(text: String) -> Self {
        Content::Text(text)
    }
}

impl From<&String> for Content {
    fn from(text: &String) -> Self {
        Content::Text(text.to_owned())
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
#[derive(Debug, Clone, Copy, Serialize, Deserialize, strum::AsRefStr, strum::Display)]
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
#[serde(rename_all = "kebab-case")]
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
#[derive(Clone, Debug, Serialize, Deserialize, Default)]
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

impl Options {
    /// Popup will not disappear on itself nor allow user to dismiss it.
    ///
    /// The only way fot this notification to disappear is to call `dismiss()` manually. Otherwise,
    /// it will stay on screen indefinitely.
    pub fn always_present(mut self) -> Self {
        self.set_always_present();
        self
    }

    /// Mutable version of [`Options::always_present`].
    pub fn set_always_present(&mut self) {
        self.auto_close = Some(AutoClose::Never());
        self.close_button = Some(false);
        self.close_on_click = Some(false);
        self.draggable = Some(false);
    }
}


// === Update ===

/// Update options for a toast.
///
/// Just like [`Options`], but also includes a `render` field that allows to update the
/// notification message.
#[derive(Clone, Debug, Serialize, Deserialize, Default, Deref, DerefMut)]
#[serde(rename_all = "camelCase")]
pub struct UpdateOptions {
    /// New state of the toast.
    #[deref]
    #[deref_mut]
    #[serde(flatten)]
    pub options: Options,
    /// Used to update a toast content.
    pub render:  Option<Content>,
}

impl_try_from_jsvalue! { UpdateOptions }

impl UpdateOptions {
    /// Allows set a new message in the notification.
    ///
    /// Useful only for update a toast.
    pub fn raw_text_content(mut self, render: impl AsRef<str>) -> Self {
        let message = render.as_ref();
        self.render = Some(message.into());
        self
    }

    /// Allows set a new message in the notification.
    ///
    /// This sets is as plain text (it will not be interpreted as HTML).
    pub fn set_raw_text_content(&mut self, render: impl Into<String>) {
        let content = Content::Text(render.into());
        self.render = Some(content);
    }
}



// ====================
// === Notification ===
// ====================

/// A persistent notification.
///
/// It can be updated or dismissed.
#[derive(Clone, CloneRef, Debug)]
pub struct Notification {
    /// The unique notification id. We use it to dismiss or update the notification.
    id:         Rc<RefCell<Id>>,
    /// The notification options. They will be used to show/update the notification.
    options:    Rc<RefCell<UpdateOptions>>,
    /// The options that we have used to show the notification the last time.
    last_shown: Rc<RefCell<Option<UpdateOptions>>>,
}

impl Default for Notification {
    fn default() -> Self {
        Self::new(default())
    }
}

impl From<Id> for Notification {
    fn from(id: Id) -> Self {
        let id = Rc::new(RefCell::new(id));
        let options = default();
        let last_shown = default();
        Self { id, options, last_shown }
    }
}

impl Display for Notification {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&*self.id.borrow(), f)
    }
}

impl Notification {
    /// Create a new notification archetype.
    ///
    /// It will not be shown until you call [`Notification::show`].
    pub fn new(options: UpdateOptions) -> Self {
        let id = Id::new_unique();
        let id = Rc::new(RefCell::new(id));
        let options = Rc::new(RefCell::new(options));
        let last_shown = default();
        Self { id, options, last_shown }
    }

    /// Update the notification state.
    ///
    /// If the visualization is being shown, it will be updated. If not, changes will appear the
    /// next time the notification is [`shown`](Notification::show).
    pub fn set_options(&self, options: UpdateOptions) -> Result<(), JsValue> {
        self.options.replace(options);
        let ret = self.id.borrow().update(&self.options.borrow());
        if ret.is_ok() {
            let new_options = self.options.borrow().clone();
            // If the ID was updated, we need to keep track of it.
            if let Some(new_id) = new_options.toast_id.as_ref() {
                self.id.replace(new_id.as_str().into());
            }
            self.last_shown.replace(Some(new_options));
        }
        ret
    }

    /// Convenience wrapper for [`Notification::set_options`].
    ///
    /// Allows to modify the options in place.
    pub fn update(&self, f: impl FnOnce(&mut UpdateOptions)) -> Result<(), JsValue> {
        let mut options = self.options.take();
        f(&mut options);
        self.set_options(options)
    }

    /// Display the notification.
    ///
    /// If it is already being shown, nothing will happen.
    pub fn show(&self) -> Result<(), JsValue> {
        if !self.id.borrow().is_active()? {
            let options = self.options.borrow_mut();
            let content = options.render.as_ref();
            let options_js = (&*options).try_into()?;
            let toast_id_field = JsValue::from_str(js::TOAST_ID_FIELD_IN_OPTIONS);
            let toast_type = options.r#type.unwrap_or(Type::Info);
            js_sys::Reflect::set(&options_js, &toast_id_field, &self.id.borrow())?;
            let toast = |content| js::toast(content, toast_type, &options_js);
            if let Some(content) = content {
                toast(content)?;
            } else {
                const EMPTY_MESSAGE: &str = "";
                toast(&EMPTY_MESSAGE.into())?;
            }
        }
        Ok(())
    }

    /// Dismiss the notification.
    pub fn dismiss(&self) -> Result<(), JsValue> {
        let ret = self.id.borrow().dismiss();
        if ret.is_ok() {
            // We generate a new ID. This is because we don't want to reuse the same ID.
            // Otherwise, while the old notification is still fading out, an attempt to
            // show a new notification with the same ID will silently fail.
            let id = Id::new_unique();
            self.id.replace(id);
        }
        ret
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
    fn auto_close_delay() {
        let auto_close = AutoClose::After(5000);
        let serialized = serde_json::to_string(&auto_close).unwrap();
        assert_eq!(serialized, "5000");
    }

    #[test]
    fn auto_close_never() {
        let auto_close = AutoClose::Never();
        let serialized = serde_json::to_string(&auto_close).unwrap();
        assert_eq!(serialized, "false");
    }

    #[wasm_bindgen_test]
    fn options_marshalling() {
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

    #[wasm_bindgen_test]
    fn contents_marshalling() {
        let text_contents = Content::Text("Hello, world!".into());
        let js_value = JsValue::from_serde(&text_contents).unwrap();
        // Make sure that `js_value` is a valid JS String.
        assert!(js_value.is_string());
        let js_string = js_value.as_string().unwrap();
        // Make sure that `js_string` is equal to "Hello, world!".
        assert_eq!(js_string, "Hello, world!");
        // Check for round-trip.
        let back = js_value.into_serde::<Content>().unwrap();
        assert_eq!(back, text_contents);
    }
}
