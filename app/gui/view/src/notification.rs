//! This is Rust wrapper for the [`react-toastify`](https://fkhadra.github.io/react-toastify) library.

use crate::prelude::*;
use serde::Deserialize;
use serde::Serialize;
use wasm_bindgen::prelude::*;

pub mod js;

/// Macro that implements TryFrom for given `type` to/from JsValue using [`serde_wasm_bindgen`].
///
/// Implements:
/// - `TryFrom<&type> for JsValue`
/// - `TryFrom<JsValue> for type`
macro_rules! impl_try_from_jsvalue {
    ($type:ty) => {
        impl TryFrom<&$type> for JsValue {
            type Error = JsValue;

            fn try_from(value: &$type) -> Result<Self, Self::Error> {
                serde_wasm_bindgen::to_value(value).map_err(|e| e.into())
            }
        }

        impl TryFrom<JsValue> for $type {
            type Error = JsValue;

            fn try_from(js_value: JsValue) -> Result<Self, Self::Error> {
                serde_wasm_bindgen::from_value(js_value).map_err(|e| e.into())
            }
        }
    };
}

/// Helper structure for [`AutoClose`] so it serializes in a way compatible with the JS library.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AutoCloseInner {
    /// Auto close after a delay expressed in milliseconds.
    Delay(u32),
    /// Do not auto close. The boolean value must be `false`.
    ShouldEver(bool),
}

/// Represents the auto close delay of a toast notification.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Deref)]
#[serde(transparent)]
pub struct AutoClose(AutoCloseInner);

impl AutoClose {
    /// Auto close after a delay expressed in milliseconds.
    #[allow(non_snake_case)]
    pub fn After(delay_ms: u32) -> Self {
        Self(AutoCloseInner::Delay(delay_ms))
    }

    /// Do not auto close. The boolean value must be `false`.
    #[allow(non_snake_case)]
    pub fn Never() -> Self {
        Self(AutoCloseInner::ShouldEver(false))
    }
}

/// Represents the type of a toast notification.
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

/// Represents the position of a toast notification on the screen.
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

/// Direction that the toast can be dragged (swiped) to dismiss it.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DraggableDirection {
    X,
    Y,
}

/// Themes supported by the library.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[allow(missing_docs)]
pub enum Theme {
    Light,
    Dark,
    Colored,
}

/// Identifies a [`ToastContainer`](https://fkhadra.github.io/react-toastify/api/toast-container)
/// instance.
///
/// This is used to identify the container to which a toast should be added. Not needed when there
/// is only one container.
#[derive(Debug, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ContainerId {
    /// The `containerId` as set on the `<ToastContainer>` element.
    pub container_id: Option<String>,
}

impl_try_from_jsvalue! {ContainerId}

impl ContainerId {
    /// Clear queue of notifications for this container (relevant if limit is set).
    pub fn clear_waiting_queue(&self) -> Result<(), JsValue> {
        js::get_toast()?.clear_waiting_queue_in(&self.try_into()?)
    }
}

/// Customization options for Toast.
///
/// Note that it is not necessary to set any of these. All options marked as `None` will be
/// auto-filled with default values.
#[derive(Debug, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct Options {
    pub toast_id:            Option<String>,
    ///
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
    #[serde(flatten)]
    pub container_id:        Option<ContainerId>,
    /// Define the [ARIA role](https://www.w3.org/WAI/PF/aria/roles) for the toast notification. `Default: "alert"`.
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

/// Update options for a toast.
#[derive(Debug, Serialize, Deserialize, Default, Deref, DerefMut)]
#[serde(rename_all = "camelCase")]
pub struct UpdateOptions {
    #[deref]
    #[deref_mut]
    #[serde(flatten)]
    pub options: Options,
    /// Used to update a toast. Pass any valid ReactNode(string, number, component).
    pub render:  Option<SerializableJsValue>,
}

impl_try_from_jsvalue! {UpdateOptions}

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

/// A wrapper around a `JsValue` that implements the `Serialize` and `Deserialize` traits.
#[derive(Clone, Debug)]
pub struct SerializableJsValue(pub JsValue);

impl Deref for SerializableJsValue {
    type Target = JsValue;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for SerializableJsValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<JsValue> for SerializableJsValue {
    fn as_ref(&self) -> &JsValue {
        &self.0
    }
}

impl From<JsValue> for SerializableJsValue {
    fn from(js_value: JsValue) -> Self {
        SerializableJsValue(js_value)
    }
}

impl Serialize for SerializableJsValue {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let de = serde_wasm_bindgen::Deserializer::from(self.0.clone());
        serde_transcode::transcode(de, serializer)
    }
}

impl<'de> Deserialize<'de> for SerializableJsValue {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let ser = serde_wasm_bindgen::Serializer::new();
        let js_value =
            serde_transcode::transcode(deserializer, &ser).map_err(serde::de::Error::custom)?;
        Ok(SerializableJsValue(js_value))
    }
}



#[derive(Debug, Serialize, Deserialize)]
pub struct ToastStyle {
    background: Option<String>,
    color:      Option<String>,
}



pub use js::Id;

pub fn toast_any(message: &str, r#type: Type, options: &Option<Options>) -> Result<Id, JsValue> {
    warn!("options: {:?}", serde_json::to_string(options));
    let options = match options {
        Some(options) => serde_wasm_bindgen::to_value(options)?,
        None => JsValue::UNDEFINED,
    };
    warn!("options: {:?}", js_sys::JSON::stringify(&options));
    js::toast(message, r#type, &options)
}

pub fn info(message: &str, options: &Option<Options>) -> Result<Id, JsValue> {
    toast_any(message, Type::Info, options)
}

pub fn error(message: &str, options: &Option<Options>) -> Result<Id, JsValue> {
    toast_any(message, Type::Error, options)
}

pub fn success(message: &str, options: &Option<Options>) -> Result<Id, JsValue> {
    toast_any(message, Type::Success, options)
}

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
    fn test_toast() {
        let options = Options { auto_close: Some(AutoClose::After(5000)), ..Default::default() };
        let js_value = serde_wasm_bindgen::to_value(&options).unwrap();
        assert!(js_value.is_object());
        let js_object = js_value.dyn_into::<js_sys::Object>().unwrap();
        // JS autoclose field
        assert_eq!("{..}", js_sys::JSON::stringify(&js_object).unwrap().as_string().unwrap());
        let js_auto_close = js_sys::Reflect::get(&js_object, &"autoClose".into()).unwrap();
        let js_auto_close_number = js_auto_close.as_f64().unwrap() as u32;
        assert_eq!(5000, js_auto_close_number);
        // assert_eq!(5000, js_object.get("autoClose").unwrap().as_f64().unwrap() as u32);
        // assert_eq!(1, js_object.().length());
    }
}
