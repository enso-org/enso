//! This is Rust wrapper for the [`react-toastify`](https://fkhadra.github.io/react-toastify) library.

use crate::prelude::*;
use serde::Deserialize;
use serde::Serialize;
use wasm_bindgen::prelude::*;


// use frp::web::JsValue;


/// Toastify library import within the application.
const TOASTIFY_FIELD_NAME: &str = "toast";


/// Helper structure for [`AutoClose`] so it serializes in a way compatible with the JS library.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(untagged)]
enum AutoCloseInner {
    /// Auto close after a delay expressed in milliseconds.
    Delay(u32),
    /// Do not auto close. The boolean value must be `false`.
    ShouldEver(bool),
}

/// Represents the auto close delay of a toast notification.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
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
pub enum Theme {
    Light,
    Dark,
    Colored,
}

/// Customization options for Toast.
///
/// Note that it is not necessary to set any of these. All options marked as `None` will be
/// auto-filled with default values.
#[derive(Debug, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
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
    pub container_id:        Option<String>,
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


pub mod js {
    use super::*;

    pub fn get_toast() -> Result<JsValue, JsValue> {
        // let window = &ensogl::system::web::window; // JSValue wrapper
        let window = ensogl::system::web::binding::wasm::get_window();
        let app_field_name = enso_config::CONFIG.window_app_scope_name;
        // Hopefully, window contains a field with the name, so we can access it.
        let app = js_sys::Reflect::get(window.as_ref(), &app_field_name.into())?;
        let toastify = js_sys::Reflect::get(app.as_ref(), &TOASTIFY_FIELD_NAME.into())?;
        Ok(toastify)
    }


    // Wrappers for [`toast`](https://react-hot-toast.com/docs/toast) API.
    #[wasm_bindgen(inline_js = r#"
                export function sendToast(toast, message, method, options) {
                    const target = method ? toast[method] : toast;
                    console.warn("toast", toast, "message", message, "method", method, "options", options);
                    console.warn("target", target);
                    return target(message, options);
                }

                export function dismissToast(toast, id) {
                    return toast.dismiss(id);
                }
                "#)]
    extern "C" {
        /// The wrapper for the toast API.
        pub type ToastAPI;

        /// The unique identifier of a toast.
        pub type Id;

        /// Generalized wrapper for calling any kind of toast.
        #[wasm_bindgen(catch)]
        pub fn sendToast(
            toast: &JsValue,
            message: &str,
            method: &JsValue,
            options: &JsValue,
        ) -> Result<Id, JsValue>;

        /// Wrapper for dismissing a toast.
        #[wasm_bindgen(catch)]
        pub fn dismissToast(toast: &JsValue, id: &Id) -> Result<(), JsValue>;

        /// Wrapper for dismissing a toast.
        #[wasm_bindgen(catch, js_name = dismissToast)]
        pub fn dismissAllToasts(toast: &JsValue) -> Result<(), JsValue>;

    }

    impl Debug for Id {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let obj_as_js_value = JsValue::from(self);
            write!(f, "{obj_as_js_value:?}")
        }
    }

    impl Id {
        /// Dismisses the toast.
        pub fn dismiss(&self) -> Result<(), JsValue> {
            dismissToast(&get_toast()?, self)
        }
    }

    /// Wrapper for any toast.
    pub fn toast(message: &str, r#type: Type, options: &JsValue) -> Result<Id, JsValue> {
        let method: &str = r#type.as_ref();
        let toast = get_toast()?;
        warn!("toast: {:?}", toast);
        warn!("message: {:?}", message);
        sendToast(&get_toast()?, message, &method.into(), options)
    }
}

pub use js::Id;

pub fn toast_any(message: &str, r#type: Type, options: &Option<Options>) -> Result<Id, JsValue> {
    let options = match options {
        Some(options) => serde_wasm_bindgen::to_value(options)?,
        None => JsValue::UNDEFINED,
    };
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
}
