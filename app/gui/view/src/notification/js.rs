//! This module provides relatively low-level wrappers for the
//! [`react-toastify`](https://fkhadra.github.io/react-toastify/introduction) library API.

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::notification::api::Content;
use crate::notification::api::Type;
use crate::notification::api::UpdateOptions;

use gloo_utils::format::JsValueSerdeExt;
use uuid::Uuid;



// ==================
// === Constants ===
// =================

/// Toastify's [`toast API`](https://fkhadra.github.io/react-toastify/api/toast) field name within
/// our application JS object.
pub const TOAST_FIELD_NAME: &str = "toast";

/// The name of the field with the toast ID in the options object.
///
/// See [the documentation](https://fkhadra.github.io/react-toastify/api/toast/#props) for details.
pub const TOAST_ID_FIELD_IN_OPTIONS: &str = "toastId";


// ========================
// === Toast API Handle ===
// ========================

/// Get the global (set in JS app object) toast API handle.
pub fn get_toast() -> Result<ToastAPI, JsValue> {
    // let window = &ensogl::system::web::window; // JSValue wrapper
    let window = ensogl::system::web::binding::wasm::get_window();
    let app_field_name = enso_config::CONFIG.window_app_scope_name;
    // Hopefully, window contains a field with the name, so we can access it.
    let app = js_sys::Reflect::get(window.as_ref(), &app_field_name.into())?;
    let toastify = js_sys::Reflect::get(app.as_ref(), &TOAST_FIELD_NAME.into())?;
    Ok(toastify.into())
}



// ===================
// === JS bindings ===
// ===================

/// Wrappers for [`toast`](https://react-hot-toast.com/docs/toast) API and related helpers.
#[wasm_bindgen(inline_js = r#"
    export function sendToast(toast, message, method, options) {
        const target = toast[method];
        return target(message, options);
    }

    // See [`pretty_print`] Rust wrapper below for the documentation.
    export function prettyPrint(value) {
        try {
            // If it's an `Error`, print its standard properties.
            if (value instanceof Error) {
                const { name, message, stack } = value;
                const errorDetails = { name, message, stack };
                return JSON.stringify(errorDetails, null, 2);
            }

            // If it's an object (not `null` and not an `Array`), pretty-print its properties.
            if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
                return JSON.stringify(value, null, 2);
            }

            // If it's a primitive value or an `Array`, just convert it to a string.
            return String(value);
        } catch (error) {
            return prettyPrint(error);
        }
    }
    "#)]
extern "C" {
    /// The wrapper for the toastify API.
    #[derive(Clone, Debug)]
    pub type ToastifyAPI;

    /// The wrapper for the toast API.
    #[derive(Clone, Debug)]
    pub type ToastAPI;

    /// The unique identifier of a toast.
    #[derive(Clone, Debug)]
    pub type Id;

    /// The unique identifier of a toast container.
    #[derive(Clone, Debug)]
    pub type ContainerId;

    /// Generalized wrapper for calling any kind of toast.
    #[wasm_bindgen(catch)]
    #[allow(unsafe_code)]
    pub fn sendToast(
        this: &ToastAPI,
        message: &JsValue,
        method: &str,
        options: &JsValue,
    ) -> Result<Id, JsValue>;

    /// Supply a promise or a function that return a promise and the notification will be
    /// updated if it resolves or fails. When the promise is pending a spinner is displayed.
    #[wasm_bindgen(catch, method, js_name = promise)]
    #[allow(unsafe_code)]
    pub fn promise(
        this: &ToastAPI,
        promise: &js_sys::Promise,
        promise_params: &JsValue,
        options: &JsValue,
    ) -> Result<Id, JsValue>;

    /// Wrapper for dismissing a toast.
    #[wasm_bindgen(catch, method)]
    #[allow(unsafe_code)]
    pub fn dismiss(this: &ToastAPI, id: &Id) -> Result<(), JsValue>;

    /// Wrapper for dismissing a toast.
    #[wasm_bindgen(catch, method, js_name = dismiss)]
    #[allow(unsafe_code)]
    pub fn dismiss_all(this: &ToastAPI) -> Result<(), JsValue>;

    /// Check if a toast is displayed or not.
    #[wasm_bindgen(catch, method, js_name = isActive)]
    #[allow(unsafe_code)]
    pub fn is_active(this: &ToastAPI, id: &Id) -> Result<bool, JsValue>;

    /// Update a toast.
    #[wasm_bindgen(catch, method)]
    #[allow(unsafe_code)]
    pub fn update(this: &ToastAPI, id: &Id, options: &JsValue) -> Result<(), JsValue>;

    /// Clear waiting queue when working with limit in a specific container.
    #[wasm_bindgen(catch, method, js_name = clearWaitingQueue)]
    #[allow(unsafe_code)]
    pub fn clear_waiting_queue_in(this: &ToastAPI, opts: &ContainerId) -> Result<(), JsValue>;

    /// Clear waiting queue when working with limit in the default container.
    #[wasm_bindgen(catch, method, js_name = clearWaitingQueue)]
    #[allow(unsafe_code)]
    pub fn clear_waiting_queue(this: &ToastAPI) -> Result<(), JsValue>;

    /// Completes the controlled progress bar.
    #[wasm_bindgen(catch, method)]
    #[allow(unsafe_code)]
    pub fn done(this: &ToastAPI, id: &Id) -> Result<(), JsValue>;

    /// Pretty-prints any value to a string.
    ///
    /// This function's primary purpose is to pretty-print `Error` values, including all
    /// accessible details. However, it can also handle other types of values, converting
    /// them to strings as accurately as possible. For `Error` instances, it outputs standard
    /// properties (`name`, `message`, `stack`). For non-null objects and arrays, it converts
    /// them to a JSON string with indentation. For all other types of values, it uses
    /// JavaScript's default string conversion.
    #[wasm_bindgen(js_name = prettyPrint)]
    #[allow(unsafe_code)]
    pub fn pretty_print(value: &JsValue) -> String;
}



// ===========================
// === JS-types extensions ===
// ===========================

impl ToastAPI {
    /// Send the toast notification.
    pub fn send(&self, message: &Content, method: &str, options: &JsValue) -> Result<Id, JsValue> {
        let message =
            JsValue::from_serde(message).map_err(crate::notification::api::to_js_error)?;
        sendToast(self, &message, method, options)
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&pretty_print(self), f)
    }
}
impl From<&str> for Id {
    fn from(id: &str) -> Self {
        let js_value = JsValue::from_str(id);
        js_value.into()
    }
}

impl From<Uuid> for Id {
    fn from(id: Uuid) -> Self {
        Self::new(id.to_string())
    }
}

impl Id {
    /// Create a new identifier, using the given string.
    ///
    /// The two identifiers are equal if their string representations are equal. Please note, that
    /// identifiers of different notifications must not be equal.
    pub fn new(id: impl AsRef<str>) -> Self {
        id.as_ref().into()
    }

    /// Generate a new, unique identifier.
    pub fn new_unique() -> Self {
        let uuid = uuid::Uuid::new_v4();
        Self::new(uuid.to_string())
    }

    /// Dismisses the toast.
    pub fn dismiss(&self) -> Result<(), JsValue> {
        get_toast()?.dismiss(self)
    }

    /// Completes the controlled progress bar.
    pub fn done(&self) -> Result<(), JsValue> {
        get_toast()?.done(self)
    }

    /// Check if a toast is displayed or not.
    pub fn is_active(&self) -> Result<bool, JsValue> {
        get_toast()?.is_active(self)
    }

    /// Update a toast.
    pub fn update(&self, options: &UpdateOptions) -> Result<(), JsValue> {
        get_toast()?.update(self, &options.try_into()?)
    }
}

impl ContainerId {
    /// Clear queue of notifications for this container (relevant if limit is set).
    pub fn clear_waiting_queue(&self) -> Result<(), JsValue> {
        get_toast()?.clear_waiting_queue_in(self)
    }
}

/// Wrapper for sending arbitrary kind of toast.
pub fn toast(message: &Content, r#type: Type, options: &JsValue) -> Result<Id, JsValue> {
    let method: &str = r#type.as_ref();
    get_toast()?.send(message, method, options)
}



// ======================
// === Error handling ===
// ======================

/// Extension trait for `Result<T, JsValue>` that provides a method for handling JavaScript errors.
pub trait HandleJsError<T>: Sized {
    /// Format pretty error message.
    fn pretty_print_error(message: Option<&str>, error: &JsValue) -> String {
        let mut ret = String::from("Error received from JavaScript.");
        if let Some(message) = message {
            ret.push(' ');
            ret.push_str(message);
        }
        ret.push(' ');
        ret.push_str(&pretty_print(error));
        ret
    }

    /// Handle JS error by logging it and returning `None`.
    fn handle_js_err(self, message: &str) -> Option<T>;

    /// Handle JS error by logging it along with dynamically generated message and returning `None`.
    fn handle_js_err_with(self, message_provider: impl FnOnce() -> String) -> Option<T> {
        self.handle_js_err(&message_provider())
    }
}

impl<T> HandleJsError<T> for Result<T, JsValue> {
    fn handle_js_err(self, message: &str) -> Option<T> {
        self.handle_err(|e| {
            error!("{}", Self::pretty_print_error(Some(message), &e));
        })
    }
}
