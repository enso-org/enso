//! This module provides relatively low-level wrappers for the
//! [`react-toastify`](https://fkhadra.github.io/react-toastify/introduction) library API.

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::notification::Type;
use crate::notification::UpdateOptions;



// ==================
// === Constants ===
// =================

/// Toastify's [`toast API`](https://fkhadra.github.io/react-toastify/api/toast) field name within
/// our application JS object.
const TOAST_FIELD_NAME: &str = "toast";



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

// Wrappers for [`toast`](https://react-hot-toast.com/docs/toast) API.
#[wasm_bindgen(inline_js = r#"
    export function sendToast(toast, message, method, options) {
        const target = toast[method];
        return target(message, options);
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

    /// The unique identifier of a toast.
    #[derive(Clone, Debug)]
    pub type ContainerId;

    /// Generalized wrapper for calling any kind of toast.
    #[wasm_bindgen(catch)]
    #[allow(unsafe_code)]
    pub fn sendToast(
        this: &ToastAPI,
        message: &str,
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
}



// ===========================
// === JS-types extensions ===
// ===========================

impl ToastAPI {
    /// Send the toast notification.
    pub fn send(&self, message: &str, method: &str, options: &JsValue) -> Result<Id, JsValue> {
        sendToast(self, message, method, options)
    }
}

impl Id {
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
pub fn toast(message: &str, r#type: Type, options: &JsValue) -> Result<Id, JsValue> {
    let method: &str = r#type.as_ref();
    get_toast()?.send(message, method, options)
}
