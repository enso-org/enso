//! Provides an API to send data to our remote logging service. Requires the remote logging
//! to be already set up on the JS side. That means, there needs to exist a `window.enso.remoteLog`
//! method that takes a string and does the actual logging.

use crate::data::*;


// ==============
// === Export ===
// ==============

pub use wasm_bindgen::prelude::*;



mod js {
    use super::*;

    #[wasm_bindgen(inline_js = "
export function remote_log(msg, value) {
    try {
        window.enso.remoteLog(msg,value)
    } catch (error) {
        console.error(\"Error while logging message. \" + error );
    }
}

export function remote_log_value(msg, field_name, value) {
    const data = {}
    data[field_name] = value
    remote_log(msg,data)
}
")]
    extern "C" {
        #[allow(unsafe_code)]
        pub fn remote_log_value(msg: JsValue, field_name: JsValue, value: JsValue);
        #[allow(unsafe_code)]
        pub fn remote_log(msg: JsValue, value: JsValue);
    }
}


/// Send the provided public event to our logging service.
#[allow(unused_variables)] // used only on wasm target
pub fn remote_log_event(message: &str) {
    // Note: Disabling on non-wasm targets
    #[cfg(target_arch = "wasm32")]
    {
        js::remote_log(JsValue::from(message.to_string()), JsValue::UNDEFINED);
    }
}

/// Send the provided public event with a named value to our logging service.
#[allow(unused_variables)] // used only on wasm target
pub fn remote_log_value<T: Loggable>(message: &str, field_name: &str, data: AnonymousData<T>) {
    // Note: Disabling on non-wasm targets
    #[cfg(target_arch = "wasm32")]
    {
        let msg = JsValue::from(message.to_string());
        let field_name = JsValue::from(field_name.to_string());
        js::remote_log_value(msg, field_name, data.0.get());
    }
}

// Note: Disabling on non-wasm targets
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The code must be disabled on non-wasm targets, because trying to construct JS values would
// immediately panic. As remote logs are invoked from controller code, that would prevent having
// some controller tests native.
