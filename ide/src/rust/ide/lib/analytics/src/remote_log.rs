//! Provides an API to send data to our remote logging service. Requires the remote logging
//! to be already set up on the JS side. That means, there needs to exist a `window.enso.remote_log`
//! method that takes a string and does the actual logging.

use crate::data::*;

pub use wasm_bindgen::prelude::*;



#[wasm_bindgen(inline_js="
export function _remote_log(msg, data) {
    if (window !== undefined && window.enso !== undefined && window.enso.remote_log !== undefined) {
        window.enso.remote_log(msg,{data:data})
    } else {
        console.warn(\"Failed to send log message.\")
    }
}
")]
extern "C" {
     #[allow(unsafe_code)]
     fn _remote_log(msg:JsValue,data:JsValue);
}

/// Send the provided public event to our logging service.
pub fn remote_log<T:Loggable>(message: AnonymousData<T>) {
    _remote_log(message.0.get(), JsValue::NULL);
}

/// Send the provided public event with data to our logging service.
pub fn remote_log_data
<T1:Loggable,T2:Loggable>(message: AnonymousData<T1>, data: AnonymousData<T2>) {
    _remote_log(message.0.get(), data.0.get());
}

