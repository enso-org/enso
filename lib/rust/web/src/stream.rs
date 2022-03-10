//! Helpers for the Web Streaming API in Rust, mostly the missing bindings in the [`web_sys`] crate.

use crate::prelude::*;

use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;



// ===================================
// === ReadableStreamDefaultReader ===
// ===================================

#[wasm_bindgen]
extern "C" {
    /// The wrapper for ReadableStreamDefaultReader js class.
    ///
    /// See https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamDefaultReader.
    pub type ReadableStreamDefaultReader;

    /// Returns a Promise providing access to the next chunk in the stream's internal queue.
    ///
    /// See https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamDefaultReader/read.
    #[allow(unsafe_code)]
    #[wasm_bindgen(method)]
    pub fn read(this: &ReadableStreamDefaultReader) -> js_sys::Promise;
}



// ===============
// === BlobExt ===
// ===============

/// The extension for [`js_sys::Blob`] API.
// TODO[ao] Those functions are part of the official API on newer web_sys versions, however the
//     version bump is tricky, see https://github.com/enso-org/ide/issues/1591.
pub trait BlobExt {
    /// Returns a ReadableStream which upon reading returns the data contained within the Blob.
    /// See https://developer.mozilla.org/en-US/docs/Web/API/Blob/stream.
    fn stream(&self) -> Result<web_sys::ReadableStream, JsValue>;

    /// Returns a Reader of the Blob data. It assumes that the reader is of
    /// [`ReadableStreamDefaultReader`] type. See also
    /// https://developer.mozilla.org/en-US/docs/Web/API/Blob/stream and
    /// https://developer.mozilla.org/en-US/docs/Web/API/ReadableStream/getReader
    fn stream_reader(&self) -> Result<ReadableStreamDefaultReader, JsValue>;
}

impl BlobExt for web_sys::Blob {
    #[allow(unused_qualifications)]
    fn stream(&self) -> Result<web_sys::ReadableStream, JsValue> {
        let this = self.as_ref();
        let method_as_value = js_sys::Reflect::get(this, &"stream".into())?;
        let method = method_as_value.dyn_into::<js_sys::Function>()?;
        method.call0(this)?.dyn_into()
    }

    #[allow(unused_qualifications)]
    fn stream_reader(&self) -> Result<ReadableStreamDefaultReader, JsValue> {
        let stream = self.stream();
        let method_as_value = js_sys::Reflect::get(&stream, &"getReader".into())?;
        let method = method_as_value.dyn_into::<js_sys::Function>()?;
        method.call0(&stream)?.dyn_into()
    }
}
