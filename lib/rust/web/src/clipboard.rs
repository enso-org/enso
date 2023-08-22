//! Clipboard management utilities.
//!
//! Please note:
//! - Every function here uses the [Clipboard API](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard_API)
//!   under the hood.
//! - Every function is asynchronous. The delay for receiving or sending data may be caused for
//!   example by waiting for permission from the user.
//! - Every function will probably display a permission prompt to the user for the first time it is
//!   used.
//! - The website has to be served over HTTPS for these functions to work correctly.
//! - These functions needs to be called from within user-initiated event callbacks, like mouse or
//!   key press. Otherwise it may not work.
//! - Web browsers do not support MIME types other than `text/plain`, `text/html`, and `image/png`
//!   in general. However, using
//!   [Clipboard pickling](https://github.com/w3c/editing/blob/gh-pages/docs/clipboard-pickling/explainer.md),
//!   we can practically use any MIME type.
//!
//! To learn more, see this [StackOverflow question](https://stackoverflow.com/questions/400212/how-do-i-copy-to-the-clipboard-in-javascript).

use crate::prelude::*;

use js_sys::Uint8Array;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::prelude::Closure;



// =============
// === Types ===
// =============

/// MIME type of the data.
pub type MimeType = String;
/// The data to be written to the clipboard.
pub type BinaryData<'a> = &'a [u8];

type ReadTextClosure = Closure<dyn Fn(String)>;
type ReadClosure = Closure<dyn Fn(Vec<u8>)>;



// ===================
// === JS Bindings ===
// ===================

#[wasm_bindgen(module = "/js/clipboard.js")]
extern "C" {
    #[allow(unsafe_code)]
    fn writeText(text: String);

    #[allow(unsafe_code)]
    fn readText(closure: &ReadTextClosure);

    #[allow(unsafe_code)]
    fn writeCustom(mime_type: String, data: Uint8Array, text_data: String);

    #[allow(unsafe_code)]
    fn readCustom(
        expected_mime_type: String,
        when_expected: &ReadClosure,
        plain_text_fallback: &ReadTextClosure,
    );
}

/// Write the provided data to the clipboard, using the provided MIME type.
/// If `text_data` is present, it will be added to the clipboard with a `text/plain` MIME type.
///
/// See the module documentation for mode details.
///
/// - Unlike `write_text`, there is no special fallback mechanism in case of failures or unavailable
///   clipboard. The function will simply report an error to the console.
pub fn write(data: BinaryData<'_>, mime_type: MimeType, text_data: Option<String>) {
    let data = Uint8Array::from(data);
    writeCustom(mime_type, data, text_data.unwrap_or_default());
}

/// Read the arbitrary binary data from the console. It is expected to have `expected_mime_type`.
/// If the value of such type is not present in the clipboard content, the `plain/text` MIME type
/// is requested and the result is passed to the `plain_text_fallback` callback.
///
/// See the module documentation for more details.
///
/// - Unlike `read_text`, there is no special fallback mechanism in case of failures or unavailable
///   clipboard. The function will simply report an error to the console.
pub fn read(
    expected_mime_type: MimeType,
    when_expected: impl Fn(Vec<u8>) + 'static,
    plain_text_fallback: impl Fn(String) + 'static,
) {
    let when_expected_handler = create_handler_binary(when_expected);
    let fallback_handler = create_handler_string(plain_text_fallback);
    readCustom(
        expected_mime_type,
        when_expected_handler.borrow().as_ref().unwrap(),
        fallback_handler.borrow().as_ref().unwrap(),
    );
}

/// Write the provided text to the clipboard.
///
/// See the module documentation for more details.
///
/// In case something fails, this function implements a fallback mechanism which tries
/// to create a hidden text field, fill it with the text and use the obsolete
/// [Document.execCommand](https://developer.mozilla.org/en-US/docs/Web/API/Document/execCommand)
/// function.
pub fn write_text(text: impl Into<String>) {
    let text = text.into();
    writeText(text)
}

/// Read the text from the clipboard.
///
/// See the module documentation for more details.
///
/// This function works in a very strange way in Firefox.
/// [Firefox only supports reading the clipboard in browser extensions](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/readText).
/// In such case this function fallbacks to the `paste` event. Whenever it is triggered, it
/// remembers its value and passes it to the callback. This means, that in Firefox this function
/// will work correctly only when called as a direct action to the `cmd + v` shortcut.
pub fn read_text(callback: impl Fn(String) + 'static) {
    let handler = create_handler_string(callback);
    readText(handler.borrow().as_ref().unwrap());
}



// ===============
// === Helpers ===
// ===============

fn create_handler_string(
    callback: impl Fn(String) + 'static,
) -> Rc<RefCell<Option<Closure<dyn Fn(String)>>>> {
    let handler: Rc<RefCell<Option<ReadTextClosure>>> = default();
    let handler_clone = handler.clone_ref();
    let closure: ReadTextClosure = Closure::new(move |result| {
        *handler_clone.borrow_mut() = None;
        callback(result);
    });
    *handler.borrow_mut() = Some(closure);
    handler
}

fn create_handler_binary(
    callback: impl Fn(Vec<u8>) + 'static,
) -> Rc<RefCell<Option<Closure<dyn Fn(Vec<u8>)>>>> {
    let handler: Rc<RefCell<Option<ReadClosure>>> = default();
    let handler_clone = handler.clone_ref();
    let closure: ReadClosure = Closure::new(move |result| {
        *handler_clone.borrow_mut() = None;
        callback(result);
    });
    *handler.borrow_mut() = Some(closure);
    handler
}
