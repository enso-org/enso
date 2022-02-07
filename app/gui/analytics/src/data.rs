//! Provides data wrappers for our analytics api. This is intended to ensure we are conscious of
//! whether we are sending public or private data. No private data should be logged at the moment.
//!
//! Note: this is meant to be a little bit un-ergonomic to ensure the data has been vetted by the
//! API user and allow the reader of the code to see the intent behind the data.

use wasm_bindgen::JsValue;

/// Trait that allows us to log an object remotely.
pub trait Loggable {
    /// Return the log message as JsValue.
    fn get(self) -> JsValue;
}

impl Loggable for bool {
    fn get(self) -> JsValue {
        self.into()
    }
}
impl Loggable for &str {
    fn get(self) -> JsValue {
        self.into()
    }
}
impl Loggable for String {
    fn get(self) -> JsValue {
        self.into()
    }
}
impl Loggable for &String {
    fn get(self) -> JsValue {
        self.into()
    }
}
impl<F, S> Loggable for F
where
    F: Fn() -> S,
    S: Loggable,
{
    fn get(self) -> JsValue {
        self().get()
    }
}


/// Wrapper struct for data that can be made public and has no privacy implications.
#[derive(Clone, Copy, Debug)]
pub struct AnonymousData<T: Loggable>(pub T);
