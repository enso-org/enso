#![cfg(target_arch = "wasm32")]

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::api;



pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "JSON (de)serialization failed: {:?}", _0)]
    JsonSerializationError(#[cause] serde_json::error::Error),

    #[fail(display = "Scala parser failed: {:?}.", _0)]
    ScalaException(String),
}

impl From<Error> for api::Error {
    fn from(e: Error) -> Self {
        api::interop_error(e)
    }
}

impl From<serde_json::error::Error> for Error {
    fn from(error: serde_json::error::Error) -> Self {
        Error::JsonSerializationError(error)
    }
}

impl From<JsValue> for Error {
    fn from(jsvalue: JsValue) -> Self {
        Error::ScalaException(format!("{:?}", jsvalue))
    }
}

#[wasm_bindgen(module = "/pkg/scala-parser.js")]
extern "C" {
    #[wasm_bindgen(catch)]
    fn doc_parser_generate_html_from_doc(content: String) -> std::result::Result<String, JsValue>;
}

/// Wrapper over the JS-compiled parser.
///
/// Can only be used when targeting WebAssembly.
#[derive(Debug, Clone, Copy)]
pub struct Client {}

impl Client {
    /// Creates a `Client`.
    pub fn new() -> Result<Client> {
        Ok(Client {})
    }

    /// Calls JS doc parser to generate HTML from pure doc code without Enso's AST.
    #[profile(Detail)]
    pub fn generate_html_doc_pure(&self, code: String) -> api::Result<String> {
        let html_code = || {
            let html_code = doc_parser_generate_html_from_doc(code)?;
            Result::Ok(html_code)
        };
        Ok(html_code()?)
    }
}
