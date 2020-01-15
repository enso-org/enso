#![cfg(target_arch = "wasm32")]

use crate::{api, api::IsParser};
use prelude::*;
use wasm_bindgen::prelude::*;
use crate::api::Error::{ParsingError, InteropError};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "JSON (de)serialization failed: {:?}", _0)]
    JsonSerializationError(#[cause] serde_json::error::Error),

    #[fail(display = "Scala parser threw an unexpected exception.")]
    ScalaException(),
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

#[wasm_bindgen(module = "/pkg/scala-parser.js")]
extern "C" {
    #[wasm_bindgen(catch)]
    fn parse(input: String) -> std::result::Result<String, JsValue>;
    #[wasm_bindgen(catch)]
    #[wasm_bindgen(js_name = parseWithIDs)]
    fn parse_with_IDs
    (input: String, ids: String) -> std::result::Result<String, JsValue>;
}

/// Wrapper over the JS-compiled parser.
///
/// Can only be used when targeting WebAssembly.
pub struct Client {}

impl Client {
    pub fn new() -> Result<Client> {
        Ok(Client {})
    }
}

impl IsParser for Client {
    fn parse(&mut self, _program: String) -> api::Result<api::Ast> {
        match parse(_program) {
            Ok(json_ast) => Err(ParsingError(json_ast)),
            Err(_)       => Err(api::interop_error(Error::ScalaException())),
        }
    }
}
