#![cfg(target_arch = "wasm32")]

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::api;
use crate::api::Ast;
use crate::from_json_str_without_recursion_limit;

use ast::id_map::JsonIdMap;
use ast::IdMap;



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
    fn parse(input: String, ids: String) -> std::result::Result<String, JsValue>;
    #[wasm_bindgen(catch)]
    fn parse_with_metadata(content: String) -> std::result::Result<String, JsValue>;
    #[wasm_bindgen(catch)]
    fn doc_parser_generate_html_source(content: String) -> std::result::Result<String, JsValue>;
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

    /// Parses Enso code with JS-based parser.
    pub fn parse(&self, program: String, ids: IdMap) -> api::Result<Ast> {
        let ast = || {
            let ids = JsonIdMap::from_id_map(&ids, &program);
            let json_ids = serde_json::to_string(&ids)?;
            let json_ast = parse(program, json_ids)?;
            let ast = from_json_str_without_recursion_limit(&json_ast)?;
            Result::Ok(ast)
        };
        Ok(ast()?)
    }

    /// Parses Enso code with metadata.
    pub fn parse_with_metadata<M: api::Metadata>(
        &self,
        program: String,
    ) -> api::Result<api::ParsedSourceFile<M>> {
        let result = || {
            let json = &parse_with_metadata(program)?;
            let module = from_json_str_without_recursion_limit(&json)?;
            Result::Ok(module)
        };
        Ok(result()?)
    }

    /// Calls JS doc parser to generate HTML from documented Enso code.
    pub fn generate_html_docs(&self, program: String) -> api::Result<String> {
        let html_code = || {
            let html_code = doc_parser_generate_html_source(program)?;
            Result::Ok(html_code)
        };
        Ok(html_code()?)
    }

    /// Calls JS doc parser to generate HTML from pure doc code without Enso's AST.
    pub fn generate_html_doc_pure(&self, code: String) -> api::Result<String> {
        let html_code = || {
            let html_code = doc_parser_generate_html_from_doc(code)?;
            Result::Ok(html_code)
        };
        Ok(html_code()?)
    }
}
