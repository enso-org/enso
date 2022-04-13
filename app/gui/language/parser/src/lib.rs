//! Crate wrapping parser API in nice-to-use Rust code.
//!
//! The Parser is a library written in scala. There are two implementations of Rust wrappers to
//! this parser: one for local parser which binds scala parser compiled to WebAssembly to the Rust
//! crate. The second is calling a Parser running remotely using WebSockets.

// === Features ===
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]


// ==============
// === Export ===
// ==============

pub mod api;



mod jsclient;
pub mod test_utils;
mod wsclient;

use crate::prelude::*;

use ast::Ast;
use ast::BlockLine;
use ast::IdMap;
use std::panic;



#[allow(missing_docs)]
pub mod prelude {
    pub use ast::traits::*;
    pub use enso_prelude::*;
    pub use enso_profiler as profiler;
    pub use enso_profiler::prelude::*;
}



// ==============
// === Parser ===
// ==============

/// Websocket parser client.
/// Used as an interface for our (scala) parser.
#[cfg(not(target_arch = "wasm32"))]
type Client = wsclient::Client;
/// Javascript parser client.
/// Used as an interface for our (scala) parser.
#[cfg(target_arch = "wasm32")]
type Client = jsclient::Client;

/// Handle to a parser implementation.
///
/// Currently this component is implemented as a wrapper over parser written
/// in Scala. Depending on compilation target (native or wasm) it uses either
/// implementation provided by `wsclient` or `jsclient`.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct Parser(pub Rc<RefCell<Client>>);

impl Parser {
    /// Obtains a default parser implementation.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new() -> api::Result<Parser> {
        let client = wsclient::Client::new()?;
        let parser = Rc::new(RefCell::new(client));
        Ok(Parser(parser))
    }

    /// Obtains a default parser implementation.
    #[cfg(target_arch = "wasm32")]
    pub fn new() -> api::Result<Parser> {
        let client = jsclient::Client::new()?;
        let parser = Rc::new(RefCell::new(client));
        Ok(Parser(parser))
    }

    /// Obtains a default parser implementation, panicking in case of failure.
    pub fn new_or_panic() -> Parser {
        Parser::new().unwrap_or_else(|e| panic!("Failed to create a parser: {:?}", e))
    }

    /// Parse program.
    pub fn parse(&self, program: String, ids: IdMap) -> api::Result<Ast> {
        self.borrow_mut().parse(program, ids)
    }

    /// Parse contents of the program source file, where program code may be followed by idmap and
    /// metadata.
    ///
    /// If metadata deserialization fails, error is ignored and default value for metadata is used.
    /// Other errors are returned through `Result`.
    #[profile(Detail)]
    pub fn parse_with_metadata<M: api::Metadata>(
        &self,
        program: String,
    ) -> api::Result<api::ParsedSourceFile<M>> {
        self.borrow_mut().parse_with_metadata(program)
    }

    /// Parse program into module.
    pub fn parse_module(&self, program: impl Str, ids: IdMap) -> api::Result<ast::known::Module> {
        let ast = self.parse(program.into(), ids)?;
        ast::known::Module::try_from(ast).map_err(|_| api::Error::NonModuleRoot)
    }

    /// Program is expected to be single non-empty line module. The line's AST is
    /// returned. The program is parsed with empty IdMap.
    pub fn parse_line_ast(&self, program: impl Str) -> FallibleResult<Ast> {
        self.parse_line_with_id_map(program, default()).map(|line| line.elem)
    }

    /// Program is expected to be single non-empty line module. The line's AST is
    /// returned. The program is parsed with empty IdMap.
    pub fn parse_line(&self, program: impl Str) -> FallibleResult<BlockLine<Ast>> {
        self.parse_line_with_id_map(program, default())
    }

    /// Program is expected to be single non-empty line module. The line's AST is returned.
    pub fn parse_line_ast_with_id_map(
        &self,
        program: impl Str,
        id_map: IdMap,
    ) -> FallibleResult<Ast> {
        self.parse_line_with_id_map(program, id_map).map(|line| line.elem)
    }

    /// Program is expected to be single non-empty line module. Return the parsed line.
    pub fn parse_line_with_id_map(
        &self,
        program: impl Str,
        id_map: IdMap,
    ) -> FallibleResult<BlockLine<Ast>> {
        let module = self.parse_module(program, id_map)?;

        let mut lines =
            module.lines.clone().into_iter().filter_map(|line| line.map(|elem| elem).transpose());
        if let Some(first_non_empty_line) = lines.next() {
            if lines.next().is_some() {
                Err(api::TooManyLinesProduced.into())
            } else {
                Ok(first_non_empty_line)
            }
        } else {
            Err(api::NoLinesProduced.into())
        }
    }
}

/// Deserialize value from json text.
///
/// Unlike `serde_json::from_str` it runs with recursion limit disabled, allowing deserialization of
/// deeply nested ASTs.
pub fn from_json_str_without_recursion_limit<'de, Value: serde::Deserialize<'de>>(
    json_text: &'de str,
) -> Result<Value, serde_json::Error> {
    let mut de = serde_json::Deserializer::from_str(json_text);
    de.disable_recursion_limit();
    Value::deserialize(&mut de)
}



// ==========================================
// === Documentation Parser and Generator ===
// ==========================================

/// Handle to a doc parser implementation.
///
/// Currently this component is implemented as a wrapper over documentation
/// parser written in Scala. Depending on compilation target (native or wasm)
/// it uses either implementation provided by `wsclient` or `jsclient`.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct DocParser(pub Rc<RefCell<Client>>);

impl DocParser {
    /// Obtains a default doc parser implementation.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new() -> api::Result<DocParser> {
        let client = wsclient::Client::new()?;
        let doc_parser = Rc::new(RefCell::new(client));
        Ok(DocParser(doc_parser))
    }

    /// Obtains a default doc parser implementation.
    #[cfg(target_arch = "wasm32")]
    pub fn new() -> api::Result<DocParser> {
        let client = jsclient::Client::new()?;
        let doc_parser = Rc::new(RefCell::new(client));
        Ok(DocParser(doc_parser))
    }

    /// Obtains a default doc parser implementation, panicking in case of failure.
    pub fn new_or_panic() -> DocParser {
        DocParser::new().unwrap_or_else(|e| panic!("Failed to create doc parser: {:?}", e))
    }

    /// Parses program with documentation and generates HTML code.
    /// If the program does not have any documentation will return empty string.
    pub fn generate_html_docs(&self, program: String) -> api::Result<String> {
        self.borrow_mut().generate_html_docs(program)
    }

    /// Parses pure documentation code and generates HTML code.
    /// Will return empty string for empty entry.
    pub fn generate_html_doc_pure(&self, code: String) -> api::Result<String> {
        self.borrow_mut().generate_html_doc_pure(code)
    }
}
