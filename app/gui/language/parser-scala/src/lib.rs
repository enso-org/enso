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
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
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
mod wsclient;

use crate::prelude::*;

use std::panic;



#[allow(missing_docs)]
pub mod prelude {
    pub use ast::traits::*;
    pub use enso_prelude::*;
    pub use enso_profiler as profiler;
    pub use enso_profiler::prelude::*;
}



// ==========================================
// === Documentation Parser and Generator ===
// ==========================================

/// Handle to a doc parser implementation.
///
/// Currently this component is implemented as a wrapper over documentation
/// parser written in Scala. Depending on compilation target (native or wasm)
/// it uses either implementation provided by `wsclient` or `jsclient`.
#[derive(Clone, CloneRef, Debug, Deref, DerefMut)]
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
        DocParser::new().unwrap_or_else(|e| panic!("Failed to create doc parser: {e:?}"))
    }

    /// Parses pure documentation code and generates HTML code.
    /// Will return empty string for empty entry.
    pub fn generate_html_doc_pure(&self, code: String) -> api::Result<String> {
        self.borrow_mut().generate_html_doc_pure(code)
    }
}


// === Support ===

/// Websocket parser client.
/// Used as an interface for our (scala) parser.
#[cfg(not(target_arch = "wasm32"))]
type Client = wsclient::Client;
/// Javascript parser client.
/// Used as an interface for our (scala) parser.
#[cfg(target_arch = "wasm32")]
type Client = jsclient::Client;
