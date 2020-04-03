//! Crate wrapping parser API in nice-to-use Rust code.
//!
//! The Parser is a library written in scala. There are two implementations of Rust wrappers to
//! this parser: one for local parser which binds scala parser compiled to WebAssembly to the Rust
//! crate. The second is calling a Parser running remotely using WebSockets.

#![feature(trait_alias)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

pub mod api;
pub mod test_utils;
mod jsclient;
mod wsclient;

use crate::prelude::*;

use ast::Ast;
use ast::IdMap;
use std::panic;
use utils::fail::FallibleResult;

pub use enso_prelude as prelude;


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
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
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
    pub fn parse(&self, program:String, ids:IdMap) -> api::Result<Ast> {
        self.borrow_mut().parse(program,ids)
    }

    /// Parse contents of the program source file, where program code may be followed by idmap and
    /// metadata.
    pub fn parse_with_metadata<M:api::Metadata>
    (&self, program:String) -> api::Result<api::SourceFile<M>> {
        self.borrow_mut().parse_with_metadata(program)
    }

    /// Parse program into module.
    pub fn parse_module(&self, program:impl Str, ids:IdMap) -> api::Result<ast::known::Module> {
        let ast = self.parse(program.into(),ids)?;
        ast::known::Module::try_from(ast).map_err(|_| api::Error::NonModuleRoot)
    }

    /// Program is expected to be single non-empty line module. The line's AST is
    /// returned. Panics otherwise.
    pub fn parse_line(&self, program:impl Str) -> FallibleResult<Ast> {
        let module = self.parse_module(program,default())?;

        let mut lines = module.lines.clone().into_iter().filter_map(|line| {
            line.elem
        });
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
