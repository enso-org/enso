//! A module containing structures and traits used in parser API.

use crate::prelude::*;

use ast::IdMap;

pub use ast::Ast;


// ============
// == Parser ==
// ============

/// Entity being able to parse programs into AST.
pub trait IsParser : Debug {
    /// Parse program.
    fn parse(&mut self, program:String, ids:IdMap) -> Result<Ast>;

    /// Parse program into module.
    fn parse_module(&mut self, program:String, ids:IdMap) -> Result<ast::known::Module> {
        let ast = self.parse(program,ids)?;
        ast::known::Module::try_from(ast).map_err(|_| Error::NonModuleRoot)
    }
}


// ===========
// == Error ==
// ===========

/// A result of parsing code.
pub type Result<T> = std::result::Result<T, Error>;

/// An error which may be result of parsing code.
#[derive(Debug, Fail)]
pub enum Error {
    /// Error due to inner workings of the parser.
    #[fail(display = "Internal parser error: {:?}.", _0)]
    ParsingError(String),
    /// Parser returned non-module AST root.
    #[fail(display = "Internal parser error: non-module root node.")]
    NonModuleRoot,
    /// Error related to wrapping = communication with the parser service.
    #[fail(display = "Interop error: {}.", _0)]
    InteropError(#[cause] Box<dyn Fail>),
}

/// Wraps an arbitrary `std::error::Error` as an `InteropError.`
pub fn interop_error<T>(error:T) -> Error
    where T: Fail {
    Error::InteropError(Box::new(error))
}
