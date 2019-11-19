use prelude::*;

// ============
// == Parser ==
// ============

/// Entity being able to parse Luna programs into Luna's AST.
pub trait IsParser {
    fn parse(&mut self, program: String) -> Result<AST>;
}

// =========
// == AST ==
// =========

// TODO: placeholder until we have real AST, see:
// https://github.com/luna/enso/issues/296
pub type AST = String;

// ===========
// == Error ==
// ===========

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Fail)]
pub enum Error {
    /// Error due to inner workings of the parser.
    #[fail(display = "Internal parser error: {:?}", _0)]
    ParsingError(String),
    /// Error related to wrapping = communication with the parser service.
    #[fail(display = "Interop error: {}", _0)]
    InteropError(#[cause] Box<dyn failure::Fail>),
}

/// Wraps an arbitrary `std::error::Error` as an `InteropError.`
pub fn interop_error<T>(error: T) -> Error
    where T: Fail {
    Error::InteropError(Box::new(error))
}
