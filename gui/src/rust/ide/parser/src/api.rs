//! A module containing structures and traits used in parser API.

use crate::prelude::*;

use ast::HasRepr;
use ast::HasIdMap;

pub use ast::Ast;

use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;


// ================
// == SourceFile ==
// ================

/// Things that are metadata.
pub trait Metadata:Serialize+DeserializeOwned {}

/// Raw netadata.
impl Metadata for serde_json::Value {}

/// Parsed file / module with metadata.
#[derive(Debug,Clone,Serialize,Deserialize,PartialEq,Eq)]
pub struct SourceFile<Metadata> {
    /// Ast representation.
    pub ast: ast::known::Module,
    /// Raw metadata in json.
    pub metadata: Metadata
}

const METADATA_TAG:&str = "\n\n\n#### METADATA ####\n";

fn to_json_single_line
(val:&impl Serialize) -> std::result::Result<String,serde_json::Error> {
    let json = serde_json::to_string(val)?;
    let line = json.chars().filter(|c| c != &'\n' && c != &'\r').collect();
    Ok(line)
}

impl<M:Metadata> TryFrom<&SourceFile<M>> for String {
    type Error = serde_json::Error;
    fn try_from(val:&SourceFile<M>) -> std::result::Result<Self,Self::Error> {
        let code = val.ast.repr();
        let ids  = to_json_single_line(&val.ast.id_map())?;
        let meta = to_json_single_line(&val.metadata)?;
        Ok(iformat!("{code}{METADATA_TAG}{ids}\n{meta}"))
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

/// When trying to parse a line, not a single line was produced.
#[derive(Debug,Fail,Clone,Copy)]
#[fail(display = "Expected a single line, parsed none.")]
pub struct NoLinesProduced;

/// When trying to parse a single line, more were generated.
#[derive(Debug,Fail,Clone,Copy)]
#[fail(display = "Expected just a single line, found more.")]
pub struct TooManyLinesProduced;

/// Wraps an arbitrary `std::error::Error` as an `InteropError.`
pub fn interop_error<T>(error:T) -> Error
    where T: Fail {
    Error::InteropError(Box::new(error))
}
