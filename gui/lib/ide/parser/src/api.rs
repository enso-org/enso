//! A module containing structures and traits used in parser API.

use crate::prelude::*;

use ast::IdMap;
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
    pub ast: Ast,
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
    fn try_from(val:&SourceFile<M>) -> std::result::Result<String,Self::Error> {
        let code = val.ast.repr();
        let ids  = to_json_single_line(&val.ast.id_map())?;
        let meta = to_json_single_line(&val.metadata)?;
        Ok(iformat!("{code}{METADATA_TAG}{ids}\n{meta}"))
    }
}


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

    /// Parse contents of the program source file,
    /// where program code may be followed by idmap and metadata.
    fn parse_with_metadata<M:Metadata>
    (&mut self, program:String) -> Result<SourceFile<M>>;
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
