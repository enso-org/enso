//! A module containing structures and traits used in parser API.

use crate::prelude::*;

use ast::HasRepr;
use ast::HasIdMap;
use enso_data::text::ByteIndex;

pub use ast::Ast;

use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;



// ================
// == SourceFile ==
// ================


// === Metadata ===

/// Things that are metadata.
pub trait Metadata:Serialize+DeserializeOwned {}

/// Raw metadata.
impl Metadata for serde_json::Value {}


// === Source File ===

/// Source File content with information about section placement.
#[derive(Clone,Debug,Eq,PartialEq)]
pub struct SourceFile {
    /// The whole content of file.
    pub content : String,
    /// The range in bytes of module's "Code" section.
    pub code : Range<ByteIndex>,
    /// The range in bytes of module's "Id Map" section.
    pub id_map : Range<ByteIndex>,
    /// The range in bytes of module's "Metadata" section.
    pub metadata : Range<ByteIndex>,
}

impl SourceFile {
    /// Get fragment of serialized string with code.
    pub fn code_slice(&self) -> &str { &self.slice(&self.code) }

    /// Get fragment of serialized string with id map.
    pub fn id_map_slice  (&self) -> &str { &self.slice(&self.id_map) }

    /// Get fragment of serialized string with metadata.
    pub fn metadata_slice(&self) -> &str { &self.slice(&self.metadata) }

    fn slice(&self, range:&Range<ByteIndex>) -> &str {
        &self.content[range.start.value..range.end.value]
    }
}


// === Parsed Source File ===

/// Parsed file / module with metadata.
#[derive(Clone,Debug,Deserialize,Eq,PartialEq)]
pub struct ParsedSourceFile<Metadata> {
    /// Ast representation.
    pub ast: ast::known::Module,
    /// Raw metadata in json.
    pub metadata: Metadata
}

impl<M:Metadata> TryFrom<&ParsedSourceFile<M>> for String {
    type Error = serde_json::Error;
    fn try_from(val:&ParsedSourceFile<M>) -> std::result::Result<String,Self::Error> {
        Ok(val.serialize()?.content)
    }
}


// === Parsed Source File Serialization ===

const METADATA_TAG:&str = "\n\n\n#### METADATA ####\n";

fn to_json_single_line(val:&impl Serialize) -> std::result::Result<String,serde_json::Error> {
    let json = serde_json::to_string(val)?;
    let line = json.chars().filter(|c| *c != '\n' && *c != '\r').collect();
    Ok(line)
}

impl<M:Metadata> ParsedSourceFile<M> {
    /// Serialize to the SourceFile structure,
    pub fn serialize(&self) -> std::result::Result<SourceFile,serde_json::Error> {
        let code                  = self.ast.repr();
        let id_map                = to_json_single_line(&self.ast.id_map())?;
        let metadata              = to_json_single_line(&self.metadata)?;
        let id_map_start          = code.len() + METADATA_TAG.len();
        let newlines_after_id_map = 1;
        let metadata_start        = id_map_start + id_map.len() + newlines_after_id_map;
        Ok(SourceFile {
            content  : iformat!("{code}{METADATA_TAG}{id_map}\n{metadata}"),
            code     : ByteIndex::new(0             )..ByteIndex::new(code.len()                     ),
            id_map   : ByteIndex::new(id_map_start  )..ByteIndex::new(id_map_start   + id_map.len()  ),
            metadata : ByteIndex::new(metadata_start)..ByteIndex::new(metadata_start + metadata.len()),
        })
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



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;



    #[derive(Clone,Debug,Deserialize,Serialize)]
    struct Metadata {
        foo : usize,
    }

    impl crate::api::Metadata for Metadata {}

    #[test]
    fn serializing_parsed_source_file() {
        let main       = ast::Ast::var("main");
        let node       = ast::Ast::infix_var("2","+","2");
        let infix      = ast::Ast::infix(main,"=",node);
        let ast        = ast::Ast::one_line_module(infix).try_into().unwrap();
        let metadata   = Metadata{foo:321};
        let source     = ParsedSourceFile {ast,metadata};
        let serialized = source.serialize().unwrap();

        let expected_id_map   = to_json_single_line(&source.ast.id_map()).unwrap();
        let expected_metadata = to_json_single_line(&source.metadata).unwrap();
        let expected_content  = iformat!(r#"main = 2 + 2


#### METADATA ####
{expected_id_map}
{expected_metadata}"#);

        assert_eq!(serialized.content         , expected_content.to_string());
        assert_eq!(serialized.code_slice()    , "main = 2 + 2");
        assert_eq!(serialized.id_map_slice()  , expected_id_map.as_str());
        assert_eq!(serialized.metadata_slice(), expected_metadata.as_str())
    }
}
