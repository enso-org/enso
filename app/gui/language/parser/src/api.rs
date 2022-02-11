//! A module containing structures and traits used in parser API.

use crate::prelude::*;

use ast::HasIdMap;
use ast::HasRepr;

use enso_text::traits::*;
use enso_text::unit::*;
use enso_text::Range;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;

use ast::id_map::JsonIdMap;
pub use ast::Ast;


// ================
// == SourceFile ==
// ================


// === Metadata ===

/// Things that are metadata.
pub trait Metadata: Default + Serialize + DeserializeOwned {}

/// Raw metadata.
impl Metadata for serde_json::Value {}


// === Source File ===

/// Source File content with information about section placement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceFile {
    /// The whole content of file.
    pub content:  String,
    /// The range in bytes of module's "Code" section.
    pub code:     Range<Bytes>,
    /// The range in bytes of module's "Id Map" section.
    pub id_map:   Range<Bytes>,
    /// The range in bytes of module's "Metadata" section.
    pub metadata: Range<Bytes>,
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.content)
    }
}

impl SourceFile {
    /// Describe source file contents. Uses heuristics to locate the metadata section.
    ///
    /// Method investigates the last `METADATA_LINES` lines of content to check for metadata tag and
    /// whether idmap and metadata looks "reasonable enough". If proper metadata is not recognized,
    /// the whole contents is treated as the code.
    pub fn new(content: String) -> Self {
        pub const METADATA_LINES: usize = 3;
        let nl_offsets = content.char_indices().filter_map(|(ix, c)| (c == '\n').as_some(ix));
        let nl_offsets_bytes = nl_offsets.map(Bytes::from);
        let nl_offsets_from_end = nl_offsets_bytes.rev().take(METADATA_LINES).collect_vec();
        match nl_offsets_from_end.as_slice() {
            [last, before_last, two_before_last] => {
                // Last line should be metadata. Line before should be id map. Line before is the
                // metadata tag.
                // We check that tag matches and that trailing lines looks like JSON list/object
                // respectively.
                let code_length = *two_before_last + 1.bytes() - Bytes::from(NEWLINES_BEFORE_TAG);
                let code_range = 0.bytes()..code_length;
                let tag_range = two_before_last + 1.bytes()..*before_last;
                let id_map_range = before_last + 1.bytes()..*last;
                let metadata_range = last + 1.bytes()..Bytes::from(content.len());
                let tag = &content[tag_range.start.as_usize()..tag_range.end.as_usize()];
                let idmap = &content[id_map_range.start.as_usize()..id_map_range.end.as_usize()];
                let metadata =
                    &content[metadata_range.start.as_usize()..metadata_range.end.as_usize()];
                let tag_matching = tag == METADATA_TAG;
                let idmap_matching = Self::looks_like_idmap(idmap);
                let metadata_matching = Self::looks_like_metadata(metadata);
                if tag_matching && idmap_matching && metadata_matching {
                    SourceFile {
                        code: code_range.into(),
                        id_map: id_map_range.into(),
                        metadata: metadata_range.into(),
                        content,
                    }
                } else {
                    Self::new_without_metadata(content)
                }
            }
            _ => Self::new_without_metadata(content),
        }
    }

    /// Create a description of source file consisting only of code, with no metadata.
    fn new_without_metadata(content: String) -> Self {
        let length = Bytes::from(content.len());
        Self {
            code: (0.bytes()..length).into(),
            id_map: (length..length).into(),
            metadata: (length..length).into(),
            content,
        }
    }

    /// Checks if given line might be an ID map.
    pub fn looks_like_idmap(line: &str) -> bool {
        line.is_enclosed('[', ']')
    }

    /// Checks if given line might be a metadata map.
    pub fn looks_like_metadata(line: &str) -> bool {
        line.is_enclosed('{', '}')
    }

    /// Get fragment of serialized string with code.
    pub fn code_slice(&self) -> &str {
        self.slice(&self.code)
    }

    /// Get fragment of serialized string with id map.
    pub fn id_map_slice(&self) -> &str {
        self.slice(&self.id_map)
    }

    /// Get fragment of serialized string with metadata.
    pub fn metadata_slice(&self) -> &str {
        self.slice(&self.metadata)
    }

    fn slice(&self, range: &Range<Bytes>) -> &str {
        let start = range.start.as_usize();
        let end = range.end.as_usize();
        &self.content[start..end]
    }
}


// === Parsed Source File ===

/// Parsed file / module with metadata.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct ParsedSourceFile<Metadata> {
    /// Ast representation.
    pub ast:      ast::known::Module,
    /// Raw metadata in json.
    #[serde(bound(deserialize = "Metadata:Default+DeserializeOwned"))]
    #[serde(deserialize_with = "enso_prelude::deserialize_or_default")]
    pub metadata: Metadata,
}

impl<M: Metadata> TryFrom<&ParsedSourceFile<M>> for String {
    type Error = serde_json::Error;
    fn try_from(val: &ParsedSourceFile<M>) -> std::result::Result<String, Self::Error> {
        Ok(val.serialize()?.content)
    }
}

impl<M: Metadata> Display for ParsedSourceFile<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.serialize() {
            Ok(serialized) => write!(f, "{}", serialized),
            Err(_) => write!(f, "[NOT REPRESENTABLE SOURCE FILE]"),
        }
    }
}

// === Parsed Source File Serialization ===

const NEWLINES_BEFORE_TAG: usize = 3;

const METADATA_TAG: &str = "#### METADATA ####";

fn to_json_single_line(val: &impl Serialize) -> std::result::Result<String, serde_json::Error> {
    let json = serde_json::to_string(val)?;
    let line = json.chars().filter(|c| *c != '\n' && *c != '\r').collect();
    Ok(line)
}

impl<M: Metadata> ParsedSourceFile<M> {
    /// Serialize to the SourceFile structure,
    pub fn serialize(&self) -> std::result::Result<SourceFile, serde_json::Error> {
        let code = self.ast.repr();
        let before_tag = "\n".repeat(NEWLINES_BEFORE_TAG);
        let before_idmap = "\n";
        let json_id_map = JsonIdMap::from_id_map(&self.ast.id_map(), &code);
        let id_map = to_json_single_line(&json_id_map)?;
        let before_metadata = "\n";
        let metadata = to_json_single_line(&self.metadata)?;

        let id_map_start = code.len() + before_tag.len() + METADATA_TAG.len() + before_idmap.len();
        let id_map_start_bytes = Bytes::from(id_map_start);
        let metadata_start = id_map_start + id_map.len() + before_metadata.len();
        let metadata_start_bytes = Bytes::from(metadata_start);
        Ok(SourceFile {
            content:  iformat!(
                "{code}{before_tag}{METADATA_TAG}{before_idmap}{id_map}\
                                 {before_metadata}{metadata}"
            ),
            code:     (0.bytes()..Bytes::from(code.len())).into(),
            id_map:   (id_map_start_bytes..id_map_start_bytes + Bytes::from(id_map.len())).into(),
            metadata: (metadata_start_bytes..metadata_start_bytes + Bytes::from(metadata.len()))
                .into(),
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
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "Expected a single line, parsed none.")]
pub struct NoLinesProduced;

/// When trying to parse a single line, more were generated.
#[derive(Debug, Fail, Clone, Copy)]
#[fail(display = "Expected just a single line, found more.")]
pub struct TooManyLinesProduced;

/// Wraps an arbitrary `std::error::Error` as an `InteropError.`
pub fn interop_error<T>(error: T) -> Error
where T: Fail {
    Error::InteropError(Box::new(error))
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;



    #[derive(Clone, Debug, Default, Deserialize, Serialize)]
    struct Metadata {
        foo: usize,
    }

    impl crate::api::Metadata for Metadata {}

    #[test]
    fn serializing_parsed_source_file() {
        let main = ast::Ast::var("main");
        let node = ast::Ast::infix_var("2", "+", "2");
        let infix = ast::Ast::infix(main, "=", node);
        let ast: ast::known::Module = ast::Ast::one_line_module(infix).try_into().unwrap();
        let repr = ast.repr();
        let metadata = Metadata { foo: 321 };
        let source = ParsedSourceFile { ast, metadata };
        let serialized = source.serialize().unwrap();

        let expected_json_id_map = JsonIdMap::from_id_map(&source.ast.id_map(), &repr);
        let expected_id_map = to_json_single_line(&expected_json_id_map).unwrap();
        let expected_metadata = to_json_single_line(&source.metadata).unwrap();
        let expected_content = iformat!(
            r#"main = 2 + 2


#### METADATA ####
{expected_id_map}
{expected_metadata}"#
        );

        assert_eq!(serialized.content, expected_content);
        assert_eq!(serialized.code_slice(), "main = 2 + 2");
        assert_eq!(serialized.id_map_slice(), expected_id_map.as_str());
        assert_eq!(serialized.metadata_slice(), expected_metadata.as_str());

        // Check that SourceFile round-trips.
        let source_file = SourceFile::new(serialized.content.clone());
        assert_eq!(source_file, serialized);
    }
}
