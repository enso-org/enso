//! A module containing structures and traits used in parser API.

use crate::prelude::*;
use enso_text::index::*;
use enso_text::traits::*;
use enso_text::unit::*;

use ast::id_map::JsonIdMap;
use ast::HasIdMap;
use ast::HasRepr;
use ast::IdMap;
use enso_text::Range;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;


// ==============
// === Export ===
// ==============

pub use ast::Ast;




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
    /// Error related to wrapping = communication with the parser service.
    #[fail(display = "Interop error: {}.", _0)]
    InteropError(#[cause] Box<dyn Fail>),
}

/// Wraps an arbitrary `std::error::Error` as an `InteropError.`
pub fn interop_error<T>(error: T) -> Error
    where T: Fail {
    Error::InteropError(Box::new(error))
}
<<<<<<< HEAD



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

    impl PruneUnusedIds for Metadata {}
    impl crate::api::Metadata for Metadata {}

    #[test]
    fn serializing_parsed_source_file() {
        let main = ast::Ast::var("main");
        let node = ast::Ast::infix_var("2", "+", "2");
        let infix = ast::Ast::infix(main, "=", node);
        let ast: ast::known::Module = ast::Ast::one_line_module(infix).try_into().unwrap();
        let repr = ast.repr().into();
        let metadata = Metadata { foo: 321 };
        let source = ParsedSourceFile { ast, metadata };
        let serialized = source.serialize().unwrap();

        let expected_json_id_map = JsonIdMap::from_id_map(&source.ast.id_map(), &repr);
        let expected_id_map = to_json_single_line(&expected_json_id_map).unwrap();
        let expected_metadata = to_json_single_line(&source.metadata).unwrap();
        let expected_content = format!(
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
=======
>>>>>>> 9a56303f0 (replace scala parser in frontend)
