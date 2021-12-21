//! A module containing structures describing id-map.
//!
//! The Id Map is a mapping between code spans and some particular id. Its a part of our language's
//! source file: the parser gives the id of particular span to the AST node representing that span.

use crate::prelude::*;

use crate::Id;

use enso_text::unit::*;
use serde::Deserialize;
use serde::Serialize;
use uuid::Uuid;



// =============
// === IdMap ===
// =============

/// A mapping between text position and immutable ID.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct IdMap {
    pub vec: Vec<(enso_text::Range<Bytes>, Id)>,
}

impl IdMap {
    /// Create a new instance.
    pub fn new(vec: Vec<(enso_text::Range<Bytes>, Id)>) -> IdMap {
        IdMap { vec }
    }
    /// Assigns Span to given ID.
    pub fn insert(&mut self, span: impl Into<enso_text::Range<Bytes>>, id: Id) {
        self.vec.push((span.into(), id));
    }
    /// Generate random Uuid for span.
    pub fn generate(&mut self, span: impl Into<enso_text::Range<Bytes>>) {
        self.vec.push((span.into(), Uuid::new_v4()));
    }
}



// ======================
// === IdMapForParser ===
// ======================

/// Strongly typed index of char.
///
/// Part of json representation of id_map: see [`JsonIdMap`].
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
struct Index {
    value: usize,
}

/// A size expressed in chars.
///
/// Part of json representation of id_map: see [`JsonIdMap`].
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
struct Size {
    value: usize,
}

/// The index and size of a span of some text.
///
/// Part of json representation of id_map: see [`JsonIdMap`].
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
struct Span {
    index: Index,
    size:  Size,
}

/// An another representation of id map, which is the exact mirror of the id-map json stored in
/// a source file.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(transparent)]
pub struct JsonIdMap {
    vec: Vec<(Span, Id)>,
}

impl JsonIdMap {
    /// Create from the [`IdMap`] structure.
    ///
    /// The code is needed for transforming byte offsets to codepoint offsets.
    pub fn from_id_map(id_map: &IdMap, code: &str) -> Self {
        let char_offsets = code.char_indices().map(|(idx, _)| idx).collect_vec();
        let mapped_vec = id_map.vec.iter().map(|(range, id)| {
            let byte_start = range.start.as_usize();
            let byte_end = range.end.as_usize();
            let start: Chars = char_offsets.binary_search(&byte_start).unwrap_both().into();
            let end: Chars = char_offsets.binary_search(&byte_end).unwrap_both().into();
            let size = end - start;
            let span = Span {
                index: Index { value: start.as_usize() },
                size:  Size { value: size.as_usize() },
            };
            (span, *id)
        });
        Self { vec: mapped_vec.collect() }
    }
}
