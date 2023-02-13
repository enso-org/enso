//! A module containing structures describing id-map.
//!
//! The Id Map is a mapping between code spans and some particular id. Its a part of our language's
//! source file: the parser gives the id of particular span to the AST node representing that span.

use crate::prelude::*;
use enso_text::index::*;

use crate::Id;

use enso_text::rope::xi_rope;
use enso_text::rope::xi_rope::rope::Utf16CodeUnitsMetric;
use serde::Deserialize;
use serde::Serialize;
use uuid::Uuid;



// =============
// === IdMap ===
// =============

/// A mapping between text position and immutable ID.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct IdMap {
    pub vec: Vec<(enso_text::Range<Byte>, Id)>,
}

impl IdMap {
    /// Create a new instance.
    pub fn new(vec: Vec<(enso_text::Range<Byte>, Id)>) -> IdMap {
        IdMap { vec }
    }
    /// Assigns Span to given ID.
    pub fn insert(&mut self, span: impl Into<enso_text::Range<Byte>>, id: Id) {
        self.vec.push((span.into(), id));
    }
    /// Generate random Uuid for span.
    pub fn generate(&mut self, span: impl Into<enso_text::Range<Byte>>) -> Uuid {
        let uuid = Uuid::new_v4();
        self.vec.push((span.into(), uuid));
        uuid
    }
}



// =================
// === JsonIdMap ===
// =================

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
    pub fn from_id_map(id_map: &IdMap, code: &enso_text::Rope) -> Self {
        // let char_offsets = code.char_indices().map(|(idx, _)| idx).collect_vec();
        let mut cursor = xi_rope::Cursor::new(&code.rope, 0);
        let char_offsets = iter::once(0).chain(cursor.iter::<Utf16CodeUnitsMetric>()).collect_vec();
        let mapped_vec = id_map.vec.iter().map(|(range, id)| {
            let byte_start = range.start.value;
            let byte_end = range.end.value;
            let start = char_offsets.binary_search(&byte_start).unwrap_both();
            let end = char_offsets.binary_search(&byte_end).unwrap_both();
            let size = end - start;
            let span = Span { index: Index { value: start }, size: Size { value: size } };
            (span, *id)
        });
        Self { vec: mapped_vec.collect() }
    }
}
