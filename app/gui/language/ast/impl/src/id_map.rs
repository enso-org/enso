use crate::prelude::*;

use crate::Id;

use enso_text::unit::*;
use serde::Deserialize;
use serde::Serialize;
use uuid::Uuid;



/// Strongly typed index into container.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Index {
    pub value: usize,
}

#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Size {
    pub value: usize,
}


#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Span {
    pub index: Index,
    pub size:  Size,
}

impl<T: Into<enso_text::Range<Codepoints>>> From<T> for Span {
    fn from(value: T) -> Self {
        let range = value.into();
        Self {
            index: Index { value: range.start.as_usize() },
            size:  Size { value: range.size().as_usize() },
        }
    }
}

impl Span {
    pub fn as_range(&self) -> enso_text::Range<Codepoints> {
        let start: Codepoints = self.index.value.into();
        let len: Codepoints = self.size.value.into();
        (start..start + len).into()
    }
}

/// A mapping between text position and immutable ID.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(transparent)]
pub struct IdMapForParser {
    pub vec: Vec<(Span, Id)>,
}

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

    pub fn for_parser(self, code: &str) -> IdMapForParser {
        let char_offsets = code.char_indices().map(|(idx, _)| idx).collect_vec();
        IdMapForParser {
            vec: self
                .vec
                .into_iter()
                .map(|(range, id)| {
                    let byte_start = range.start.as_usize();
                    let byte_end = range.end.as_usize();
                    let start: Codepoints =
                        char_offsets.binary_search(&byte_start).unwrap_both().into();
                    let end: Codepoints =
                        char_offsets.binary_search(&byte_end).unwrap_both().into();
                    let size = end - start;
                    let span = Span {
                        index: Index { value: start.as_usize() },
                        size:  Size { value: size.as_usize() },
                    };
                    (span, id)
                })
                .collect(),
        }
    }
}
