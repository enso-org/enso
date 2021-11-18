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

/// A mapping between text position and immutable ID.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(transparent)]
pub struct IdMap {
    pub vec: Vec<(Span, Id)>,
}

impl IdMap {
    /// Create a new instance.
    pub fn new(vec: Vec<(Span, Id)>) -> IdMap {
        IdMap { vec }
    }
    /// Assigns Span to given ID.
    pub fn insert(&mut self, span: impl Into<Span>, id: Id) {
        self.vec.push((span.into(), id));
    }
    /// Generate random Uuid for span.
    pub fn generate(&mut self, span: impl Into<Span>) {
        self.vec.push((span.into(), Uuid::new_v4()));
    }
}
