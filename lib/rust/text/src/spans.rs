//! Text spans used to store metadata information.

use crate::prelude::*;

use crate::range::Range;
use crate::rope;
use crate::unit::*;



// =============
// === Spans ===
// =============

/// Spans (interval tree), useful for rich text annotations. It is parameterized over a data type,
/// so can be used for storing different annotations.
#[derive(Clone, Debug, Default)]
pub struct Spans<T: Clone> {
    raw: rope::Spans<T>,
}

impl<T: Clone> Spans<T> {
    /// The number of bytes of this span.
    pub fn len(&self) -> Bytes {
        self.raw.len().into()
    }

    /// Checks whether the span is empty.
    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    /// Replace the provided `range` with the new `value` spanned over `length` bytes.
    ///
    /// Spans are like byte chunk where each byte is associated with a value. This function first
    /// removes the subsequence of bytes described in `range` (shifting later bytes to the left),
    /// and then creates a new byte subsequence of the length `length` and associates it with the
    /// `value`. Use with caution, as it can easily lead to wrong amount of bytes covered by the
    /// span.
    pub fn replace_resize(&mut self, range: Range<Bytes>, length: Bytes, value: T) {
        let mut builder = rope::spans::Builder::new(length.as_usize());
        builder.add_span(.., value);
        self.raw.edit(range.into_rope_interval(), builder.build())
    }

    /// Return all spans contained in the provided range.
    pub fn sub(&self, range: Range<Bytes>) -> Self {
        Self { raw: self.raw.subseq(range.into_rope_interval()) }
    }

    /// Convert the span tree to vector of non-overlapping ranges and their values.
    pub fn to_vector(&self) -> Vec<(Range<Bytes>, T)> {
        self.raw
            .iter()
            .map(|t| {
                let start: Bytes = t.0.start.into();
                let end: Bytes = t.0.end.into();
                ((start..end).into(), t.1.clone())
            })
            .collect()
    }
}
