//! Definition of strongly typed units, like `Line`, `CodePointIndex`, or `Location`. Used to
//! express type level dependencies in the whole library.

use crate::prelude::*;

use enso_types::newtype;
use enso_types::unit;



// ===============
// === Exports ===
// ===============

/// Common traits.
pub mod traits {
    pub use super::bytes::Into as TRAIT_bytes_into;
    // pub use super::chars::Into as TRAIT_chars_into;
    pub use super::code_point_index::Into as TRAIT_column_into;
    pub use super::line::Into as TRAIT_line_into;
    pub use super::ubytes::Into as TRAIT_ubytes_into;
}
pub use traits::*;



// =============
// === Bytes ===
// =============

unit! {
/// An offset in the buffer in bytes.
Bytes::bytes(i32)
}

impl Bytes {
    /// Saturating conversion to `usize`.
    pub fn as_usize(self) -> usize {
        self.value.max(0) as usize
    }
}

impl<T: Into<Bytes>> bytes::Into for Range<T> {
    type Output = Range<Bytes>;
    fn bytes(self) -> Self::Output {
        let start = self.start.into();
        let end = self.end.into();
        Range { start, end }
    }
}

impl From<usize> for Bytes {
    fn from(t: usize) -> Self {
        (t as i32).into()
    }
}

impl From<&usize> for Bytes {
    fn from(t: &usize) -> Self {
        (*t as i32).into()
    }
}

impl From<UBytes> for Bytes {
    fn from(t: UBytes) -> Self {
        (t.value as i32).into()
    }
}

impl From<&UBytes> for Bytes {
    fn from(t: &UBytes) -> Self {
        (t.value as i32).into()
    }
}



// ==============
// === UBytes ===
// ==============

unit! {
/// Unsigned bytes unit.
UBytes::ubytes(usize) NO_SUB
}

impl<T: Into<UBytes>> ubytes::Into for Range<T> {
    type Output = Range<UBytes>;
    fn ubytes(self) -> Self::Output {
        let start = self.start.into();
        let end = self.end.into();
        Range { start, end }
    }
}

impl Sub<UBytes> for UBytes {
    type Output = Bytes;
    fn sub(self, rhs: UBytes) -> Self::Output {
        (self.value as i32 - rhs.value as i32).into()
    }
}

impl Sub<Bytes> for UBytes {
    type Output = Bytes;
    fn sub(self, rhs: Bytes) -> Self::Output {
        (self.value as i32 - rhs.value).into()
    }
}

impl Sub<UBytes> for Bytes {
    type Output = Bytes;
    fn sub(self, rhs: UBytes) -> Self::Output {
        (self.value - rhs.value as i32).into()
    }
}

impl Add<Bytes> for UBytes {
    type Output = Bytes;
    fn add(self, rhs: Bytes) -> Self::Output {
        (self.value as i32 + rhs.value).into()
    }
}

impl Add<UBytes> for Bytes {
    type Output = Bytes;
    fn add(self, rhs: UBytes) -> Self::Output {
        (self.value + rhs.value as i32).into()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BytesToUBytesConversionError;

impl TryFrom<Bytes> for UBytes {
    type Error = BytesToUBytesConversionError;

    fn try_from(bytes: Bytes) -> Result<Self, Self::Error> {
        if bytes.value < 0 {
            Err(BytesToUBytesConversionError)
        } else {
            Ok(UBytes::from(bytes.value as usize))
        }
    }
}



// =============
// === Chars ===
// =============

// FIXME: use CodePointIndex instead
unit! {
/// An offset in the buffer in Rust's chars (being roughly the Unicode code points.
///
/// See [`crate`] documentation to know more about codepoints.
Chars::chars(i32)
}

impl Chars {
    /// Saturating conversion to `usize`.
    pub fn as_usize(self) -> usize {
        self.value.max(0) as usize
    }
}

impl<T: Into<Chars>> chars::Into for Range<T> {
    type Output = Range<Chars>;
    fn chars(self) -> Self::Output {
        let start = self.start.into();
        let end = self.end.into();
        Range { start, end }
    }
}

impl From<usize> for Chars {
    fn from(t: usize) -> Self {
        (t as i32).into()
    }
}

impl From<&usize> for Chars {
    fn from(t: &usize) -> Self {
        (*t as i32).into()
    }
}

impl serde::Serialize for Chars {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        i32::from(self).serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Chars {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: serde::Deserializer<'de> {
        i32::deserialize(deserializer).map(|val| val.into())
    }
}


// ============
// === Line ===
// ============

// TODO: Improvement idea. Create `i32Saturated` type which will have all operations saturated.
//       This will make this unit safer.
unit! {
/// A type representing vertical measurements.
Line::line(i32)
}

impl Line {
    /// Saturating conversion to `usize`.
    pub fn as_usize(self) -> usize {
        self.value.max(0) as usize
    }

    /// Compute the absolute value of this line.
    pub fn abs(self) -> Self {
        self.value.saturating_abs().into()
    }
}

impl From<usize> for Line {
    fn from(t: usize) -> Self {
        (t as i32).into()
    }
}

impl From<&usize> for Line {
    fn from(t: &usize) -> Self {
        (*t as i32).into()
    }
}



// ==============
// === CodePointIndex ===
// ==============

// TODO: Improvement idea. Create `i32Saturated` type which will have all operations saturated.
//       This will make this unit safer.
unit! {
/// A type representing horizontal measurements expressed as number of Rust's chars (being roughly
/// the Unicode code points.
///
/// See [`crate`] documentation to know more about codepoints.
///
/// Note: The reason of representing CodePointIndex as a code point is that our text rendering engine
/// display each codepoint as a separate glyph (so it does not support the _grapheme clusters_).
/// This should be fixed when doing
/// https://www.pivotaltracker.com/n/projects/2539304/stories/180392693: after that, the column
/// should be measured in grapheme clusters, to have Text Area cursors behave correctly (and the
/// usages shall be then fixed, e.g. [`crate::text::Text::column_of_byte_offset`]).
CodePointIndex::code_point_index(i32)
}

impl CodePointIndex {
    /// Saturating conversion to `usize`.
    pub fn as_usize(self) -> usize {
        self.value.max(0) as usize
    }

    /// Compute the absolute value of this column.
    pub fn abs(self) -> Self {
        self.value.saturating_abs().into()
    }
}

impl From<usize> for CodePointIndex {
    fn from(t: usize) -> Self {
        (t as i32).into()
    }
}

impl From<&usize> for CodePointIndex {
    fn from(t: &usize) -> Self {
        (*t as i32).into()
    }
}



// ================
// === Location ===
// ================

newtype! {
/// A type representing 2d measurements.
Location {
    line:   Line,
    code_point_index: CodePointIndex,
}}

impl Location {
    /// Line setter.
    pub fn with_line(self, line: Line) -> Self {
        Self { line, ..self }
    }

    /// CodePointIndex setter.
    pub fn with_column(self, code_point_index: CodePointIndex) -> Self {
        Self { code_point_index, ..self }
    }
}
