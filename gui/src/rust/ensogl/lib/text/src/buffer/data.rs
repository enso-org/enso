//! The data hold by the text buffer. Under the hood it is implemented as an efficient string rope.

use crate::prelude::*;
use rope::Rope;


// ===============
// === Exports ===
// ===============

pub mod range;
pub mod rope;
pub mod spans;
pub mod unit;

pub use range::Range;
pub use range::RangeBounds;
pub use rope::Cursor;
pub use rope::metric;
pub use spans::Spans;
pub use unit::traits;
pub use unit::*;



// ============
// === Data ===
// ============

/// Efficient text container used by the text buffer. Implemented as a rope under the hood.
///
/// A [rope](https://en.wikipedia.org/wiki/Rope_(data_structure)) is a data structure for strings,
/// specialized for incremental editing operations. Most operations (such as insert, delete,
/// substring) are O(log n). This module provides an immutable (also known as
/// [persistent](https://en.wikipedia.org/wiki/Persistent_data_structure)) version of Ropes, and if
/// there are many copies of similar strings, the common parts are shared.
///
/// Internally, the implementation uses thread safe reference counting. Mutations are generally
/// copy-on-write, though in-place edits are supported as an optimization when only one reference
/// exists, making the implementation as efficient as a mutable version.
///
/// This type provides multiple `From` implementations for easy conversions from string-like types,
/// and vice-versa.
///
/// Please note that the underlying rope implementation comes from `xi-rope` crate which does not
/// use strong types for all units (like line number, column number, byte offset), so part of
/// responsibility of this struct is to wrap the underlying API with strong types introduced in this
/// library.
#[derive(Debug,Clone,Default,Deref)]
#[allow(missing_docs)]
pub struct Data {
    pub rope : Rope,
}
impl_clone_ref_as_clone!(Data);


impl Data {
    /// Return the len of the text in bytes.
    pub fn len(&self) -> Bytes {
        Bytes(self.rope.len())
    }

    /// Check whether the text is empty.
    pub fn is_empty(&self) -> bool {
        self.rope.is_empty()
    }

    /// Range of the text in bytes.
    pub fn range(&self) -> Range<Bytes> {
        (..self.len()).into()
    }

    /// Crop the provided range so it will be contained of the range of this data. This ensures that
    /// the provided range will be valid for operations on this data.
    pub fn crop_range(&self, range:impl RangeBounds) -> Range<Bytes> {
        range.with_upper_bound(self.len())
    }

    /// Return the offset to the previous grapheme if any. See the documentation of the library to
    /// learn more about graphemes.
    pub fn prev_grapheme_offset(&self, offset:Bytes) -> Option<Bytes> {
        self.rope.prev_grapheme_offset(offset.value).map(Bytes)
    }

    /// Return the offset to the next grapheme if any. See the documentation of the library to
    /// learn more about graphemes.
    pub fn next_grapheme_offset(&self, offset:Bytes) -> Option<Bytes> {
        self.rope.next_grapheme_offset(offset.value).map(Bytes)
    }
}


// === Conversions ===

impl From<Rope>     for Data { fn from(t:Rope)     -> Self { Self {rope:t} } }
impl From<&Rope>    for Data { fn from(t:&Rope)    -> Self { t.clone().into() } }

impl From<&str>     for Data { fn from(t:&str)     -> Self { Self {rope:t.into()} } }
impl From<String>   for Data { fn from(t:String)   -> Self { Self {rope:t.into()} } }
impl From<&String>  for Data { fn from(t:&String)  -> Self { Self {rope:t.into()} } }
impl From<&&String> for Data { fn from(t:&&String) -> Self { (*t).into() } }
impl From<&&str>    for Data { fn from(t:&&str)    -> Self { (*t).into() } }

impl From<Data>     for String { fn from(t:Data)   -> Self { t.rope.into() } }
impl From<&Data>    for String { fn from(t:&Data)  -> Self { t.clone().into() } }
impl From<&&Data>   for String { fn from(t:&&Data) -> Self { (*t).into() } }
