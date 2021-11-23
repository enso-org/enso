//! The text operation utilities.
//!
//! This crate contains several utility structures for operations on text:
//! * The effective [`Text`] structure, optimized for middle-insertions, based on the rope
//!   structure.
//! * A set of units, forcing the developers to think about how the text positions are expressed (in
//!   chars, or in bytes? Or maybe in _grapheme clusters_)?
//! * An alternative [`Range`] with text-related trait implementations + copyable.
//! * Interval tree structure [`Spans`] useful for text rich decorations.

#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

pub mod range;
pub mod rope;
pub mod spans;
pub mod text;
pub mod unit;

pub use range::Range;
pub use range::RangeBounds;
pub use rope::metric;
pub use rope::Cursor;
pub use spans::Spans;
pub use text::Change;
pub use text::Text;
pub use text::TextCell;
pub use unit::traits;
pub use unit::*;

/// Commonly used utilities.
pub mod prelude {
    pub use enso_prelude::*;
    pub use enso_types::*;
}
