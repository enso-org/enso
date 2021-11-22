//! The data hold by the text buffer. Under the hood it is implemented as an efficient string rope.

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
pub use text::Text;
pub use text::TextCell;
pub use unit::traits;
pub use unit::*;
