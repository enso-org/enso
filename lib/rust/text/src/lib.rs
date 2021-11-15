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
pub use text::Text;
pub use text::TextCell;
pub use unit::traits;
pub use unit::*;

use enso_prelude as prelude;
