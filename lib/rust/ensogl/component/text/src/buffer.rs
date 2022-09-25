//! Root of text buffer implementation. The text buffer is a sophisticated model for text styling
//! and editing operations.

use crate::prelude::*;


// ==============
// === Export ===
// ==============

pub mod formatted_rope;
pub mod formatting;
pub mod view;


/// Common traits.
pub mod traits {
    pub use enso_text::traits::*;
}

pub use formatting::*;
pub use view::*;

pub use enso_text::unit::*;
pub use enso_text::Range;
pub use enso_text::Rope;
pub use enso_text::RopeCell;
pub use formatted_rope::*;
