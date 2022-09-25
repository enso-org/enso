//! Root of text buffer implementation. The text buffer is a sophisticated model for text styling
//! and editing operations.

use crate::prelude::*;


// ==============
// === Export ===
// ==============

pub mod formatting;
pub mod movement;
pub mod rope;
pub mod selection;
pub mod view;


/// Common traits.
pub mod traits {
    pub use enso_text::traits::*;
}

pub use formatting::*;
pub use movement::*;
pub use view::*;

pub use enso_text::unit::*;
pub use enso_text::Range;
pub use enso_text::Rope;
pub use enso_text::RopeCell;
