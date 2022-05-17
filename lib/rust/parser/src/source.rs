//! Enso language source code related utilities, including a structure attaching source code to
//! other types or an abstraction allowing for getting the representation of an entity, such as
//! [`Token`] (tokens remember the location only, in order to get their representation, the source
//! code needs to be sampled).


// ==============
// === Export ===
// ==============

pub mod code;
pub mod span;

pub use code::Code;
pub use span::Offset;
pub use span::Span;
pub use span::VisibleOffset;



/// Popular traits.
pub mod traits {
    pub use super::span::traits::*;
}
pub use traits::*;
