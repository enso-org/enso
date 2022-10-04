//! EnsoGL is a blazing fast vector rendering engine. To learn more about its features and
//! architecture design, read the [`README.md`].

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::let_and_return)]


// ==============
// === Export ===
// ==============

pub use ensogl_core::*;



/// Data type declarations.
pub mod data {
    pub use ensogl_core::data::*;
    pub use ensogl_text as text;
}

/// Graphical interface related components, like buttons, sliders, or text areas.
pub mod gui {
    pub use ensogl_core::gui::*;
    pub use ensogl_text::component as text;
}
