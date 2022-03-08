//! EnsoGL is a blazing fast vector rendering engine. To learn more about its features and
//! architecture design, read the [`README.md`].

// === Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



//
// === Non-Standard Linter Configuration ===



// === Non-Standard Linter Configuration ===



// === Non-standard linter configuration ===



// === Standard linter configuration ===
#![warn(missing_copy_implementations)]#![warn(missing_debug_implementations)]#![warn(missing_docs)]#![warn(trivial_casts)]#![warn(trivial_numeric_casts)]#![warn(unsafe_code)]#![warn(unused_import_braces)]#![warn(unused_qualifications)]
// === Non-standard linter configuration ===


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
