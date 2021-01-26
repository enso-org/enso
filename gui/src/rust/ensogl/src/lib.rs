//! EnsoGL is a blazing fast 2D vector rendering engine with a rich set of primitives and a GUI
//! component library. It is able to display millions of shapes 60 frames per second in a web
//! browser on a modern laptop hardware. This is the main entry point to the library, which
//! re-exports several components to a common namespace.

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
