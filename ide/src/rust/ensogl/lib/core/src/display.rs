//! Root module for all display-related abstractions, including display objects, shapes, geometries,
//! rendering utilities, etc.

pub mod camera;
pub mod layout;
pub mod object;
pub mod navigation;
pub mod render;
pub mod scene;
pub mod shape;
pub mod symbol;
pub mod style;
pub mod world;



// ===============
// === Exports ===
// ===============

/// Common traits.
pub mod traits {
    use super::*;
    pub use object::traits::*;
}

/// Common types.
pub mod types {
    use super::*;
    pub use object::Object;
    pub use scene::Scene;
    pub use scene::dom::DomScene;
    pub use super::symbol::*;
    pub use super::traits::*;
}
pub use types::*;
