//! Root module for compound geometries. Compound geometries are defined by using primitive
//! geometries and behave like smart constructors for commonly used shapes.

pub mod screen;
pub mod mask_composer;
pub mod sprite;



// ===============
// === Exports ===
// ===============

/// Common types.
pub mod types {
    use super::*;
    pub use screen::Screen;
    pub use mask_composer::MaskComposer;
    pub use sprite::{SpriteSystem,Sprite};
}
