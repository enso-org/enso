//! Root module for compound geometries. Compound geometries are defined by using primitive
//! geometries and behave like smart constructors for commonly used shapes.

pub mod mask_composer;
pub mod screen;
pub mod sprite;



// ===============
// === Exports ===
// ===============

/// Common types.
pub mod types {
    use super::*;
    pub use mask_composer::MaskComposer;
    pub use screen::Screen;
    pub use sprite::Sprite;
    pub use sprite::SpriteSystem;
}
