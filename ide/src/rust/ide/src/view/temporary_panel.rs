//! This module contains a temporary definition of Panel. There is a plan to implement proper
//! panels in the future.

use nalgebra::Vector2;



// ===============
// === Padding ===
// ===============

/// A struct containing the padding values.
/// This code is temporary and should be used with cautious.
#[derive(Clone,Copy,Debug,Default)]
pub struct TemporaryPadding {
    #[allow(missing_docs)]
    pub left : f32,
    #[allow(missing_docs)]
    pub top : f32,
    #[allow(missing_docs)]
    pub right : f32,
    #[allow(missing_docs)]
    pub bottom : f32
}



// =============
// === Panel ===
// =============

/// A trait defining a panel interface.
/// This code is temporary and should be used with cautious.
pub trait TemporaryPanel {
    /// Sets padding.
    fn set_padding(&mut self, padding: TemporaryPadding);

    /// Gets padding.
    fn padding(&self) -> TemporaryPadding;

    /// Sets size.
    fn set_size(&mut self, size:Vector2<f32>);

    /// Gets size.
    fn size(&self) -> Vector2<f32>;

    /// Sets position.
    fn set_position(&mut self, position:Vector2<f32>);

    /// Gets position.
    fn position(&self) -> Vector2<f32>;
}
