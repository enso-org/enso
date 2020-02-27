//! Root module for metric abstractions.

use super::var::*;

use crate::math::topology::unit;

pub use crate::math::topology::unit::Distance;
pub use crate::math::topology::unit::Angle;
pub use crate::math::topology::unit::Unit;
pub use crate::math::topology::unit::Pixels;
pub use crate::math::topology::unit::Radians;
pub use crate::math::topology::unit::Degrees;



// =====================
// === PixelDistance ===
// =====================

/// Provides a `px` method to every unit that can be converted to a pixel distance.
pub trait PixelDistance {
    /// Distance in pixels.
    fn px(&self) -> Var<Distance<Pixels>>;
}

impl<T: unit::PixelDistance> PixelDistance for T {
    fn px(&self) -> Var<Distance<Pixels>> {
        unit::PixelDistance::px(self).into()
    }
}



// ===============
// === Exports ===
// ===============

/// Common types.
pub mod types {
    use super::*;
    pub use super::PixelDistance;
    pub use unit::Distance;
    pub use unit::Angle;
    pub use unit::Unit;
    pub use unit::Pixels;
    pub use unit::Radians;
    pub use unit::Degrees;
}

pub use types::*;
