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
#[allow(missing_docs)]
pub trait PixelDistance {
    type Output;
    /// Distance in pixels.
    fn px(&self) -> Self::Output;
}

impl PixelDistance for i32 {
    type Output = Var<Distance<Pixels>>;
    fn px(&self) -> Self::Output {
        unit::PixelDistance::px(self).into()
    }
}

impl PixelDistance for f32 {
    type Output = Var<Distance<Pixels>>;
    fn px(&self) -> Self::Output {
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
