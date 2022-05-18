//! Root module for metric abstractions.

use super::var::*;

use crate::types::topology;


// ==============
// === Export ===
// ==============

pub use crate::types::topology::Degrees;
pub use crate::types::topology::Pixels;
pub use crate::types::topology::Radians;



// =====================
// === PixelDistance ===
// =====================

/// Provides a `px` method to every unit that can be converted to a pixel distance.
#[allow(missing_docs)]
pub trait PixelDistance {
    type Output;
    fn px(&self) -> Self::Output;
}

impl PixelDistance for i32 {
    type Output = Var<Pixels>;
    fn px(&self) -> Self::Output {
        topology::pixels::Into::pixels(self).into()
    }
}

impl PixelDistance for f32 {
    type Output = Var<Pixels>;
    fn px(&self) -> Self::Output {
        topology::pixels::Into::pixels(self).into()
    }
}

impl PixelDistance for Var<f32> {
    type Output = Var<Pixels>;
    fn px(&self) -> Self::Output {
        match self {
            Var::Static(v) => v.px(),
            Var::Dynamic(v) => Var::Dynamic(v.clone()),
        }
    }
}



// ===============
// === Exports ===
// ===============

/// Common types.
pub mod types {
    pub use super::PixelDistance;
    use super::*;
    pub use topology::Degrees;
    pub use topology::Pixels;
    pub use topology::Radians;
}

pub use types::*;
