//! Defines the tristimulus values of the CIE Illuminants.
//!
//! White point is the reference white or target white as seen by a standard observer under a
//! standard illuminant. For example, photographs taken indoors may be lit by incandescent lights,
//! which are relatively orange compared to daylight. Defining "white" as daylight will give
//! unacceptable results when attempting to color-correct a photograph taken with incandescent
//! lighting.
//!
//! Note that this module implements currently only `D65` white point. In case other will be needed
//! they can be easily ported from the Rust `Palette` library implementation.

use super::super::component::*;
use super::def::*;


/// Common traits.
pub mod traits {
    pub use super::WhitePoint;
}


// =================
// === WhiePoint ===
// =================

/// Xyz color co-ordinates for a given white point.
///
/// A white point (often referred to as reference white or target white in technical documents)
/// is a set of tristimulus values or chromaticity coordinates that serve to define the color
/// "white" in image capture, encoding, or reproduction.
///
/// Custom white points can be easily defined on an empty struct with the tristimulus values
/// and can be used in place of the ones defined in this library.
pub trait WhitePoint {
    ///Get the Xyz chromacity co-ordinates for the white point.
    fn get_xyz() -> Xyz;
}



// =======================
// === Implementations ===
// =======================

/// CIE D series standard illuminant - D65.
///
/// D65 White Point is the natural daylight with a color temperature of 6500K for 2° Standard
/// Observer.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct D65;
impl WhitePoint for D65 {
    fn get_xyz() -> Xyz {
        from_components(Components((0.95047, 1.0, 1.08883)))
    }
}
