//! Root module for topology-related utilities.
//! Defines unit of measurement abstraction. See: https://en.wikipedia.org/wiki/Unit_of_measurement

use crate::algebra::*;



// =============
// === Units ===
// =============

crate::unit!(Pixels::pixels(f32));
crate::unit!(Radians::radians(f32));
crate::unit!(Degrees::degrees(f32));

impl From<i32>   for Pixels { fn from(t:i32)   -> Self { (t as f32).into() } }
impl From<&i32>  for Pixels { fn from(t:&i32)  -> Self { (*t).into() } }
impl From<&&i32> for Pixels { fn from(t:&&i32) -> Self { (*t).into() } }

impl pixels::Into for i32 {
    type Output = Pixels;
    fn pixels(self) -> Pixels { self.into() }
}

impl pixels::Into for &i32 {
    type Output = Pixels;
    fn pixels(self) -> Pixels { self.into() }
}

impl pixels::Into for &&i32 {
    type Output = Pixels;
    fn pixels(self) -> Pixels { self.into() }
}



// ==============
// === Traits ===
// ==============

/// Commonly used traits.
pub mod traits {
    pub use super::pixels::Into  as TRAIT_IntoPixels;
    pub use super::radians::Into as TRAIT_IntoRadians;
    pub use super::degrees::Into as TRAIT_IntoDegrees;
}

pub use traits::*;
