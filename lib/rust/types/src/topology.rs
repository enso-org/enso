//! Root module for topology-related utilities.
//! Defines unit of measurement abstraction. See: https://en.wikipedia.org/wiki/Unit_of_measurement

use crate::algebra::*;



// =============
// === Units ===
// =============

crate::unit!(Pixels::pixels(f32));
crate::unit!(Degrees::degrees(f32));
crate::unit!(Radians::radians(f32));


// === Pixels ===

impl From<i32> for Pixels {
    fn from(t: i32) -> Self {
        (t as f32).into()
    }
}
impl From<&i32> for Pixels {
    fn from(t: &i32) -> Self {
        (*t).into()
    }
}
impl From<&&i32> for Pixels {
    fn from(t: &&i32) -> Self {
        (*t).into()
    }
}

impl pixels::Into for i32 {
    type Output = Pixels;
    fn pixels(self) -> Pixels {
        self.into()
    }
}

impl pixels::Into for &i32 {
    type Output = Pixels;
    fn pixels(self) -> Pixels {
        self.into()
    }
}

impl pixels::Into for &&i32 {
    type Output = Pixels;
    fn pixels(self) -> Pixels {
        self.into()
    }
}

impl Min for Pixels {
    fn min(a: Self, b: Self) -> Self {
        Pixels::from(min(a.value, b.value))
    }
}


// === Degrees ===

impl From<Radians> for Degrees {
    fn from(rad: Radians) -> Self {
        Degrees(rad.value * 180.0 / PI)
    }
}


// === Radians ===

impl From<Degrees> for Radians {
    fn from(deg: Degrees) -> Self {
        Radians(deg.value * PI / 180.0)
    }
}


// ==============
// === Traits ===
// ==============

/// Commonly used traits.
pub mod traits {
    pub use super::degrees::Into as TRAIT_IntoDegrees;
    pub use super::pixels::Into as TRAIT_IntoPixels;
    pub use super::radians::Into as TRAIT_IntoRadians;
}

use std::f32::consts::PI;
pub use traits::*;



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    // Clippy complains about `-180.0.degrees()`, but the precedence is irrelevant, and code looks
    // cleaner without parentheses
    #[allow(clippy::precedence)]
    fn degree_radian_conversions() {
        fn assert_equivalence(deg: Degrees, rad: Radians) {
            let deg_from_rad = Degrees::from(rad);
            let rad_from_deg = Radians::from(deg);
            assert_eq!(deg, deg_from_rad);
            assert_eq!(rad, rad_from_deg);
        }

        assert_equivalence(720.0.degrees(), (PI * 4.0).radians());
        assert_equivalence(360.0.degrees(), (PI * 2.0).radians());
        assert_equivalence(270.0.degrees(), (PI * 3.0 / 2.0).radians());
        assert_equivalence(180.0.degrees(), PI.radians());
        assert_equivalence(90.0.degrees(), (PI / 2.0).radians());
        assert_equivalence(60.0.degrees(), (PI / 3.0).radians());
        assert_equivalence(45.0.degrees(), (PI / 4.0).radians());
        assert_equivalence(0.0.degrees(), 0.0.radians());
        assert_equivalence(-180.0.degrees(), -PI.radians());
    }
}
