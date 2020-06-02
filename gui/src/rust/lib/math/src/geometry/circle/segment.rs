//! Provides functionality related to circle segments.

use crate::algebra::Cos;
use crate::algebra::Field;
use crate::algebra::Sin;

use core::fmt::Debug;



// ===============
// === Segment ===
// ===============

/// Implements computations related to circle segments. For details and
/// background on the formulas, see https://en.wikipedia.org/wiki/Circular_segment
#[derive(Clone,Debug,PartialEq)]
pub struct Segment<T> {
    radius : T,
    angle  : T,
}

/// Helper trait to define the traits required in the Segment computations.
pub trait SegmentInput<T> = Field<T> + Sin<Output=T> + Cos<Output=T> + From<f32> + Clone + Debug;

impl<T> Segment<T>
where T: SegmentInput<T> {

    /// Constructor. Angle is required to be in radians.
    pub fn new(radius:T, angle:T) -> Self {
        Segment {radius,angle}
    }

    /// The length of the direct line between the segment end points.
    pub fn chord_length(&self) -> T {
        let radius = self.radius.clone();
        let theta  = self.angle.clone();
        T::from(2.0_f32) * radius * (theta * T::from(0.5_f32)).sin()
    }

    /// The arc length of the circle segment.
    pub fn arc_length(&self) -> T {
        let radius = self.radius.clone();
        let theta  = self.angle.clone();
        radius * theta
    }

    /// The sagitta (height) of the segment.
    pub fn sagitta(&self) -> T {
        let radius = self.radius.clone();
        let theta  = self.angle.clone();
        radius * (T::from(1.0_f32) - (theta * T::from(0.5_f32)).cos())
    }

    /// The apothem (height) of the triangular portion.
    pub fn apothem(self) -> T {
        self.radius.clone() - self.sagitta()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use assert_approx_eq::*;

    #[test]
    fn check_arc_length() {
        let segment = Segment::new(1.0,0_f32.to_radians());
        assert_approx_eq!(segment.arc_length(),0.0);

        let segment = Segment::new(1.0,15_f32.to_radians());
        assert_approx_eq!(segment.arc_length(),0.2617994);

        let segment = Segment::new(1.0,45_f32.to_radians());
        assert_approx_eq!(segment.arc_length(),0.7853982);

        let segment = Segment::new(1.0,65_f32.to_radians());
          assert_approx_eq!(segment.arc_length(),1.1344640);

        let segment = Segment::new(1.0,75_f32.to_radians());
        assert_approx_eq!(segment.arc_length(),1.3089969);

        let segment = Segment::new(1.0,180_f32.to_radians());
         assert_approx_eq!(segment.arc_length(),3.1415927);

        let segment = Segment::new(1.0,359_f32.to_radians());
        assert_approx_eq!(segment.arc_length(),6.2657320);

        let segment = Segment::new(1.0,360_f32.to_radians());
        assert_approx_eq!(segment.arc_length(),6.2831853);
    }

    #[test]
    fn check_chord_length() {
        let segment = Segment::new(1.0,0_f32.to_radians());
        assert_approx_eq!(segment.chord_length(),0.0);

        let segment = Segment::new(1.0,15_f32.to_radians());
        assert_approx_eq!(segment.chord_length(),0.2610524);

        let segment = Segment::new(1.0,45_f32.to_radians());
        assert_approx_eq!(segment.chord_length(),0.7653669);

        let segment = Segment::new(1.0,65_f32.to_radians());
        assert_approx_eq!(segment.chord_length(),1.0745992);

        let segment = Segment::new(1.0,75_f32.to_radians());
        assert_approx_eq!(segment.chord_length(),1.2175229);

        let segment = Segment::new(1.0,180_f32.to_radians());
        assert_approx_eq!(segment.chord_length(),2.0);

        let segment = Segment::new(1.0,359_f32.to_radians());
        assert_approx_eq!(segment.chord_length(),0.0174531);

        let segment = Segment::new(1.0,360_f32.to_radians());
        assert_approx_eq!(segment.chord_length(),0.0);
    }

    #[test]
    fn check_sagitta() {
        let segment = Segment::new(1.0,0_f32.to_radians());
        assert_approx_eq!(segment.sagitta(),0.0);

        let segment = Segment::new(1.0,15_f32.to_radians());
        assert_approx_eq!(segment.sagitta(),0.0085551);

        let segment = Segment::new(1.0,45_f32.to_radians());
        assert_approx_eq!(segment.sagitta(),0.0761205);

        let segment = Segment::new(1.0,65_f32.to_radians());
        assert_approx_eq!(segment.sagitta(),0.1566086);

        let segment = Segment::new(1.0,75_f32.to_radians());
        assert_approx_eq!(segment.sagitta(),0.2066467);

        let segment = Segment::new(1.0,180_f32.to_radians());
        assert_approx_eq!(segment.sagitta(),1.0);

        let segment = Segment::new(1.0,359_f32.to_radians());
        assert_approx_eq!(segment.sagitta(),1.9999619);

        let segment = Segment::new(1.0,360_f32.to_radians());
        assert_approx_eq!(segment.sagitta(),2.0);
    }

    #[test]
    fn check_apothem() {
        let segment = Segment::new(1.0,0_f32.to_radians());
        assert_approx_eq!(segment.apothem(),1.0 - 0.0);

        let segment = Segment::new(1.0,15_f32.to_radians());
        assert_approx_eq!(segment.apothem(),1.0 - 0.0085551);

        let segment = Segment::new(1.0,45_f32.to_radians());
        assert_approx_eq!(segment.apothem(),1.0 - 0.0761205);

        let segment = Segment::new(1.0,65_f32.to_radians());
        assert_approx_eq!(segment.apothem(),1.0 - 0.1566086);

        let segment = Segment::new(1.0,75_f32.to_radians());
        assert_approx_eq!(segment.apothem(),1.0 - 0.2066467);

        let segment = Segment::new(1.0,180_f32.to_radians());
        assert_approx_eq!(segment.apothem(),1.0 - 1.0);

        let segment = Segment::new(1.0,359_f32.to_radians());
        assert_approx_eq!(segment.apothem(),1.0 - 1.9999619);

        let segment = Segment::new(1.0,360_f32.to_radians());
        assert_approx_eq!(segment.apothem(),1.0 - 2.0);
    }
}
