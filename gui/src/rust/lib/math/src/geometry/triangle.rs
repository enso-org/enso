//! This module defines an computations related to triangle geometry.

use crate::algebra::Acos;
use crate::algebra::Cos;
use crate::algebra::Field;
use crate::algebra::Sin;
use crate::algebra::Sqrt;

use core::f32::consts::PI;
use std::ops::Add;
use std::ops::Mul;
use std::ops::Sub;



/// Represents a triangle through its angles and side lengths. This struct is only meant to be a
/// return type within this module and should not otherwise be instantiated.
///
///
/// Triangle Schematic
/// -------------------
/// ```text
///                        C
///                      /  \
///                    /      \
///                  / angle_c  \
///     side_ca    /              \  side_bc
///              /                  \
///            / angle_a     angle_b \
///          A ----------------------- B
///                    side_ab
/// ```
/// Where side_ab, side_bc and side_ca are the length of the respective side.
/// Where alpha, beta and gamma are the respective angle.
///
/// Example
/// -------
/// ```
/// # use assert_approx_eq::assert_approx_eq;
/// # use enso_math::geometry::triangle::Triangle;
/// let result = Triangle::<f32>::from_sides_and_angle(1.0, 1.0 , 60.0_f32.to_radians());
///
/// assert_approx_eq!(result.side_bc(), 1.0);
/// assert_approx_eq!(result.angle_a(), 60.0_f32.to_radians());
/// assert_approx_eq!(result.angle_c(), 60.0_f32.to_radians());
/// ```
#[allow(missing_docs)]
#[derive(Debug)]
pub struct Triangle<T> {
    side_bc: T,
    side_ca: T,
    side_ab: T,
    angle_a: T,
    angle_b: T,
    angle_c: T,
}

#[allow(missing_docs)]
impl<T> Triangle<T> {
    pub fn side_bc(&self) -> &T {
        &self.side_bc
    }

    pub fn side_ca(&self) -> &T {
        &self.side_ca
    }

    pub fn side_ab(&self) -> &T {
        &self.side_ab
    }

    pub fn angle_a(&self) -> &T {
        &self.angle_a
    }

    pub fn angle_b(&self) -> &T {
        &self.angle_b
    }

    pub fn angle_c(&self) -> &T {
        &self.angle_c
    }
}

impl<T> Triangle<T>
// This uses higher-ranked trait bounds to ensure we can do some of the arithmetic operations
// also on references. This allows us to both avoid requiring `Copy` (which we don't have in
// `Var<_>` types and also cloning values that are used in multiple computations.
// For more information see https://stackoverflow.com/q/34630695/1175813
where        T: Field<T> + Sin<Output=T> + Cos<Output=T> + Acos<Output=T>
                + Sqrt<Output=T> + Clone + From<f32>,
for <'a> &'a T: Add<&'a T,Output=T> + Mul<&'a T,Output=T> + Sub<&'a T,Output=T>{
    /// Compute a triangle from two sides and the included angle (SAS).
    /// See https://en.wikipedia.org/wiki/Solution_of_triangles#Two_sides_and_the_included_angle_given_(SAS)
    pub fn from_sides_and_angle(side_bc:T, side_ca:T, angle_c:T) -> Triangle<T> {
        let side_bc_squared = &side_bc * &side_bc;
        let side_ca_squared = &side_ca * &side_ca;

        let two = T::from(2.0_f32);

        let side_ab_squared_minuend    = &side_bc_squared + &side_ca_squared;
        let side_ab_squared_subtrahend = (&(&two * &side_bc) * &side_ca) * angle_c.cos();
        let side_ab_squared            = side_ab_squared_minuend - side_ab_squared_subtrahend;
        let side_ab                    = side_ab_squared.sqrt();
        let angle_a_cos_numerator      = side_ca_squared + side_ab_squared - side_bc_squared;
        let angle_a_cos_denominator    = two * (&side_ca * &side_ab);
        let angle_a_cos                = angle_a_cos_numerator / angle_a_cos_denominator;
        let angle_a                    = angle_a_cos.acos();
        let angle_b                    = &(&T::from(PI) -&angle_a) - &angle_c;

        Triangle{side_bc,side_ca,side_ab,angle_a,angle_b,angle_c}
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
    fn test_from_sides_and_angle() {
        let result = Triangle::<f32>::from_sides_and_angle(1.0,1.0,60.0_f32.to_radians());

        assert_approx_eq!(result.side_bc(),1.0);
        assert_approx_eq!(result.side_ca(),1.0);
        assert_approx_eq!(result.angle_c(),60_f32.to_radians());

        assert_approx_eq!(result.side_ab(),1.0);
        assert_approx_eq!(result.angle_b(),60.0_f32.to_radians());
        assert_approx_eq!(result.angle_a(),60.0_f32.to_radians());

        let result = Triangle::<f32>::from_sides_and_angle(1.0,1.0,90.0_f32.to_radians());

        assert_approx_eq!(result.side_bc(),1.0);
        assert_approx_eq!(result.side_ca(),1.0);
        assert_approx_eq!(result.angle_c(),90_f32.to_radians());

        assert_approx_eq!(result.side_ab(),1.4142135);
        assert_approx_eq!(result.angle_b(),45.0_f32.to_radians());
        assert_approx_eq!(result.angle_a(),45.0_f32.to_radians());

        let result = Triangle::<f32>::from_sides_and_angle(1.0,4.0,128.0_f32.to_radians());

        assert_approx_eq!(result.side_bc(),1.0);
        assert_approx_eq!(result.side_ca(),4.0);
        assert_approx_eq!(result.angle_c(),128_f32.to_radians());

        assert_approx_eq!(result.side_ab(),4.682445);
        assert_approx_eq!(result.angle_b(),0.7384765);
        assert_approx_eq!(result.angle_a(),0.16909483);
    }
}
