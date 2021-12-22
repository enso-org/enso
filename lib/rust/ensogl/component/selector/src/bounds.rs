//! Bounds represent an interval with inclusive ends. They are used to indicates the lowest and
//! highest value that can be selected in a selection component.
//!
//! Note: this is used instead of `Range`, as `Range` cannot easily be used in our FRP because it
//! does not implement `Default`.

use crate::prelude::*;



// ==============
// === Bounds ===
// ==============

/// Bounds of a selection. This indicates the lowest and highest value that can be selected in a
/// selection component.
#[derive(Clone, Copy, Debug, Default)]
pub struct Bounds {
    /// Start of the bounds interval (inclusive).
    pub start: f32,
    /// End of the bounds interval (inclusive).
    pub end:   f32,
}

impl Bounds {
    /// Constructor.
    pub fn new(start: f32, end: f32) -> Self {
        Bounds { start, end }
    }

    /// Return the `Bound` with the lower bound as `start` and the upper bound as `end`.
    pub fn sorted(self) -> Self {
        if self.start > self.end {
            Bounds { start: self.end, end: self.start }
        } else {
            self
        }
    }

    /// Return the distance between start and end point.
    pub fn width(self) -> f32 {
        self.end - self.start
    }

    /// Return the center between start and end.
    pub fn center(self) -> f32 {
        self.start + self.width() / 2.0
    }
}

impl From<(f32, f32)> for Bounds {
    fn from((start, end): (f32, f32)) -> Self {
        Bounds { start, end }
    }
}

/// Frp utility method to normalise the given value to the given Bounds.
///
/// Example usage:
/// ```text
/// normalised <- all2(&value,&bounds).map(normalise_value);
/// ````
pub fn normalise_value((value, bounds): &(f32, Bounds)) -> f32 {
    let width = bounds.width();
    if width == 0.0 {
        return 0.0;
    }
    (value - bounds.start) / width
}

/// Frp utility method to compute the absolute value from a normalised value.
/// Inverse of `normalise_value`.
///
/// Example usage:
/// ```text
/// value  <- all(&bounds,&normalised).map(absolute_value);
/// ````
pub fn absolute_value((bounds, normalised_value): &(Bounds, f32)) -> f32 {
    (normalised_value * bounds.width()) + bounds.start
}

/// Returns the normalised value that correspond to the click position on the shape.
/// Note that the shape is centered on (0,0), thus half the width extends into the negative values.
/// For use in FRP `map` method, thus taking references.
#[allow(clippy::trivially_copy_pass_by_ref)]
pub fn position_to_normalised_value(pos: &Vector2, width: &f32) -> f32 {
    if *width == 0.0 {
        return 0.0;
    }
    ((pos.x / (width / 2.0)) + 1.0) / 2.0
}

/// Check whether the given value is within the given bounds.
fn value_in_bounds(value: f32, bounds: Bounds) -> bool {
    let bounds_sorted = bounds.sorted();
    value >= bounds_sorted.start && value <= bounds_sorted.end
}

/// Check whether the given bounds are completely contained in the second bounds.
pub fn bounds_in_bounds(bounds_inner: Bounds, bounds_outer: Bounds) -> bool {
    value_in_bounds(bounds_inner.start, bounds_outer)
        && value_in_bounds(bounds_inner.end, bounds_outer)
}

/// Clamp `value` to the `overflow_bounds`, or to [0, 1] if no bounds are given.
/// For use in FRP `map` method, thus taking references.
///
/// Example usage:
/// ```text
///  clamped <- value_update.map2(&normalised_overflow_bounds,clamp_with_overflow);
/// ```
#[allow(clippy::trivially_copy_pass_by_ref)]
pub fn clamp_with_overflow(value: &f32, overflow_bounds: &Option<Bounds>) -> f32 {
    if let Some(overflow_bounds) = overflow_bounds {
        value.clamp(overflow_bounds.start, overflow_bounds.end)
    } else {
        value.clamp(0.0, 1.0)
    }
}

/// Indicates whether the `bounds` would be clamped when given to `clamp_with_overflow`.
/// For use in FRP `map` method, thus taking references.
///
/// Example usage:
/// ```text
///  is_in_bounds <- bounds_update.map2(&normalised_overflow_bounds,should_clamp_with_overflow);
/// ```
#[allow(clippy::trivially_copy_pass_by_ref)]
pub fn should_clamp_with_overflow(bounds: &Bounds, overflow_bounds: &Option<Bounds>) -> bool {
    if let Some(overflow_bounds) = overflow_bounds {
        bounds_in_bounds(*bounds, *overflow_bounds)
    } else {
        bounds_in_bounds(*bounds, (0.0, 1.0).into())
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use float_eq::assert_float_eq;
    use std::f32::NAN;

    #[test]
    fn test_normalise_value() {
        let test = |start, end, value, expected| {
            let bounds = Bounds::new(start, end);
            let normalised = normalise_value(&(value, bounds));
            assert_float_eq!(normalised, expected, ulps <= 7)
        };

        test(0.0, 1.0, 0.0, 0.0);
        test(0.0, 1.0, 0.1, 0.1);
        test(0.0, 1.0, 0.2, 0.2);
        test(0.0, 1.0, 0.3, 0.3);
        test(0.0, 1.0, 0.4, 0.4);
        test(0.0, 1.0, 0.5, 0.5);
        test(0.0, 1.0, 0.6, 0.6);
        test(0.0, 1.0, 0.7, 0.7);
        test(0.0, 1.0, 0.8, 0.8);
        test(0.0, 1.0, 0.9, 0.9);
        test(0.0, 1.0, 1.0, 1.0);

        test(0.0, 1.0, -2.0, -2.0);
        test(0.0, 1.0, -1.0, -1.0);
        test(0.0, 1.0, 2.0, 2.0);
        test(0.0, 1.0, 3.0, 3.0);

        test(-1.0, 1.0, -1.0, 0.0);
        test(-1.0, 1.0, -0.5, 0.25);
        test(-1.0, 1.0, 0.0, 0.5);
        test(-1.0, 1.0, 0.5, 0.75);
        test(-1.0, 1.0, 1.0, 1.0);

        test(1.0, -1.0, -1.0, 1.0);
        test(1.0, -1.0, -0.5, 0.75);
        test(1.0, -1.0, 0.0, 0.5);
        test(1.0, -1.0, 0.5, 0.25);
        test(1.0, -1.0, 1.0, 0.0);

        test(-10.0, 20.0, -10.0, 0.0);
        test(-10.0, 20.0, 20.0, 1.0);
        test(-10.0, 20.0, 0.0, 0.33333333);

        test(-999999999.0, 999999999.0, -999999999.0, 0.0);
        test(-999999999.0, 999999999.0, 0.0, 0.5);
        test(-999999999.0, 999999999.0, 999999999.0, 1.0);

        test(0.0, 0.0, 1.0, 0.0);
        test(0.0, 0.0, 0.0, 0.0);
        test(0.0, 0.0, -1.0, 0.0);
    }

    #[test]
    fn test_absolute_value() {
        let test = |start, end, value, expected| {
            let bounds = Bounds::new(start, end);
            let normalised = absolute_value(&(bounds, value));
            assert_float_eq!(normalised, expected, ulps <= 7)
        };

        test(0.0, 1.0, 0.0, 0.0);
        test(0.0, 1.0, 0.1, 0.1);
        test(0.0, 1.0, 0.2, 0.2);
        test(0.0, 1.0, 0.3, 0.3);
        test(0.0, 1.0, 0.4, 0.4);
        test(0.0, 1.0, 0.5, 0.5);
        test(0.0, 1.0, 0.6, 0.6);
        test(0.0, 1.0, 0.7, 0.7);
        test(0.0, 1.0, 0.8, 0.8);
        test(0.0, 1.0, 0.9, 0.9);
        test(0.0, 1.0, 1.0, 1.0);

        test(0.0, 1.0, -2.0, -2.0);
        test(0.0, 1.0, -1.0, -1.0);
        test(0.0, 1.0, 2.0, 2.0);
        test(0.0, 1.0, 3.0, 3.0);

        test(-1.0, 1.0, 0.0, -1.0);
        test(-1.0, 1.0, 0.25, -0.5);
        test(-1.0, 1.0, 0.5, 0.0);
        test(-1.0, 1.0, 0.75, 0.5);
        test(-1.0, 1.0, 1.0, 1.0);

        test(1.0, -1.0, 1.0, -1.0);
        test(1.0, -1.0, 0.75, -0.5);
        test(1.0, -1.0, 0.5, 0.0);
        test(1.0, -1.0, 0.25, 0.5);
        test(1.0, -1.0, 0.0, 1.0);

        test(-10.0, 20.0, 0.0, -10.0);
        test(-10.0, 20.0, 1.0, 20.0);
        test(-10.0, 20.0, 0.33333333, 0.0);

        test(-999999999.0, 999999999.0, 0.0, -999999999.0);
        test(-999999999.0, 999999999.0, 0.5, 0.0);
        test(-999999999.0, 999999999.0, 1.0, 999999999.0);

        test(0.0, 0.0, 1.0, 0.0);
        test(1.0, 1.0, 1.0, 1.0);
        test(1.0, 1.0, 2.0, 1.0);
        test(1.0, 1.0, -2.0, 1.0);
    }


    #[test]
    fn test_position_to_normalised_value() {
        let test = |pos, width, expected| {
            let result = position_to_normalised_value(&pos, &width);
            assert_float_eq!(result, expected, ulps <= 7)
        };

        for &y in &[-100.0, 0.0, 100.0, NAN] {
            test(Vector2::new(50.0, y), 100.0, 1.0);
            test(Vector2::new(0.0, y), 100.0, 0.5);
            test(Vector2::new(-50.0, y), 100.0, 0.0);

            test(Vector2::new(100.0, y), 100.0, 1.5);
            test(Vector2::new(-100.0, y), 100.0, -0.5);
            test(Vector2::new(150.0, y), 100.0, 2.0);
            test(Vector2::new(-150.0, y), 100.0, -1.0);
            test(Vector2::new(200.0, y), 100.0, 2.5);
            test(Vector2::new(-200.0, y), 100.0, -1.5);

            test(Vector2::new(-200.0, y), 0.0, 0.0);
        }
    }

    #[test]
    fn test_value_in_bounds() {
        let test = |start, end, value, expected| {
            let result = value_in_bounds(value, Bounds::new(start, end));
            assert_eq!(result, expected, "Testing whether {} in ]{},{}[", value, start, end)
        };

        test(0.0, 1.0, 0.0, true);
        test(0.0, 1.0, 0.5, true);
        test(0.0, 1.0, 1.0, true);
        test(0.0, 1.0, 1.00001, false);
        test(0.0, 1.0, -0.00001, false);

        test(0.0, 10.0, 10.0, true);
        test(0.0, 10.0, 9.999999, true);
        test(0.0, 10.0, 11.0, false);

        test(-100.0, 10.0, 11.0, false);
        test(-101.0, 10.0, -100.0, true);
        test(-101.0, 10.0, -101.0, true);
        test(-101.0, 10.0, -101.1, false);

        test(0.0, 0.0, 0.0, true);
        test(0.0, 0.0, 1.0, false);
        test(0.0, 0.0, -1.0, false);
    }

    #[test]
    fn test_bounds_in_bounds() {
        let test = |start1, end1, start2, end2, expected| {
            let result = bounds_in_bounds(Bounds::new(start1, start2), Bounds::new(start2, end2));
            assert_eq!(
                result, expected,
                "Testing whether ]{},{}[ in ]{},{}[",
                start1, end1, start2, end2
            );
        };

        test(0.0, 1.0, 0.0, 1.0, true);
        test(0.0, 1.0, 1.0, 2.0, false);
        test(0.0, 1.0, 0.5, 2.0, false);
        test(0.0, 1.0, -100.0, 100.0, true);
        test(0.0, 1.0, -100.0, -99.0, false);
        test(0.0, 1.0, 0.1, 0.9, false);
        test(-100.0, 200.0, 50.0, 75.0, false);
        test(-100.0, 200.0, -50.0, 75.0, false);
        test(-100.0, 200.0, -50.0, -75.0, false);
        test(-100.0, 200.0, -50.0, 99999.0, false);
        test(-100.0, 200.0, -99999.0, 0.0, true);
        test(-100.0, 200.0, -99999.0, 99999.0, true);

        test(0.0, 0.0, 0.0, 0.0, true);
        test(0.0, 0.0, -1.0, 2.0, true);
        test(0.0, 0.0, 1.0, 2.0, false);
    }

    #[test]
    fn test_clamp_with_overflow() {
        let test = |value, bounds, expected| {
            let result = clamp_with_overflow(&value, &bounds);
            assert_float_eq!(result, expected, ulps <= 7)
        };

        test(0.0, Some(Bounds::new(0.0, 1.0)), 0.0);
        test(-1.0, Some(Bounds::new(0.0, 1.0)), 0.0);
        test(2.0, Some(Bounds::new(0.0, 1.0)), 1.0);

        test(-1.0, None, 0.0);
        test(2.0, None, 1.0);

        test(-999.0, Some(Bounds::new(-1.0, 100.0)), -1.0);
        test(999.0, Some(Bounds::new(-1.0, 100.0)), 100.0);
        test(-1.0, Some(Bounds::new(-1.0, 100.0)), -1.0);
        test(0.0, Some(Bounds::new(-1.0, 100.0)), 0.0);
        test(99.0, Some(Bounds::new(-1.0, 100.0)), 99.0);
        test(100.0, Some(Bounds::new(-1.0, 100.0)), 100.0);
        test(100.01, Some(Bounds::new(-1.0, 100.0)), 100.0);
    }

    #[test]
    fn test_should_clamp_with_overflow() {
        let test = |inner, outer, expected| {
            let result = should_clamp_with_overflow(&inner, &outer);
            assert_eq!(result, expected);
        };

        test(Bounds::new(0.0, 1.0), Some(Bounds::new(0.0, 1.0)), true);
        test(Bounds::new(0.0, 1.0), Some(Bounds::new(1.0, 2.0)), false);
        test(Bounds::new(0.0, 1.0), Some(Bounds::new(0.5, 2.0)), false);
        test(Bounds::new(0.0, 1.0), Some(Bounds::new(-100.0, 100.0)), true);
        test(Bounds::new(0.0, 1.0), Some(Bounds::new(-100.0, -99.0)), false);
        test(Bounds::new(0.0, 1.0), Some(Bounds::new(0.1, 0.9)), false);
        test(Bounds::new(-100.0, 200.0), Some(Bounds::new(50.0, 75.0)), false);
        test(Bounds::new(-100.0, 200.0), Some(Bounds::new(-50.0, 75.0)), false);
        test(Bounds::new(-100.0, 200.0), Some(Bounds::new(-50.0, -75.0)), false);
        test(Bounds::new(-100.0, 200.0), Some(Bounds::new(-50.0, 99999.0)), false);
        test(Bounds::new(-100.0, 200.0), Some(Bounds::new(-99999.0, 0.0)), false);
        test(Bounds::new(-100.0, 200.0), Some(Bounds::new(-99999.0, 99999.0)), true);
        test(Bounds::new(-100.0, 0.0), None, false);
        test(Bounds::new(0.1, 1.1), None, false);
        test(Bounds::new(-9.1, 2.1), None, false);
        test(Bounds::new(0.25, 0.7), None, true);

        test(Bounds::new(0.0, 0.0), None, true);
    }
}
