//! This module provides the [`path`] function for the construction of path shapes.

use crate::display::shape::*;
use crate::prelude::*;



/// A path following the given points. All joints and endpoints are rounded.
pub fn path(width: f32, points: &[(f32, f32)]) -> AnyShape {
    let points = points.iter().map(|(x, y)| (x.px(), y.px()));
    let mut result: AnyShape = EmptyShape().into();
    for (start, end) in points.clone().zip(points.skip(1)) {
        result = (result + Segment(start, end, width.px())).into();
    }
    result
}
