use crate::prelude::*;

use ensogl_core::display::shape::AnyShape;
use std::f32::consts::PI;



// =====================
// === Common Shapes ===
// =====================

/// An arrow shape consisting of a straight line and a triangular head. The arrow points upwards and
/// the tip is positioned at the origin.
pub fn arrow(length: f32, width: f32, head_length: f32, head_width: f32) -> AnyShape {
    // We overlap the line with the head by this amount to make sure that the renderer does not
    // display a gap between them.
    const OVERLAP: f32 = 1.0;
    let line_length = length - head_length + OVERLAP;
    let line = Rect((width.px(), line_length.px()));
    let line = line.translate_y((-line_length / 2.0 - head_length + OVERLAP).px());
    let head = Triangle(head_width, head_length).translate_y((-head_length / 2.0).px());
    (line + head).into()
}

/// An infinite grid of horizontal and vertical lines. The width of each line is given by
/// `stroke_width`, the distance between horizontal lines and the distance between vertical lines
/// are both given by `cell_size`. The origin is at the intersection of a horizontal and a vertical
/// line, touching the horizontal line from the top and the vertical line from the left.
pub fn grid(stroke_width: f32, cell_size: f32) -> AnyShape {
    let horizontal = HalfPlane();
    let horizontal = horizontal.translate_y(stroke_width.px()) - horizontal;
    let vertical = HalfPlane().rotate((PI / 2.0).radians());
    let vertical = vertical.translate_x(stroke_width.px()) - vertical;
    (horizontal + vertical).repeat((cell_size.px(), cell_size.px())).into()
}

/// A cursor shape, looking roughly like a capital "I".
pub fn cursor() -> AnyShape {
    let middle = Rect((2.0.px(), 16.0.px()));
    let top = Rect((6.0.px(), 2.0.px())).translate_y(7.0.px());
    let bottom = Rect((6.0.px(), 2.0.px())).translate_y((-7.0).px());
    (middle + top + bottom).into()
}

/// An arc around the origin. `outer_radius` determines the distance from the origin to the outer
/// edge of the arc, `stroke_width` the width of the arc. The arc starts at `start_angle`, relative
/// to the origin and ends at `end_angle`. The ends are flat not rounded, as in [`RoundedArc`].
pub fn arc(outer_radius: f32, stroke_width: f32, start_angle: f32, end_angle: f32) -> AnyShape {
    let circle = Circle(outer_radius.px()) - Circle((outer_radius - stroke_width).px());
    let inner_angle = (end_angle - start_angle).rem_euclid(2.0 * PI);
    let mid_angle = start_angle + inner_angle / 2.0;
    let angle = PlaneAngleFast(inner_angle.radians()).rotate(mid_angle.radians());
    // The implementation of `PlaneAngleFast` adds 0.5 px to the actual distance to avoid artifacts
    // in corner cases. We apply `grow` to compensate for that and get the shape that we really
    // want.
    let angle = angle.grow(0.5.px());
    (circle * angle).into()
}

/// An 2d-array of squares of size `cell_size`.
pub fn table(columns: i32, rows: i32, cell_size: f32) -> AnyShape {
    const GAP: f32 = 1.0;

    let cell = Rect((cell_size.px(), cell_size.px()));
    let table = cell.repeat(((cell_size + GAP).px(), (cell_size + GAP).px()));
    let table = table.translate_x((-cell_size / 2.0 - GAP).px());
    let table = table.translate_y((-cell_size / 2.0 - GAP).px());

    let width = (cell_size + GAP) * columns as f32 - GAP;
    let height = (cell_size + GAP) * rows as f32 - GAP;

    let bounds = Rect((width.px(), height.px()));
    let bounds = bounds.translate_x((width / 2.0).px());
    let bounds = bounds.translate_y((height / 2.0).px());

    let shape = table * bounds;
    shape.into()
}

/// A plus, consisting of two strokes of length `size` and width `stroke_width`, intersecting at the
/// origin.
pub fn plus(size: f32, stroke_width: f32) -> AnyShape {
    let horizontal = Rect((size.px(), stroke_width.px()));
    let vertical = Rect((stroke_width.px(), size.px()));
    (horizontal + vertical).into()
}

/// A shape resembling a lightning bolt, centered at the origin.
pub fn lightning_bolt() -> AnyShape {
    let top = Triangle(3.0.px(), 6.0.px()).translate(((-1.0).px(), 1.9.px()));
    let bottom = Triangle(3.0.px(), 6.0.px()).rotate(PI.radians());
    let bottom = bottom.translate((1.0.px(), (-1.9).px()));
    let lightning = (top + bottom).rotate((PI / 6.0).radians());
    lightning.into()
}
