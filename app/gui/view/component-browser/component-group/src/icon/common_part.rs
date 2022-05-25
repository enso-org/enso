use crate::prelude::*;

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
    let middle = Rect((1.0.px(), 15.0.px()));
    let top = Rect((5.0.px(), 1.0.px())).translate_y(7.5.px());
    let bottom = Rect((5.0.px(), 1.0.px())).translate_y((-7.5).px());
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

/// The shape of a table, given by a grid with size `columns` x `rows`. The stroke width is 1.0 and
/// the cell size 4.0. The origin is at the lower left corner.
pub fn table(columns: i32, rows: i32) -> AnyShape {
    const STROKE_WIDTH: f32 = 1.0;
    const CELL_SIZE: f32 = 4.0;

    let width = columns as f32 * CELL_SIZE + STROKE_WIDTH;
    let height = rows as f32 * CELL_SIZE + STROKE_WIDTH;
    let bounds =
        Rect((width.px(), height.px())).translate(((width / 2.0).px(), (height / 2.0).px()));
    let grid = grid(STROKE_WIDTH, CELL_SIZE);
    (grid * bounds).into()
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
