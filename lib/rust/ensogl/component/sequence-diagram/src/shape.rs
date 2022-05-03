//! Shape used for the LabeledLine.
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::shape::*;



// =================
// === Constants ===
// =================

/// Width of the arrow cap.
pub const CAP_WIDTH: f32 = 8.0;
/// Width of the line used for tha arrow shaft.
pub const LINE_WIDTH: f32 = 2.0;
/// Size of the extra invisible space for hover interactions.
pub const HOVER_PADDING: f32 = 5.0;



// =============
// === Shape ===
// =============

/// Arrow shape that consists of a line and a triangle as arrow head.
pub mod arrow {
    use super::*;
    ensogl_core::define_shape_system! {
        (style:Style,color_rgba:Vector4<f32>) {
            let height : Var<Pixels> = "input_size.y".into();
            let zoom                 = Var::<f32>::from("1.0/zoom()");

            let width: Var<Pixels> = (&zoom * LINE_WIDTH).into();
            let line = Rect((&width,&height));

            let cap = Triangle(CAP_WIDTH.px(), (CAP_WIDTH * 2.0).px()).translate_y(height / 2.0 - (CAP_WIDTH).px());
            let shape = line + cap;

            let touch_area: Var<Pixels> = (&zoom * HOVER_PADDING).into();
            let shape_expanded = shape.grow(touch_area);

            let shape = shape.fill(color::Rgba::black());
            let shape_expanded = shape_expanded.fill(HOVER_COLOR);

            (shape + shape_expanded).into()
        }
    }
}
