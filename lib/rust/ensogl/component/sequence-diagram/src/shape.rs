use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::shape::*;



// =================
// === Constants ===
// =================

pub const CAP_WIDTH: f32 = 8.0;
pub const LINE_WIDTH: f32 = 2.0;
pub const TOUCH_PADDING: f32 = 5.0;


// =============
// === Shape ===
// =============

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

            let touch_area: Var<Pixels> = (&zoom * TOUCH_PADDING).into();
            let shape_expanded = shape.grow(touch_area);

            let shape = shape.fill(color::Rgba::black());
            let shape_expanded = shape_expanded.fill(HOVER_COLOR);

            (shape + shape_expanded).into()
        }
    }
}
