use crate::prelude::*;
use ensogl::data::color;
use ensogl::display;

mod shape {
    ensogl::shape! {
        above = [
            crate::component::node::background,
            crate::component::node::drag_area
        ];
        pointer_events = false;
        (style: Style, color: Vector4) {
            let color = Var::<color::Rgba>::from(color);
            let shape = Rect(Var::canvas_size()).fill(color);
            let inner = shape.shrink(2.0.px());
            let border = shape - inner;
            border.into()
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstanceWithBg {
    pub bg:    shape::View,
    pub outer: display::object::Instance,
    pub inner: display::object::Instance,
}

impl InstanceWithBg {
    pub fn magenta() -> Self {
        Self::with_color(color::Rgba::new(0.5, 0.0, 0.5, 0.15))
    }

    pub fn olive() -> Self {
        Self::with_color(color::Rgba::new(0.5, 0.8, 0.0, 0.2))
    }

    pub fn gray() -> Self {
        Self::with_color(color::Rgba::new(0.5, 0.5, 0.5, 0.2))
    }

    pub fn with_color(color: color::Rgba) -> Self {
        let bg = shape::View::new();
        let outer = bg.display_object().clone();
        // let outer = display::object::Instance::new();
        let inner = display::object::Instance::new();
        // bg.allow_grow();
        bg.color.set(color.into());
        // outer.add_child(&bg);
        outer.add_child(&inner);
        Self { bg, outer, inner }
    }
}
