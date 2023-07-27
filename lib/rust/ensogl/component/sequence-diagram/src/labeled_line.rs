//! A visual line capped with an arrow, that shows a tooltip on mouse hover.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::shape;
use crate::shape::CAP_WIDTH;
use crate::shape::HOVER_PADDING;

use ensogl::frp;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::data::color::Lcha;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_gui_component::component;



// ===========
// === Cap ===
// ===========

/// Indicated the position of the lines cap.
#[derive(Clone, Copy, Debug)]
pub enum Cap {
    /// Place the cap at the start of the line.
    Start,
    /// Place the cap at the end of the line.
    End,
}

impl Default for Cap {
    fn default() -> Self {
        Cap::Start
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_content(String),
        set_size(Vector2),
        set_color(Lcha),
        set_cap(Cap),
    }
    Output {}
}

impl component::Frp<Model> for Frp {
    fn init(
        network: &frp::Network,
        api: &Self::Private,
        app: &Application,
        model: &Model,
        _style: &StyleWatchFrp,
    ) {
        let line = &model.line.events_deprecated;
        frp::extend! { network
            eval api.input.set_size((size) model.set_size(*size));
            eval api.input.set_color((color) model.set_color(*color));
            eval api.input.set_cap((direction) model.set_cap(*direction));
            tooltip_update <- api.input.set_content.sample(&line.mouse_over).map(|content| tooltip::Style::set_label(content.clone()));
            app.frp.set_tooltip <+ tooltip_update;
            app.frp.set_tooltip <+ line.mouse_out.constant(tooltip::Style::unset_label());
        }
    }
}



// =============
// === Model ===
// =============

/// Internal model of the LabeledLine.
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct Model {
    line: shape::arrow::View,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "LabeledLine"
    }

    fn new(_app: &Application) -> Self {
        let line = shape::arrow::View::new();
        Model { line }
    }
}

impl Model {
    fn set_size(&self, size: Vector2) {
        self.line.set_size(size + Vector2::new(CAP_WIDTH + HOVER_PADDING, 0.0));
    }

    fn set_color(&self, color: Lcha) {
        self.line.color_rgba.set(color::Rgba::from(color).into());
    }

    fn set_cap(&self, direction: Cap) {
        let rotation = match direction {
            Cap::Start => 0_f32,
            Cap::End => 180.0,
        }
        .to_radians();
        self.line.set_rotation_z(rotation);
    }
}

/// A line that shows a tooltip on mouse hover.
pub type LabeledLine = component::ComponentView<Model, Frp>;
