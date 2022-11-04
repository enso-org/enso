//! UI components that allows picking a number or range through mouse interaction.

#![recursion_limit = "1024"]

pub mod model;

use crate::model::*;
use ensogl_core::prelude::*;

use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;



// ========================
// === Slider component ===
// ========================

pub struct Slider {
    pub frp: Rc<Frp>,
    model:   Rc<Model>,
    pub app: Application,
}

impl Slider {
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(&app));
        let app = app.clone_ref();
        let frp = Rc::new(Frp::new());

        Self { frp, model, app }.init()
    }
}

impl display::Object for Slider {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}

impl Deref for Slider {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl FrpNetworkProvider for Slider {
    fn network(&self) -> &enso_frp::Network {
        self.frp.network()
    }
}

impl application::View for Slider {
    fn label() -> &'static str {
        "Slider"
    }

    fn new(app: &Application) -> Self {
        Self::new(app)
    }

    fn app(&self) -> &Application {
        &self.app
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_width(f32),
        set_height(f32),
        set_color(color::Rgba),

        set_default(f32),
        set_value(f32),
        set_min(f32),
        set_max(f32),
        set_precision(f32),

        set_tooltip(Option<String>),
        set_label(Option<String>),

        resize(Vector2), // remove in favor of set_width/height?
    }
    Output {
        width(f32),
        height(f32),
        value(f32),
    }
}

impl Slider {
    fn init(self) -> Self {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;

        let scene = &self.app.display.default_scene;
        let mouse = &scene.mouse.frp;

        frp::extend! { network

            // User input

            background_click        <- model.background.events.mouse_down.constant(());
            background_release      <- model.background.events.mouse_release.constant(());
            background_drag         <- bool(&background_release, &background_click);

            track_click             <- model.track.events.mouse_down.constant(());
            track_release           <- model.track.events.mouse_release.constant(());
            track_drag              <- bool(&track_release, &track_click);

            component_click         <- any2(&background_click, &track_click);
            component_drag          <- any2(&background_drag, &track_drag);
            component_release       <- any2(&background_release, &track_release);

            drag_pos_start          <- mouse.position.sample(&component_click);
            drag_pos_end            <- mouse.position.gate(&component_drag);
            drag_pos_end            <- any2(&drag_pos_end, &drag_pos_start);


            // FIXME: also update on set_width, set_height
            component_width         <- input.resize.map(|v| v.x );
            component_height        <- input.resize.map(|v| v.y );

            // distance dragged
            drag_delta              <- all2(&drag_pos_end, &drag_pos_start).map(|(end, start)| end - start);
            // horizontal drag as fraction of element width
            drag_x_fract            <- all2(&drag_delta, &component_width).map(
                |(delta, width)| delta.x / width
            );
            // vertical drag as fraction of element height
            drag_y_fract            <- all2(&drag_delta, &component_height).map(
                |(delta, height)| delta.y / height
            );


            // value calculation

            precision               <- input.set_precision.sample(&component_click);
            precision_adjusted      <- all2(&precision, &drag_y_fract).map(
                |(base, offset)| *base * (offset/5.0).exp()
            );
            range                   <- all2(&input.set_min, &input.set_max).map(
                |(min, max)| max - min
            );

            value_start             <- output.value.sample(&component_click);
            value                   <- all4(&value_start, &range, &precision_adjusted, &drag_x_fract).map(
                |(value, range, precision, delta)| value + delta * precision * range
            );
            value_clamped           <- all3(&value, &input.set_min, &input.set_max).map(
                |(value, min, max)| value.max(*min).min(*max)
            );
            output.value            <+ any2(&input.set_value, &value_clamped);

            track_pos               <- all3(&value_clamped, &input.set_min, &input.set_max).map(
                |(value, min, max)| (value - min) / (max - min)
            );


            // model update

            eval value_clamped (
                (v) {
                    model.update_value(*v);
                }
            );
            eval track_pos (
                (v) {
                    model.update_track(*v);
                }
            );
            eval_ component_click (
                model.set_active();
            );
            eval_ component_release (
                model.set_inactive();
            );
            eval input.resize (
                (v) {
                    model.set_size(*v);
                }
            );
        }

        self.frp.set_min(0.0);
        self.frp.set_max(5.0);
        self.frp.set_value(0.5);
        self.frp.set_precision(1.0);

        self
    }
}
