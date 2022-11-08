//! UI components that allows picking a number or range through mouse interaction.

#![recursion_limit = "1024"]

pub mod model;

use crate::model::*;
use ensogl_core::prelude::*;

use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::Animation;



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

        let track_pos = Animation::new_non_init(&network);

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


            // Componenet size

            output.width            <+ input.set_width;
            output.height           <+ input.set_height;

            // distance dragged
            drag_delta              <- all2(&drag_pos_end, &drag_pos_start).map(|(end, start)| end - start);
            // vertical drag as fraction of element height
            drag_y_fract            <- all2(&drag_delta, &output.height).map(
                |(delta, height)| delta.y / height
            );


            // value calculation

            precision_adjusted      <- all2(&input.set_precision, &drag_y_fract).map(
                |(base, offset)| *base * (10.0).pow(offset.round())
            );

            value_start             <- output.value.sample(&component_click);
            value_update            <- bool(&component_release, &value_start); // update only after value_start is sampled
            value                   <- all3(&value_start, &precision_adjusted, &drag_delta).map(
                |(value, precision, delta)| value + delta.x * precision
            ).gate(&value_update);
            value                   <- any2(&input.set_value, &value);
            value                   <- all3(&value, &input.set_min, &input.set_max).map(
                |(value, min, max)| value.clamp(*min, *max)
            );
            value                   <- all2(&value, &precision_adjusted).map(
                |(value, precision)| (value / precision).round() * precision
            );
            output.value            <+ value;

            track_pos.target        <+ all3(&value, &input.set_min, &input.set_max).map(
                |(value, min, max)| (value - min) / (max - min)
            );


            // model update

            eval track_pos.value (
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
            eval input.set_width (
                (v) {
                    model.set_width(*v);
                }
            );
            eval input.set_height (
                (v) {
                    model.set_height(*v);
                }
            );


            // text alignment

            model.value.set_content <+ all2(&value, &precision_adjusted).map(
                |(value, precision)| {
                    let (left, right) = value_truncate_precision(*value, *precision);

                    if let Some(right) = right {
                        format!("{}.{}", left, right).to_im_string()
                    } else {
                        left.to_im_string()
                    }
                }
            );

            eval model.value.width (
                (w) {
                    model.text_align_width(*w);
                }
            );
            eval model.value.height (
                (h) {
                    model.text_align_height(*h);
                }
            );


        }

        self.frp.set_min(0.0);
        self.frp.set_max(5.0);
        self.frp.set_value(0.5);
        self.frp.set_precision(0.1);

        self
    }
}



// ========================
// === Helper functions ===
// ========================

fn value_truncate_precision(value: f32, precision: f32) -> (String, Option<String>) {
    if precision < 1.0 {
        let digits = (-precision.log10()).ceil() as usize;

        // round before string conversion, as string is only truncated and not rounded
        let scale = 10f32.powi(digits as i32);
        let value = (value * scale).round() / scale;

        let text_left = format!("{:.0}", value.trunc());
        let text_right = format!("{:.6}", value.fract().abs());
        let text_right = text_right.chars().skip(2).take(digits).collect();
        (text_left, Some(text_right))
    } else {
        let text_left = format!("{:.0}", value.trunc());
        (text_left, None)
    }
}
