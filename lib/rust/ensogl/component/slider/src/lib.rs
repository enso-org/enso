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
use ensogl_text::formatting;



// ========================
// === Slider component ===
// ========================

pub struct Slider {
    pub frp: Frp,
    model:   Rc<Model>,
    pub app: Application,
}

impl Slider {
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(&app));
        let app = app.clone_ref();
        let frp = Frp::new();

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
        set_slider_color(color::Lcha),
        set_background_color(color::Lcha),

        set_value(f32),
        set_value_default(f32),
        set_value_min(f32),
        set_value_max(f32),
        set_value_color(color::Lcha),

        set_precision(f32),

        set_tooltip(Option<ImString>),
        set_label(Option<ImString>),
        set_label_color(color::Lcha),

        set_slider_enabled(bool),
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
        let keyboard = &scene.keyboard.frp;

        let track_pos = Animation::new_non_init(&network);

        frp::extend! { network

            // User input

            background_click        <- model.background.events.mouse_down_primary.constant(());
            background_release      <- model.background.events.mouse_release_primary.constant(());
            background_drag         <- bool(&background_release, &background_click);

            track_click             <- model.track.events.mouse_down_primary.constant(());
            track_release           <- model.track.events.mouse_release_primary.constant(());
            track_drag              <- bool(&track_release, &track_click);

            component_click         <- any2(&background_click, &track_click).gate(&input.set_slider_enabled);
            component_drag          <- any2(&background_drag, &track_drag).gate(&input.set_slider_enabled);
            component_release       <- any2(&background_release, &track_release);

            component_ctrl_click    <- component_click.gate(&keyboard.is_control_down);

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

            value_reset             <- input.set_value_default.sample(&component_ctrl_click);
            value_start             <- output.value.sample(&component_click);
            value_start             <- any2(&value_reset, &value_start);

            value_update            <- bool(&component_release, &value_start); // update only after value_start is sampled
            value                   <- all3(&value_start, &precision_adjusted, &drag_delta).map(
                |(value, precision, delta)| value + delta.x * precision
            ).gate(&value_update);
            value                   <- any2(&input.set_value, &value);
            value                   <- all2(&value, &precision_adjusted).map(
                |(value, precision)| (value / precision).round() * precision
            );
            value                   <- all3(&value, &input.set_value_min, &input.set_value_max).map(
                |(value, min, max)| value.clamp(*min, *max)
            );
            output.value            <+ value;

            track_pos.target        <+ all3(&value, &input.set_value_min, &input.set_value_max).map(
                |(value, min, max)| (value - min) / (max - min)
            );


            // model update

            eval track_pos.value (
                (v) model.track.value.set(*v);
            );
            eval input.set_width (
                (v) model.set_width(*v);
            );
            eval input.set_height (
                (v) model.set_height(*v);
            );

            value_is_default        <- all2(&value, &input.set_value_default).map(
                |(value, default)| value==default
            );
            value_is_default_true   <- value_is_default.on_true();
            value_is_default_false  <- value_is_default.on_false();
            eval_ value_is_default_true (
                model.value.set_property_default(formatting::Weight::Normal);
            );
            eval_ value_is_default_false (
                model.value.set_property_default(formatting::Weight::Bold);
            );


            // colors
            background_color        <- all2(&input.set_background_color, &input.set_slider_enabled).map(
                |(color, enabled)| if *enabled { *color } else { color.to_grayscale() }
            );
            slider_color        <- all2(&input.set_slider_color, &input.set_slider_enabled).map(
                |(color, enabled)| if *enabled { *color } else { color.to_grayscale() }
            );
            value_color        <- all2(&input.set_value_color, &input.set_slider_enabled).map(
                |(color, enabled)| if *enabled { *color } else { color.to_grayscale() }
            );
            label_color        <- all2(&input.set_label_color, &input.set_slider_enabled).map(
                |(color, enabled)| if *enabled { *color } else { color.to_grayscale() }
            );

            eval background_color (
                (color) model.set_background_color(*color);
            );
            eval slider_color (
                (color) model.set_track_color(*color);
            );
            eval value_color (
                (color) model.value.set_property_default(color);
            );
            eval label_color (
                (color) model.label.set_property_default(color);
            );


            // text alignment

            model.value.set_content <+ all2(&value, &precision_adjusted).map(
                |(value, precision)| {
                    let text_left_right = value_truncate_precision(*value, *precision);

                    match text_left_right {
                        (Some(left), Some(right)) => format!("{}.{}", left, right).to_im_string(),
                        (Some(left), None) => left,
                        _ => unreachable!(), // FIXME How to handle this case? Format will always have a left part
                    }
                }
            );

            eval model.value.width (
                (w) model.value.set_position_x(-*w / 2.0);
            );
            eval model.value.height (
                (h) model.value.set_position_y(*h / 2.0);
            );


        }

        self.frp.set_precision(0.1);

        self.frp.set_value_min(0.0);
        self.frp.set_value_max(5.0);
        self.frp.set_value_default(0.5);
        self.frp.set_value(0.5);

        self.frp.set_background_color(color::Lcha(0.8, 0.0, 0.0, 1.0));
        self.frp.set_slider_color(color::Lcha(0.5, 0.5, 0.0, 1.0));
        self.frp.set_slider_enabled(true);

        self
    }
}



// ========================
// === Helper functions ===
// ========================

fn value_truncate_precision(value: f32, precision: f32) -> (Option<ImString>, Option<ImString>) {
    if precision < 1.0 {
        let digits = (-precision.log10()).ceil() as usize;

        let text = format!("{:.prec$}", value, prec = digits);
        let mut text_iter = text.split('.');
        let text_left = text_iter.next().map(|s| s.into());
        let text_right = text_iter.next().map(|s| s.into());

        (text_left, text_right)
    } else {
        let text_left = format!("{:.0}", value.trunc()).into();
        (Some(text_left), None)
    }
}
