//! UI components that allows picking a number or range through mouse interaction.

#![recursion_limit = "1024"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use crate::model::*;
use ensogl_core::prelude::*;

use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::Animation;
use ensogl_text::formatting;


// ==============
// === Export ===
// ==============

pub mod model;



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
        set_precision_step_margin(f32),
        set_precision_step_size(f32),

        set_tooltip(Option<ImString>),

        set_label(Option<ImString>),
        set_label_color(color::Lcha),
        set_label_visible(bool),
        set_label_inside(bool),

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
            drag_delta              <- all2(&drag_pos_end, &drag_pos_start).map(|(end, start)| end - start);

            mouse_y_local           <- mouse.position.map(
                f!([scene, model] (pos) scene.screen_to_object_space(&model.background, *pos).y )
            ).gate(&component_drag);

            // Componenet size

            output.width            <+ input.set_width;
            output.height           <+ input.set_height;


            // precision calculation
            precision_adjust_margin <- all2(&input.set_height, &input.set_precision_step_margin).map(
                |(height, margin)| height / 2.0 + margin
            );
            precision_adjusted      <- all4(
                &input.set_precision,
                &mouse_y_local,
                &precision_adjust_margin,
                &input.set_precision_step_size,
            ).map(
                |(base, offset, margin, step_size)| {
                    let steps = ((offset.abs() - margin) / step_size).max(0.0).ceil() * offset.signum();
                    *base * (10.0).pow(steps)
                }
            );


            // value calculation

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
            eval_ value_is_default_true ({
                model.value_left.set_property_default(formatting::Weight::Normal);
                model.value_dot.set_property_default(formatting::Weight::Normal);
                model.value_right.set_property_default(formatting::Weight::Normal);
            });
            eval_ value_is_default_false ({
                model.value_left.set_property_default(formatting::Weight::Bold);
                model.value_dot.set_property_default(formatting::Weight::Bold);
                model.value_right.set_property_default(formatting::Weight::Bold);
            });


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
                (color) {
                    model.value_left.set_property_default(color);
                    model.value_dot.set_property_default(color);
                    model.value_right.set_property_default(color);
                }
            );
            eval label_color (
                (color) model.label.set_property_default(color);
            );


            // text alignment

            value_text <- all2(&value, &precision_adjusted).map(
                |(value, precision)| {
                    value_text_truncate_precision(*value, *precision)
                }
            );
            model.value_left.set_content <+ value_text.map(|t| t.0.clone() );
            model.value_right.set_content <+ value_text.map(|t| t.1.clone() );

            value_text_left_pos_x <- all2(&model.value_left.width, &model.value_dot.width).map(
                |(left, dot)| -*left - *dot / 2.0
            );
            eval value_text_left_pos_x (
                (x) model.value_left.set_position_x(*x);
            );
            eval model.value_left.height (
                (h) model.value_left.set_position_y(*h / 2.0);
            );
            eval model.value_dot.width (
                (w) model.value_dot.set_position_x(-*w / 2.0);
            );
            eval model.value_dot.height (
                (h) model.value_dot.set_position_y(*h / 2.0);
            );
            eval model.value_dot.width (
                (w) model.value_right.set_position_x(*w / 2.0);
            );
            eval model.value_right.height (
                (h) model.value_right.set_position_y(*h / 2.0);
            );

            label_text <- all2(&input.set_label, &input.set_label_visible).map(
                |(label, visible)| match (label, visible) {
                    (Some(label), true) => label.clone(),
                    (None, _) | (_, false) => ImString::default(),
                }
            );
            model.label.set_content <+ label_text;

            eval model.label.height (
                (h) model.label.set_position_y(*h / 2.0);
            );
            label_pos_x <- all4(
                &input.set_width,
                &input.set_height,
                &model.label.width,
                &input.set_label_inside
            ).map(
                |(comp_width, comp_height, lab_width, inside)| if *inside {
                    -comp_width / 2.0 + comp_height / 2.0
                } else {
                    -comp_width / 2.0 - comp_height / 2.0 - lab_width
                }
            );
            eval label_pos_x (
                (x) model.label.set_position_x(*x);
            );

        }

        self.frp.set_precision(0.1);
        self.frp.set_precision_step_margin(10.0);
        self.frp.set_precision_step_size(50.0);

        self.frp.set_value_min(0.0);
        self.frp.set_value_max(5.0);
        self.frp.set_value_default(0.5);
        self.frp.set_value(0.5);

        self.frp.set_background_color(color::Lcha(0.8, 0.0, 0.0, 1.0));
        self.frp.set_slider_color(color::Lcha(0.5, 0.5, 0.0, 1.0));
        self.frp.set_slider_enabled(true);

        self.frp.set_label_visible(true);
        self
    }
}



// ========================
// === Helper functions ===
// ========================

fn value_text_truncate_precision(value: f32, precision: f32) -> (ImString, ImString) {
    if precision < 1.0 {
        let digits = (-precision.log10()).ceil() as usize;

        let text = format!("{:.prec$}", value, prec = digits);
        let mut text_iter = text.split('.');
        let text_left = text_iter.next().map(|s| s.to_im_string()).unwrap_or(ImString::default());
        let text_right = text_iter.next().map(|s| s.to_im_string()).unwrap_or(ImString::default());

        (text_left, text_right)
    } else {
        let text_left = format!("{:.0}", value.trunc()).to_im_string();
        (text_left, ImString::default())
    }
}
