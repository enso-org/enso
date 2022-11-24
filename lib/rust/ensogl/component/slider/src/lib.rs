//! A slider UI component that allows adjusting a value through mouse interaction.

#![recursion_limit = "256"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

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



// =================
// === Constants ===
// =================

/// Default slider precision when slider dragging is initiated. The precision indicates both how
/// much the value is changed per pixel dragged and how many digits are displayed after the decimal.
const PRECISION_DEFAULT: f32 = 0.1;
/// Default upper limit of the slider value.
const MAX_VALUE_DEFAULT: f32 = 1.0;
/// Default for the maximum number of digits after the decimal point that is displayed.
const MAX_DISP_DECIMAL_PLACES_DEFAULT: usize = 8;
/// Margin above/below the component within which vertical mouse movement will not affect slider
/// precision.
const PRECISION_ADJUSTMENT_MARGIN: f32 = 10.0;
/// The vertical mouse movement (in pixels) needed to change the slider precision by one step.
/// Dragging the mouse upward beyond the margin will decrease the precision by one step for every
/// `STEP_SIZE` pixels and adjust the slider value more quickly. Dragging the mouse downwards will
/// increase the precision and change the value more slowly.
const PRECISION_ADJUSTMENT_STEP_SIZE: f32 = 50.0;
/// The actual slider precision changes exponentially with each adjustment step. When the adjustment
/// is changed by one step, the slider's precision is changed to the next power of `STEP_BASE`. A
/// `STEP_BASE` of 10.0 results in the precision being powers of 10 for consecutive steps, e.g [1.0,
/// 10.0, 100.0, ...] when decreasing the precision and [0.1, 0.01, 0.001, ...] when increasing the
/// precision.
const PRECISION_ADJUSTMENT_STEP_BASE: f32 = 10.0;



// ===========================
// === Label position enum ===
// ===========================

/// Position of the slider label.
#[derive(Clone, Copy, Debug, Default)]
pub enum LabelPosition {
    #[default]
    /// Place the label outside the slider component, on its left side.
    Outside,
    /// Place the label inside the slider component, on the left side.
    Inside,
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        /// Set the width of the slider component.
        set_width(f32),
        /// Set the height of the slider component.
        set_height(f32),
        /// Set the color of the slider's track.
        set_slider_track_color(color::Lcha),
        /// Set the color of the slider's background.
        set_background_color(color::Lcha),
        /// Set the slider value.
        set_value(f32),
        /// Set the default value to reset a slider to when `ctrl` + `click`-ed.
        set_default_value(f32),
        /// Set the value's lower limit. The value cannot be dragged lower than this limit. At the
        /// lower limit the slider's track will be empty.
        set_min_value(f32),
        /// Set the value's upper limit. The value cannot be dragged higher than this limit. At the
        ///  upper limit the slider's track will be full.
        set_max_value(f32),
        /// Set the color of the text displaying the current value.
        set_value_text_color(color::Lcha),
        /// Set the default precision at which the slider operates. The slider's precision
        /// determines by what increment the value will be changed on mouse movement. It also
        /// affects the number of digits after the decimal point displayed.
        set_default_precision(f32),
        /// The slider's precision can be adjusted by dragging the mouse in the vertical direction.
        /// The `adjustment_margin` defines a margin above/below the slider within which no
        /// precision adjustment will be performed.
        set_precision_adjustment_margin(f32),
        /// The slider's precision can be adjusted by dragging the mouse in the vertical direction.
        /// The `adjustment_step_size` defines the distance the mouse must be moved to increase or
        /// decrease the precision by one step.
        set_precision_adjustment_step_size(f32),
        /// Set the slider's label. The label will be displayed to the left of the slider's value
        /// display.
        set_label(ImString),
        /// Set the color of the slider's label.
        set_label_color(color::Lcha),
        /// Set whether the slider's label is displayed.
        set_label_hidden(bool),
        /// Set the position of the slider's label.
        set_label_position(LabelPosition),
        /// Set whether the slider is disabled. When disabled, the slider's value cannot be changed
        /// and the slider is greyed out.
        set_slider_disabled(bool),
        /// The maximum number of digits after the decimal point to be displayed when showing the
        /// component's value.
        set_max_disp_decimal_places(usize),
    }
    Output {
        width(f32),
        height(f32),
        value(f32),
        precision(f32),
    }
}



// ========================
// === Slider component ===
// ========================

/// A slider UI component that allows adjusting a value through mouse interaction. Dragging the
/// slider in a horizontal direction changes the value, limited to a range between `min_value` and
/// `max_value`. The selected value is displayed, and a track fills the slider proportional to the
/// value within the specified range. Dragging the slider in a vertical direction adjusts the
/// precision of the slider. The precision affects the increments by which the value changes when
/// the mouse is moved.
#[derive(Debug, Deref)]
pub struct Slider {
    /// Public FRP api of the component.
    #[deref]
    pub frp: Frp,
    model:   Rc<Model>,
    /// Reference to the application the component belongs to. Generally required for implementing
    /// `application::View` and initialising the `Model` and `Frp` and thus provided by the
    /// component.
    pub app: Application,
}

impl Slider {
    /// Construct a new slider component.
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app));
        let app = app.clone_ref();
        let frp = Frp::new();
        Self { frp, model, app }.init()
    }

    fn init(self) -> Self {
        self.init_value_update();
        self.init_value_display();
        self.init_component_layout();
        self.init_component_colors();
        self.init_precision_defaults();
        self
    }

    /// Initialize the slider value update FRP network.
    fn init_value_update(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;
        let scene = &self.app.display.default_scene;
        let mouse = &scene.mouse.frp;
        let keyboard = &scene.keyboard.frp;
        let component_events = &model.background.events;

        frp::extend! { network

            // === User input ===

            component_click <- component_events.mouse_down_primary
                .gate_not(&input.set_slider_disabled);
            component_release <- component_events.mouse_release_primary
                .gate_not(&input.set_slider_disabled);
            component_drag <- bool(&component_release, &component_click);
            component_drag <- component_drag.gate_not(&input.set_slider_disabled);
            component_ctrl_click <- component_click.gate(&keyboard.is_control_down);
            drag_start_pos <- mouse.position.sample(&component_click);
            drag_end_pos <- mouse.position.gate(&component_drag);
            drag_end_pos <- any2(&drag_end_pos, &drag_start_pos);
            drag_delta_x <- all2(&drag_end_pos, &drag_start_pos);
            drag_delta_x <- drag_delta_x.map(|(end, start)| end.x - start.x).on_change();
            mouse_position_click <- mouse.position.sample(&component_click);
            mouse_position_drag <- mouse.position.gate(&component_drag);
            mouse_position_click_or_drag <- any2(&mouse_position_click, &mouse_position_drag);
            mouse_y_local <- mouse_position_click_or_drag.map(
                f!([scene, model] (pos) scene.screen_to_object_space(&model.background, *pos).y)
            );


            // === Precision calculation ===

            precision_adjustment_margin <- all2(
                &input.set_height,
                &input.set_precision_adjustment_margin,
            ).map(|(h, m)| h / 2.0 + m);
            precision_offset_steps <- all3(
                &mouse_y_local,
                &precision_adjustment_margin,
                &input.set_precision_adjustment_step_size,
            ).map(
                |(offset, margin, step_size)| {
                    let sign = offset.signum();
                    // Calculate mouse y-position offset beyond margin, or 0 if within margin.
                    let offset = (offset.abs() - margin).max(0.0);
                    // Calculate number of steps and direction of the precision adjustment.
                    (offset / step_size).ceil() * sign
                }
            ).on_change();
            precision <- all2(&input.set_default_precision, &precision_offset_steps);
            precision <- precision.map(
                // Adjust the precision by the number of offset steps.
                |(precision, offset)| *precision * (PRECISION_ADJUSTMENT_STEP_BASE).pow(*offset)
            );
            output.precision <+ precision;


            // === Value calculation ===

            value_reset <- input.set_default_value.sample(&component_ctrl_click);
            value_on_click <- output.value.sample(&component_click);
            value_on_click <- any2(&value_reset, &value_on_click);
            update_value <- bool(&component_release, &value_on_click);
            value <- all3(&value_on_click, &precision, &drag_delta_x).gate(&update_value);
            value <- value.map(|(value, precision, delta)| value + delta * precision);
            value <- any2(&input.set_value, &value);
            // Snap the slider's value to the nearest precision increment.
            value <- all2(&value, &precision);
            value <- value.map(|(value, precision)| (value / precision).round() * precision);
            // Clamp the slider's value to within the slider's min/max limits.
            value <- all3(&value, &input.set_min_value, &input.set_max_value);
            value <- value.map(|(value, min, max)| value.clamp(*min, *max));
            output.value <+ value;
        };
    }

    /// Initialize the value display FRP network
    fn init_value_display(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;

        frp::extend! { network
            value <- output.value.on_change();
            precision <- output.precision.on_change();

            value_is_default <- all2(&value, &input.set_default_value).map(|(val, def)| val==def);
            value_is_default_true <- value_is_default.on_true();
            value_is_default_false <- value_is_default.on_false();
            eval_ value_is_default_true(model.set_value_text_property(formatting::Weight::Normal));
            eval_ value_is_default_false(model.set_value_text_property(formatting::Weight::Bold));

            value_text_left_right <- all3(&value, &precision, &input.set_max_disp_decimal_places);
            value_text_left_right <- value_text_left_right.map(value_text_truncate_split);
            value_text_left <- value_text_left_right._0();
            value_text_right <- value_text_left_right._1();
            model.value_text_left.set_content <+ value_text_left;
            value_text_right_is_visible <- value_text_right.map(|t| t.is_some());
            value_text_right <- value_text_right.gate(&value_text_right_is_visible);
            model.value_text_right.set_content <+ value_text_right.unwrap();
            value_text_right_visibility_change <- value_text_right_is_visible.on_change();
            eval value_text_right_visibility_change((v) model.set_value_text_right_visible(*v));
        };
    }

    /// Initialize the component layout FRP network.
    fn init_component_layout(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;
        let track_pos_anim = Animation::new_non_init(network);

        frp::extend! { network
            comp_size <- all2(&input.set_width, &input.set_height).map(|(w, h)| Vector2(*w,*h));
            eval comp_size((size) model.set_size(*size));
            output.width <+ input.set_width;
            output.height <+ input.set_height;

            track_pos <- all3(&output.value, &input.set_min_value, &input.set_max_value);
            track_pos_anim.target <+ track_pos.map(|(value, min, max)| (value - min) / (max - min));
            eval track_pos_anim.value((v) model.track.slider_fraction_filled.set(*v));

            value_text_left_pos_x <- all2(
                &model.value_text_left.width,
                &model.value_text_dot.width,
            );
            value_text_left_pos_x <- value_text_left_pos_x.map(|(left, dot)| -*left - *dot / 2.0);
            eval value_text_left_pos_x((x) model.value_text_left.set_position_x(*x));
            eval model.value_text_left.height((h) model.value_text_left.set_position_y(*h / 2.0));
            eval model.value_text_dot.width((w) model.value_text_dot.set_position_x(-*w / 2.0));
            eval model.value_text_dot.height((h) model.value_text_dot.set_position_y(*h / 2.0));
            eval model.value_text_dot.width((w) model.value_text_right.set_position_x(*w / 2.0));
            eval model.value_text_right.height((h) model.value_text_right.set_position_y(*h / 2.0));

            model.label.set_content <+ input.set_label;
            eval input.set_label_hidden((v) model.set_label_hidden(*v));
            eval model.label.height((h) model.label.set_position_y(*h / 2.0));
            label_pos_x <- all4(
                &input.set_width,
                &input.set_height,
                &model.label.width,
                &input.set_label_position,
            ).map(
                |(comp_width, comp_height, lab_width, position)| match *position {
                    LabelPosition::Inside => -comp_width / 2.0 + comp_height / 2.0,
                    LabelPosition::Outside => -comp_width / 2.0 - comp_height / 2.0 - lab_width,
                }
            );
            eval label_pos_x((x) model.label.set_position_x(*x));
        };
    }

    /// Initialize the component color FRP network.
    fn init_component_colors(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let model = &self.model;
        let background_color_anim = color::Animation::new(network);
        let track_color_anim = color::Animation::new(network);
        let value_text_color_anim = color::Animation::new(network);
        let label_color_anim = color::Animation::new(network);

        frp::extend! { network
            background_color <- all2(&input.set_background_color, &input.set_slider_disabled);
            background_color_anim.target <+ background_color.map(desaturate_color);
            eval background_color_anim.value((color) model.set_background_color(color));
            track_color <- all2(&input.set_slider_track_color, &input.set_slider_disabled);
            track_color_anim.target <+ track_color.map(desaturate_color);
            eval track_color_anim.value((color) model.set_track_color(color));
            value_text_color <- all2(&input.set_value_text_color, &input.set_slider_disabled);
            value_text_color_anim.target <+ value_text_color.map(desaturate_color);
            eval value_text_color_anim.value((color) model.set_value_text_property(color));
            label_color <- all2(&input.set_label_color, &input.set_slider_disabled);
            label_color_anim.target <+ label_color.map(desaturate_color);
            eval label_color_anim.value((color) model.label.set_property_default(color));
        };
    }

    /// Initialize the precision adjustment areas above/below the slider and the default precision
    /// value.
    fn init_precision_defaults(&self) {
        self.frp.set_default_precision(PRECISION_DEFAULT);
        self.frp.set_precision_adjustment_margin(PRECISION_ADJUSTMENT_MARGIN);
        self.frp.set_precision_adjustment_step_size(PRECISION_ADJUSTMENT_STEP_SIZE);
        self.frp.set_max_value(MAX_VALUE_DEFAULT);
        self.frp.set_max_disp_decimal_places(MAX_DISP_DECIMAL_PLACES_DEFAULT);
    }
}

impl display::Object for Slider {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
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



// =============================
// === Value text formatting ===
// =============================

/// Rounds a floating point value to a specified precision and provides two strings: one with the
/// digits left of the decimal point, and one optional with the digits right of the decimal point.
fn value_text_truncate_split(
    (value, precision, max_digits): &(f32, f32, usize),
) -> (ImString, Option<ImString>) {
    if *precision < 1.0 || *max_digits == 0 {
        let digits = (-precision.log10()).ceil() as usize;
        let digits = digits.min(*max_digits);
        let text = format!("{:.prec$}", value, prec = digits);
        let mut text_iter = text.split('.');
        let text_left = text_iter.next().map(|s| s.to_im_string());
        let text_left = text_left.unwrap_or_default();
        let text_right = text_iter.next().map(|s| s.to_im_string());
        (text_left, text_right)
    } else {
        let text_left = format!("{:.0}", value.trunc()).to_im_string();
        (text_left, None)
    }
}



// =========================
// === Desaturate colors ===
// =========================

/// Conditionally desaturates an input color. Used when a component is to be grayed out when
/// disabled.
fn desaturate_color((color, desaturate): &(color::Lcha, bool)) -> color::Lcha {
    if *desaturate {
        color.to_grayscale()
    } else {
        *color
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::NAN;

    #[test]
    fn test_high_precision() {
        let (left, right) = value_text_truncate_split(&(123.4567, 0.01, 8));
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, Some("46".to_im_string()));
    }

    #[test]
    fn test_low_precision() {
        let (left, right) = value_text_truncate_split(&(123.4567, 10.0, 8));
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, None);
    }

    #[test]
    fn test_precision_is_zero() {
        let (left, right) = value_text_truncate_split(&(123.4567, 0.0, 8));
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, Some("45670319".to_im_string()));
    }

    #[test]
    fn test_precision_is_nan() {
        let (left, right) = value_text_truncate_split(&(123.4567, NAN, 8));
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, None);
    }

    #[test]
    fn test_value_is_nan() {
        let (left, right) = value_text_truncate_split(&(NAN, 0.01, 8));
        assert_eq!(left, "NaN".to_im_string());
        assert_eq!(right, None);
    }

    #[test]
    fn test_zero_decimal_places() {
        let (left, right) = value_text_truncate_split(&(123.4567, 0.01, 0));
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, None);
    }
}
