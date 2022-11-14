//! UI component that allows picking a number through mouse interaction. Horizontal mouse movement
//! changes the value within a set range. Vertical mouse movement changes the precision of the
//! slider, this influences the response to horizontal mouse movement and the increment steps to
//! which the value is snapped.

#![recursion_limit = "1024"]
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

/// The slider precision when slider dragging is initiated.
pub const PRECISION_DEFAULT: f32 = 0.1;
/// Margin above/below the component within which vertical mouse movement will not result in a
/// change in slider precision.
pub const PRECISION_ADJUST_MARGIN: f32 = 10.0;
/// The vertical mouse movement in px needed to increase/decrease the slider precision by one step.
pub const PRECISION_ADJUST_STEP_SIZE: f32 = 50.0;
/// Base of the exponentiation for precision increment steps of 10^x
pub const PRECISION_ADJUST_STEP_BASE: f32 = 10.0;



// ========================
// === Slider component ===
// ========================

/// Slider component structure
#[derive(Debug)]
pub struct Slider {
    pub frp: Frp,
    model:   Rc<Model>,
    pub app: Application,
}

impl Slider {
    /// Constructor
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app));
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

/// Position of the slider label.
#[derive(Clone, Copy, Debug, Default)]
pub enum LabelPosition {
    #[default]
    /// Place the label outside the slider component, on its left side..
    Outside,
    /// Place the label inside the slider component, on the left side.
    Inside,
}


// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        /// Set the width of the component
        set_width(f32),
        /// Set the height of the component
        set_height(f32),
        /// Set the color of the slider
        set_slider_color(color::Lcha),
        /// Set the color of the background
        set_background_color(color::Lcha),
        /// Set the slider value
        set_value(f32),
        /// Set the default value to reset a slider to
        set_value_default(f32),
        /// Set the minimum to which the value is clamped
        set_value_min(f32),
        /// Set the maximum to which the value is clamped
        set_value_max(f32),
        /// Set the color of the value text display
        set_value_color(color::Lcha),
        /// Set the default precision at which the slider operates
        set_precision(f32),
        /// Set the margin above/below the slider beyond which the precision is adjusted up/downwards
        set_precision_adjust_margin(f32),
        /// Set the distance of vertical mouse movement needed to increment/decrement the precision to the next step
        set_precision_adjust_step_size(f32),
        /// Set a slider label
        set_label(ImString),
        /// Set the color of the slider label
        set_label_color(color::Lcha),
        /// Set whether the label is displayed
        set_label_hidden(bool),
        /// Set whether the label is shown inside the slider, as opposed to left of it
        set_label_position(LabelPosition),
        /// Set whether the slider is enabled, when disabled the slider is greyed out and cannot be interacted with
        set_slider_disabled(bool),
    }
    Output {
        width(f32),
        height(f32),
        value(f32),
    }
}

impl Slider {
    /// Initialise a slider component
    fn init(self) -> Self {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;
        let scene = &self.app.display.default_scene;
        let mouse = &scene.mouse.frp;
        let keyboard = &scene.keyboard.frp;
        let track_pos_anim = Animation::new_non_init(network);
        let background_color_anim = color::Animation::new(network);
        let track_color_anim = color::Animation::new(network);
        let value_text_color_anim = color::Animation::new(network);
        let label_color_anim = color::Animation::new(network);

        frp::extend! { network


            // User input

            background_click <- model.background.events.mouse_down_primary.constant(());
            background_release <- model.background.events.mouse_release_primary.constant(());
            background_drag <- bool(&background_release,&background_click);

            track_click <- model.track.events.mouse_down_primary.constant(());
            track_release <- model.track.events.mouse_release_primary.constant(());
            track_drag <- bool(&track_release,&track_click);

            component_click <- any2(&background_click,&track_click).gate_not(&input.set_slider_disabled);
            component_drag <- any2(&background_drag,&track_drag).gate_not(&input.set_slider_disabled);
            component_release <- any2(&background_release,&track_release);

            component_ctrl_click <- component_click.gate(&keyboard.is_control_down);

            drag_pos_start <- mouse.position.sample(&component_click);
            drag_pos_end <- mouse.position.gate(&component_drag);
            drag_pos_end <- any2(&drag_pos_end,&drag_pos_start);
            drag_delta <- all2(&drag_pos_end,&drag_pos_start).map(|(end,start)| end - start);

            mouse_y_local <- mouse.position.map(
                f!([scene,model] (pos) scene.screen_to_object_space(&model.background,*pos).y )
            ).gate(&component_drag);



            // Componenet size

            output.width <+ input.set_width;
            output.height <+ input.set_height;


            // precision calculation

            precision_adjust_margin <- all2(&input.set_height,&input.set_precision_adjust_margin);
            // Calculate margin from center of component
            precision_adjust_margin <- precision_adjust_margin.map(|(h,margin)| h / 2.0 + margin);
            precision_adjusted <- all4(
                &input.set_precision,
                &mouse_y_local,
                &precision_adjust_margin,
                &input.set_precision_adjust_step_size,
            ).map(
                |(default,offset,margin,step_size)| {
                    let sign = offset.signum();
                    // Calculate y-position offset beyond margin, or 0 if within margin.
                    let offset = (offset.abs() - margin).max(0.0);
                    // Calculate number of steps, rounding up, and apply sign
                    let steps = (offset / step_size).ceil() * sign;
                    // Set the precision to base to the power of steps
                    *default * (PRECISION_ADJUST_STEP_BASE).pow(steps)
                }
            );


            // value calculation

            value_reset <- input.set_value_default.sample(&component_ctrl_click);
            value_start <- output.value.sample(&component_click);
            value_start <- any2(&value_reset,&value_start);

            // value_update is updated only after value_start is sampled
            value_update <- bool(&component_release,&value_start);
            value <- all3(&value_start,&precision_adjusted,&drag_delta).map(
                |(value,precision,delta)| value + delta.x * precision
            ).gate(&value_update);
            value <- any2(&input.set_value,&value);
            value <- all2(&value,&precision_adjusted).map(
                |(value,precision)| (value / precision).round() * precision
            );
            value <- all3(&value,&input.set_value_min,&input.set_value_max).map(
                |(value,min,max)| value.clamp(*min,*max)
            );
            output.value <+ value;

            track_pos_anim.target <+ all3(&value,&input.set_value_min,&input.set_value_max).map(
                |(value,min,max)| (value - min) / (max - min)
            );


            // model update

            eval track_pos_anim.value ((v) model.track.slider_fraction_filled.set(*v));
            component_size <- all2(&input.set_width,&input.set_height).map(|(w,h)| Vector2(*w,*h));
            eval component_size ((size) model.set_size(*size));

            value_is_default <- all2(&value,&input.set_value_default).map(|(val,def)| val==def);
            value_is_default_true <- value_is_default.on_true();
            value_is_default_false <- value_is_default.on_false();
            eval_ value_is_default_true ({
                model.set_value_text_property_default(formatting::Weight::Normal);
            });
            eval_ value_is_default_false ({
                model.set_value_text_property_default(formatting::Weight::Bold);
            });


            // colors

            background_color <- all2(&input.set_background_color,&input.set_slider_disabled);
            background_color_anim.target <+ background_color.map(desaturate_color);
            eval background_color_anim.value ((color) model.set_background_color(color));
            track_color <- all2(&input.set_slider_color, &input.set_slider_disabled);
            track_color_anim.target <+ track_color.map(desaturate_color);
            eval track_color_anim.value ((color) model.set_track_color(color));
            value_text_color <- all2(&input.set_value_color, &input.set_slider_disabled);
            value_text_color_anim.target <+ value_text_color.map(desaturate_color);
            eval value_text_color_anim.value ((color) model.set_value_text_property_default(color));
            label_color <- all2(&input.set_label_color, &input.set_slider_disabled);
            label_color_anim.target <+ label_color.map(desaturate_color);
            eval label_color_anim.value ((color) model.label.set_property_default(color));


            // text alignment

            value_text_left_right <- all2(&value,&precision_adjusted).map(value_text_truncate_split);
            value_text_left <- value_text_left_right._0();
            value_text_right <- value_text_left_right._1();
            value_text_right_visible <- value_text_right.map(|t| t.is_some());
            value_text_right <- value_text_right.gate(&value_text_right_visible);
            model.value_left.set_content <+ value_text_left;
            model.value_right.set_content <+ value_text_right.unwrap();
            value_text_right_visibility_change <- value_text_right_visible.on_change();
            eval value_text_right_visibility_change ((v) model.set_value_decimal_visible(*v));

            value_text_left_pos_x <- all2(&model.value_left.width,&model.value_dot.width).map(
                |(left,dot)| -*left - *dot / 2.0
            );
            eval value_text_left_pos_x ((x) model.value_left.set_position_x(*x));
            eval model.value_left.height ((h) model.value_left.set_position_y(*h / 2.0));
            eval model.value_dot.width ((w) model.value_dot.set_position_x(-*w / 2.0));
            eval model.value_dot.height ((h) model.value_dot.set_position_y(*h / 2.0));
            eval model.value_dot.width ((w) model.value_right.set_position_x(*w / 2.0));
            eval model.value_right.height ((h) model.value_right.set_position_y(*h / 2.0));

            model.label.set_content <+ input.set_label;
            eval input.set_label_hidden((v) model.set_label_hidden(*v));

            eval model.label.height ((h) model.label.set_position_y(*h / 2.0));
            label_pos_x <- all4(
                &input.set_width,
                &input.set_height,
                &model.label.width,
                &input.set_label_position,
            ).map(
                |(comp_width,comp_height,lab_width,position)| match *position {
                    LabelPosition::Inside => -comp_width / 2.0 + comp_height / 2.0,
                    LabelPosition::Outside => -comp_width / 2.0 - comp_height / 2.0 - lab_width,
                }
            );
            eval label_pos_x ((x) model.label.set_position_x(*x));
        };
        self.init_precision_defaults()
    }

    /// Initiate the precision adjustment areas above/below the slider and the default precision
    /// value
    fn init_precision_defaults(self) -> Self {
        self.frp.set_precision(PRECISION_DEFAULT);
        self.frp.set_precision_adjust_margin(PRECISION_ADJUST_MARGIN);
        self.frp.set_precision_adjust_step_size(PRECISION_ADJUST_STEP_SIZE);
        self
    }
}



// =============================
// === Value text formatting ===
// =============================

/// Rounds a floating point value to a specified precision and provides two strings: one with the
/// digits left of the decimal point, and one optional with the digits right of the decimal point.
fn value_text_truncate_split((value, precision): &(f32, f32)) -> (ImString, Option<ImString>) {
    if *precision < 1.0 {
        let digits = (-precision.log10()).ceil() as usize;
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
