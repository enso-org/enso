//! A slider UI component that allows adjusting a value through mouse interaction.

#![recursion_limit = "512"]
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

use ensogl_core::animation::animation::delayed::DelayedAnimation;
use ensogl_core::application;
use ensogl_core::application::shortcut;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::control::io::mouse;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::Animation;
use ensogl_text::formatting::Weight;


// ==============
// === Export ===
// ==============

pub mod model;



// =================
// === Constants ===
// =================

/// Default slider precision when slider dragging is initiated. The precision indicates both how
/// much the value is changed per pixel dragged and how many digits are displayed after the decimal.
const PRECISION_DEFAULT: f32 = 1.0;
/// Default upper limit of the slider value.
const MAX_VALUE_DEFAULT: f32 = 100.0;
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
/// Limit the number of precision steps to prevent overflow or rounding to zero of the precision.
const MAX_PRECISION_ADJUSTMENT_STEPS: usize = 8;
/// A pop-up is displayed whenever the slider's precision is changed. This is the duration for
/// which the pop-up is visible.
const PRECISION_ADJUSTMENT_POPUP_DURATION: f32 = 1000.0;
/// The delay before an information tooltip is displayed when hovering over a slider component.
const INFORMATION_TOOLTIP_DELAY: f32 = 1000.0;
/// The default size of the slider's thumb as a fraction of the slider's length.
const THUMB_SIZE_DEFAULT: f32 = 0.2;
/// The threshold for shrinking an extended slider limit as a fraction of the current range. If the
/// slider's value is adjusted below this threshold then the limit will be shrunk. This threshold is
/// lower than 1/2 to prevent rapid switching of limits as the extend and shrink thresholds would
/// otherwise coincide.
const ADAPTIVE_LIMIT_SHRINK_THRESHOLD: f32 = 0.4;



// ======================
// === Label position ===
// ======================

/// Position of the slider label.
#[derive(Clone, Copy, Debug, Default)]
pub enum LabelPosition {
    #[default]
    /// Place the label outside the slider component, on its left side.
    Outside,
    /// Place the label inside the slider component, on the left side.
    Inside,
}



// ==========================
// === Slider orientation ===
// ==========================

// /// The orientation of the slider component.
// #[derive(Clone, Copy, Debug, Default)]
// pub enum Axis2 {
//     #[default]
//     /// The slider value is changed by dragging the slider horizontally.
//     Horizontal,
//     /// The slider value is changed by dragging the slider vertically.
//     Vertical,
// }



// =================================
// === Slider position indicator ===
// =================================

/// The type of element that indicates the slider's value along its length.
#[derive(Clone, Copy, Debug, Default)]
pub enum Kind {
    #[default]
    /// A track is a bar that fills the slider as the value increases. The track is empty when the
    /// slider's value is at the lower limit and filled when the value is at the upper limit.
    SingleValue,
    /// A thumb is a small element that moves across the slider as the value changes. The thumb is
    /// on the left/lower end of the slider when the slider's value is at the lower limit and on
    /// the right/upper end of the slider when the value is at the upper limit.
    Scrollbar(f32),
}



// =============================
// === Slider limit behavior ===
// =============================

/// The behavior of the slider when the value is adjusted beyond the slider's limits. This can be
/// set independently for the upper and the lower limits.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum SliderLimit {
    #[default]
    /// The hard limit behavior clamps the value to always be within the slider's limits.
    Hard,
    /// The soft limit behavior allows the value to exceed the slider's limits. If the the slider
    /// value is beyond either limit then an overflow indicator is displayed. In this case the
    /// slider's track does not indicate the value adjustments.
    Soft,
    /// The adaptive limit behavior extends the slider's range when the originally set limit is
    /// reached. Specifically, when a limit is exceeded the slider range is doubled in that
    /// direction. This happens iteratively so that the slider's value is always contained within
    /// the extended range. If the slider's range is extended and the value is able to fit in a
    /// range that is half of the current range, then the slider's range will be shrunk by half.
    /// This again happens iteratively until the slider is at its original range.
    Adaptive,
}

/// Adaptive upper limit adjustment.
fn adapt_upper_limit(
    value: f32,
    min: f32,
    max: f32,
    max_ext: f32,
    upper_limit: SliderLimit,
) -> f32 {
    if upper_limit == SliderLimit::Adaptive && value > max {
        let range = max_ext - min;
        let extend = value > max_ext;
        let shrink = value < min + range * ADAPTIVE_LIMIT_SHRINK_THRESHOLD;
        let max_ext = match (extend, shrink) {
            (true, _) => adapt_upper_limit(value, min, max, min + range * 2.0, upper_limit),
            (_, true) => adapt_upper_limit(value, min, max, min + range * 0.5, upper_limit),
            _ => max_ext,
        };
        max_ext.max(max) // Do no set extended limit below original `max`.
    } else {
        max
    }
}

/// Adaptive lower limit adjustment.
fn adapt_lower_limit(
    value: f32,
    min: f32,
    max: f32,
    min_ext: f32,
    lower_limit: SliderLimit,
) -> f32 {
    if lower_limit == SliderLimit::Adaptive && value < min {
        let range = max - min_ext;
        let extend = value < min_ext;
        let shrink = value > max - range * ADAPTIVE_LIMIT_SHRINK_THRESHOLD;
        let min_ext = match (extend, shrink) {
            (true, _) => adapt_lower_limit(value, min, max, max - range * 2.0, lower_limit),
            (_, true) => adapt_lower_limit(value, min, max, max - range * 0.5, lower_limit),
            _ => min_ext,
        };
        min_ext.min(min) // Do no set extended limit above original `min`.
    } else {
        min
    }
}

/// Clamp the slider's value according to the selected limits setting.
fn value_limit_clamp(
    &(value, min, max, lower_limit, upper_limit): &(f32, f32, f32, SliderLimit, SliderLimit),
) -> f32 {
    match (lower_limit, upper_limit) {
        (SliderLimit::Hard, SliderLimit::Hard) => value.clamp(min, max),
        (SliderLimit::Hard, _) => value.max(min),
        (_, SliderLimit::Hard) => value.min(max),
        _ => value,
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        // /// Set the width of the slider component.
        // set_width(f32),
        // /// Set the height of the slider component.
        // set_height(f32),
        /// Set the type of the slider's value indicator.
        kind(Kind),
        /// Set the color of the slider's value indicator.
        set_value_indicator_color(color::Lcha),
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
        /// Set whether the slider's value text is hidden.
        show_value(bool),
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
        /// Set the maximum number of precision steps to prevent overflow or rounding to zero of the
        /// precision increments.
        set_max_precision_adjustment_steps(usize),
        /// Set whether the precision adjustment mechansim is disabled.
        set_precision_adjustment_disabled(bool),
        /// Set the slider's label. The label will be displayed to the left of the slider's value
        /// display.
        set_label(ImString),
        /// Set the color of the slider's label.
        set_label_color(color::Lcha),
        /// Set whether the slider's label is displayed.
        set_label_hidden(bool),
        /// Set the position of the slider's label.
        set_label_position(LabelPosition),
        /// Set the orientation of the slider component.
        orientation(Axis2),
        /// Set a tooltip that pops up when the mose hovers over the component.
        set_tooltip(ImString),
        /// Set the delay of the tooltip showing after the mouse hovers over the component.
        set_tooltip_delay(f32),
        /// A pop-up is displayed whenever the slider's precision is changed. This is the duration
        /// for which the pop-up is visible.
        set_precision_popup_duration(f32),
        /// Set whether the slider is disabled. When disabled, the slider's value cannot be changed
        /// and the slider is greyed out.
        set_slider_disabled(bool),
        /// The maximum number of digits after the decimal point to be displayed when showing the
        /// component's value.
        set_max_disp_decimal_places(usize),
        /// Set the behavior of the slider when its value is adjusted to below the `min_value`.
        set_lower_limit_type(SliderLimit),
        /// Set the behavior of the slider when its value is adjusted to above the `max_value`.
        set_upper_limit_type(SliderLimit),
        /// Begin textual editing of the slider value.
        start_value_editing(),
        /// End textual editing of the slider value and apply the edited value to the slider.
        finish_value_editing(),
        /// End textual editing of the slider value and revert to the slider value before editing.
        cancel_value_editing(),
        /// Set the slider's thumb size as fraction of the slider's length.
        set_thumb_size(f32),
    }
    Output {
        /// The component's width.
        width(f32),
        /// The component's height.
        height(f32),
        /// The slider's value.
        value(f32),
        /// The slider's precision.
        precision(f32),
        /// The slider value's lower limit. This takes into account limit extension if an adaptive
        /// slider limit is set.
        min_value(f32),
        /// The slider value's upper limit. This takes into account limit extension if an adaptive
        /// slider limit is set.
        max_value(f32),
        /// Indicates whether the mouse is currently hovered over the component.
        hovered(bool),
        /// Indicates whether the slider is currently being dragged.
        dragged(bool),
        /// Indicates whether the slider is disabled.
        disabled(bool),
        /// Indicates whether the slider's value is being edited currently.
        editing(bool),
        // /// The orientation of the slider, either horizontal or vertical.
        // orientation(Axis2),
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
#[derive(Debug, Deref, Clone)]
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
        let frp = Frp::new();
        let model = Rc::new(Model::new(app, frp.network()));
        let app = app.clone_ref();
        Self { frp, model, app }.init()
    }

    fn init(self) -> Self {
        self.init_value_update();
        self.init_limit_handling();
        self.init_value_display();

        self.init_value_editing();
        self.init_precision_popup();
        self.init_information_tooltip();
        self.init_component_layout();
        // self.init_component_colors();
        self.init_slider_defaults();
        self
    }

    /// Initialize the slider value update FRP network.
    fn init_value_update(&self) {
        let network = self.frp.network();
        let frp = &self.frp;
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;
        let scene = &self.app.display.default_scene;
        let mouse = &scene.mouse.frp_deprecated;
        let keyboard = &scene.keyboard.frp;

        let ptr_down_any = model.background.on_event::<mouse::Down>();
        let ptr_up_any = scene.on_event::<mouse::Up>();
        let ptr_out = model.background.on_event::<mouse::Out>();
        let ptr_over = model.background.on_event::<mouse::Over>();

        let obj = model.display_object();

        frp::extend! { network
            ptr_down <- ptr_down_any.map(|e| e.button() == mouse::PrimaryButton).on_true();
            ptr_up <- ptr_up_any.map(|e| e.button() == mouse::PrimaryButton).on_true();
            pos <- mouse.position.map(
                f!([scene, model] (p) scene.screen_to_object_space(&model.background, *p))
            );
            value_on_ptr_down <- output.value.sample(&ptr_down);

            ptr_down <- ptr_down.gate_not(&frp.set_slider_disabled);
            ptr_down <- ptr_down.gate_not(&output.editing);
            on_disabled <- input.set_slider_disabled.on_true();
            on_editing <- output.editing.on_true();
            on_drag_start <- ptr_down.gate_not(&keyboard.is_control_down);
            on_drag_stop <- any3(&ptr_up, &on_disabled, &on_editing);
            dragging <- bool(&on_drag_stop, &on_drag_start);
            drag_start <- pos.sample(&on_drag_start);
            drag_end <- pos.gate(&dragging).any2(&drag_start);
            drag_delta <- all2(&drag_end, &drag_start).map(|(end, start)| end - start);
            drag_delta1 <- all_with(&drag_delta, &frp.orientation, |t, d| t.get_dim(d)).on_change();
            orientation_orth <- frp.orientation.map(|o| o.orthogonal());
            prec_delta <- all_with(&drag_end, &orientation_orth, |t, d| t.get_dim(d)).on_change();

            output.hovered <+ bool(&ptr_out, &ptr_over);
            output.dragged <+ dragging;


            // === Precision calculation ===

            length <- all_with(&obj.on_resized, &frp.orientation, |size, dim| size.get_dim(dim));
            width <- all_with(&obj.on_resized, &orientation_orth, |size, dim| size.get_dim(dim));

            empty_space <- all_with3(&length, &frp.kind, &frp.set_thumb_size,
                |length, indicator, _thumb_size|
                match indicator {
                    Kind::Scrollbar(thumb_size) => length * (1.0 - thumb_size),
                    Kind::SingleValue => *length,
                }
            );

            slider_range <- all_with(&output.max_value, &output.min_value, |max, min| *max - *min);
            native_precision <- all2(&empty_space, &slider_range).map(|(l, r)| r / l);

            non_native_precision <- all_with5(
                &width,
                &frp.set_precision_adjustment_margin,
                &prec_delta,
                &frp.set_precision_adjustment_step_size,
                &frp.set_max_precision_adjustment_steps,
            |width, margin, prec_delta, step_size, max_steps| {
                    let prec_margin = width / 2.0 + margin;
                    let sign = prec_delta.signum() as i32;
                    let offset = prec_delta.abs() - prec_margin;
                    let level = min(*max_steps as i32, (offset / step_size).ceil() as i32) * sign;
                    (level != 0).as_some_from(|| {
                        let exp = if level > 0 { level - 1 } else { level };
                        let precision = 10.0_f32.powf(exp as f32);
                        precision
                    })
                }
            ).on_change();
            precision <- all_with(&non_native_precision, &native_precision, |t,s| t.unwrap_or(*s));
            output.precision <+ precision;


            // === Value calculation ===

            value <- drag_delta1.map3(&value_on_ptr_down, &precision,
                |delta, value, precision| value + delta * precision);
            value <- any2(&frp.set_value, &value);
            value <- all5(
                &value,
                &frp.set_min_value,
                &frp.set_max_value,
                &frp.set_lower_limit_type,
                &frp.set_upper_limit_type,
            ).map(value_limit_clamp);
            output.value <+ value;


            // === Value Reset ===

            reset_value <- ptr_down.gate(&keyboard.is_control_down);
            value_on_reset <- input.set_default_value.sample(&reset_value);
            output.value <+ value_on_reset;


            // === Value Animation ===
            model.value_animation.target <+ output.value;
        };
    }

    /// Initialize the slider limit handling FRP network.
    fn init_limit_handling(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;

        frp::extend! { network
            min_value <- all_with5(
                &output.value,
                &input.set_min_value,
                &input.set_max_value,
                &output.min_value,
                &input.set_lower_limit_type,
                |a,b,c,d,e| adapt_lower_limit(*a,*b,*c,*d,*e)
            ).on_change();
            output.min_value <+ min_value;

            max_value <- all_with5(
                &output.value,
                &input.set_min_value,
                &input.set_max_value,
                &output.max_value,
                &input.set_upper_limit_type,
                |a,b,c,d,e|adapt_upper_limit(*a,*b,*c,*d,*e)
            ).on_change();
            output.max_value <+ max_value;

            overflow_lower <- all_with(&output.value, &min_value, |v, min| v < min).on_change();
            overflow_upper <- all_with(&output.value, &max_value, |v, max| v > max).on_change();
            eval overflow_lower((v) model.set_overflow_lower_visible(*v));
            eval overflow_upper((v) model.set_overflow_upper_visible(*v));
        };
    }

    /// Initialize the value display FRP network. Sets text to bold if the value is not the default
    /// one and manages the value display on the slider.
    fn init_value_display(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;

        frp::extend! { network
            eval input.show_value((v) model.show_value(*v));

            value <- output.value.sampled_gate(&input.show_value);
            default_value <- input.set_default_value.sampled_gate(&input.show_value);
            is_default <- all_with(&value, &default_value, |val, def| val == def);
            text_weight <- switch_constant(&is_default, Weight::Bold, Weight::Normal);
            eval text_weight ((v) model.set_value_text_property(*v));

            precision <- output.precision.sampled_gate(&input.show_value);
            max_decimal_places <- input.set_max_disp_decimal_places.sampled_gate(&input.show_value);
            text <- all_with3(&value, &precision, &max_decimal_places, display_value);
            text_left <- text._0();
            text_right <- text._1();
            model.value_text_left.set_content <+ text_left;
            text_right_visible <- text_right.map(|t| t.is_some()).on_change();
            new_text_right <= text_right.gate(&text_right_visible);
            model.value_text_right.set_content <+ new_text_right;
            eval text_right_visible((v) model.set_value_text_right_visible(*v));
        };
    }

    /// Initialize the precision pop-up FRP network.
    fn init_precision_popup(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;
        let component_events = &model.background.events_deprecated;
        let popup_anim = DelayedAnimation::new(network);

        frp::extend! { network
            popup_anim.set_duration <+ input.set_precision_popup_duration;
            component_drag <- bool(
                &component_events.mouse_release_primary,
                &component_events.mouse_down_primary
            );
            precision <- output.precision.on_change().gate(&component_drag);
            model.tooltip.frp.set_style <+ precision.map(|precision| {
                let prec_text = format!(
                    "Precision: {precision:.MAX_DISP_DECIMAL_PLACES_DEFAULT$}",
                );
                let prec_text = prec_text.trim_end_matches('0');
                let prec_text = prec_text.trim_end_matches('.');
                tooltip::Style::set_label(prec_text.into())
            });
            precision_changed <- precision.constant(());
            popup_anim.reset <+ precision_changed;
            popup_anim.start <+ precision_changed;
            popup_hide <- any2(&popup_anim.on_end, &component_events.mouse_release_primary);
            model.tooltip.frp.set_style <+ popup_hide.map(|_|
                tooltip::Style::unset_label()
            );
        };
    }

    /// Initialize the information tooltip FRP network.
    fn init_information_tooltip(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;
        let component_events = &model.background.events_deprecated;
        let tooltip_anim = DelayedAnimation::new(network);

        frp::extend! { network
            tooltip_anim.set_delay <+ input.set_tooltip_delay;
            tooltip_start <- any2(&component_events.mouse_over, &component_events.mouse_up_primary);
            tooltip_start <- tooltip_start.gate_not(&output.dragged);
            tooltip_start <- tooltip_start.gate_not(&output.editing);
            tooltip_empty <- input.set_tooltip.sample(&tooltip_start).map(|s| s.trim().is_empty());
            tooltip_start <- tooltip_start.gate_not(&tooltip_empty);
            tooltip_anim.start <+ tooltip_start;
            tooltip_anim.reset <+ any2(
                &component_events.mouse_out,
                &component_events.mouse_down_primary
            );
            tooltip_show <- input.set_tooltip.sample(&tooltip_anim.on_end);
            model.tooltip.frp.set_style <+ tooltip_show.map(|tooltip| {
                tooltip::Style::set_label(format!("{tooltip}"))
            });
            model.tooltip.frp.set_style <+ tooltip_anim.on_reset.map(|_|
                tooltip::Style::unset_label()
            );
        };
    }

    /// Initialize the component layout FRP network.
    fn init_component_layout(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;
        let min_limit_anim = Animation::new_non_init(network);
        let max_limit_anim = Animation::new_non_init(network);

        let obj = model.display_object();

        frp::extend! { network
            // comp_size <- all2(&input.set_width, &input.set_height).map(|(w, h)| Vector2(*w,*h));
            eval obj.on_resized((size) model.update_size(*size));
            eval input.kind((i) model.kind(i));
            // output.width <+ input.set_width;
            // output.height <+ input.set_height;
            min_limit_anim.target <+ output.min_value;
            max_limit_anim.target <+ output.max_value;
            indicator_pos <- all3(&model.value_animation.value, &min_limit_anim.value, &max_limit_anim.value);
            indicator_pos <- indicator_pos.map(|(value, min, max)| (value - min) / (max - min));
            indicator_pos <- all3(&indicator_pos, &input.set_thumb_size, &input.orientation);
            eval indicator_pos((v) model.set_indicator_position(v));

            value_text_left_pos_x <- all3(
                &model.value_text_left.width,
                &model.value_text_dot.width,
                &output.precision,
            );
            value_text_left_pos_x <- value_text_left_pos_x.map(
                // Center text if precision higher than 1.0 (integer display), else align to dot.
                |(left, dot, prec)| if *prec >= 1.0 {- *left / 2.0} else {- *left - *dot / 2.0}
            );
            eval value_text_left_pos_x((x) model.value_text_left.set_x(*x));
            eval model.value_text_left.height((h) model.value_text_left.set_y(*h / 2.0));
            eval model.value_text_dot.width((w) {
                model.value_text_dot.set_x(-*w / 2.0);
                model.value_text_right.set_x(*w / 2.0);
            });
            eval model.value_text_dot.height((h) model.value_text_dot.set_y(*h / 2.0));
            eval model.value_text_right.height((h) model.value_text_right.set_y(*h / 2.0));
            eval model.value_text_edit.width((w) model.value_text_edit.set_x(-*w / 2.0));
            eval model.value_text_edit.height((h) model.value_text_edit.set_y(*h / 2.0));

            // overflow_marker_position <- all3(
            //     &input.set_width,
            //     &input.set_height,
            //     &input.orientation,
            // );
            // eval overflow_marker_position((p) model.set_overflow_marker_position(p));
            // overflow_marker_shape <- all2(&model.value_text_left.height, &input.orientation);
            // eval overflow_marker_shape((s) model.set_overflow_marker_shape(s));
            //
            // eval input.set_label_hidden((v) model.set_label_hidden(*v));
            // model.label.set_content <+ input.set_label;
            // label_position <- all6(
            //     &input.set_width,
            //     &input.set_height,
            //     &model.label.width,
            //     &model.label.height,
            //     &input.set_label_position,
            //     &input.orientation,
            // );
            // eval label_position((p) model.set_label_position(p));

            // output.orientation <+ input.orientation;
        };
    }

    /// Initialize the component color FRP network.
    fn init_component_colors(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;
        let background_color_anim = color::Animation::new(network);
        let indicator_color_anim = color::Animation::new(network);
        let value_text_color_anim = color::Animation::new(network);
        let label_color_anim = color::Animation::new(network);

        frp::extend! { network
            background_color <- all2(&input.set_background_color, &input.set_slider_disabled);
            background_color_anim.target <+ background_color.map(desaturate_color);
            eval background_color_anim.value((color) model.set_background_color(color));
            indicator_color <- all2(&input.set_value_indicator_color, &input.set_slider_disabled);
            indicator_color_anim.target <+ indicator_color.map(desaturate_color);
            eval indicator_color_anim.value((color) model.set_indicator_color(color));
            value_text_color <- all2(&input.set_value_text_color, &input.set_slider_disabled);
            value_text_color_anim.target <+ value_text_color.map(desaturate_color);
            eval value_text_color_anim.value((color) model.set_value_text_property(color));
            label_color <- all2(&input.set_label_color, &input.set_slider_disabled);
            label_color_anim.target <+ label_color.map(desaturate_color);
            eval label_color_anim.value((color) model.label.set_property_default(color));

            output.disabled <+ input.set_slider_disabled;
        };
    }

    /// Initialize the textual value editing FRP network.
    fn init_value_editing(&self) {
        let network = self.frp.network();
        let input = &self.frp.input;
        let output = &self.frp.private.output;
        let model = &self.model;

        frp::extend! { network
            start_editing <- input.start_value_editing.gate_not(&output.disabled);
            start_editing <- start_editing.gate(&input.show_value);
            value_on_edit <- output.value.sample(&start_editing);
            prec_on_edit <- output.precision.sample(&start_editing);
            max_places_on_edit <-
                input.set_max_disp_decimal_places.sample(&start_editing);
            value_text_on_edit <- all3(&value_on_edit, &prec_on_edit, &max_places_on_edit);
            value_text_on_edit <- value_text_on_edit.map(|t| value_text_truncate(t).to_im_string());
            model.value_text_edit.set_content <+ value_text_on_edit;
            stop_editing <- any2(&input.finish_value_editing, &input.cancel_value_editing);
            editing <- bool(&stop_editing, &start_editing);

            value_text_after_edit <-
                model.value_text_edit.content.sample(&input.finish_value_editing);
            value_text_after_edit <- value_text_after_edit.map(|s| String::from(s).to_im_string());
            value_after_edit <- value_text_after_edit.map(|s| f32::from_str(s).ok());
            edit_success <- value_after_edit.map(|v| v.is_some());
            value_after_edit <- value_after_edit.map(|v| v.unwrap_or_default());
            prec_after_edit <- value_text_after_edit.map(|s| get_value_text_precision(s));
            prec_after_edit <- all2(&prec_after_edit, &input.set_default_precision);
            prec_after_edit <- prec_after_edit.map(|(prec, default_prec)| prec.min(*default_prec));
            value_after_edit <- all5(
                &value_after_edit,
                &input.set_min_value,
                &input.set_max_value,
                &input.set_lower_limit_type,
                &input.set_upper_limit_type,
            ).map(value_limit_clamp);

            output.editing <+ editing;
            output.precision <+ prec_after_edit.gate(&edit_success);
            value_after_edit <- value_after_edit.gate(&edit_success);
            output.value <+ value_after_edit;
            model.value_animation.target <+ value_after_edit;
            editing_event <- any2(&start_editing, &stop_editing);
            editing <- all2(&editing, &output.precision).sample(&editing_event);
            eval editing((t) model.set_edit_mode(t));
        };
    }

    /// Initialize the compinent with default values.
    fn init_slider_defaults(&self) {
        self.frp.set_default_precision(PRECISION_DEFAULT);
        self.frp.set_precision_adjustment_margin(PRECISION_ADJUSTMENT_MARGIN);
        self.frp.set_precision_adjustment_step_size(PRECISION_ADJUSTMENT_STEP_SIZE);
        self.frp.set_max_precision_adjustment_steps(MAX_PRECISION_ADJUSTMENT_STEPS);
        self.frp.set_max_value(MAX_VALUE_DEFAULT);
        self.frp.set_max_disp_decimal_places(MAX_DISP_DECIMAL_PLACES_DEFAULT);
        self.frp.set_tooltip_delay(INFORMATION_TOOLTIP_DELAY);
        self.frp.set_precision_popup_duration(PRECISION_ADJUSTMENT_POPUP_DURATION);
        self.frp.set_thumb_size(THUMB_SIZE_DEFAULT);
        self.show_value(true);
        self.orientation(Axis2::X);
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

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::DoublePress;
        use shortcut::ActionType::Press;
        vec![
            Self::self_shortcut_when(
                DoublePress,
                "left-mouse-button",
                "start_value_editing",
                "hovered & !editing",
            ),
            Self::self_shortcut_when(
                DoublePress,
                "left-mouse-button",
                "cancel_value_editing",
                "!hovered & editing",
            ),
            Self::self_shortcut_when(Press, "enter", "finish_value_editing", "editing"),
            Self::self_shortcut_when(Press, "escape", "cancel_value_editing", "editing"),
        ]
    }
}



// =============================
// === Value text formatting ===
// =============================

/// Rounds and truncates a floating point value to a specified precision.
fn value_text_truncate((value, precision, max_digits): &(f32, f32, usize)) -> String {
    if *precision < 1.0 || *max_digits == 0 {
        let digits = (-precision.log10()).ceil() as usize;
        let digits = digits.min(*max_digits);
        format!("{value:.digits$}")
    } else {
        format!("{value:.0}")
    }
}

/// Rounds a floating point value to a specified precision and provides two strings: one with the
/// digits left of the decimal point, and one optional with the digits right of the decimal point.
fn display_value(value: &f32, precision: &f32, max_digits: &usize) -> (ImString, Option<ImString>) {
    let text = value_text_truncate(&(*value, *precision, *max_digits));
    let mut text_iter = text.split('.');
    let text_left = text_iter.next().map(|s| s.to_im_string()).unwrap_or_default();
    let text_right = text_iter.next().map(|s| s.to_im_string());
    (text_left, text_right)
}

/// Get the precision of a string containing a decimal value.
fn get_value_text_precision(text: &str) -> f32 {
    let mut text_iter = text.split('.').skip(1);
    let text_right_len = text_iter.next().map(|t| t.len());
    match text_right_len {
        None => 1.0,
        Some(n) => 10f32.powi(-(n as i32)),
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
        let (left, right) = display_value(&123.4567, &0.01, &8);
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, Some("46".to_im_string()));
    }

    #[test]
    fn test_low_precision() {
        let (left, right) = display_value(&123.4567, &10.0, &8);
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, None);
    }

    #[test]
    fn test_precision_is_zero() {
        let (left, right) = display_value(&123.4567, &0.0, &8);
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, Some("45670319".to_im_string()));
    }

    #[test]
    fn test_precision_is_nan() {
        let (left, right) = display_value(&123.4567, &NAN, &8);
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, None);
    }

    #[test]
    fn test_value_is_nan() {
        let (left, right) = display_value(&NAN, &0.01, &8);
        assert_eq!(left, "NaN".to_im_string());
        assert_eq!(right, None);
    }

    #[test]
    fn test_zero_decimal_places() {
        let (left, right) = display_value(&123.4567, &0.01, &0);
        assert_eq!(left, "123".to_im_string());
        assert_eq!(right, None);
    }
}
