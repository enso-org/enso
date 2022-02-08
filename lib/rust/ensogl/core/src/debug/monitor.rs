//! This module implements performance monitoring utils.

use crate::prelude::*;

use crate::debug::stats::StatsData;
use crate::system::web;
use crate::system::web::StyleSetter;

use num_traits::cast::AsPrimitive;
use std::collections::VecDeque;
use std::f64;
use wasm_bindgen;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;



// ==============
// === Config ===
// ==============

/// Look and feel configuration for the performance monitor.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct ConfigTemplate<Str, Num> {
    pub background_color:      Str,
    pub label_color_ok:        Str,
    pub label_color_warn:      Str,
    pub label_color_err:       Str,
    pub plot_color_ok:         Str,
    pub plot_color_warn:       Str,
    pub plot_color_err:        Str,
    pub plot_background_color: Str,
    pub plot_bar_size:         Option<Num>,
    pub plot_step_size:        Num,
    pub margin:                Num,
    pub outer_margin:          Num,
    pub panel_height:          Num,
    pub labels_width:          Num,
    pub results_width:         Num,
    pub plots_width:           Num,
    pub font_size:             Num,
    pub font_vertical_offset:  Num,
}

/// Specialization of the `ConfigTemplate` for users of the library.
pub type Config = ConfigTemplate<String, u32>;

/// Specialization of the `ConfigTemplate` for the usage in JS environment.
pub type SamplerConfig = ConfigTemplate<JsValue, f64>;

fn dark_theme() -> Config {
    Config {
        background_color:      "#222222".into(),
        label_color_ok:        "#8e939a".into(),
        label_color_warn:      "#ffba18".into(),
        label_color_err:       "#eb3941".into(),
        plot_color_ok:         "#8e939a".into(),
        plot_color_warn:       "#ffba18".into(),
        plot_color_err:        "#eb3941".into(),
        plot_background_color: "#333333".into(),
        plot_bar_size:         None,
        plot_step_size:        1,
        margin:                4,
        outer_margin:          6,
        panel_height:          15,
        labels_width:          130,
        results_width:         30,
        plots_width:           100,
        font_size:             9,
        font_vertical_offset:  4,
    }
}

fn light_theme() -> Config {
    Config {
        background_color:      "#f1f1f0".into(),
        label_color_ok:        "#202124".into(),
        label_color_warn:      "#f58025".into(),
        label_color_err:       "#eb3941".into(),
        plot_color_ok:         "#202124".into(),
        plot_color_warn:       "#f58025".into(),
        plot_color_err:        "#eb3941".into(),
        plot_background_color: "#f1f1f0".into(),
        plot_bar_size:         Some(2),
        plot_step_size:        1,
        margin:                6,
        outer_margin:          4,
        panel_height:          15,
        labels_width:          130,
        results_width:         30,
        plots_width:           100,
        font_size:             9,
        font_vertical_offset:  4,
    }
}

impl Default for Config {
    fn default() -> Config {
        light_theme()
    }
}


impl Config {
    /// Translates the configuration to JS values.
    pub fn to_js_config(&self) -> SamplerConfig {
        let ratio = web::window().device_pixel_ratio();
        SamplerConfig {
            background_color:      (&self.background_color).into(),
            label_color_ok:        (&self.label_color_ok).into(),
            label_color_warn:      (&self.label_color_warn).into(),
            label_color_err:       (&self.label_color_err).into(),
            plot_color_ok:         (&self.plot_color_ok).into(),
            plot_color_warn:       (&self.plot_color_warn).into(),
            plot_color_err:        (&self.plot_color_err).into(),
            plot_background_color: (&self.plot_background_color).into(),
            plot_bar_size:         self.plot_bar_size.map(|t| t as f64 * ratio),
            plot_step_size:        self.plot_step_size as f64 * ratio,
            outer_margin:          self.outer_margin as f64 * ratio,
            margin:                self.margin as f64 * ratio,
            panel_height:          self.panel_height as f64 * ratio,
            labels_width:          self.labels_width as f64 * ratio,
            results_width:         self.results_width as f64 * ratio,
            plots_width:           self.plots_width as f64 * ratio,
            font_size:             self.font_size as f64 * ratio,
            font_vertical_offset:  self.font_vertical_offset as f64 * ratio,
        }
    }
}



// ===========
// === Dom ===
// ===========

/// Dom elements of the monitor. Please note that it uses `Rc` to both implement cheap copy as well
/// as to use `Drop` to clean the HTML when not used anymore.
#[derive(Clone, Debug, Shrinkwrap)]
pub struct Dom {
    rc: Rc<DomData>,
}

/// Internal representation of `Dom`.
#[derive(Debug)]
pub struct DomData {
    root:    web::HtmlDivElement,
    canvas:  web::HtmlCanvasElement,
    context: web::CanvasRenderingContext2d,
}

impl Dom {
    /// Constructor.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let data = DomData::new();
        let rc = Rc::new(data);
        Self { rc }
    }
}


impl DomData {
    /// Constructor.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let root = web::create_div();
        root.set_class_name("performance-monitor");
        root.set_style_or_panic("position", "absolute");
        root.set_style_or_panic("z-index", "100");
        root.set_style_or_panic("left", "8px");
        root.set_style_or_panic("top", "8px");
        root.set_style_or_panic("overflow", "hidden");
        root.set_style_or_panic("border-radius", "6px");
        root.set_style_or_panic("box-shadow", "0px 0px 20px -4px rgba(0,0,0,0.44)");
        web::body().prepend_with_node_1(&root).unwrap();

        let canvas = web::create_canvas();
        canvas.set_style_or_panic("display", "block");

        let context = canvas.get_context("2d").unwrap().unwrap();
        let context: web::CanvasRenderingContext2d = context.dyn_into().unwrap();
        root.append_child(&canvas).unwrap();
        Self { root, canvas, context }
    }
}

impl Drop for DomData {
    fn drop(&mut self) {
        self.root.remove()
    }
}



// ===============
// === Monitor ===
// ===============

/// Implementation of the monitoring panel.
#[derive(Debug)]
pub struct Monitor {
    user_config: Config,
    config:      SamplerConfig,
    width:       f64,
    height:      f64,
    dom:         Option<Dom>,
    panels:      Vec<Panel>,
    first_draw:  bool,
}


// === Public API ===

impl Default for Monitor {
    fn default() -> Self {
        let user_config = Config::default();
        let panels = default();
        let width = default();
        let height = default();
        let first_draw = true;
        let config = user_config.to_js_config();
        let dom = None;
        let mut out = Self { user_config, config, width, height, dom, panels, first_draw };
        out.update_config();
        out
    }
}

impl Monitor {
    /// Cnstructor.
    pub fn new() -> Self {
        default()
    }

    /// Modify the Monitor's config and update the view.
    pub fn mod_config<F: FnOnce(&mut Config)>(&mut self, f: F) {
        f(&mut self.user_config);
        self.update_config();
    }

    /// Add new display element.
    pub fn add<S: Sampler + Default + 'static>(&mut self) -> Panel {
        let panel = Panel::new(self.config.clone(), S::default());
        self.panels.push(panel.clone());
        self.resize();
        panel
    }

    /// Check whether the mointor is visible.
    pub fn visible(&self) -> bool {
        self.dom.is_some()
    }

    /// Show the monitor and add it's DOM to the scene.
    pub fn show(&mut self) {
        if !self.visible() {
            self.first_draw = true;
            self.dom = Some(Dom::new());
            self.resize();
        }
    }

    /// Hides the monitor and remove it's DOM from the scene.
    pub fn hide(&mut self) {
        self.dom = None;
    }

    /// Toggle the visibility of the monitor.
    pub fn toggle(&mut self) {
        if self.visible() {
            self.hide();
        } else {
            self.show();
        }
    }

    /// Draw the Monitor and update all of it's values.
    pub fn draw(&mut self) {
        if let Some(dom) = self.dom.clone() {
            if self.first_draw {
                self.first_draw = false;
                self.first_draw(&dom);
            }
            self.shift_plot_area_left(&dom);
            self.clear_labels_area(&dom);
            self.draw_plots(&dom);
        }
    }
}


// === Private API ===

impl Monitor {
    fn update_config(&mut self) {
        self.config = self.user_config.to_js_config()
    }

    fn resize(&mut self) {
        if let Some(dom) = &self.dom {
            let ratio = web::window().device_pixel_ratio();
            let width = self.config.labels_width
                + self.config.results_width
                + self.config.plots_width
                + 4.0 * self.config.margin
                + self.config.outer_margin; // no outer_margin on the left side.
            let mut height = self.config.outer_margin;
            for _panel in &self.panels {
                height += self.config.margin + self.config.panel_height;
            }
            height += self.config.margin;
            height += self.config.outer_margin;
            let u_width = width as u32;
            let u_height = height as u32;
            self.width = width;
            self.height = height;
            dom.canvas.set_width(u_width);
            dom.canvas.set_height(u_height);
            dom.canvas.set_style_or_panic("width", format!("{}px", width / ratio));
            dom.canvas.set_style_or_panic("height", format!("{}px", height / ratio));
        }
    }

    fn shift_plot_area_left(&mut self, dom: &Dom) {
        let width = self.width;
        let height = self.height;
        let shift = -(self.config.plot_step_size);
        dom.context
            .draw_image_with_html_canvas_element_and_sw_and_sh_and_dx_and_dy_and_dw_and_dh(
                &dom.canvas,
                0.0,
                0.0,
                width,
                height,
                shift,
                0.0,
                self.width,
                self.height,
            )
            .unwrap();
    }

    fn clear_labels_area(&mut self, dom: &Dom) {
        let step = self.config.plot_step_size;
        let width = self.config.labels_width + self.config.results_width + 3.0 * self.config.margin;
        dom.context.set_fill_style(&self.config.background_color);
        dom.context.fill_rect(0.0, 0.0, width, self.height);
        dom.context.fill_rect(self.width - step, 0.0, step, self.height);
    }

    fn draw_plots(&mut self, dom: &Dom) {
        self.with_all_panels(dom, |panel| panel.draw(dom));
    }

    fn first_draw(&self, dom: &Dom) {
        dom.context.set_fill_style(&self.config.background_color);
        dom.context.fill_rect(0.0, 0.0, self.width, self.height);
        self.with_all_panels(dom, |panel| panel.first_draw(dom));
    }

    fn with_all_panels<F: Fn(&Panel)>(&self, dom: &Dom, f: F) {
        let mut total_off = self.config.outer_margin;
        dom.context.translate(0.0, total_off).unwrap();
        for panel in &self.panels {
            let off = self.config.margin;
            dom.context.translate(0.0, off).unwrap();
            total_off += off;
            f(panel);
            let off = self.config.panel_height;
            dom.context.translate(0.0, off).unwrap();
            total_off += off;
        }
        dom.context.translate(0.0, -total_off).unwrap();
    }
}



// =============
// === Panel ===
// =============

/// A single element in the `Monitor`. It can display labels, values and plots. Each `Panel` uses
/// a `Sampler` under the hood, which defines both its behavior and its look and feel.
#[derive(Clone, Debug)]
pub struct Panel {
    rc: Rc<RefCell<PanelData>>,
}

impl Panel {
    /// Creates a new, empty Panel with a given sampler.
    pub fn new<S: Sampler + 'static>(config: SamplerConfig, sampler: S) -> Self {
        let rc = Rc::new(RefCell::new(PanelData::new(config, sampler)));
        Self { rc }
    }

    /// Display results of last measurements.
    pub fn draw(&self, dom: &Dom) {
        self.rc.borrow_mut().draw(dom)
    }

    /// Fetch the measured value from stats, using the panel's sampler, then post-process the value
    /// to make it useful for displaying on a human-readable graph.
    pub fn sample_and_postprocess(&self, stats: &StatsData) {
        self.rc.borrow_mut().sample_and_postprocess(stats)
    }

    fn first_draw(&self, dom: &Dom) {
        self.rc.borrow_mut().first_draw(dom)
    }
}



// ==================
// === ValueCheck ===
// ==================

/// Values drawn in the monitor can be assigned with a check: `Correct`, `Warning`, and `Error`.
/// It affects the way they are visually displayed.
#[derive(Copy, Clone, Debug)]
#[allow(missing_docs)]
pub enum ValueCheck {
    Correct,
    Warning,
    Error,
}

impl Default for ValueCheck {
    fn default() -> Self {
        Self::Correct
    }
}

// To be removed after this gets resolved: https://github.com/rust-lang/rust-clippy/issues/4971
#[allow(clippy::collapsible_else_if)]
impl ValueCheck {
    /// Construct the check by comparing the provided value to two threshold values.
    pub fn from_threshold(warn_threshold: f64, err_threshold: f64, value: f64) -> Self {
        if warn_threshold > err_threshold {
            if value >= warn_threshold {
                ValueCheck::Correct
            } else if value >= err_threshold {
                ValueCheck::Warning
            } else {
                ValueCheck::Error
            }
        } else {
            if value <= warn_threshold {
                ValueCheck::Correct
            } else if value <= err_threshold {
                ValueCheck::Warning
            } else {
                ValueCheck::Error
            }
        }
    }
}



// ===============
// === Sampler ===
// ===============

/// Abstraction for a sampling utility. Samplers gather the data and expose it in a way suitable for
/// the monitor.
pub trait Sampler: Debug {
    /// Label of the sampler in the monitor window.
    fn label(&self) -> &str;

    /// Get the newest value of the sampler. The value will be displayed in the monitor panel.
    fn value(&self, stats: &StatsData) -> f64;

    /// Check whether the newest value is correct, or should be displayed as warning or error.
    fn check(&self, _stats: &StatsData) -> ValueCheck {
        ValueCheck::Correct
    }

    /// Returns the maximum expected value in order to set proper scaling of the monitor plots.
    /// If the real value will be bigger than this parameter, it will be clamped.
    fn max_value(&self) -> Option<f64> {
        None
    }

    /// Returns the minimum expected value in order to set proper scaling of the monitor plots.
    /// If the real value will be smaller than this parameter, it will be clamped.
    fn min_value(&self) -> Option<f64> {
        None
    }

    /// Returns the maximum expected value in order to set proper scaling of the monitor plots.
    /// If the real value will be bigger than this parameter, the graphs will be re-scaled
    /// automatically.
    fn min_size(&self) -> Option<f64> {
        None
    }

    /// Returns the number describing the amount of last values which should be consideration
    /// when displaying the final value. The final value will be the average of # previous values.
    fn smooth_range(&self) -> usize {
        2
    }

    /// The number of digits after the dot which should be displayed in the monitor panel.
    fn precision(&self) -> usize {
        2
    }
}



// =================
// === PanelData ===
// =================

/// A `Panel` is a single row in the monitor view.
#[derive(Debug)]
pub struct PanelData {
    label:       String,
    config:      SamplerConfig,
    min_value:   f64,
    max_value:   f64,
    value:       f64,
    last_values: VecDeque<f64>,
    norm_value:  f64,
    draw_offset: f64,
    value_check: ValueCheck,
    precision:   usize,
    sampler:     Box<dyn Sampler>,
}


// === Construction ===

impl PanelData {
    /// Constructor.
    pub fn new<S: Sampler + 'static>(config: SamplerConfig, sampler: S) -> Self {
        let label = sampler.label().into();
        let min_value = f64::INFINITY;
        let max_value = f64::NEG_INFINITY;
        let value = default();
        let last_values = default();
        let norm_value = default();
        let draw_offset = 0.0;
        let value_check = default();
        let sampler = Box::new(sampler);
        let precision = sampler.precision();
        Self {
            label,
            config,
            min_value,
            max_value,
            value,
            last_values,
            norm_value,
            draw_offset,
            value_check,
            precision,
            sampler,
        }
    }
}


// === Begin / End ===

impl PanelData {
    /// Fetch the measured value from stats, using the panel's sampler, then post-process the value
    /// to make it useful for displaying on a human-readable graph.
    pub fn sample_and_postprocess(&mut self, stats: &StatsData) {
        self.value = self.sampler.value(stats);
        self.value_check = self.sampler.check(stats);
        self.clamp_value();
        self.smooth_value();
        self.normalize_value();
    }

    /// Clamp the measured values to the `max_value` and `min_value`.
    fn clamp_value(&mut self) {
        if let Some(max_value) = self.sampler.max_value() {
            if self.value > max_value {
                self.value = max_value;
            }
        }
        if let Some(min_value) = self.sampler.min_value() {
            if self.value > min_value {
                self.value = min_value;
            }
        }
        if self.value > self.max_value {
            self.max_value = self.value;
        }
        if self.value < self.min_value {
            self.min_value = self.value;
        }
    }

    /// Smooth the final value based on the last measured values.
    fn smooth_value(&mut self) {
        self.last_values.push_front(self.value);
        if self.last_values.len() > self.sampler.smooth_range() {
            self.last_values.pop_back();
        }

        self.value = self.last_values.iter().sum();
        self.value /= self.last_values.len() as f64
    }

    /// Normalize the value to the monitor's plot size.
    fn normalize_value(&mut self) {
        let mut size = self.max_value - self.min_value;
        if let Some(min_size) = self.sampler.min_size() {
            if size < min_size {
                size = min_size;
            }
        }
        self.norm_value = (self.value - self.min_value) / size;
    }
}


// === Draw ===

impl PanelData {
    /// Draws the panel to the screen.
    pub fn draw(&mut self, dom: &Dom) {
        self.init_draw(dom);
        self.draw_plots(dom);
        self.finish_draw(dom);
    }

    fn first_draw(&mut self, dom: &Dom) {
        self.init_draw(dom);
        dom.context.set_fill_style(&self.config.plot_background_color);
        dom.context.fill_rect(0.0, 0.0, self.config.plots_width, self.config.panel_height);
        self.finish_draw(dom);
    }

    fn move_pen_to_next_element(&mut self, dom: &Dom, offset: f64) {
        dom.context.translate(offset, 0.0).unwrap();
        self.draw_offset += offset;
    }

    fn finish_draw(&mut self, dom: &Dom) {
        dom.context.translate(-self.draw_offset, 0.0).unwrap();
        self.draw_offset = 0.0;
    }

    fn init_draw(&mut self, dom: &Dom) {
        self.move_pen_to_next_element(dom, self.config.margin);
        self.draw_labels(dom);
        self.draw_results(dom);
    }

    fn draw_labels(&mut self, dom: &Dom) {
        let fonts = "Helvetica,Arial,sans-serif";
        let y_pos = self.config.panel_height - self.config.font_vertical_offset;
        dom.context.set_font(&format!("bold {}px {}", self.config.font_size, fonts));
        dom.context.set_text_align("right");
        dom.context.set_fill_style(&self.config.label_color_ok);
        dom.context.fill_text(&self.label, self.config.labels_width, y_pos).unwrap();
        self.move_pen_to_next_element(dom, self.config.labels_width + self.config.margin);
    }

    fn draw_results(&mut self, dom: &Dom) {
        let display_value = format!("{1:.0$}", self.precision, self.value);
        let y_pos = self.config.panel_height - self.config.font_vertical_offset;
        let color = match self.value_check {
            ValueCheck::Correct => &self.config.label_color_ok,
            ValueCheck::Warning => &self.config.label_color_warn,
            ValueCheck::Error => &self.config.label_color_err,
        };
        dom.context.set_fill_style(color);
        dom.context.fill_text(&display_value, self.config.results_width, y_pos).unwrap();
        self.move_pen_to_next_element(dom, self.config.results_width + self.config.margin);
    }

    fn draw_plots(&mut self, dom: &Dom) {
        self.move_pen_to_next_element(dom, self.config.plots_width - self.config.plot_step_size);
        dom.context.set_fill_style(&self.config.plot_background_color);
        dom.context.fill_rect(0.0, 0.0, self.config.plot_step_size, self.config.panel_height);
        let value_height = self.norm_value * self.config.panel_height;
        let y_pos = self.config.panel_height - value_height;
        let bar_height = self.config.plot_bar_size.unwrap_or(value_height);
        let color = match self.value_check {
            ValueCheck::Correct => &self.config.plot_color_ok,
            ValueCheck::Warning => &self.config.plot_color_warn,
            ValueCheck::Error => &self.config.plot_color_err,
        };
        dom.context.set_fill_style(color);
        dom.context.fill_rect(0.0, y_pos, self.config.plot_step_size, bar_height);
    }
}



// =================================================================================================
// === Samplers ====================================================================================
// =================================================================================================

// ======================
// === Stats Samplers ===
// ======================

/// Utility to generate Samplers for stats parameters. See the usages below this declaration to
/// discover more.
macro_rules! stats_sampler {
    ( $label:tt, $name:ident, $stats_field:ident, $warn_threshold:expr, $err_threshold:expr
    , $precision:expr, $value_divisor:expr) => {
        stats_sampler!(
            $label,
            $name,
            $stats_field,
            $warn_threshold,
            $err_threshold,
            $precision,
            $value_divisor,
            None
        );
    };

    ( $label:tt, $name:ident, $stats_field:ident, $warn_threshold:expr, $err_threshold:expr
    , $precision:expr, $value_divisor:expr, $max_value:expr) => {
        /// Sampler implementation.
        #[derive(Copy, Clone, Debug, Default)]
        pub struct $name {}

        impl Sampler for $name {
            fn label(&self) -> &str {
                $label
            }
            fn value(&self, stats: &StatsData) -> f64 {
                let raw_value: f64 = stats.$stats_field.as_();
                raw_value / $value_divisor
            }
            fn min_size(&self) -> Option<f64> {
                Some($warn_threshold)
            }
            fn precision(&self) -> usize {
                $precision
            }
            fn check(&self, stats: &StatsData) -> ValueCheck {
                // Before the first frame, all stat values will be 0, which in (only) this case is
                // correct.
                if stats.frame_counter == 0 {
                    ValueCheck::Correct
                } else {
                    let value = self.value(&stats);
                    ValueCheck::from_threshold($warn_threshold, $err_threshold, value)
                }
            }
            fn max_value(&self) -> Option<f64> {
                $max_value
            }
        }
    };
}

const MB: f64 = (1024 * 1024) as f64;

stats_sampler!("Frames per second", Fps, fps, 55.0, 25.0, 2, 1.0, Some(60.0));
stats_sampler!("Frame time (ms)", FrameTime, frame_time, 1000.0 / 55.0, 1000.0 / 25.0, 2, 1.0);
stats_sampler!("WASM memory usage (Mb)", WasmMemory, wasm_memory_usage, 50.0, 100.0, 2, MB);
stats_sampler!("GPU memory usage (Mb)", GpuMemoryUsage, gpu_memory_usage, 100.0, 500.0, 2, MB);
stats_sampler!("Draw call count", DrawCallCount, draw_call_count, 100.0, 500.0, 0, 1.0);
stats_sampler!("Buffer count", BufferCount, buffer_count, 100.0, 500.0, 0, 1.0);
stats_sampler!("Data upload count", DataUploadCount, data_upload_count, 100.0, 500.0, 0, 1.0);
stats_sampler!("Data upload size (Mb)", DataUploadSize, data_upload_size, 1.0, 10.0, 2, MB);
stats_sampler!("Sprite system count", SpriteSystemCount, sprite_system_count, 100.0, 500.0, 0, 1.0);
stats_sampler!("Symbol count", SymbolCount, symbol_count, 100.0, 500.0, 0, 1.0);
stats_sampler!("Sprite count", SpriteCount, sprite_count, 100_000.0, 500_000.0, 0, 1.0);
stats_sampler!("Shader count", ShaderCount, shader_count, 100.0, 500.0, 0, 1.0);
stats_sampler!(
    "Shader compile count",
    ShaderCompileCount,
    shader_compile_count,
    10.0,
    100.0,
    0,
    1.0
);

#[cfg(test)]
mod tests {
    use super::*;

    use crate::debug::stats::Stats;

    use assert_approx_eq::assert_approx_eq;


    #[derive(Default)]
    struct TestSampler<S: Sampler> {
        stats:   Stats,
        sampler: S,
        t:       f64,
    }

    macro_rules! test_and_advance_frame {
        ($test:expr, $expected_value:expr, $expected_check:path
        ; next: $frame_time:expr, $post_frame_delay:expr) => {
            let prev_frame_stats = $test.stats.begin_frame($test.t);
            let tested_value = $test.sampler.value(&prev_frame_stats);
            let tested_check = $test.sampler.check(&prev_frame_stats);
            assert_approx_eq!(tested_value, $expected_value, 0.001);
            assert!(matches!(tested_check, $expected_check));
            $test.t += $frame_time;
            $test.stats.end_frame($test.t);
            $test.t += $post_frame_delay;
        };
    }

    #[test]
    fn frame_time() {
        // Note: 60 FPS means there's 16.6(6) ms budget for 1 frame. The test will be written under
        // assumption we're trying to be around this FPS.

        let mut test: TestSampler<FrameTime> = default();

        // Frame 1: simulate we managed to complete the work in 10ms, and then we wait 6ms before
        // starting next frame.
        //
        // Note: there is no frame before "Frame 1", so the tested value for the previous frame
        // will always be 0.0 and "Correct" at this point (which is a special case - depending on
        // tested stat, for later frames 0.0 could result in a threshold warning/error).
        test_and_advance_frame!(test, 0.0, ValueCheck::Correct; next: 10.0, 6.0);

        // Frame 2: simulate we managed to complete the work in 5ms, and then we wait 11ms before
        // starting next frame.
        test_and_advance_frame!(test, 10.0, ValueCheck::Correct; next: 5.0, 11.0);

        // Frame 3: simulate we went over the budget of 16.6(6) ms, at 30ms. No extra delay
        // afterwards before starting next frame.
        test_and_advance_frame!(test, 5.0, ValueCheck::Correct; next: 30.0, 0.0);

        // Frame 4: simulate a really slow frame (1000ms), crossing the configured error threshold.
        test_and_advance_frame!(test, 30.0, ValueCheck::Warning; next: 1000.0, 0.0);

        // For the final test, we're not interested in the next frame, so we're using some dummy
        // values for it.
        let dummy = 0.0;
        test_and_advance_frame!(test, 1000.0, ValueCheck::Error; next: dummy, dummy);
    }

    #[test]
    fn fps() {
        // Note: 60 FPS means there's 16.6(6) ms budget for 1 frame. The test will be written under
        // assumption we're trying to be around this FPS.

        let mut test: TestSampler<Fps> = default();

        // Frame 1: simulate we managed to complete the work in 10ms, and then we wait 6ms before
        // starting next frame.
        //
        // Note: there is no earlier frame before "Frame 1", so the tested value for the previous
        // frame will always be 0.0 and [`Correct`] at this point (which is a special case -
        // depending on tested stat, for later frames 0.0 could result in a threshold
        // warning/error).
        test_and_advance_frame!(test, 0.0, ValueCheck::Correct; next: 10.0, 6.0);

        // Frame 2: simulate we managed to complete the work in 5ms, and then we wait 11.67ms before
        // starting next frame.
        //
        // Note: FPS takes into account delays between frames - for example, if a frame took only
        // 1ms, but the next frame will not be started immediately (will be started only e.g. 15ms
        // later), we cannot show FPS only based the 1ms duration of the frame - it would not be
        // true, as the subsequent delay means less frames per second will be rendered in reality.
        //
        // Previous frame+delay was 16.0 ms; we'd fit 62.5 such frames in 1s.
        test_and_advance_frame!(test, 62.5, ValueCheck::Correct; next: 5.0, 11.67);

        // Frame 3: simulate we went over the budget of 16.6(6) ms, at 20ms. No extra delay
        // afterwards before starting next frame.
        // Previous frame+delay was 16.67 ms; we'd fit ~59.988 such frames in 1s.
        test_and_advance_frame!(test, 59.988, ValueCheck::Correct; next: 20.0, 0.0);

        // Frame 4: simulate a really slow frame (1000ms), crossing the FPS error threshold.
        // Previous frame+delay was 20.0 ms; we'd fit 50.0 such frames in 1s.
        test_and_advance_frame!(test, 50.0, ValueCheck::Warning; next: 1000.0, 0.0);

        // For the final test, we're not interested in the next frame, so we're using some dummy
        // values for it.
        // Previous frame+delay was 1000.0 ms; we'd fit 1 such frame in 1s.
        let dummy = 0.0;
        test_and_advance_frame!(test, 1.0, ValueCheck::Error; next: dummy, dummy);
    }
}
