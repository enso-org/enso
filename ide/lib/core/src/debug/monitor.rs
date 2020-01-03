use crate::prelude::*;

use js_sys::ArrayBuffer;
use js_sys::WebAssembly::Memory;
use std::collections::VecDeque;
use std::f64;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use wasm_bindgen;
use web_sys::*;



// ===============
// === Helpers ===
// ===============

fn window() -> web_sys::Window {
    web_sys::window().unwrap_or_else(|| panic!("Cannot access window."))
}

fn document() -> web_sys::Document {
    window().document().unwrap_or_else(|| panic!("Cannot access window.document."))
}

fn body() -> web_sys::HtmlElement {
    document().body().unwrap_or_else(|| panic!("Cannot access window.document.body."))
}

fn performance() -> Performance {
    window().performance().unwrap_or_else(|| panic!("Cannot access window.performance."))
}



// ==============
// === Config ===
// ==============

#[derive(Clone,Debug)]
pub struct ConfigTemplate<Str,Num> {
    pub background_color      : Str,
    pub label_color_ok        : Str,
    pub label_color_warn      : Str,
    pub label_color_err       : Str,
    pub plot_color_ok         : Str,
    pub plot_color_warn       : Str,
    pub plot_color_err        : Str,
    pub plot_background_color : Str,
    pub plot_step_size        : Num,
    pub margin                : Num,
    pub panel_height          : Num,
    pub labels_width          : Num,
    pub results_width         : Num,
    pub plots_width           : Num,
    pub font_size             : Num,
    pub font_vertical_offset  : Num,
}

pub type Config        = ConfigTemplate<String,u32>;
pub type SamplerConfig = ConfigTemplate<JsValue,f64>;

impl Default for Config {
    fn default() -> Config {
        Config {
            background_color      : "#222222".into(),
            label_color_ok        : "#8e939a".into(),
            label_color_warn      : "#ffba18".into(),
            label_color_err       : "#eb3941".into(),
            plot_color_ok         : "#8e939a".into(),
            plot_color_warn       : "#ffba18".into(),
            plot_color_err        : "#eb3941".into(),
            plot_background_color : "#333333".into(),
            plot_step_size        : 1,
            margin                : 4,
            panel_height          : 15,
            labels_width          : 120,
            results_width         : 30,
            plots_width           : 100,
            font_size             : 9,
            font_vertical_offset  : 4,
        }
    }
}

impl Config {
    pub fn to_plot_config(&self) -> SamplerConfig {
        let ratio      = window().device_pixel_ratio();
        SamplerConfig {
            background_color      : (&self.background_color)      . into(),
            label_color_ok        : (&self.label_color_ok)        . into(),
            label_color_warn      : (&self.label_color_warn)      . into(),
            label_color_err       : (&self.label_color_err)       . into(),
            plot_color_ok         : (&self.plot_color_ok)         . into(),
            plot_color_warn       : (&self.plot_color_warn)       . into(),
            plot_color_err        : (&self.plot_color_err)        . into(),
            plot_background_color : (&self.plot_background_color) . into(),
            plot_step_size        : self.plot_step_size       as f64 * ratio,
            margin                : self.margin               as f64 * ratio,
            panel_height          : self.panel_height         as f64 * ratio,
            labels_width          : self.labels_width         as f64 * ratio,
            results_width         : self.results_width        as f64 * ratio,
            plots_width           : self.plots_width          as f64 * ratio,
            font_size             : self.font_size            as f64 * ratio,
            font_vertical_offset  : self.font_vertical_offset as f64 * ratio,
        }
    }
}



// ===============
// === Monitor ===
// ===============

#[derive(Debug)]
pub struct Monitor {
    user_config   : Config,
    config        : SamplerConfig,
    width         : f64,
    height        : f64,
    dom           : Element,
    panels        : Vec<Panel>,
    canvas        : HtmlCanvasElement,
    context       : CanvasRenderingContext2d,
    is_first_draw : bool,
}


// === Public API ===

// TODO: All the `unwraps` below should be handled nicer when this lib will be finished.

impl Default for Monitor {
    fn default() -> Self {
        let user_config: Config = default();
        let panels              = default();
        let width               = default();
        let height              = default();
        let is_first_draw       = true;
        let config              = user_config.to_plot_config();

        let dom = document().create_element("div").unwrap();
        dom.set_attribute("style", "position:absolute;").unwrap();
        body().prepend_with_node_1(&dom).unwrap();

        let canvas = document().create_element("canvas").unwrap();
        let canvas: HtmlCanvasElement = canvas.dyn_into().unwrap();

        let context = canvas.get_context("2d").unwrap().unwrap();
        let context: CanvasRenderingContext2d = context.dyn_into().unwrap();
        dom.append_child(&canvas).unwrap();

        let mut out = Self{user_config,config,width,height,dom,panels,canvas,context,is_first_draw};
        out.update_config();
        out
    }
}

impl Monitor {
    /// Creates a new, empty monitor instance. Ise the `add` method to fill it with samplers.
    pub fn new() -> Self { default() }

    /// Modifies the Monitor's config and updates the view.
    pub fn mod_config<F:FnOnce(&mut Config)>(&mut self, f:F) {
        f(&mut self.user_config);
        self.update_config();
    }

    /// Adds new display element.
    pub fn add<M:Sampler+'static>(&mut self, monitor:M) -> Panel {
        let panel = Panel::new(self.context.clone(),self.config.clone(),monitor);
        self.panels.push(panel.clone());
        self.resize();
        panel
    }

    /// Draws the Monitor and updates all of it's values.
    pub fn draw(&mut self) {
        if self.is_first_draw {
            self.is_first_draw = false;
            self.first_draw();
        }
        self.shift_plot_area_left();
        self.clear_labels_area();
        self.draw_plots();
    }
}


// === Private API ===

impl Monitor {
    fn update_config(&mut self) {
        self.config = self.user_config.to_plot_config()
    }

    fn resize(&mut self) {
        let width = self.config.labels_width
            + self.config.results_width
            + self.config.plots_width
            + 4.0 * self.config.margin;
        let mut height = 0.0;
        for _panel in &self.panels {
            height += self.config.margin + self.config.panel_height;
        }
        height += self.config.margin;

        let u_width  = width  as u32;
        let u_height = height as u32;
        let style    = format!("width:{}px; height:{}px",u_width/2,u_height/2);
        self.width   = width;
        self.height  = height;
        self.canvas.set_width  (u_width);
        self.canvas.set_height (u_height);
        self.canvas.set_attribute("style",&style).unwrap();
    }

    fn shift_plot_area_left(&mut self) {
        let width  = self.width as f64;
        let height = self.height as f64;
        let shift  = -(self.config.plot_step_size as f64);
        self.context.draw_image_with_html_canvas_element_and_sw_and_sh_and_dx_and_dy_and_dw_and_dh
        (&self.canvas,0.0,0.0,width,height,shift,0.0,self.width,self.height).unwrap();
    }

    fn clear_labels_area(&mut self) {
        let step  = self.config.plot_step_size;
        let width = self.config.labels_width + self.config.results_width + 3.0 * self.config.margin;
        self.context.set_fill_style(&self.config.background_color);
        self.context.fill_rect(0.0,0.0,width,self.height);
        self.context.fill_rect(self.width-step,0.0,step,self.height);
    }

    fn draw_plots(&mut self) {
        self.with_all_panels(|panel| panel.draw());
    }

    fn first_draw(&self) {
        self.context.set_fill_style(&self.config.background_color);
        self.context.fill_rect(0.0,0.0,self.width,self.height);
        self.with_all_panels(|panel| panel.first_draw());
    }

    fn with_all_panels<F:Fn(&Panel)>(&self,f:F) {
        let mut total_off = 0.0;
        for panel in &self.panels {
            let off = self.config.margin;
            self.context.translate(0.0,off).unwrap();
            total_off += off;
            f(panel);
            let off = self.config.panel_height;
            self.context.translate(0.0,off).unwrap();
            total_off += off;
        }
        self.context.translate(0.0,-total_off).unwrap();
    }
}




// =============
// === Panel ===
// =============

/// A single element in the `Monitor`. It can display labels, values and plots. Each `Panel` uses
/// a `Sampler` under the hood, which defines both its behavior and its look and feel.
#[derive(Clone,Debug)]
pub struct Panel {
    rc: Rc<RefCell<PanelData>>
}

impl Panel {
    /// Creates a new, empty Panel with a given sampler.
    pub fn new<S:Sampler+'static>
    (context:CanvasRenderingContext2d, config:SamplerConfig, sampler:S) -> Self {
        let rc = Rc::new(RefCell::new(PanelData::new(context,config,sampler)));
        Self {rc}
    }

    /// Display results of last measurements.
    pub fn draw(&self) {
        self.rc.borrow_mut().draw()
    }

    /// Start measuring the data.
    pub fn begin(&self) {
        self.rc.borrow_mut().begin()
    }

    /// Stop measuring the data.
    pub fn end(&self) {
        self.rc.borrow_mut().end()
    }

    fn first_draw(&self) {
        self.rc.borrow_mut().first_draw()
    }
}



// ==================
// === ValueCheck ===
// ==================

#[derive(Copy,Clone,Debug)]
pub enum ValueCheck {Correct,Warning,Error}

impl Default for ValueCheck {
    fn default() -> Self {
        Self::Correct
    }
}



// ===============
// === Sampler ===
// ===============

pub trait Sampler: Debug {
    fn label        (&self) -> &str;
    fn begin        (&mut self, _time:f64) {}
    fn end          (&mut self, _time:f64) {}
    fn value        (&self) -> f64;
    fn check        (&self) -> ValueCheck  { ValueCheck::Correct }
    fn max_value    (&self) -> Option<f64> { None }
    fn min_value    (&self) -> Option<f64> { None }
    fn min_size     (&self) -> Option<f64> { None }
    fn smooth_range (&self) -> usize { 2 }
}



// =================
// === PanelData ===
// =================

#[derive(Debug)]
pub struct PanelData {
    label       : String,
    context     : CanvasRenderingContext2d,
    performance : Performance,
    config      : SamplerConfig,
    min_value   : f64,
    max_value   : f64,
    begin_value : f64,
    value       : f64,
    last_values : VecDeque<f64>,
    norm_value  : f64,
    draw_offset : f64,
    value_check : ValueCheck,
    sampler     : Box<dyn Sampler>
}


// === Construction ===

impl PanelData {
    pub fn new<S:Sampler+'static>
    (context:CanvasRenderingContext2d, config:SamplerConfig, sampler:S) -> Self {
        let label         = sampler.label().into();
        let performance   = performance();
        let min_value     = f64::INFINITY;
        let max_value     = f64::NEG_INFINITY;
        let begin_value   = default();
        let value         = default();
        let last_values   = default();
        let norm_value    = default();
        let draw_offset   = 0.0;
        let value_check   = default();
        let sampler       = Box::new(sampler);
        Self {label,context,performance,config,min_value,max_value,begin_value,value,last_values
             ,norm_value,draw_offset,value_check,sampler}
    }
}


// === Begin / End ===

impl PanelData {
    pub fn begin(&mut self) {
        let time = self.performance.now();
        self.sampler.begin(time);
    }

    pub fn end(&mut self) {
        let time = self.performance.now();
        self.sampler.end(time);
        self.value_check = self.sampler.check();
        self.value       = self.sampler.value();
        self.clamp_value();
        self.smooth_value();
        self.normalize_value();
    }

    fn clamp_value(&mut self) {
        if let Some(max_value) = self.sampler.max_value() {
            if self.value > max_value { self.value = max_value; }
        }
        if let Some(min_value) = self.sampler.min_value() {
            if self.value > min_value { self.value = min_value; }
        }
        if self.value > self.max_value { self.max_value = self.value; }
        if self.value < self.min_value { self.min_value = self.value; }
    }

    fn smooth_value(&mut self) {
        self.last_values.push_front(self.value);
        if self.last_values.len() > self.sampler.smooth_range() {
            self.last_values.pop_back();
        }

        self.value = self.last_values.iter().sum();
        self.value /= self.last_values.len() as f64
    }

    fn normalize_value(&mut self) {
        let mut size = self.max_value - self.min_value;
        if let Some(min_size) = self.sampler.min_size() {
            if size < min_size { size = min_size; }
        }
        self.norm_value = (self.value - self.min_value) / size;
    }
}


// === Draw ===

impl PanelData {
    pub fn draw(&mut self) {
        self.init_draw();
        self.draw_plots();
        self.finish_draw();
    }

    pub fn first_draw(&mut self) {
        self.init_draw();
        self.context.set_fill_style(&self.config.plot_background_color);
        self.context.fill_rect(0.0,0.0,self.config.plots_width,self.config.panel_height);
        self.finish_draw();
    }

    fn move_pen_to_next_element(&mut self, offset:f64) {
        self.context.translate(offset,0.0).unwrap();
        self.draw_offset += offset;
    }

    fn finish_draw(&mut self) {
        self.context.translate(-self.draw_offset,0.0).unwrap();
        self.draw_offset = 0.0;
    }

    fn init_draw(&mut self) {
        self.move_pen_to_next_element(self.config.margin);
        self.draw_labels();
        self.draw_results();
    }

    fn draw_labels(&mut self) {
        let fonts = "Helvetica,Arial,sans-serif";
        let y_pos = self.config.panel_height - self.config.font_vertical_offset;
        self.context.set_font(&format!("bold {}px {}",self.config.font_size,fonts));
        self.context.set_text_align("right");
        self.context.set_fill_style(&self.config.label_color_ok);
        self.context.fill_text(&self.label,self.config.labels_width,y_pos).unwrap();
        self.move_pen_to_next_element(self.config.labels_width + self.config.margin);
    }

    fn draw_results(&mut self) {
        let display_value = (self.value * 100.0).round() / 100.0;
        let display_value = format!("{:.*}",2,display_value);
        let y_pos         = self.config.panel_height - self.config.font_vertical_offset;
        let color         = match self.value_check {
            ValueCheck::Correct => &self.config.label_color_ok,
            ValueCheck::Warning => &self.config.label_color_warn,
            ValueCheck::Error   => &self.config.label_color_err
        };
        self.context.set_fill_style(color);
        self.context.fill_text(&display_value,self.config.results_width,y_pos).unwrap();
        self.move_pen_to_next_element(self.config.results_width + self.config.margin);
    }

    fn draw_plots(&mut self) {
        self.move_pen_to_next_element(self.config.plots_width - self.config.plot_step_size);
        self.context.set_fill_style(&self.config.plot_background_color);
        self.context.fill_rect(0.0,0.0,self.config.plot_step_size,self.config.panel_height);
        let value_height = self.norm_value * self.config.panel_height;
        let y_pos        = self.config.panel_height-value_height;
        let color        = match self.value_check {
            ValueCheck::Correct => &self.config.plot_color_ok,
            ValueCheck::Warning => &self.config.plot_color_warn,
            ValueCheck::Error   => &self.config.plot_color_err
        };
        self.context.set_fill_style(color);
        self.context.fill_rect(0.0,y_pos,self.config.plot_step_size,value_height);
    }
}



// =================================================================================================
// === Samplers ====================================================================================
// =================================================================================================

// =================
// === FrameTime ===
// =================

#[derive(Debug,Default)]
pub struct FrameTime {
    begin_time  : f64,
    value       : f64,
    value_check : ValueCheck,
}

impl FrameTime {
    pub fn new() -> Self { default() }
}

impl Sampler for FrameTime {
    fn label (&self) -> &str       { "Frame time (ms)" }
    fn value (&self) -> f64        { self.value }
    fn check (&self) -> ValueCheck { self.value_check }
    fn begin (&mut self, time:f64) { self.begin_time = time; }
    fn end   (&mut self, time:f64) {
        let end_time     = time;
        self.value       = end_time - self.begin_time;
        self.value_check =
            if      self.value < 1000.0 / 55.0 { ValueCheck::Correct }
            else if self.value < 1000.0 / 25.0 { ValueCheck::Warning }
            else                               { ValueCheck::Error   };
    }
}



// ===========
// === Fps ===
// ===========

#[derive(Debug,Default)]
pub struct Fps {
    begin_time  : f64,
    value       : f64,
    value_check : ValueCheck,
}

impl Fps {
    pub fn new() -> Self { default() }
}

impl Sampler for Fps {
    fn label     (&self) -> &str        { "Frames per second" }
    fn value     (&self) -> f64         { self.value }
    fn check     (&self) -> ValueCheck  { self.value_check }
    fn max_value (&self) -> Option<f64> { Some(60.0) }
    fn begin     (&mut self, time:f64)  {
        if self.begin_time > 0.0 {
            let end_time     = time;
            self.value       = 1000.0 / (end_time - self.begin_time);
            self.value_check =
                if      self.value >= 55.0 { ValueCheck::Correct }
                else if self.value >= 25.0 { ValueCheck::Warning }
                else                       { ValueCheck::Error   };
        }
        self.begin_time = time;
    }
}



// ==================
// === WasmMemory ===
// ==================

#[derive(Debug,Default)]
pub struct WasmMemory {
    value       : f64,
    value_check : ValueCheck,
}

impl WasmMemory {
    pub fn new() -> Self { default() }
}

impl Sampler for WasmMemory {
    fn label    (&self) -> &str        { "WASM memory usage (Mb)" }
    fn value    (&self) -> f64         { self.value }
    fn check    (&self) -> ValueCheck  { self.value_check }
    fn min_size (&self) -> Option<f64> { Some(100.0) }
    fn end      (&mut self, _time:f64)  {
        let memory: Memory      = wasm_bindgen::memory().dyn_into().unwrap();
        let buffer: ArrayBuffer = memory.buffer().dyn_into().unwrap();
        self.value              = (buffer.byte_length() as f64) / (1024.0 * 1024.0);
        self.value_check        =
            if      self.value <=  50.0 { ValueCheck::Correct }
            else if self.value <= 100.0 { ValueCheck::Warning }
            else                        { ValueCheck::Error   };
    }
}
