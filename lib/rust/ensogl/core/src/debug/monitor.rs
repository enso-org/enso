//! This module implements the stats monitor view, which can be visible on the screen in debug mode.

use crate::prelude::*;
use crate::system::web::traits::*;

use crate::debug::stats::StatsData;
use crate::display::world;
use crate::system::web;
use crate::system::web::dom::Shape;
use crate::system::web::JsValue;
use num_traits::cast::AsPrimitive;
use std::collections::VecDeque;
use std::f64;


// =================
// === Constants ===
// =================

const SELECTION_MOVEMENT_SPEED: f64 = 0.5;
const PADDING_LEFT: f64 = 8.0;
const PADDING_TOP: f64 = 8.0;
const FONTS: &str =
    "\"SF Pro Text\",\"SF Pro Icons\",\"Helvetica Neue\",\"Helvetica\",\"Arial\",sans-serif";

// ==============
// === Config ===
// ==============

/// Look and feel configuration for the performance monitor.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct ConfigTemplate<Str, Num> {
    pub background_color:        Str,
    pub label_color_ok:          Str,
    pub label_color_warn:        Str,
    pub label_color_err:         Str,
    pub label_color_ok_selected: Str,
    pub plot_color_ok:           Str,
    pub plot_color_warn:         Str,
    pub plot_color_err:          Str,
    pub plot_background_color:   Str,
    pub plot_bar_size:           Option<Num>,
    pub plot_step_size:          Num,
    pub plot_selection_border:   Num,
    pub plot_selection_width:    Num,
    pub plot_selection_color:    Str,
    pub margin:                  Num,
    pub outer_margin:            Num,
    pub panel_height:            Num,
    pub labels_width:            Num,
    pub results_width:           Num,
    pub sample_count:            usize,
    pub font_size:               Num,
    pub font_vertical_offset:    Num,
}

impl<Str> ConfigTemplate<Str, f64> {
    fn plot_width(&self) -> f64 {
        self.plot_step_size * self.sample_count as f64
    }

    fn plot_x(&self) -> f64 {
        self.margin + self.labels_width + self.margin + self.results_width + self.margin
    }

    fn plot_right_x(&self) -> f64 {
        self.plot_x() + self.plot_width()
    }
}

/// Specialization of the `ConfigTemplate` for users of the library.
pub type Config = ConfigTemplate<String, f64>;

/// Specialization of the `ConfigTemplate` for the usage in JS environment.
pub type SamplerConfig = ConfigTemplate<JsValue, f64>;

// fn dark_theme() -> Config {
//     Config {
//         background_color:      "#222222".into(),
//         label_color_ok:        "#8e939a".into(),
//         label_color_warn:      "#ffba18".into(),
//         label_color_err:       "#eb3941".into(),
//         plot_color_ok:         "#8e939a".into(),
//         plot_color_warn:       "#ffba18".into(),
//         plot_color_err:        "#eb3941".into(),
//         plot_background_color: "#333333".into(),
//         plot_bar_size:         None,
//         plot_step_size:        1.0,
//         margin:                6.0,
//         outer_margin:          4.0,
//         panel_height:          15.0,
//         labels_width:          130.0,
//         results_width:         30.0,
//         font_size:             9.0,
//         font_vertical_offset:  4.0,
//     }
// }

fn light_theme() -> Config {
    Config {
        background_color:        "#f1f1f0".into(),
        label_color_ok:          "#202124".into(),
        label_color_warn:        "#f58025".into(),
        label_color_err:         "#eb3941".into(),
        label_color_ok_selected: "#008cff".into(),
        plot_color_ok:           "#202124".into(),
        plot_color_warn:         "#f58025".into(),
        plot_color_err:          "#eb3941".into(),
        plot_background_color:   "#f1f1f0".into(),
        plot_bar_size:           Some(2.0),
        plot_step_size:          1.0,
        plot_selection_border:   1.0,
        plot_selection_width:    1.0,
        plot_selection_color:    "#008cff20".into(),
        margin:                  6.0,
        outer_margin:            4.0,
        panel_height:            15.0,
        labels_width:            130.0,
        results_width:           30.0,
        sample_count:            100,
        font_size:               9.0,
        font_vertical_offset:    4.0,
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
        let ratio = web::window.device_pixel_ratio();
        SamplerConfig {
            background_color:        (&self.background_color).into(),
            label_color_ok:          (&self.label_color_ok).into(),
            label_color_warn:        (&self.label_color_warn).into(),
            label_color_err:         (&self.label_color_err).into(),
            label_color_ok_selected: (&self.label_color_ok_selected).into(),
            plot_color_ok:           (&self.plot_color_ok).into(),
            plot_color_warn:         (&self.plot_color_warn).into(),
            plot_color_err:          (&self.plot_color_err).into(),
            plot_background_color:   (&self.plot_background_color).into(),
            plot_bar_size:           self.plot_bar_size.map(|t| t * ratio),
            plot_step_size:          self.plot_step_size * ratio,
            plot_selection_border:   self.plot_selection_border * ratio,
            plot_selection_width:    self.plot_selection_width * ratio,
            plot_selection_color:    (&self.plot_selection_color).into(),
            outer_margin:            self.outer_margin * ratio,
            margin:                  self.margin * ratio,
            panel_height:            self.panel_height * ratio,
            labels_width:            self.labels_width * ratio,
            results_width:           self.results_width * ratio,
            sample_count:            self.sample_count,
            font_size:               self.font_size * ratio,
            font_vertical_offset:    self.font_vertical_offset * ratio,
        }
    }
}



// ===========
// === Dom ===
// ===========

/// Dom elements of the monitor. Please note that it uses `Rc` to both implement cheap copy as well
/// as to use `Drop` to clean the HTML when not used anymore.
#[derive(Clone, Debug, Deref)]
pub struct Dom {
    #[deref]
    rc:             Rc<DomData>,
    on_pause_press: Rc<web::Closure<dyn Fn()>>,
    on_play_press:  Rc<web::Closure<dyn Fn()>>,
    on_main_press:  Rc<web::Closure<dyn Fn()>>,
}

/// Internal representation of `Dom`.
#[derive(Debug)]
pub struct DomData {
    config:           Config,
    root:             web::HtmlDivElement,
    details:          web::HtmlDivElement,
    selection_offset: Cell<f64>,
    plot_area:        MainArea,
    control_button:   ControlButton,
}

impl Dom {
    /// Constructor.
    #[allow(clippy::new_without_default)]
    pub fn new(frp: &api::Public, config: &Config, screen_shape: Shape) -> Self {
        let data = DomData::new(config, screen_shape);
        let button = &data.control_button;
        let on_pause_press = web::Closure::<dyn Fn()>::new(f!(frp.pause_data_processing()));
        let on_play_press = web::Closure::<dyn Fn()>::new(f!(frp.resume_data_processing()));
        let on_main_press = web::Closure::<dyn Fn()>::new(f!(frp.on_main_press()));
        let on_pause_press_fn = on_pause_press.as_ref().unchecked_ref();
        let on_play_press_fn = on_play_press.as_ref().unchecked_ref();
        let on_main_press_fn = on_main_press.as_ref().unchecked_ref();
        button.pause.add_event_listener_with_callback("mousedown", on_pause_press_fn).unwrap();
        button.play.add_event_listener_with_callback("mousedown", on_play_press_fn).unwrap();
        data.plot_area.add_event_listener_with_callback("mousedown", on_main_press_fn).unwrap();
        let rc = Rc::new(data);
        let on_pause_press = Rc::new(on_pause_press);
        let on_play_press = Rc::new(on_play_press);
        let on_main_press = Rc::new(on_main_press);
        Self { rc, on_pause_press, on_play_press, on_main_press }
    }
}


impl DomData {
    /// Constructor.
    #[allow(clippy::new_without_default)]
    pub fn new(config: &Config, screen_shape: Shape) -> Self {
        let config = config.clone();
        let root = web::document.create_div_or_panic();
        root.set_class_name("performance-monitor");
        root.set_style_or_warn("position", "absolute");
        root.set_style_or_warn("z-index", "100");
        root.set_style_or_warn("left", format!("{PADDING_LEFT}px"));
        root.set_style_or_warn("top", format!("{PADDING_TOP}px"));

        root.set_style_or_warn("display", "flex");
        root.set_style_or_warn("align-items", "stretch");
        root.set_style_or_warn("font-family", FONTS);
        root.set_style_or_warn("font-size", "12px");



        web::document.body_or_panic().prepend_with_node_1(&root).unwrap();

        let plot_area = MainArea::new(&config);
        root.append_child(&plot_area).unwrap();

        let details = web::document.create_div_or_panic();
        details.set_style_or_warn("height", "100%");
        details.set_style_or_warn("overflow", "scroll");
        details.set_style_or_warn("padding-left", "8px");
        details.set_style_or_warn("padding-right", "8px");
        details.set_style_or_warn("margin-left", "4px");
        details.set_style_or_warn("border-radius", "6px");
        details.set_style_or_warn("border", "2px solid #000000c4");
        details.set_style_or_warn("background", &config.background_color);
        root.append_child(&details).unwrap();

        let control_button = ControlButton::default();
        root.append_child(&control_button).unwrap();

        let selection_offset = default();
        let out = Self { config, root, details, plot_area, control_button, selection_offset };
        out.set_screen_shape(screen_shape);
        out.hide_details();
        out.hide_selection();
        out
    }

    fn hide_selection(&self) {
        self.plot_area.selection.set_style_or_warn("display", "none");
    }

    fn show_selection(&self) {
        self.plot_area.selection.set_style_or_warn("display", "flex");
    }

    pub fn move_selection(&self, offset: f64) -> usize {
        let start_x = self.config.plot_x();
        let end_x = self.config.plot_right_x();
        let width = end_x - start_x - 1.0;
        let offset_normalized = (self.selection_offset.get() + offset).max(0.0).min(width);
        self.selection_offset.set(offset_normalized);
        let offset_rounded = offset_normalized.round();
        let global_offset = start_x + offset_rounded as f64;
        self.plot_area.selection.set_style_or_warn("left", format!("{global_offset}px"));
        offset_rounded as usize
    }



    fn set_screen_shape(&self, shape: Shape) {
        self.root
            .set_style_or_warn("height", format!("{}px", shape.height as f64 - 2.0 * PADDING_TOP));
    }

    fn hide_details(&self) {
        self.details.set_style_or_warn("display", "none");
    }

    fn show_details(&self) {
        self.details.set_style_or_warn("display", "block");
    }

    fn pause(&self) {
        self.control_button.pause();
        self.show_selection();
        self.move_selection(f64::MAX);
    }

    fn resume(&self) {
        self.control_button.resume();
        self.hide_selection();
    }
}

#[derive(Debug, Deref)]
pub struct MainArea {
    #[deref]
    root:         web::HtmlDivElement,
    plot:         web::HtmlCanvasElement,
    plot_context: web::CanvasRenderingContext2d,
    selection:    web::HtmlDivElement,
}

impl MainArea {
    fn new(config: &Config) -> Self {
        let root = web::document.create_div_or_panic();
        root.set_style_or_warn("overflow", "hidden");
        root.set_style_or_warn("border-radius", "6px");
        root.set_style_or_warn("border", "2px solid #000000c4");
        root.set_style_or_warn("position", "relative");

        let plot = web::document.create_canvas_or_panic();
        plot.set_style_or_warn("display", "block");
        root.append_child(&plot).unwrap();


        let plot_context = plot.get_context("2d").unwrap().unwrap();
        let plot_context: web::CanvasRenderingContext2d = plot_context.dyn_into().unwrap();

        let bg_color = &config.background_color;
        let selection_border = config.plot_selection_border;
        let selection_width = config.plot_selection_width + 2.0 * selection_border;
        let selection_left =
            config.plot_right_x() - 0.5 - config.plot_selection_width / 2.0 - selection_border;
        let selection = web::document.create_div_or_panic();
        selection.set_style_or_warn("position", "absolute");
        selection.set_style_or_warn("width", format!("{}px", selection_width));
        selection.set_style_or_warn("height", "100%");
        selection.set_style_or_warn("left", format!("{selection_left}px"));
        selection.set_style_or_warn("display", "flex");
        selection.set_style_or_warn("margin-top", "5px");

        let selection_inner = web::document.create_div_or_panic();
        selection_inner.set_style_or_warn("width", "100%");
        selection_inner.set_style_or_warn("border", format!("{selection_border}px solid red"));
        selection_inner.set_style_or_warn("margin-bottom", "10px");
        selection_inner.set_style_or_warn("position", "relative");
        selection_inner.set_style_or_warn("left", format!("-{}px", selection_border));
        selection.append_child(&selection_inner).unwrap();
        root.append_child(&selection).unwrap();

        Self { root, plot, plot_context, selection }
    }

    fn set_size(&self, width: u32, height: u32) {
        let selection_width = 4;
        let ratio = web::window.device_pixel_ratio();
        self.plot.set_width(width);
        self.plot.set_height(height);
        self.root.set_style_or_warn("display", "flex");
        self.root.set_style_or_warn("width", format!("{}px", width as f64 / ratio));
        self.root.set_style_or_warn("height", format!("{}px", height as f64 / ratio));
    }
}

impl Drop for DomData {
    fn drop(&mut self) {
        self.root.remove()
    }
}

#[derive(Debug, Deref)]
pub struct ControlButton {
    #[deref]
    root:  web::HtmlDivElement,
    pause: web::HtmlDivElement,
    play:  web::HtmlDivElement,
}

impl ControlButton {
    fn new() -> Self {
        let root = web::document.create_div_or_panic();
        root.set_class_name("control-button");
        root.set_style_or_warn("position", "absolute");
        root.set_style_or_warn("top", "8px");
        root.set_style_or_warn("left", "8px");

        let pause_icon = "<svg width=\"16\" height=\"16\">
           <circle cx=\"8\" cy=\"8\" r=\"8\" fill=\"#00000020\" />
           <rect x=\"5\" y=\"4\" width=\"2\" height=\"8\" style=\"fill:#00000080;\" />
           <rect x=\"9\" y=\"4\" width=\"2\" height=\"8\" style=\"fill:#00000080;\" />
        </svg>";

        let play_icon = "<svg width=\"16\" height=\"16\">
           <circle cx=\"8\" cy=\"8\" r=\"8\" fill=\"#00000020\" />
           <polygon points=\"6,4 12,8 6,12\" style=\"fill:#00000080;\" />
        </svg>";

        let pause = web::document.create_div_or_panic();
        pause.set_inner_html(pause_icon);
        root.append_child(&pause).unwrap();

        let play = web::document.create_div_or_panic();
        play.set_inner_html(play_icon);
        play.set_style_or_warn("display", "none");
        root.append_child(&play).unwrap();
        Self { root, pause, play }
    }

    fn pause(&self) {
        self.pause.set_style_or_warn("display", "none");
        self.play.set_style_or_warn("display", "block");
    }

    fn resume(&self) {
        self.pause.set_style_or_warn("display", "block");
        self.play.set_style_or_warn("display", "none");
    }
}

impl Default for ControlButton {
    fn default() -> Self {
        Self::new()
    }
}


// ===============
// === Monitor ===
// ===============

crate::define_endpoints_2! {
    Input {
        pause_data_processing(),
        resume_data_processing(),
        on_main_press(),
    }
    Output {}
}

/// Visual panel showing performance-related statistics.
#[derive(Debug, Clone, CloneRef)]
pub struct Monitor {
    renderer:    Rc<RefCell<Renderer>>,
    frp:         Frp,
    initialized: Rc<Cell<bool>>,
}

impl Default for Monitor {
    fn default() -> Self {
        let frp = Frp::new();
        let mut renderer = Renderer::new(&frp.public);
        renderer.add::<FrameTime>();
        renderer.add::<Fps>();
        renderer.add::<WasmMemory>();
        renderer.add::<GpuMemoryUsage>();
        renderer.add::<DrawCallCount>();
        renderer.add::<DataUploadCount>();
        renderer.add::<DataUploadSize>();
        renderer.add::<BufferCount>();
        renderer.add::<SymbolCount>();
        renderer.add::<ShaderCount>();
        renderer.add::<ShaderCompileCount>();
        renderer.add::<SpriteSystemCount>();
        renderer.add::<SpriteCount>();
        let initialized = default();
        Self { renderer: Rc::new(RefCell::new(renderer)), frp, initialized }
    }
}

impl Monitor {
    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Draw the monitor and update its graphs based on the provided stats data.
    /// Does nothing if the monitor is not visible (see: [`toggle()`]).
    pub fn sample_and_draw(&self, stats: &StatsData) {
        self.renderer.borrow_mut().sample_and_draw(stats);
    }

    /// Toggle the visibility of the monitor.
    pub fn toggle(&self) {
        if !self.initialized.get() {
            self.initialized.set(true);
            let renderer = &self.renderer;
            let config = renderer.borrow().user_config.clone();
            let scene = world::scene();
            let network = &self.frp.network;
            let mouse = &scene.mouse.frp;
            let label_width = config.outer_margin + 2.0 * config.margin + config.labels_width;
            enso_frp::extend! { network
                init <- source_();
                init_shape <- scene.frp.shape.sample(&init);
                screen_shape <- any(&init_shape, &scene.frp.shape);
                pos_on_click <- mouse.position_top_left.sample(&self.frp.input.on_main_press);
                pos_on_label_click <- pos_on_click.filter(move |p| p.x < label_width as f32);
                pos_on_plot_click <- pos_on_click.filter(move |p| p.x > label_width as f32);
                plot_drag <- bool(&mouse.up_primary, &pos_on_plot_click);
                plot_drag_diff <- mouse.translation.gate(&plot_drag);
                eval pos_on_label_click ((p) renderer.borrow_mut().on_label_click(*p));
                eval screen_shape ((s) renderer.borrow_mut().set_screen_shape(*s));
                focus_sample <- plot_drag_diff.map(f!((p) renderer.borrow_mut().on_plot_drag(*p)));
                eval focus_sample ((t) renderer.borrow_mut().set_focus_sample(Some(*t)));
                eval_ self.frp.input.pause_data_processing (renderer.borrow_mut().pause());
                eval_ self.frp.input.resume_data_processing (renderer.borrow_mut().resume());
            }
            init.emit(());
        }
        self.renderer.borrow_mut().toggle();
    }
}


// ================
// === Renderer ===
// ================

/// Code responsible for drawing [`Monitor`]'s data.
#[derive(Debug)]
struct Renderer {
    frp:               api::Public,
    user_config:       Config,
    config:            SamplerConfig,
    screen_shape:      Shape,
    width:             f64,
    height:            f64,
    dom:               Option<Dom>,
    panels:            Vec<Panel>,
    selected_panel:    Option<usize>,
    first_draw:        bool,
    paused:            bool,
    samples:           Vec<StatsData>,
    next_sample_index: usize,
    focus_sample:      Option<usize>,
}

impl Renderer {
    fn new(frp: &api::Public) -> Self {
        let frp = frp.clone_ref();
        let user_config = Config::default();
        let panels = default();
        let screen_shape = default();
        let width = default();
        let height = default();
        let first_draw = true;
        let config = user_config.to_js_config();
        let dom = default();
        let selected_panel = default();
        let paused = default();
        let samples = default();
        let next_sample_index = default();
        let focus_sample = default();
        let mut out = Self {
            frp,
            user_config,
            config,
            screen_shape,
            width,
            height,
            dom,
            panels,
            selected_panel,
            first_draw,
            paused,
            samples,
            next_sample_index,
            focus_sample,
        };
        out.update_config();
        out
    }

    fn pause(&mut self) {
        self.paused = true;
        if let Some(dom) = &self.dom {
            dom.pause();
        }
    }

    fn resume(&mut self) {
        self.paused = false;
        if let Some(dom) = &self.dom {
            dom.resume();
        }
    }

    fn set_screen_shape(&mut self, shape: Shape) {
        if let Some(dom) = &mut self.dom {
            dom.set_screen_shape(shape);
        }
        self.screen_shape = shape;
    }

    /// Add new display element.
    fn add<S: Sampler + Default + 'static>(&mut self) {
        let panel = Panel::new(self.config.clone(), S::default());
        self.panels.push(panel);
        self.resize();
    }

    /// Check whether the monitor is visible.
    fn visible(&self) -> bool {
        self.dom.is_some()
    }

    /// Show the monitor and add it's DOM to the scene.
    fn show(&mut self) {
        if !self.visible() {
            self.first_draw = true;
            self.dom = Some(Dom::new(&self.frp, &self.user_config, self.screen_shape));
            self.resize();
        }
    }

    /// Hides the monitor and remove it's DOM from the scene.
    fn hide(&mut self) {
        self.dom = None;
    }

    /// Toggle the visibility of the monitor.
    fn toggle(&mut self) {
        if self.visible() {
            self.hide();
        } else {
            self.show();
        }
    }

    fn on_label_click(&mut self, position: Vector2) {
        let panel_index = ((position.y as f64
            - PADDING_TOP
            - self.user_config.outer_margin
            - self.user_config.margin / 2.0)
            / (self.user_config.margin + self.user_config.panel_height))
            .floor() as usize;
        let panel_index = panel_index.max(0).min(self.panels.len() - 1);
        self.selected_panel = Some(panel_index);
    }

    fn on_plot_drag(&mut self, offset: Vector2) -> usize {
        if let Some(dom) = &self.dom {
            dom.move_selection((offset.x as f64) * SELECTION_MOVEMENT_SPEED)
        } else {
            0
        }
    }

    fn set_focus_sample(&mut self, index: Option<usize>) {
        self.focus_sample = index;
        if let Some(index) = index {
            let local_index = index + self.next_sample_index;
            let local_index = local_index % self.config.sample_count;
            let sample = self.samples.get(local_index).or_else(|| self.samples.last());
            if let Some(sample) = sample {
                for panel in &self.panels {
                    panel.sample_and_postprocess(sample);
                }
                self.draw_paused(sample);
            } else {
                warn!("No sample to focus on.");
            }
        }
    }

    fn sample_and_draw(&mut self, stats: &StatsData) {
        if !self.paused {
            if self.samples.len() < self.config.sample_count {
                self.samples.push(stats.clone());
            } else {
                self.samples[self.next_sample_index] = stats.clone();
                self.next_sample_index = (self.next_sample_index + 1) % self.config.sample_count;
            }
            if self.visible() {
                for panel in &self.panels {
                    panel.sample_and_postprocess(stats);
                }
                self.draw(stats);
            }
        } else {
            if self.visible() {
                self.draw_paused(stats);
            }
        }
    }

    /// Draw the widget and update all of the graphs.
    fn draw(&mut self, stats: &StatsData) {
        if let Some(dom) = self.dom.clone() {
            if self.first_draw {
                self.first_draw = false;
                self.first_draw(&dom);
            }
            self.shift_plot_area_left(&dom);
            self.clear_old_areas(&dom);
            self.draw_plots(&dom, stats);
        }
    }

    fn draw_paused(&self, stats: &StatsData) {
        if let Some(dom) = self.dom.clone() {
            self.clear_labels_area(&dom);
            self.clear_values_area(&dom);
            self.with_all_panels(&dom, |selected, panel| panel.draw_paused(selected, &dom, stats));
        }
    }

    fn update_config(&mut self) {
        self.config = self.user_config.to_js_config()
    }

    fn resize(&mut self) {
        if let Some(dom) = &self.dom {
            let ratio = web::window.device_pixel_ratio();
            let width = self.config.labels_width
                + self.config.results_width
                + self.config.plot_width()
                + 4.0 * self.config.margin
                + self.config.outer_margin; // no outer_margin on the left side.
            let mut height = self.config.outer_margin;
            for _panel in &self.panels {
                height += self.config.margin + self.config.panel_height;
            }
            height += self.config.margin;
            height += self.config.outer_margin;
            self.width = width;
            self.height = height;
            let scaled_width = width / ratio;
            // dom.root.set_style_or_warn("width", format!("{scaled_width}px"));
            // let width = 1000.0;
            let u_width = width as u32;
            let u_height = height as u32;
            dom.plot_area.set_size(u_width, u_height);
        }
    }

    fn shift_plot_area_left(&mut self, dom: &Dom) {
        let width = self.width;
        let height = self.height;
        let shift = -(self.config.plot_step_size);
        dom.plot_area
            .plot_context
            .draw_image_with_html_canvas_element_and_sw_and_sh_and_dx_and_dy_and_dw_and_dh(
                &dom.plot_area.plot,
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

    fn clear_old_areas(&mut self, dom: &Dom) {
        self.clear_labels_area(dom);
        self.clear_values_area(dom);
        self.clear_newest_plot_value_area(dom);
    }

    fn clear_labels_area(&self, dom: &Dom) {
        let width = self.config.margin + self.config.labels_width;
        dom.plot_area.plot_context.set_fill_style(&self.config.background_color);
        dom.plot_area.plot_context.fill_rect(0.0, 0.0, width, self.height);
    }

    fn clear_values_area(&self, dom: &Dom) {
        let offset = self.config.margin + self.config.labels_width;
        let width = self.config.margin + self.config.results_width + self.config.margin;
        dom.plot_area.plot_context.set_fill_style(&self.config.background_color);
        dom.plot_area.plot_context.fill_rect(offset, 0.0, width, self.height);
    }

    fn clear_newest_plot_value_area(&mut self, dom: &Dom) {
        let step = self.config.plot_step_size;
        dom.plot_area.plot_context.fill_rect(self.width - step, 0.0, step, self.height);
    }

    fn draw_plots(&mut self, dom: &Dom, stats: &StatsData) {
        self.with_all_panels(dom, |selected, panel| panel.draw(selected, dom, stats));
    }

    fn first_draw(&self, dom: &Dom) {
        dom.plot_area.plot_context.set_fill_style(&self.config.background_color);
        dom.plot_area.plot_context.fill_rect(0.0, 0.0, self.width, self.height);
        self.with_all_panels(dom, |_, panel| panel.first_draw(dom));
    }

    fn with_all_panels<F: Fn(bool, &Panel)>(&self, dom: &Dom, f: F) {
        let mut total_off = self.config.outer_margin;
        dom.plot_area.plot_context.translate(0.0, total_off).unwrap();
        for (panel_index, panel) in self.panels.iter().enumerate() {
            let off = self.config.margin;
            dom.plot_area.plot_context.translate(0.0, off).unwrap();
            total_off += off;
            let selected = self.selected_panel == Some(panel_index);
            f(selected, panel);
            let off = self.config.panel_height;
            dom.plot_area.plot_context.translate(0.0, off).unwrap();
            total_off += off;
        }
        dom.plot_area.plot_context.translate(0.0, -total_off).unwrap();
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
    pub fn draw(&self, selected: bool, dom: &Dom, stats: &StatsData) {
        self.rc.borrow_mut().draw(selected, dom, stats)
    }

    pub fn draw_paused(&self, selected: bool, dom: &Dom, stats: &StatsData) {
        self.rc.borrow_mut().draw_paused(selected, dom, stats)
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

    fn details(&self) -> Option<Box<dyn FnMut(&StatsData) -> &[&'static str]>> {
        None
    }

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
    norm_value:  f64,
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
        let norm_value = default();
        let value_check = default();
        let sampler = Box::new(sampler);
        let precision = sampler.precision();
        Self {
            label,
            config,
            min_value,
            max_value,
            value,
            norm_value,
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
    pub fn draw(&mut self, selected: bool, dom: &Dom, stats: &StatsData) {
        self.draw_label(dom, selected);
        self.draw_value(dom);
        self.draw_plot_update(dom);
        self.draw_details(dom, selected, stats);
    }

    pub fn draw_paused(&mut self, selected: bool, dom: &Dom, stats: &StatsData) {
        self.draw_label(dom, selected);
        self.draw_value(dom);
        self.draw_details(dom, selected, stats);
    }

    fn first_draw(&mut self, dom: &Dom) {
        dom.plot_area.plot_context.set_fill_style(&self.config.plot_background_color);
        dom.plot_area.plot_context.fill_rect(
            0.0,
            0.0,
            self.config.plot_width(),
            self.config.panel_height,
        );
    }

    fn with_pen<T>(&mut self, dom: &Dom, offset: f64, f: impl FnOnce(&mut Self) -> T) -> T {
        dom.plot_area.plot_context.translate(offset, 0.0).unwrap();
        let out = f(self);
        dom.plot_area.plot_context.translate(-offset, 0.0).unwrap();
        out
    }

    fn with_pen_at_label<T>(&mut self, dom: &Dom, f: impl FnOnce(&mut Self) -> T) -> T {
        self.with_pen(dom, self.config.margin, f)
    }

    fn with_pen_at_value<T>(&mut self, dom: &Dom, f: impl FnOnce(&mut Self) -> T) -> T {
        self.with_pen_at_label(dom, |this| {
            this.with_pen(dom, this.config.labels_width + this.config.margin, f)
        })
    }

    fn with_pen_at_plot<T>(&mut self, dom: &Dom, f: impl FnOnce(&mut Self) -> T) -> T {
        self.with_pen_at_value(dom, |this| {
            this.with_pen(dom, this.config.results_width + this.config.margin, f)
        })
    }

    fn with_pen_at_new_plot_part<T>(&mut self, dom: &Dom, f: impl FnOnce(&mut Self) -> T) -> T {
        self.with_pen_at_plot(dom, |this| {
            this.with_pen(dom, this.config.plot_width() - this.config.plot_step_size, f)
        })
    }

    fn init_draw(&mut self, dom: &Dom, selected: bool) {
        self.draw_label(dom, selected);
        self.draw_value(dom);
    }

    fn draw_label(&mut self, dom: &Dom, selected: bool) {
        self.with_pen_at_label(dom, |this| {
            let y_pos = this.config.panel_height - this.config.font_vertical_offset;
            dom.plot_area
                .plot_context
                .set_font(&format!("bold {}px {}", this.config.font_size, FONTS));
            dom.plot_area.plot_context.set_text_align("right");
            let color = if selected {
                &this.config.label_color_ok_selected
            } else {
                &this.config.label_color_ok
            };
            dom.plot_area.plot_context.set_fill_style(color);
            dom.plot_area
                .plot_context
                .fill_text(&this.label, this.config.labels_width, y_pos)
                .unwrap();
        })
    }

    /// Draw the plot text value.
    fn draw_value(&mut self, dom: &Dom) {
        self.with_pen_at_value(dom, |this| {
            let display_value = format!("{1:.0$}", this.precision, this.value);
            let y_pos = this.config.panel_height - this.config.font_vertical_offset;
            let color = match this.value_check {
                ValueCheck::Correct => &this.config.label_color_ok,
                ValueCheck::Warning => &this.config.label_color_warn,
                ValueCheck::Error => &this.config.label_color_err,
            };
            dom.plot_area.plot_context.set_fill_style(color);
            dom.plot_area
                .plot_context
                .fill_text(&display_value, this.config.results_width, y_pos)
                .unwrap();
        })
    }

    /// Draw a single plot point. As the plots shift left on every frame, this function only updates
    /// the most recent plot value.
    fn draw_plot_update(&mut self, dom: &Dom) {
        self.with_pen_at_new_plot_part(dom, |this| {
            dom.plot_area.plot_context.set_fill_style(&this.config.plot_background_color);
            dom.plot_area.plot_context.fill_rect(
                0.0,
                0.0,
                this.config.plot_step_size,
                this.config.panel_height,
            );
            let value_height = this.norm_value * this.config.panel_height;
            let y_pos = this.config.panel_height - value_height;
            let bar_height = this.config.plot_bar_size.unwrap_or(value_height);
            let color = match this.value_check {
                ValueCheck::Correct => &this.config.plot_color_ok,
                ValueCheck::Warning => &this.config.plot_color_warn,
                ValueCheck::Error => &this.config.plot_color_err,
            };
            dom.plot_area.plot_context.set_fill_style(color);
            dom.plot_area.plot_context.fill_rect(
                0.0,
                y_pos,
                this.config.plot_step_size,
                bar_height,
            );
        })
    }

    fn draw_details(&mut self, dom: &Dom, selected: bool, stats: &StatsData) {
        if selected {
            if let Some(mut details) = self.sampler.details() {
                dom.show_details();
                let ul = web::document.create_html_element_or_panic("ul");
                ul.set_style_or_warn("padding-inline-start", "10px");
                for i in 0..20 {
                    for entry in details(stats) {
                        let li = web::document.create_element_or_panic("li");
                        let a = web::document.create_html_element_or_panic("a");
                        a.set_attribute("href", &format!("enso://enso-source:{}", entry)).ok();
                        a.set_inner_html(&entry);
                        a.set_style_or_warn("color", "inherit");
                        a.set_style_or_warn("text-decoration", "none");
                        li.append_child(&a).unwrap();
                        ul.append_child(&li).unwrap();
                    }
                }
                dom.details.set_text_content(None);
                dom.details.append_child(&ul).unwrap();
            } else {
                dom.hide_details();
            }
        }
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
    ( $label:tt, $name:ident, $stats_expr:expr, $details: expr, $warn_threshold:expr, $err_threshold:expr
    , $precision:expr, $value_divisor:expr) => {
        stats_sampler!(
            $label,
            $name,
            $stats_expr,
            $details,
            $warn_threshold,
            $err_threshold,
            $precision,
            $value_divisor,
            None
        );
    };

    ( $label:tt, $name:ident, $stats_expr:expr, $details: expr, $warn_threshold:expr, $err_threshold:expr
    , $precision:expr, $value_divisor:expr, $max_value:expr) => {
        /// Sampler implementation.
        #[derive(Copy, Clone, Debug, Default)]
        pub struct $name {}

        impl Sampler for $name {
            fn label(&self) -> &str {
                $label
            }
            fn value(&self, stats: &StatsData) -> f64 {
                let raw_value: f64 = $stats_expr(stats).as_();
                raw_value / $value_divisor
            }
            fn details(&self) -> Option<Box<dyn FnMut(&StatsData) -> &[&'static str]>> {
                $details
            }
            fn min_size(&self) -> Option<f64> {
                Some($warn_threshold)
            }
            fn precision(&self) -> usize {
                $precision
            }
            fn check(&self, stats: &StatsData) -> ValueCheck {
                let value = self.value(&stats);
                ValueCheck::from_threshold($warn_threshold, $err_threshold, value)
            }
            fn max_value(&self) -> Option<f64> {
                $max_value
            }
        }
    };
}

const MB: f64 = (1024 * 1024) as f64;

stats_sampler!(
    "Frames per second",
    Fps,
    (|s: &StatsData| s.fps),
    None,
    55.0,
    25.0,
    2,
    1.0,
    Some(60.0)
);
stats_sampler!(
    "Frame time (ms)",
    FrameTime,
    (|s: &StatsData| s.frame_time),
    None,
    1000.0 / 55.0,
    1000.0 / 25.0,
    2,
    1.0
);
stats_sampler!(
    "WASM memory usage (Mb)",
    WasmMemory,
    (|s: &StatsData| s.wasm_memory_usage),
    None,
    50.0,
    100.0,
    2,
    MB
);
stats_sampler!(
    "GPU memory usage (Mb)",
    GpuMemoryUsage,
    (|s: &StatsData| s.gpu_memory_usage),
    None,
    100.0,
    500.0,
    2,
    MB
);
stats_sampler!(
    "Draw call count",
    DrawCallCount,
    (|s: &StatsData| s.draw_calls.len()),
    Some(Box::new(|s: &StatsData| &s.draw_calls)),
    100.0,
    500.0,
    0,
    1.0
);
stats_sampler!(
    "Buffer count",
    BufferCount,
    (|s: &StatsData| s.buffer_count),
    None,
    100.0,
    500.0,
    0,
    1.0
);
stats_sampler!(
    "Data upload count",
    DataUploadCount,
    (|s: &StatsData| s.data_upload_count),
    None,
    100.0,
    500.0,
    0,
    1.0
);
stats_sampler!(
    "Data upload size (Mb)",
    DataUploadSize,
    (|s: &StatsData| s.data_upload_size),
    None,
    1.0,
    10.0,
    2,
    MB
);
stats_sampler!(
    "Sprite system count",
    SpriteSystemCount,
    (|s: &StatsData| s.sprite_system_count),
    None,
    100.0,
    500.0,
    0,
    1.0
);
stats_sampler!(
    "Symbol count",
    SymbolCount,
    (|s: &StatsData| s.symbol_count),
    None,
    100.0,
    500.0,
    0,
    1.0
);
stats_sampler!(
    "Sprite count",
    SpriteCount,
    (|s: &StatsData| s.sprite_count),
    None,
    100_000.0,
    500_000.0,
    0,
    1.0
);
stats_sampler!(
    "Shader count",
    ShaderCount,
    (|s: &StatsData| s.shader_count),
    None,
    100.0,
    500.0,
    0,
    1.0
);
stats_sampler!(
    "Shader compile count",
    ShaderCompileCount,
    (|s: &StatsData| s.shader_compile_count),
    None,
    10.0,
    100.0,
    0,
    1.0
);

// FIXME[WD]: To be fixed in #183406745
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use enso_prelude::*;
//
//     use crate::debug::stats::StatsWithTimeProvider;
//     use enso_web::TimeProvider;
//     use std::ops::AddAssign;
//
//
//     // === MockTimeProvider ===
//
//     #[derive(Default, Clone)]
//     struct MockTimeProvider {
//         t: Rc<RefCell<f64>>,
//     }
//
//     impl TimeProvider for MockTimeProvider {
//         fn now(&self) -> f64 {
//             *self.t.borrow()
//         }
//     }
//
//     impl AddAssign<f64> for MockTimeProvider {
//         fn add_assign(&mut self, dt: f64) {
//             *self.t.borrow_mut() += dt;
//         }
//     }
//
//
//     // === TestSampler ===
//
//     struct TestSampler<S> {
//         stats:   StatsWithTimeProvider<MockTimeProvider>,
//         sampler: S,
//         t:       MockTimeProvider,
//     }
//
//     impl<S: Sampler + Default> Default for TestSampler<S> {
//         fn default() -> Self {
//             let t: MockTimeProvider = default();
//             let stats = StatsWithTimeProvider::new(t.clone());
//             let sampler = default();
//             Self { stats, sampler, t }
//         }
//     }
//
//     const STAT_VALUE_COMPARISON_PRECISION: f64 = 0.001;
//
//     macro_rules! test_and_advance_frame {
//         ($test:expr, $expected_value:expr, $expected_check:path
//         ; next: $frame_time:expr, $post_frame_delay:expr) => {
//             let prev_frame_stats = $test.stats.begin_frame(Duration(*$test.t.t.borrow() as f32));
//             if let Some(prev_frame_stats) = prev_frame_stats {
//                 let tested_value = $test.sampler.value(&prev_frame_stats);
//                 let tested_check = $test.sampler.check(&prev_frame_stats);
//                 assert_approx_eq!(tested_value, $expected_value,
// STAT_VALUE_COMPARISON_PRECISION);                 let mismatch_msg = format!(
//                     "Stat check was expected to return: " $expected_check;?
//                     ", but got: " tested_check;? " instead.");
//                 assert!(matches!(tested_check, $expected_check), "{}", mismatch_msg);
//             } else {
//                 assert!(false,
//                     "Expected previous frame's stats to be returned by begin_frame(), \
//                     but got none.");
//             }
//             $test.t += $frame_time;
//             $test.stats.end_frame();
//             $test.t += $post_frame_delay;
//         };
//
//         ($test:expr, None; next: $frame_time:expr, $post_frame_delay:expr) => {
//             let prev_frame_stats = $test.stats.begin_frame(Duration(*$test.t.t.borrow() as f32));
//             let mismatch_msg = format!(
//                 "Expected no stats to be returned by begin_frame(), but got: "
//                 prev_frame_stats;? " instead.");
//             assert!(matches!(prev_frame_stats, None), "{}", mismatch_msg);
//             $test.t += $frame_time;
//             $test.stats.end_frame();
//             $test.t += $post_frame_delay;
//         };
//     }
//
//
//     // === Tests ===
//
//     #[test]
//     fn frame_time() {
//         // Note: 60 FPS means there's 16.6(6) ms budget for 1 frame. The test will be written
// under         // assumption we're trying to be around this FPS.
//
//         let mut test: TestSampler<FrameTime> = default();
//
//         // Frame 1: simulate we managed to complete the work in 10ms, and then we wait 6ms before
//         // starting next frame.
//         //
//         // Note: we expect no stats data to be returned before the 1st frame was started.
//         test_and_advance_frame!(test, None; next: 10.0, 6.0);
//
//         // Frame 2: simulate we managed to complete the work in 5ms, and then we wait 11ms before
//         // starting next frame.
//         test_and_advance_frame!(test, 10.0, ValueCheck::Correct; next: 5.0, 11.0);
//
//         // Frame 3: simulate we went over the budget of 16.6(6) ms, at 30ms. No extra delay
//         // afterwards before starting next frame.
//         test_and_advance_frame!(test, 5.0, ValueCheck::Correct; next: 30.0, 0.0);
//
//         // Frame 4: simulate a really slow frame (1000ms), crossing the configured error
// threshold.         test_and_advance_frame!(test, 30.0, ValueCheck::Warning; next: 1000.0, 0.0);
//
//         // For the final test, we're not interested in the next frame, so we're using some dummy
//         // values for it.
//         let dummy = 0.0;
//         test_and_advance_frame!(test, 1000.0, ValueCheck::Error; next: dummy, dummy);
//     }
//
//     #[test]
//     fn fps() {
//         // Note: 60 FPS means there's 16.6(6) ms budget for 1 frame. The test will be written
// under         // assumption we're trying to be around this FPS.
//
//         let mut test: TestSampler<Fps> = default();
//
//         // Frame 1: simulate we managed to complete the work in 10ms, and then we wait 6ms before
//         // starting next frame.
//         //
//         // Note: we expect no stats data to be returned before the 1st frame was started.
//         test_and_advance_frame!(test, None; next: 10.0, 6.0);
//
//         // Frame 2: simulate we managed to complete the work in 5ms, and then we wait 11.67ms
// before         // starting next frame.
//         //
//         // Note: FPS takes into account delays between frames - for example, if a frame took only
//         // 1ms, but the next frame will not be started immediately (will be started only e.g.
// 15ms         // later), we cannot show FPS only based the 1ms duration of the frame - it would
// not be         // true, as the subsequent delay means less frames per second will be rendered in
// reality.         //
//         // Previous frame+delay was 16.0 ms; we'd fit 62.5 such frames in 1s.
//         test_and_advance_frame!(test, 62.5, ValueCheck::Correct; next: 5.0, 11.67);
//
//         // Frame 3: simulate we went over the budget of 16.6(6) ms, at 20ms. No extra delay
//         // afterwards before starting next frame.
//         // Previous frame+delay was 16.67 ms; we'd fit ~59.988 such frames in 1s.
//         test_and_advance_frame!(test, 59.988, ValueCheck::Correct; next: 20.0, 0.0);
//
//         // Frame 4: simulate a really slow frame (1000ms), crossing the FPS error threshold.
//         // Previous frame+delay was 20.0 ms; we'd fit 50.0 such frames in 1s.
//         test_and_advance_frame!(test, 50.0, ValueCheck::Warning; next: 1000.0, 0.0);
//
//         // For the final test, we're not interested in the next frame, so we're using some dummy
//         // values for it.
//         // Previous frame+delay was 1000.0 ms; we'd fit 1 such frame in 1s.
//         let dummy = 0.0;
//         test_and_advance_frame!(test, 1.0, ValueCheck::Error; next: dummy, dummy);
//     }
// }
