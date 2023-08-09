//! This module implements `World`, the main object responsible for handling what you see on the
//! screen.

use crate::control::callback::traits::*;
use crate::data::dirty::traits::*;
use crate::display::render::*;
use crate::prelude::*;
use wasm_bindgen::prelude::*;

use crate::animation;
use crate::application::command::FrpNetworkProvider;
use crate::control::callback;
use crate::data::dirty;
use crate::debug;
use crate::debug::stats::Stats;
use crate::display;
use crate::display::garbage;
use crate::display::render;
use crate::display::render::cache_shapes::CacheShapesPass;
use crate::display::render::passes::SymbolsRenderPass;
use crate::display::scene::DomPath;
use crate::display::scene::Scene;
use crate::display::scene::UpdateStatus;
use crate::display::shape::primitive::glsl;
use crate::display::symbol::registry::RunMode;
use crate::display::symbol::registry::SymbolRegistry;
use crate::system::gpu::context::profiler::Results;
use crate::system::gpu::shader;
use crate::system::web;

use enso_types::unit2::Duration;
use web::prelude::Closure;
use web::JsCast;
use web::JsValue;


// ==============
// === Export ===
// ==============

pub use crate::display::symbol::types::*;



// =================
// === Constants ===
// =================

/// The number of frames that need to be rendered slow/fast before the resolution mode is switched
/// to low/high one.
const FRAME_COUNT_CHECK_FOR_SWITCHING_RESOLUTION_MODE: usize = 8;

/// The time threshold for switching to low resolution mode. It will be used on platforms which
/// allow proper GPU time measurements (currently only Chrome).
const LOW_RESOLUTION_MODE_GPU_TIME_THRESHOLD_MS: f64 = 1000.0 / 30.0;

/// The FPS threshold for switching to low resolution mode. It will be used on platforms which do
/// not allow proper GPU time measurements (currently all browsers but Chrome).
const LOW_RESOLUTION_MODE_FPS_THRESHOLD: usize = 25;



// ===============
// === Context ===
// ===============

thread_local! {
    /// A global object containing registry of all symbols. In the future, it will be extended to
    /// contain buffers and other elements that are now kept in `Rc<RefCell<...>>` in different
    /// places.
    pub static CONTEXT: RefCell<Option<SymbolRegistry>> = RefCell::new(None);
}

/// Perform an action with a reference to the global context.
pub fn with_context<T>(f: impl FnOnce(&SymbolRegistry) -> T) -> T {
    CONTEXT.with_borrow(move |t| f(t.as_ref().unwrap()))
}

/// Initialize global state (set stack trace size, logger output, etc).
#[before_main(0)]
pub fn init() {
    init_global();
}

/// Initialize global context.
#[before_main(1)]
pub fn wasm_init_context() {
    init_context();
}

fn init_context() {
    let initialized = CONTEXT.with(|t| t.borrow().is_some());
    if !initialized {
        CONTEXT.with_borrow_mut(|t| *t = Some(SymbolRegistry::mk()));
    }
}



// =========================
// === Shape Definitions ===
// =========================

/// A constructor of view of some specific shape.
pub type ShapeCons = Box<dyn Fn() -> Box<dyn crate::gui::component::AnyShapeView>>;

/// The definition of shapes created with the `shape!` macro.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct ShapeDefinition {
    /// Location in the source code that the shape was defined.
    pub definition_path: &'static str,
    /// A constructor of single shape view.
    #[derivative(Debug = "ignore")]
    pub cons:            ShapeCons,
}

impl ShapeDefinition {
    /// Return `true` if it is possible that this shape is used by the main application.
    pub fn is_main_application_shape(&self) -> bool {
        // Shapes defined in `examples` directories are not used in the main application.
        !self.definition_path.contains("/examples/")
    }
}

/// The definition of shapes created with the `cached_shape!` macro.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct CachedShapeDefinition {
    /// The size of the texture's space occupied by the shape.
    pub size: Vector2,
    /// A pointer to function setting the global information about position in the cached shapes
    /// texture, usually a concrete implementation of
    /// [`set_position_in_texture`](crate::display::shape::system::cached::CachedShape::set_position_in_texture)
    #[derivative(Debug = "ignore")]
    pub position_on_texture_setter: Box<dyn Fn(Vector2)>,
    /// A pointer to function creating a shape instance properly placed for cached texture
    /// rendering, usually a concrete implementation of
    /// [`create_view_for_texture`](crate::display::shape::system::cached::CachedShape::create_view_for_texture)
    #[derivative(Debug = "ignore")]
    pub for_texture_constructor: ShapeCons,
}

thread_local! {
    /// All shapes defined with the `shape!` macro. They will be populated on the beginning of
    /// program execution, before the `main` function is called.
    pub static SHAPES_DEFINITIONS: RefCell<Vec<ShapeDefinition>> = default();

    /// All shapes defined with the `cached_shape!` macro. They will be populated on the beginning
    /// of program execution, before the `main` function is called.
    pub static CACHED_SHAPES_DEFINITIONS: RefCell<Vec<CachedShapeDefinition>> = default();
}



// ===========================
// === Precompiled Shaders ===
// ===========================

/// A precompiled shader definition. It contains the main function body for the vertex and fragment
/// shaders.
#[derive(Clone, Debug, Deref)]
#[allow(missing_docs)]
pub struct PrecompiledShader(shader::Code);

thread_local! {
    /// List of all precompiled shaders. They are registered here before main entry point is run by
    /// the EnsoGL runner in JavaScript.
    pub static PRECOMPILED_SHADERS: RefCell<HashMap<String, PrecompiledShader>> = default();
}

/// Set optimized shader code.
pub fn set_shader_code(key: String, vertex: String, fragment: String) {
    let vertex = strip_instance_declarations(&vertex);
    let precompiled_shader = PrecompiledShader(shader::Code { vertex, fragment });
    debug!("Registering precompiled shaders for '{key}'.");
    PRECOMPILED_SHADERS.with_borrow_mut(move |map| {
        map.insert(key, precompiled_shader);
    });
}

/// Remove initial instance variable declarations.
///
/// When pre-compiling vertex shaders we don't have full information about inputs, and instead treat
/// all inputs as instance variables. After the program has been optimized, we need to strip these
/// imprecise declarations so they don't conflict with the real declarations we add when building
/// the shader.
fn strip_instance_declarations(input: &str) -> String {
    let mut code = String::with_capacity(input.len());
    let mut preamble_ended = false;
    let input_prefix = display::symbol::gpu::shader::builder::INPUT_PREFIX;
    let vertex_prefix = display::symbol::gpu::shader::builder::VERTEX_PREFIX;
    for line in input.lines() {
        // Skip lines as long as they match the `input_foo = vertex_foo` pattern.
        if !preamble_ended {
            let trimmed = line.trim_start();
            if trimmed.is_empty() {
                continue;
            }
            let mut parts = trimmed.split(' ');
            if let Some(part) = parts.next() && part.starts_with(input_prefix)
                    && let Some(part) = parts.next() && part == "="
                    && let Some(part) = parts.next() && part.starts_with(vertex_prefix)
                    && let None = parts.next() {
                continue;
            }
        }
        preamble_ended = true;
        code.push_str(line);
        code.push('\n');
    }
    code
}

/// Collect the un-optimized shader code for all the shapes used by the application.
pub fn gather_shaders() -> HashMap<&'static str, shader::Code> {
    with_context(|t| t.run_mode.set(RunMode::ShaderExtraction));
    let mut map = HashMap::new();
    SHAPES_DEFINITIONS.with(|shapes| {
        for shape in shapes.borrow().iter() {
            let shape = (shape.cons)();
            let path = shape.definition_path();
            let code = shape.abstract_shader_code_in_glsl_310();
            map.insert(path, code);
        }
    });
    with_context(|t| t.run_mode.set(RunMode::Normal));
    map
}



// ================
// === Uniforms ===
// ================

/// Uniforms managed by world.
#[derive(Clone, CloneRef, Debug)]
pub struct Uniforms {
    time: Uniform<f32>,
}

impl Uniforms {
    /// Constructor.
    pub fn new(scope: &UniformScope) -> Self {
        let time = scope.add_or_panic("time", 0.0);
        Self { time }
    }
}



// =============
// === World ===
// =============

/// The root object for EnsoGL scenes.
#[derive(Clone, CloneRef, Debug, Default, display::Object)]
pub struct World {
    rc: Rc<WorldDataWithLoop>,
}

impl World {
    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Constructor modifier. Displays the default scene in the provided path.
    pub fn displayed_in(self, dom: impl DomPath) -> Self {
        self.default_scene.display_in(dom);
        self
    }

    /// Keeps the world alive even when all references are dropped. Use only if you want to keep one
    /// instance of the world forever.
    pub fn keep_alive_forever(&self) {
        mem::forget(self.clone_ref())
    }
}

impl Deref for World {
    type Target = WorldDataWithLoop;
    fn deref(&self) -> &Self::Target {
        &self.rc
    }
}

impl<'t> From<&'t World> for &'t Scene {
    fn from(world: &'t World) -> Self {
        &world.default_scene
    }
}



// ===========
// === FRP ===
// ===========

crate::define_endpoints_2! {
    Output {
        after_rendering(),
    }
}



// =========================
// === WorldDataWithLoop ===
// =========================

/// Main loop closure type.
pub type MainLoop = animation::Loop;

/// World data with a main loop implementation.
///
/// # Main Loop Performance
/// Any code repeated on each iteration of the Main Loop (each "frame") must be written with a high
/// care for performance. Any changes that has a chance of negatively impacting the constant
/// overhead of the main loop needs *explicit* explanation, review, and acceptance *at the design
/// stage* of the proposed new implementation, from performance perspective, with an
/// explicit note of the fact of Main Loop impact.
///
/// Rationale: the "Main Loop" contains the code comprising a GUI rendering "frame" (term
/// originating from a "still frame" term in filmmaking). The speed at which the Main Loop executes
/// directly translates to the perceived performance of the GUI, and the FPS (frames per second)
/// metric, impacting Users' experience with the application.
#[derive(Debug)]
pub struct WorldDataWithLoop {
    frp:  Frp,
    data: WorldData,
}

impl WorldDataWithLoop {
    /// Constructor.
    pub fn new() -> Self {
        // Context is initialized automatically before main entry point starts in WASM. We are
        // performing manual initialization for native tests to work correctly.
        init_context();
        display::shape::primitive::system::cached::initialize_cached_shape_positions_in_texture();
        let frp = Frp::new();
        let data = WorldData::new(&frp.private.output);
        let on_frame_start = animation::on_frame_start();
        let on_before_layout = animation::on_before_layout();
        let on_before_rendering = animation::on_before_rendering();
        let network = frp.network();
        crate::frp::extend! {network
            eval on_frame_start ([data] (t) {
                data.stats.calculate_prev_frame_stats(*t);
                let gpu_perf_results = data.default_scene.on_frame_start();
                data.update_stats(*t, gpu_perf_results)
            });
            layout_update <- on_before_layout.map(f!((t) data.run_next_frame_layout(*t)));
            _eval <- on_before_rendering.map2(&layout_update,
                f!((t, early) data.run_next_frame_rendering(*t, *early))
            );
        }

        Self { frp, data }
    }
}

impl Default for WorldDataWithLoop {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for WorldDataWithLoop {
    type Target = WorldData;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}



// =================
// === Callbacks ===
// =================

// FIXME[WD]: move these callbacks to the FRP interface one day.
/// Callbacks that are run during rendering of the frame.
#[derive(Clone, CloneRef, Debug, Default)]
#[allow(missing_docs)]
pub struct Callbacks {
    pub before_frame: callback::registry::Copy1<animation::TimeInfo>,
    pub after_frame:  callback::registry::Copy1<animation::TimeInfo>,
}



// ======================
// === Scene Instance ===
// ======================

thread_local! {
    /// Global scene reference. See the [`scene`] function to learn more.
    pub static SCENE: RefCell<Option<Scene>> = RefCell::new(None);
}

/// Get reference to [`Scene`] instance.
///
/// # Panics
///
/// It will panic if there is no [`World`] instance.
pub fn scene() -> Scene {
    SCENE.with_borrow(|t| t.clone().unwrap())
}



// =================
// === WorldData ===
// =================

/// The data kept by the [`World`].
#[derive(Debug, Clone, CloneRef, Deref, display::Object)]
#[allow(missing_docs)]
pub struct WorldData {
    #[deref]
    frp: api::private::Output,
    #[display_object]
    pub default_scene: Scene,
    scene_dirty: dirty::SharedBool,
    uniforms: Uniforms,
    display_mode: Rc<Cell<glsl::codes::DisplayModes>>,
    stats: Stats,
    stats_monitor: debug::monitor::Monitor,
    pub on: Callbacks,
    debug_hotkeys_handle: Rc<RefCell<Option<web::EventListenerHandle>>>,
    update_themes_handle: callback::Handle,
    garbage_collector: garbage::Collector,
    emit_measurements_handle: Rc<RefCell<Option<callback::Handle>>>,
    pixel_read_pass_threshold: Rc<RefCell<Weak<Cell<usize>>>>,
    slow_frame_count: Rc<Cell<usize>>,
    fast_frame_count: Rc<Cell<usize>>,
}

impl WorldData {
    /// Create and initialize new world instance.
    pub fn new(frp: &api::private::Output) -> Self {
        let frp = frp.clone_ref();
        let stats = with_context(|context| context.stats.clone_ref());
        let stats_monitor = debug::monitor::Monitor::new();
        let on = Callbacks::default();
        let scene_dirty = dirty::SharedBool::new(());
        let on_change = f!(scene_dirty.set());
        let display_mode = Rc::<Cell<glsl::codes::DisplayModes>>::default();
        let default_scene = Scene::new(&stats, on_change, &display_mode);
        let uniforms = Uniforms::new(&default_scene.variables);
        let debug_hotkeys_handle = default();
        let garbage_collector = default();
        let themes = with_context(|t| t.theme_manager.clone_ref());
        let update_themes_handle = on.before_frame.add(f_!(themes.update()));
        let emit_measurements_handle = default();
        SCENE.set(Some(default_scene.clone_ref()));
        let pixel_read_pass_threshold = default();
        let slow_frame_count = default();
        let fast_frame_count = default();

        Self {
            frp,
            default_scene,
            scene_dirty,
            uniforms,
            display_mode,
            stats,
            on,
            debug_hotkeys_handle,
            stats_monitor,
            update_themes_handle,
            garbage_collector,
            emit_measurements_handle,
            pixel_read_pass_threshold,
            slow_frame_count,
            fast_frame_count,
        }
        .init()
    }

    fn init(self) -> Self {
        self.init_environment();
        self.init_composer();
        self.init_debug_hotkeys();
        self
    }

    fn init_environment(&self) {
        init_global();
    }

    fn init_debug_hotkeys(&self) {
        let stats_monitor = self.stats_monitor.clone_ref();
        let display_mode = self.display_mode.clone_ref();
        let display_mode_uniform = with_context(|ctx| ctx.display_mode.clone_ref());
        let emit_measurements_handle = self.emit_measurements_handle.clone_ref();
        let closure: Closure<dyn Fn(JsValue)> = Closure::new(move |val: JsValue| {
            let event = val.unchecked_into::<web::KeyboardEvent>();
            let digit_prefix = "Digit";
            if event.alt_key() && event.ctrl_key() {
                let key = event.code();
                if key == "Backquote" {
                    stats_monitor.toggle()
                } else if key == "KeyO" {
                    if event.shift_key() {
                        let forwarding_incrementally = emit_measurements_handle.borrow().is_some();
                        // If we are submitting the data continuously, the hotkey is redundant.
                        let enable_hotkey = !forwarding_incrementally;
                        if enable_hotkey {
                            profiler::interval_stream()
                                .for_each(|interval| log_measurement(&interval));
                        }
                    } else {
                        enso_debug_api::save_profile(&profiler::internal::get_log());
                    }
                } else if key == "KeyQ" {
                    enso_debug_api::save_profile(&profiler::internal::get_log());
                    enso_debug_api::LifecycleController::new().map(|api| api.quit());
                } else if key == "KeyG" {
                    enso_debug_api::open_gpu_debug_info();
                } else if key.starts_with(digit_prefix) {
                    let code_value = key.trim_start_matches(digit_prefix).parse().unwrap_or(0);
                    if let Some(mode) = glsl::codes::DisplayModes::from_value(code_value) {
                        warn!("Setting display mode to {:?}.", mode.name());
                        display_mode.set(mode);
                    } else {
                        warn!("Invalid display mode code: {code_value}.");
                    }
                    display_mode_uniform.set(code_value as i32);
                }
            }
        });
        let handle = web::add_event_listener_with_bool(&web::window, "keydown", closure, true);
        *self.debug_hotkeys_handle.borrow_mut() = Some(handle);
    }

    fn init_composer(&self) {
        let pointer_target_encoded = self.default_scene.mouse.pointer_target_encoded.clone_ref();
        let garbage_collector = &self.garbage_collector;
        let mut pixel_read_pass = PixelReadPass::<u8>::new(&self.default_scene.mouse.position);
        pixel_read_pass.set_callback(f!([garbage_collector](v) {
            pointer_target_encoded.set(Vector4::from_iterator(v.iter().map(|value| *value as u32)));
            garbage_collector.pixel_updated();
        }));
        pixel_read_pass.set_sync_callback(f!(garbage_collector.pixel_synced()));
        *self.pixel_read_pass_threshold.borrow_mut() = pixel_read_pass.get_threshold().downgrade();
        let pipeline = render::Pipeline::new()
            .add(SymbolsRenderPass::new(&self.default_scene.layers))
            .add(ScreenRenderPass::new())
            .add(pixel_read_pass)
            .add(CacheShapesPass::new());
        self.default_scene.renderer.set_pipeline(pipeline);
    }

    fn update_stats(&self, _time: Duration, gpu_perf_results: Option<Vec<Results>>) {
        {
            if let Some(gpu_perf_results) = &gpu_perf_results {
                for result in gpu_perf_results {
                    // If run in the first frame, the results can be reported with frame offset
                    // being 0. In such a case, we are ignoring it.
                    if result.frame_offset > 0 {
                        // The monitor is not updated yet, so the last sample is from the previous
                        // frame.
                        let frame_offset = result.frame_offset - 1;
                        if frame_offset == 0 {
                            let stats_data = &mut self.stats.borrow_mut().stats_data;
                            stats_data.gpu_time = Some(result.total);
                            stats_data.cpu_and_idle_time =
                                Some(stats_data.frame_time - result.total);
                        } else {
                            // The last sampler stored in monitor is from 2 frames ago, as the last
                            // frame stats are not submitted yet.
                            let sampler_offset = result.frame_offset - 2;
                            self.stats_monitor.with_last_nth_sample(sampler_offset, |sample| {
                                sample.gpu_time = Some(result.total);
                                sample.cpu_and_idle_time = Some(sample.frame_time - result.total);
                            });
                            self.stats_monitor.redraw_historical_data(sampler_offset);
                        }
                    }
                }
            }

            let stats_borrowed = self.stats.borrow();
            let stats = &stats_borrowed.stats_data;
            self.stats_monitor.sample_and_draw(stats);

            let slow_frame = if let Some(gpu_perf_results) = gpu_perf_results {
                gpu_perf_results.last().map(|t| t.total > LOW_RESOLUTION_MODE_GPU_TIME_THRESHOLD_MS)
            } else {
                Some(stats.fps < LOW_RESOLUTION_MODE_FPS_THRESHOLD as f64)
            };

            if let Some(slow_frame) = slow_frame {
                if slow_frame {
                    self.fast_frame_count.set(0);
                    self.slow_frame_count.modify(|t| *t += 1);
                    let count = self.slow_frame_count.get();
                    if count == FRAME_COUNT_CHECK_FOR_SWITCHING_RESOLUTION_MODE {
                        SCENE.with_borrow(|t| t.as_ref().unwrap().low_resolution_mode(true));
                    }
                } else {
                    self.slow_frame_count.set(0);
                    self.fast_frame_count.modify(|t| *t += 1);
                    let count = self.fast_frame_count.get();
                    if count == FRAME_COUNT_CHECK_FOR_SWITCHING_RESOLUTION_MODE {
                        SCENE.with_borrow(|t| t.as_ref().unwrap().low_resolution_mode(false));
                    }
                }
            }
        }
        self.stats.reset_per_frame_statistics();
    }

    /// Begin incrementally submitting [`profiler`] data to the User Timing web API.
    ///
    /// This will submit all measurements logged so far, and then periodically submit any new
    /// measurements in batches.
    pub fn connect_profiler_to_user_timing(&self) {
        let mut handle = self.emit_measurements_handle.borrow_mut();
        if handle.is_none() {
            let mut intervals = profiler::interval_stream();
            *handle = Some(self.on.after_frame.add(move |_| {
                for interval in &mut intervals {
                    log_measurement(&interval);
                }
            }));
        }
    }

    /// Perform to the layout step of next frame simulation with the provided time information.
    /// See [`Scene::update_layout`] for information about actions performed in this step.
    ///
    /// Please note that the provided time information from the [`requestAnimationFrame`] JS
    /// function is more precise than time obtained from the [`window.performance().now()`] one.
    /// Follow this link to learn more:
    /// https://stackoverflow.com/questions/38360250/requestanimationframe-now-vs-performance-now-time-discrepancy.
    #[profile(Objective)]
    pub fn run_next_frame_layout(&self, time: animation::TimeInfo) -> UpdateStatus {
        self.on.before_frame.run_all(time);
        self.uniforms.time.set(time.since_animation_loop_started.unchecked_raw());
        self.scene_dirty.unset_all();
        self.default_scene.update_layout(time)
    }

    /// perform to the rendering step of next frame simulation with the provided time information.
    /// See [`Scene::update_rendering`] for information about actions performed in this step.
    ///
    /// Apart from the scene late update, this function also performs garbage collection and actual
    /// rendering of the scene using updated GPU buffers.
    #[profile(Objective)]
    pub fn run_next_frame_rendering(&self, time: animation::TimeInfo, early_status: UpdateStatus) {
        let update_status = self.default_scene.update_rendering(time, early_status);
        self.garbage_collector.mouse_events_handled();
        self.default_scene.render(update_status);
        self.on.after_frame.run_all(time);
        self.after_rendering.emit(());
    }

    /// Pass object for garbage collection.
    ///
    /// The collector is designed to handle EnsoGL component's FRP networks and models, but any
    /// structure with static timeline may be put. For details, see docs of [`garbage::Collector`].
    #[profile(Debug)]
    pub fn collect_garbage<T: 'static>(&self, object: T) {
        self.garbage_collector.collect(object);
    }

    /// Immediately drop the garbage.
    ///
    /// May be used to resolve dependence cycles if garbage keeps reference to [`World`].
    pub fn force_garbage_drop(&self) {
        self.garbage_collector.force_garbage_drop()
    }

    /// Set the maximum frequency at which the pointer location will be checked, in terms of number
    /// of frames per check.
    pub fn set_pixel_read_period(&self, period: usize) {
        if let Some(setter) = self.pixel_read_pass_threshold.borrow().upgrade() {
            // Convert from minimum number of frames per pixel-read pass to
            // minimum number of frames between each frame that does a pixel-read pass.
            let threshold = period.saturating_sub(1);
            info!("Setting pixel read pass threshold to {threshold}.");
            setter.set(threshold);
        }
    }
}

impl Drop for WorldData {
    fn drop(&mut self) {
        SCENE.set(None);
    }
}

mod js_bindings {
    use super::*;
    #[wasm_bindgen(inline_js = r#"
export function log_measurement(label, start, end) {
    window.performance.measure(label, { "start": start, "end": end })
}
"#)]
    extern "C" {
        #[allow(unsafe_code)]
        pub fn log_measurement(msg: String, start: f64, end: f64);
    }
}

fn log_measurement(interval: &profiler::Interval) {
    let label = interval.label().to_owned();
    let start = interval.start();
    let end = interval.end();
    js_bindings::log_measurement(label, start, end);
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_compilation_in_test_mode() {
        let _world = World::new().displayed_in("root");
        let _scene = &_world.default_scene;
    }
}
