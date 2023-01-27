//! This module implements `World`, the main object responsible for handling what you see on the
//! screen.

use crate::control::callback::traits::*;
use crate::data::dirty::traits::*;
use crate::display::render::*;
use crate::prelude::*;
use crate::system::web::traits::*;
use wasm_bindgen::prelude::*;

use crate::animation;
use crate::application::command::FrpNetworkProvider;
use crate::control::callback;
use crate::data::dirty;
use crate::debug;
use crate::debug::stats::Stats;
use crate::debug::stats::StatsData;
use crate::display;
use crate::display::garbage;
use crate::display::render;
use crate::display::render::passes::SymbolsRenderPass;
use crate::display::scene::DomPath;
use crate::display::scene::Scene;
use crate::display::shape::primitive::glsl;
use crate::display::symbol::registry::RunMode;
use crate::display::symbol::registry::SymbolRegistry;
use crate::system::gpu::shader;
use crate::system::js;
use crate::system::web;

use enso_types::unit2::Duration;
use web::prelude::Closure;
use web::JsCast;
use web::JsValue;


// ==============
// === Export ===
// ==============

pub use crate::display::symbol::types::*;



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
pub fn with_context<T>(f: impl Fn(&SymbolRegistry) -> T) -> T {
    CONTEXT.with_borrow(|t| f(t.as_ref().unwrap()))
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



// =====================
// === Static Shapes ===
// =====================

type ShapeCons = Box<dyn Fn() -> Box<dyn crate::gui::component::AnyShapeView>>;

thread_local! {
    /// All shapes defined with the `shape!` macro. They will be populated on the beginning of
    /// program execution, before the `main` function is called.
    pub static SHAPES_DEFINITIONS: RefCell<Vec<ShapeCons>> = default();
}



// ===========================
// === Precompiled Shaders ===
// ===========================

/// A precompiled shader definition. It contains the main function body for the vertex and fragment
/// shaders.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub struct PrecompiledShader {
    pub vertex:   String,
    pub fragment: String,
}

thread_local! {
    /// List of all precompiled shaders. They are registered here before main entry point is run by
    /// the EnsoGL runner in JavaScript.
    pub static PRECOMPILED_SHADERS: RefCell<HashMap<String, PrecompiledShader>> = default();
}

/// Registers in JS a closure to acquire non-optimized shaders code and to set back optimized
/// shaders code.
#[before_main]
pub fn register_get_and_set_shaders_fns() {
    let js_app = js::app_or_panic();
    let closure = Closure::new(|| {
        let map = gather_shaders();
        let js_map = web::Map::new();
        for (key, code) in map {
            let value = web::Object::new();
            web::Reflect::set(&value, &"vertex".into(), &code.vertex.into()).unwrap();
            web::Reflect::set(&value, &"fragment".into(), &code.fragment.into()).unwrap();
            js_map.set(&key.into(), &value);
        }
        js_map.into()
    });
    js_app.register_get_shaders_rust_fn(&closure);
    mem::forget(closure);

    let closure = Closure::new(|value: JsValue| {
        if extract_shaders_from_js(value).err().is_some() {
            warn!("Internal error. Downloaded shaders are provided in a wrong format.")
        }
    });
    js_app.register_set_shaders_rust_fn(&closure);
    mem::forget(closure);
}

/// Extract optimized shaders code from the JS value.
fn extract_shaders_from_js(value: JsValue) -> Result<(), JsValue> {
    let map = value.dyn_into::<web::Map>()?;
    for opt_entry in map.entries() {
        let entry = opt_entry?.dyn_into::<web::Array>()?;
        let key: String = entry.get(0).dyn_into::<web::JsString>()?.into();
        let value = entry.get(1).dyn_into::<web::Object>()?;
        let vertex_field = web::Reflect::get(&value, &"vertex".into())?;
        let fragment_field = web::Reflect::get(&value, &"fragment".into())?;
        let vertex: String = vertex_field.dyn_into::<web::JsString>()?.into();
        let fragment: String = fragment_field.dyn_into::<web::JsString>()?.into();
        let precompiled_shader = PrecompiledShader { vertex, fragment };
        debug!("Registering precompiled shaders for '{key}'.");
        PRECOMPILED_SHADERS.with_borrow_mut(move |map| {
            map.insert(key, precompiled_shader);
        });
    }
    Ok(())
}

fn gather_shaders() -> HashMap<&'static str, shader::Code> {
    with_context(|t| t.run_mode.set(RunMode::ShaderExtraction));
    let mut map = HashMap::new();
    SHAPES_DEFINITIONS.with(|shapes| {
        for shape_cons in shapes.borrow().iter() {
            let shape = shape_cons();
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
    time:         Uniform<f32>,
    display_mode: Uniform<i32>,
}

impl Uniforms {
    /// Constructor.
    pub fn new(scope: &UniformScope) -> Self {
        let time = scope.add_or_panic("time", 0.0);
        let display_mode = scope.add_or_panic("display_mode", 0);
        Self { time, display_mode }
    }
}


// =========================
// === Metadata Profiler ===
// =========================

profiler::metadata_logger!("RenderStats", log_render_stats(StatsData));



// =============
// === World ===
// =============

/// The root object for EnsoGL scenes.
#[derive(Clone, CloneRef, Debug, Default)]
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

impl display::Object for World {
    fn display_object(&self) -> &display::object::Instance {
        self.default_scene.display_object()
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
        let frp = Frp::new();
        let data = WorldData::new(&frp.private.output);
        let on_frame_start = animation::on_frame_start();
        let on_before_rendering = animation::on_before_rendering();
        let network = frp.network();
        crate::frp::extend! {network
            eval on_frame_start ((t) data.run_stats(*t));
            eval on_before_rendering ((t) data.run_next_frame(*t));
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
    pub prev_frame_stats: callback::registry::Ref1<StatsData>,
    pub before_frame:     callback::registry::Copy1<animation::TimeInfo>,
    pub after_frame:      callback::registry::Copy1<animation::TimeInfo>,
}



// ======================
// === Scene Instance ===
// ======================

thread_local! {
    /// Global scene reference. See the [`scene`] function to learn more.
    pub static SCENE: RefCell<Option<Scene>> = RefCell::new(None);
}

/// Get reference to [`Scene`] instance. This should always succeed. Scenes are managed by [`World`]
/// and should be instantiated before any callback is run.
pub fn scene() -> Scene {
    SCENE.with_borrow(|t| t.clone().unwrap())
}



// =================
// === WorldData ===
// =================

/// The data kept by the [`World`].
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct WorldData {
    #[deref]
    frp: api::private::Output,
    pub default_scene: Scene,
    scene_dirty: dirty::SharedBool,
    uniforms: Uniforms,
    display_mode: Rc<Cell<glsl::codes::DisplayModes>>,
    stats: Stats,
    stats_monitor: debug::monitor::Monitor,
    stats_draw_handle: callback::Handle,
    pub on: Callbacks,
    debug_hotkeys_handle: Rc<RefCell<Option<web::EventListenerHandle>>>,
    update_themes_handle: callback::Handle,
    garbage_collector: garbage::Collector,
    emit_measurements_handle: Rc<RefCell<Option<callback::Handle>>>,
}

impl WorldData {
    /// Create and initialize new world instance.
    pub fn new(frp: &api::private::Output) -> Self {
        let frp = frp.clone_ref();
        let stats = Stats::new(web::window.performance_or_panic());
        let stats_monitor = debug::monitor::Monitor::new();
        let on = Callbacks::default();
        let scene_dirty = dirty::SharedBool::new(());
        let on_change = f!(scene_dirty.set());
        let display_mode = Rc::<Cell<glsl::codes::DisplayModes>>::default();
        let default_scene = Scene::new(&stats, on_change, &display_mode);
        let uniforms = Uniforms::new(&default_scene.variables);
        let debug_hotkeys_handle = default();
        let garbage_collector = default();
        let stats_draw_handle = on.prev_frame_stats.add(f!([stats_monitor] (stats: &StatsData) {
            stats_monitor.sample_and_draw(stats);
            log_render_stats(*stats)
        }));
        let themes = with_context(|t| t.theme_manager.clone_ref());
        let update_themes_handle = on.before_frame.add(f_!(themes.update()));
        let emit_measurements_handle = default();
        SCENE.with_borrow_mut(|t| *t = Some(default_scene.clone_ref()));

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
            stats_draw_handle,
            update_themes_handle,
            garbage_collector,
            emit_measurements_handle,
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
        let display_mode_uniform = self.uniforms.display_mode.clone_ref();
        let emit_measurements_handle = self.emit_measurements_handle.clone_ref();
        let closure: Closure<dyn Fn(JsValue)> = Closure::new(move |val: JsValue| {
            let event = val.unchecked_into::<web::KeyboardEvent>();
            let digit_prefix = "Digit";
            if event.alt_key() && event.ctrl_key() {
                let key = event.code();
                if key == "Backquote" {
                    stats_monitor.toggle()
                } else if key == "KeyP" {
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
        let mouse_hover_rgba = self.default_scene.mouse.hover_rgba.clone_ref();
        let garbage_collector = &self.garbage_collector;
        let mut pixel_read_pass = PixelReadPass::<u8>::new(&self.default_scene.mouse.position);
        pixel_read_pass.set_callback(f!([garbage_collector](v) {
            mouse_hover_rgba.set(Vector4::from_iterator(v.iter().map(|value| *value as u32)));
            garbage_collector.pixel_updated();
        }));
        pixel_read_pass.set_sync_callback(f!(garbage_collector.pixel_synced()));
        // TODO: We may want to enable it on weak hardware.
        // pixel_read_pass.set_threshold(1);
        let pipeline = render::Pipeline::new()
            .add(SymbolsRenderPass::new(&self.default_scene.layers))
            .add(ScreenRenderPass::new())
            .add(pixel_read_pass);
        self.default_scene.renderer.set_pipeline(pipeline);
    }

    fn run_stats(&self, time: Duration) {
        let previous_frame_stats = self.stats.begin_frame(time);
        if let Some(stats) = previous_frame_stats {
            self.on.prev_frame_stats.run_all(&stats);
        }
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

    /// Perform to the next frame with the provided time information.
    ///
    /// Please note that the provided time information from the [`requestAnimationFrame`] JS
    /// function is more precise than time obtained from the [`window.performance().now()`] one.
    /// Follow this link to learn more:
    /// https://stackoverflow.com/questions/38360250/requestanimationframe-now-vs-performance-now-time-discrepancy.
    #[profile(Objective)]
    pub fn run_next_frame(&self, time: animation::TimeInfo) {
        self.on.before_frame.run_all(time);
        self.uniforms.time.set(time.since_animation_loop_started.unchecked_raw());
        self.scene_dirty.unset_all();
        let update_status = self.default_scene.update(time);
        self.garbage_collector.mouse_events_handled();
        self.default_scene.render(update_status);
        self.on.after_frame.run_all(time);
        self.stats.end_frame();
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
