//! This module implements `World`, the main object responsible for handling what you see on the
//! screen.

use crate::control::callback::traits::*;
use crate::data::dirty::traits::*;
use crate::display::render::*;
use crate::prelude::*;
use crate::system::web::traits::*;

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
use crate::system::web;

use enso_types::unit2::Duration;
use web::prelude::Closure;
use web::JsCast;
use web::JsValue;


// ==============
// === Export ===
// ==============

pub use crate::display::symbol::types::*;



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
    frp:                  api::private::Output,
    pub default_scene:    Scene,
    scene_dirty:          dirty::SharedBool,
    uniforms:             Uniforms,
    stats:                Stats,
    stats_monitor:        debug::monitor::Monitor,
    stats_draw_handle:    callback::Handle,
    pub on:               Callbacks,
    debug_hotkeys_handle: Rc<RefCell<Option<web::EventListenerHandle>>>,
    garbage_collector:    garbage::Collector,
}

impl WorldData {
    /// Create and initialize new world instance.
    pub fn new(frp: &api::private::Output) -> Self {
        let frp = frp.clone_ref();
        let stats = debug::stats::Stats::new(web::window.performance_or_panic());
        let stats_monitor = debug::monitor::Monitor::new();
        let on = Callbacks::default();
        let scene_dirty = dirty::SharedBool::new(());
        let on_change = enclose!((scene_dirty) move || scene_dirty.set());
        let default_scene = Scene::new(&stats, on_change);
        let uniforms = Uniforms::new(&default_scene.variables);
        let debug_hotkeys_handle = default();
        let garbage_collector = default();
        let stats_draw_handle = on.prev_frame_stats.add(f!([stats_monitor] (stats: &StatsData) {
            stats_monitor.sample_and_draw(stats);
            log_render_stats(*stats)
        }));

        SCENE.with_borrow_mut(|t| *t = Some(default_scene.clone_ref()));

        Self {
            frp,
            default_scene,
            scene_dirty,
            uniforms,
            stats,
            on,
            debug_hotkeys_handle,
            stats_monitor,
            stats_draw_handle,
            garbage_collector,
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
        let display_mode = self.uniforms.display_mode.clone_ref();
        let closure: Closure<dyn Fn(JsValue)> = Closure::new(move |val: JsValue| {
            let event = val.unchecked_into::<web::KeyboardEvent>();
            if event.alt_key() && event.ctrl_key() {
                let key = event.code();
                if key == "Backquote" {
                    stats_monitor.toggle()
                } else if key == "Digit0" {
                    display_mode.set(0)
                } else if key == "Digit1" {
                    display_mode.set(1)
                } else if key == "Digit2" {
                    display_mode.set(2)
                } else if key == "KeyP" {
                    enso_debug_api::save_profile(&profiler::internal::take_log());
                } else if key == "KeyQ" {
                    enso_debug_api::save_profile(&profiler::internal::take_log());
                    enso_debug_api::LifecycleController::new().map(|api| api.quit());
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
        let logger = Logger::new("renderer");
        let pipeline = render::Pipeline::new()
            .add(SymbolsRenderPass::new(
                &logger,
                self.default_scene.symbols(),
                &self.default_scene.layers,
            ))
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
