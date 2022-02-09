//! This module implements `World`, the main object responsible for handling what you see on the
//! screen.

pub mod stats;

pub use crate::display::symbol::types::*;

use crate::prelude::*;

use crate::animation;
use crate::control::callback;
use crate::data::dirty;
use crate::data::dirty::traits::*;
use crate::debug::stats::Stats;
use crate::debug::stats::StatsData;
use crate::display;
use crate::display::render;
use crate::display::render::passes::SymbolsRenderPass;
use crate::display::render::*;
use crate::display::scene::Scene;
use crate::system::web;

use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;



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



// =============
// === World ===
// =============

/// World is the top-level application structure. It used to manage several instances of
/// `Scene`, and there is probability that we will get back to this design in the future.
/// It is responsible for updating the system on every animation frame.
#[derive(Clone, CloneRef, Debug)]
pub struct World {
    logger:             Logger,
    scene:              Scene,
    scene_dirty:        dirty::SharedBool,
    main_loop:          animation::DynamicLoop,
    uniforms:           Uniforms,
    stats:              Stats,
    stats_monitor:      stats::Monitor,
    main_loop_frame:    callback::Handle,
    on_before_frame:    callback::SharedRegistryMut1<animation::TimeInfo>,
    on_after_frame:     callback::SharedRegistryMut1<animation::TimeInfo>,
    on_stats_available: callback::SharedRegistryMut1<StatsData>,
}

impl World {
    /// Create and initialize new world instance.
    #[allow(clippy::new_ret_no_self)]
    pub fn new(dom: &web_sys::HtmlElement) -> World {
        let logger = Logger::new("world");
        let stats = default();
        let scene_dirty = dirty::SharedBool::new(Logger::new_sub(&logger, "scene_dirty"), ());
        let on_change = enclose!((scene_dirty) move || scene_dirty.set());
        let scene = Scene::new(dom, &logger, &stats, on_change);
        let uniforms = Uniforms::new(&scene.variables);
        let main_loop = animation::DynamicLoop::new();
        let stats_monitor = stats::Monitor::new(&stats);
        let on_before_frame = <callback::SharedRegistryMut1<animation::TimeInfo>>::new();
        let on_after_frame = <callback::SharedRegistryMut1<animation::TimeInfo>>::new();
        let on_stats_available = <callback::SharedRegistryMut1<StatsData>>::new();
        let main_loop_frame = main_loop.on_frame(
            f!([stats_monitor,on_before_frame,on_after_frame,on_stats_available,uniforms,scene_dirty,scene]
            (t:animation::TimeInfo) {
                // Note [Main Loop Performance]

                stats_monitor.begin();
                on_before_frame.run_all(&t);
                if let Some(stats) = stats_monitor.previous_frame_stats() {
                    on_stats_available.run_all(&stats);
                }

                uniforms.time.set(t.local);
                scene_dirty.unset_all();
                scene.update(t);
                scene.renderer.run();

                on_after_frame.run_all(&t);
                stats_monitor.end();
            }),
        );

        Self {
            logger,
            scene,
            scene_dirty,
            main_loop,
            uniforms,
            stats,
            stats_monitor,
            main_loop_frame,
            on_before_frame,
            on_after_frame,
            on_stats_available,
        }
        .init()
    }

    // Note [Main Loop Performance]
    // ============================
    // Any code repeated on each iteration of the Main Loop (each "frame") must be written with
    // high care for performance. Any changes that has a chance of negatively impacting the
    // constant overhead of the main loop needs *explicit* explanation, review, and acceptance *at
    // design stage* of the proposed new implementation, from performance perspective, with an
    // explicit note of the fact of Main Loop impact.
    //
    // Rationale: the "Main Loop" contains the code comprising a GUI rendering "frame" (term
    // originating from a "still frame" term in filmmaking). The speed at which the Main Loop
    // executes directly translates to the perceived performance of the GUI, and the FPS (frames
    // per second) metric, impacting Users' experience with the application.

    fn init(self) -> Self {
        self.init_composer();
        self.init_hotkeys();
        self
    }

    fn init_hotkeys(&self) {
        // -----------------------------------------------------------------------------------------
        // FIXME[WD]: Hacky way of switching display_mode. To be fixed and refactored out.
        let stats_monitor = self.stats_monitor.clone_ref();
        let display_mode = self.uniforms.display_mode.clone_ref();
        let c: Closure<dyn Fn(JsValue)> = Closure::wrap(Box::new(move |val| {
            let event = val.unchecked_into::<web_sys::KeyboardEvent>();
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
                }
            }
        }));
        web::window()
            .add_event_listener_with_callback_and_bool("keydown", c.as_ref().unchecked_ref(), true)
            .unwrap();
        c.forget();
        // -----------------------------------------------------------------------------------------
    }

    fn init_composer(&self) {
        let mouse_hover_ids = self.scene.mouse.hover_ids.clone_ref();
        let mut pixel_read_pass = PixelReadPass::<u8>::new(&self.scene.mouse.position);
        pixel_read_pass.set_callback(move |v| {
            mouse_hover_ids.set(Vector4::from_iterator(v.iter().map(|value| *value as u32)))
        });
        // TODO: We may want to enable it on weak hardware.
        // pixel_read_pass.set_threshold(1);
        let logger = &self.scene.renderer.logger;
        let pipeline = render::Pipeline::new()
            .add(SymbolsRenderPass::new(
                &logger,
                &self.scene,
                self.scene.symbols(),
                &self.scene.layers,
            ))
            .add(ScreenRenderPass::new(&self.scene))
            .add(pixel_read_pass);
        self.scene.renderer.set_pipeline(pipeline);
    }

    /// Scene accessor.
    pub fn scene(&self) -> &Scene {
        &self.scene
    }

    /// Register a callback which should be run before each animation frame.
    pub fn on_before_frame<F: FnMut(animation::TimeInfo) + 'static>(
        &self,
        mut callback: F,
    ) -> callback::Handle {
        self.on_before_frame.add(move |time: &animation::TimeInfo| callback(*time))
    }

    /// Register a callback which should be run after each animation frame.
    pub fn on_after_frame<F: FnMut(animation::TimeInfo) + 'static>(
        &self,
        mut callback: F,
    ) -> callback::Handle {
        self.on_before_frame.add(move |time: &animation::TimeInfo| callback(*time))
    }

    /// Register a callback which should be run after each animation frame.
    pub fn on_frame<F: FnMut(animation::TimeInfo) + 'static>(
        &self,
        mut callback: F,
    ) -> callback::Handle {
        self.on_after_frame.add(move |time: &animation::TimeInfo| callback(*time))
    }

    /// Register a callback which should be run when runtime stats of the previous animation frame
    /// are available.
    pub fn on_stats_available<F: FnMut(StatsData) + 'static>(
        &self,
        mut callback: F,
    ) -> callback::Handle {
        self.on_stats_available.add(move |stats: &StatsData| callback(*stats))
    }

    /// Keeps the world alive even when all references are dropped. Use only if you want to keep one
    /// instance of the world forever.
    pub fn keep_alive_forever(&self) {
        mem::forget(self.clone_ref())
    }
}

impl display::Object for World {
    fn display_object(&self) -> &display::object::Instance {
        self.scene.display_object()
    }
}

impl<'t> From<&'t World> for &'t Scene {
    fn from(world: &'t World) -> Self {
        &world.scene
    }
}
