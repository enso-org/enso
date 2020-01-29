#![allow(missing_docs)]

pub mod stats;

use crate::prelude::*;

pub use crate::data::container::*;
pub use crate::display::symbol::types::*;
pub use crate::display::scene::SymbolId;

use crate::closure;
use crate::control::callback::CallbackHandle;
use crate::control::event_loop::EventLoop;
use crate::data::dirty::traits::*;
use crate::data::dirty;
use crate::debug::stats::Stats;
use crate::display::object::*;
use crate::display::scene::Scene;
use crate::display::shape::text::font::Fonts;
use crate::display::symbol::Symbol;
use crate::system::web;

use crate::display::render::*;

use wasm_bindgen::prelude::Closure;
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;
use web_sys::Performance;
use web_sys::KeyboardEvent;


pub use stats::*;



// ===========================
// === UNSAFE GLOBAL STATE ===
// ===========================

/// The following code is very unsafe and should disappear in the future. We are using it in order
/// to keep the API simple and future-proof. Each `Stage` is associated with HTML Canvas element,
/// and we currently support a single stage only. Although this is very low-priority task, as we
/// would not need it in our use case, it would be nice to have support for it in the library.
/// Supporting multiple stages means, that we would be able to create a symbol, add it to a scene,
/// and then add it to another scene (while disconnecting the previous one). This would require
/// symbol to be associated with different contexts / different buffers depending on the root
/// parent object. Currently this information is hardcoded while object creation and it is obtained
/// from these global variables. By using them we simulate the final API, where symbols do not need
/// to know scene in order to be constructed.

static mut SCENE: Option<Scene> = None;

/// Very unsafe function. Do not use. See documentation of `WORLD` to learn more.
#[allow(unsafe_code)]
pub(crate) fn get_scene() -> Scene {
    unsafe {
        SCENE.as_ref().unwrap_or_else(|| panic!("World not initialized.")).clone_ref()
    }
}

/// Very unsafe function. Do not use. See documentation of `WORLD` to learn more.
#[allow(unsafe_code)]
fn init_global_variables(world:&World) {
    unsafe {
        SCENE = Some(world.rc.borrow().scene.clone_ref());
    }
}



// =================
// === WorldData ===
// =================

// === Definition ===

/// World is the top-level application structure. It used to manage several instances of
/// `Scene`, and there is probability that we will get back to this design in the future.
/// It is responsible for updating the system on every animation frame.
#[derive(Derivative)]
#[derivative(Debug(bound=""))]
pub struct WorldData {
    pub scene         : Scene,
    pub scene_dirty   : SceneDirty,
    pub logger        : Logger,
    pub event_loop    : EventLoop,
    pub performance   : Performance,
    pub start_time    : f32,
    pub time          : Uniform<f32>,
    pub display_mode  : Uniform<i32>,
    pub fonts         : Fonts,
    pub update_handle : Option<CallbackHandle>,
    pub stats         : Stats,
    pub stats_monitor : StatsMonitor,
}


// === Types ===

pub type SceneID    = usize;
pub type SceneDirty = dirty::SharedBool;


// === Callbacks ===

closure! {
fn scene_on_change(dirty:SceneDirty) -> SceneOnChange {
    || dirty.set()
}}


// === Implementation ===

impl WorldData {
    /// Create and initialize new world instance.
    #[allow(clippy::new_ret_no_self)]
    pub fn new<Dom:Str>(dom:Dom) -> World {
        println!("NOTICE! When profiling in Chrome check 'Disable JavaScript Samples' under the \
                  gear icon in the 'Performance' tab. It can drastically slow the rendering.");
        let world          = World::new(Self::new_uninitialized(dom));
        let world_ref      = world.clone_ref();
        with(world.rc.borrow_mut(), |mut data| {
            let update = move |_| {
                world_ref.rc.borrow_mut().run();
            };
            let update_handle   = data.event_loop.add_callback(update);
            data.update_handle  = Some(update_handle);
        });

        // -----------------------------------------------------------------------------------------
        // FIXME[WD]: Hacky way of switching display_mode. To be fixed and refactored out.
        let world_copy = world.clone();
        let c: Closure<dyn Fn(JsValue)> = Closure::wrap(Box::new(move |val| {
            let val = val.unchecked_into::<KeyboardEvent>();
            let key = val.key();
            if      key == "0" { world_copy.rc.borrow_mut().display_mode.set(0) }
            else if key == "1" { world_copy.rc.borrow_mut().display_mode.set(1) }
        }));
        web::document().unwrap().add_event_listener_with_callback
        ("keydown",c.as_ref().unchecked_ref()).unwrap();
        c.forget();
        // -----------------------------------------------------------------------------------------

        world
    }

    /// Create new uninitialized world instance. You should rather not need to
    /// call this function directly.
    fn new_uninitialized<Dom:Str>(dom:Dom) -> Self {
        let stats              = default();
        let logger             = Logger::new("world");
        let scene_logger       = logger.sub("scene");
        let scene_dirty_logger = logger.sub("scene_dirty");
        let scene_dirty        = SceneDirty::new(scene_dirty_logger,());
        let scene_dirty2       = scene_dirty.clone();
        let on_change          = move || {scene_dirty2.set()};
        let scene              = Scene::new(dom,scene_logger,&stats,on_change);
        let variables          = &scene.variables();
        let time               = variables.add_or_panic("time",0.0);
        let display_mode       = variables.add_or_panic("display_mode",0);
        let fonts              = Fonts::new();
        let event_loop         = EventLoop::new();
        let update_handle      = default();
        let stats_monitor      = StatsMonitor::new(&stats);
        let performance        = web::get_performance().unwrap();
        let start_time         = performance.now() as f32;
        let stats_monitor_cp_1 = stats_monitor.clone();
        let stats_monitor_cp_2 = stats_monitor.clone();

        event_loop.set_on_loop_started  (move || { stats_monitor_cp_1.begin(); });
        event_loop.set_on_loop_finished (move || { stats_monitor_cp_2.end();   });
        Self {scene,scene_dirty,logger,event_loop,performance,start_time,time,display_mode
            ,fonts,update_handle,stats,stats_monitor}
    }


    pub fn run(&mut self) {
        let relative_time = self.performance.now() as f32 - self.start_time;
        self.time.set(relative_time);
        self.update();
    }

    /// Check dirty flags and update the state accordingly.
    pub fn update(&mut self) {
        //TODO[WD]: Re-think when should we check the condition (uniform update):
        //          if self.scene_dirty.check_all() {
        group!(self.logger, "Updating.", {
            self.scene_dirty.unset_all();
            let fonts = &mut self.fonts;
            self.scene.update(fonts);
        });
    }

    /// Dispose the world object, cancel all handlers and events.
    pub fn dispose(&mut self) {
        self.update_handle = None;
    }
}

impl Into<DisplayObjectData> for &WorldData {
    fn into(self) -> DisplayObjectData {
        (&self.scene).into()
    }
}

impl Drop for WorldData {
    fn drop(&mut self) {
        self.logger.info("Dropping.");
    }
}



// =============
// === World ===
// =============

// === Definition ===

/// Shared reference to the `World` object.
#[derive(Clone,Debug)]
pub struct World {
    pub rc: Rc<RefCell<WorldData>>,
}

impl World {
    /// Create new shared reference.
    pub fn new(world_data: WorldData) -> Self {
        let rc = Rc::new(RefCell::new(world_data));
        let out = Self {rc};
        init_global_variables(&out);
        out.init_composer();
        out
    }

    /// Cheap clone of the world reference.
    pub fn clone_ref(&self) -> Self {
        self.clone()
    }

    /// Dispose the world object, cancel all handlers and events.
    pub fn dispose(&self) {
        self.rc.borrow_mut().dispose()
    }

    pub fn stats(&self) -> Stats {
        self.rc.borrow().stats.clone_ref()
    }

    pub fn new_symbol(&self) -> Symbol {
        self.rc.borrow().scene.new_symbol()
    }

    /// Run the provided callback on every frame. Returns a `CallbackHandle`,
    /// which when dropped will cancel the callback. If you want the function
    /// to run forever, you can use the `forget` method in the handle.
    pub fn on_frame<F:FnMut(f64)+'static>
    (&self, mut callback:F) -> CallbackHandle {
        let func = move |time_ms| callback(time_ms);
        self.rc.borrow_mut().event_loop.add_callback(func)
    }

    pub fn mod_stats<F:FnOnce(&Stats)>(&self, f:F) {
        f(&self.rc.borrow().stats);
    }

    pub fn render(&self) {
        self.rc.borrow_mut().run();
    }

    pub fn event_loop(&self) -> EventLoop {
        self.rc.borrow().event_loop.clone()
    }

    pub fn scene(&self) -> Scene {
        self.rc.borrow().scene.clone()
    }

    fn init_composer(&self) {
        let root                = &self.display_object();
        let mouse_hover_ids     = self.rc.borrow().scene.mouse_hover_ids();
        let mouse_position      = self.rc.borrow().scene.mouse_position_uniform();
        let mut pixel_read_pass = PixelReadPass::<u32>::new(&mouse_position);
        pixel_read_pass.set_callback(move |v| {
            mouse_hover_ids.set(Vector4::from_iterator(v))
        });
        // TODO: We may want to enable it on weak hardware.
        // pixel_read_pass.set_threshold(1);
        let pipeline = RenderPipeline::new()
            .add(DisplayObjectRenderPass::new(root))
            .add(ScreenRenderPass::new())
            .add(pixel_read_pass);
        self.rc.borrow_mut().scene.set_render_pipeline(pipeline);
    }
}

impl<T> AddMut<T> for World where WorldData: AddMut<T> {
    type Output = <WorldData as AddMut<T>>::Output;
    fn add(&mut self, t:T) -> Self::Output {
        self.rc.borrow_mut().add(t)
    }
}

impl Into<DisplayObjectData> for &World {
    fn into(self) -> DisplayObjectData {
        let data:&WorldData = &self.rc.borrow();
        data.into()
    }
}
