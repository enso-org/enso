mod events;

use crate::prelude::*;

use crate::animation::physics;
use crate::control::callback;
use crate::display::camera::Camera2d;
use crate::display::object::traits::*;
use crate::display::Scene;

use events::NavigatorEvents;
use events::PanEvent;
use events::ZoomEvent;



// =================
// === Constants ===
// =================

/// Default maximum zoom factor (100x). This value can be changed by
/// [`NavigatorModel::set_max_zoom`].
const MAX_ZOOM: f32 = 100.0;
/// Default minimum zoom factor (0.15x).
const MIN_ZOOM: f32 = 0.15;



// ======================
// === NavigatorModel ===
// ======================

/// Navigator enables camera navigation with mouse interactions.
#[derive(Debug)]
pub struct NavigatorModel {
    _events:         NavigatorEvents,
    simulator:       physics::inertia::DynSimulator<Vector3>,
    resize_callback: callback::Handle,
    zoom_speed:      SharedSwitch<f32>,
    pan_speed:       SharedSwitch<f32>,
    /// Indicates whether events handled the navigator should be stopped from propagating further
    /// after being handled by the Navigator.
    disable_events:  Rc<Cell<bool>>,
    max_zoom_limit:  Rc<Cell<Option<f32>>>,
}

impl NavigatorModel {
    pub fn new(scene: &Scene, camera: &Camera2d) -> Self {
        let zoom_speed = Rc::new(Cell::new(Switch::On(10.0 / 1000.0)));
        let pan_speed = Rc::new(Cell::new(Switch::On(1.0)));
        let disable_events = Rc::new(Cell::new(true));
        let max_zoom_limit = default();
        let (simulator, resize_callback, _events) = Self::start_navigator_events(
            scene,
            camera,
            MIN_ZOOM,
            MAX_ZOOM,
            Rc::clone(&zoom_speed),
            Rc::clone(&pan_speed),
            Rc::clone(&disable_events),
            Rc::clone(&max_zoom_limit),
        );
        Self {
            _events,
            simulator,
            resize_callback,
            zoom_speed,
            pan_speed,
            disable_events,
            max_zoom_limit,
        }
    }

    fn create_simulator(camera: &Camera2d) -> physics::inertia::DynSimulator<Vector3> {
        let camera_ref = camera.clone_ref();
        let on_step = Box::new(move |p: Vector3| camera_ref.set_position(p));
        let simulator = physics::inertia::DynSimulator::new(on_step, (), ());
        // FIXME[WD]: This one is emitting camera position in next frame, which is not intended.
        //            Should be fixed when reworking navigator to use FRP events.
        simulator.set_value(camera.position());
        simulator.set_target_value(camera.position());
        simulator
    }

    #[allow(clippy::too_many_arguments)]
    fn start_navigator_events(
        scene: &Scene,
        camera: &Camera2d,
        min_zoom: f32,
        max_zoom: f32,
        zoom_speed: SharedSwitch<f32>,
        pan_speed: SharedSwitch<f32>,
        disable_events: Rc<Cell<bool>>,
        max_zoom_limit: Rc<Cell<Option<f32>>>,
    ) -> (physics::inertia::DynSimulator<Vector3>, callback::Handle, NavigatorEvents) {
        let simulator = Self::create_simulator(camera);
        let panning_callback = f!([scene,camera,simulator,pan_speed] (pan: PanEvent) {
            let fovy_slope                  = camera.half_fovy_slope();
            let distance                    = camera.position().z;
            let distance_to_show_full_ui    = scene.shape().value().height / 2.0 / fovy_slope;
            let pan_speed                   = pan_speed.get().into_on().unwrap_or(0.0);
            let movement_scale_for_distance = distance / distance_to_show_full_ui;
            let diff = pan_speed * Vector3::new(pan.movement.x,pan.movement.y,0.0)*movement_scale_for_distance;
            simulator.update_target_value(|p| p - diff);
        });

        let resize_callback = camera.add_screen_update_callback(
            enclose!((mut simulator,camera) move |_:&Vector2<f32>| {
                let position = camera.position();
                simulator.set_value(position);
                simulator.set_target_value(position);
                simulator.set_velocity(default());
            }),
        );

        let zoom_callback = f!([scene,camera,simulator,max_zoom_limit] (zoom:ZoomEvent) {
            let point       = zoom.focus;
            let normalized  = normalize_point2(point,scene.shape().value().into());
            let normalized  = normalized_to_range2(normalized, -1.0, 1.0);
            let half_height = 1.0;

            // Scale X and Y to compensate aspect and fov.
            let x              = -normalized.x * camera.screen().aspect();
            let y              = -normalized.y;
            let z              = half_height / camera.half_fovy_slope();
            let direction      = Vector3(x,y,z).normalize();
            let mut position   = simulator.target_value();
            let zoom_amount    = zoom.amount * position.z;
            let translation    = direction   * zoom_amount;

            // Usage of min/max prefixes might be confusing here. Remember that the max zoom means
            // the maximum visible size, thus the minimal distance from the camera and vice versa.
            let fovy_slope               = camera.half_fovy_slope();
            let distance_to_show_full_ui = scene.shape().value().height / 2.0 / fovy_slope;
            let max_zoom                 = max_zoom_limit.get().unwrap_or(max_zoom);
            let min_distance =  1.0 / max_zoom * distance_to_show_full_ui + camera.clipping().near;
            let max_distance = (1.0 / min_zoom * distance_to_show_full_ui).min(camera.clipping().far);

            let max_translation_limit = max_distance - position.z;
            let min_translation_limit = min_distance - position.z;
            let too_far               = translation.z > max_translation_limit;
            let too_close             = translation.z < min_translation_limit;
            let limiting_factor       = if too_far   { max_translation_limit / translation.z }
                                        else if too_close { min_translation_limit / translation.z }
                                        else              { 1.0 };
            position                  += translation * limiting_factor;
            simulator.set_target_value(position);
        });
        (
            simulator,
            resize_callback,
            NavigatorEvents::new(
                &scene.mouse.mouse_manager,
                panning_callback,
                zoom_callback,
                zoom_speed,
                pan_speed,
                disable_events,
            ),
        )
    }

    pub fn enable(&self) {
        self.pan_speed.update(|switch| switch.switched(true));
        self.zoom_speed.update(|switch| switch.switched(true));
        self.disable_events.set(true);
    }

    pub fn disable(&self) {
        self.pan_speed.update(|switch| switch.switched(false));
        self.zoom_speed.update(|switch| switch.switched(false));
        self.disable_events.set(false);
    }

    /// Enable or disable the restriction of maximum zoom factor. Enabling it does change camera
    /// zoom immediately, but the restriction will be applied on the next zoom event.
    pub fn set_max_zoom(&self, value: Option<f32>) {
        self.max_zoom_limit.set(value);
    }
}



// =================
// === Navigator ===
// =================

/// Navigator enables camera navigation with mouse interactions.
#[derive(Clone, CloneRef, Debug, Shrinkwrap)]
pub struct Navigator {
    #[shrinkwrap(main_field)]
    model: Rc<NavigatorModel>,
}

impl Navigator {
    pub fn new(scene: &Scene, camera: &Camera2d) -> Self {
        let model = Rc::new(NavigatorModel::new(scene, camera));
        Navigator { model }
    }
}



// =============
// === Utils ===
// =============

type SharedSwitch<T> = Rc<Cell<Switch<T>>>;

/// Normalize a `point` in (0..dimension.x, 0..dimension.y) to (0..1, 0..1).
fn normalize_point2(point: Vector2<f32>, dimension: Vector2<f32>) -> Vector2<f32> {
    Vector2::new(point.x / dimension.x, point.y / dimension.y)
}

/// Transforms a `point` normalized in (0..1, 0..1) to (a..b,a..b).
fn normalized_to_range2(point: Vector2<f32>, a: f32, b: f32) -> Vector2<f32> {
    let width = b - a;
    Vector2::new(point.x * width + a, point.y * width + a)
}
