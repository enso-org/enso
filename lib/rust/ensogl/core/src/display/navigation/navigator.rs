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
}

impl NavigatorModel {
    pub fn new(scene: &Scene, camera: &Camera2d) -> Self {
        let zoom_speed = Rc::new(Cell::new(Switch::On(10.0 / 1000.0)));
        let pan_speed = Rc::new(Cell::new(Switch::On(1.0)));
        let min_zoom = 10.0;
        let max_zoom = 10000.0;
        let disable_events = Rc::new(Cell::new(true));
        let (simulator, resize_callback, _events) = Self::start_navigator_events(
            scene,
            camera,
            min_zoom,
            max_zoom,
            Rc::clone(&zoom_speed),
            Rc::clone(&pan_speed),
            Rc::clone(&disable_events),
        );
        Self { _events, simulator, resize_callback, zoom_speed, pan_speed, disable_events }
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

    fn start_navigator_events(
        scene: &Scene,
        camera: &Camera2d,
        min_zoom: f32,
        max_zoom: f32,
        zoom_speed: SharedSwitch<f32>,
        pan_speed: SharedSwitch<f32>,
        disable_events: Rc<Cell<bool>>,
    ) -> (physics::inertia::DynSimulator<Vector3>, callback::Handle, NavigatorEvents) {
        let simulator = Self::create_simulator(camera);
        let panning_callback = enclose!((scene,camera,mut simulator,pan_speed) move |pan: PanEvent| {
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

        let zoom_callback = enclose!((scene,camera,simulator) move |zoom:ZoomEvent| {
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
            let min_zoom       = camera.clipping().near + min_zoom;
            let zoom_amount    = zoom.amount * position.z;
            let direction      = direction   * zoom_amount;
            let max_zoom_limit = max_zoom - position.z;
            let min_zoom_limit = min_zoom - position.z;
            let too_far        = direction.z > max_zoom_limit;
            let too_close      = direction.z < min_zoom_limit;
            let zoom_factor    = if too_far   { max_zoom_limit / direction.z }
                            else if too_close { min_zoom_limit / direction.z }
                            else              { 1.0 };
            position          += direction * zoom_factor;
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
