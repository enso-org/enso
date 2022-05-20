mod events;

pub use crate::display::navigation::navigator::events::PanEvent;
pub use crate::display::navigation::navigator::events::ZoomEvent;

use crate::prelude::*;

use crate::animation::physics;
use crate::control::callback;
use crate::display::camera::Camera2d;
use crate::display::navigation::navigator::events::NavigatorEvents;
use crate::display::object::traits::*;
use crate::display::Scene;



// =================
// === Constants ===
// =================

/// Default maximum zoom factor (100x). This value can be changed by
/// [`NavigatorModel::set_max_zoom`].
const DEFAULT_MAX_ZOOM: f32 = 100.0;
/// Default minimum zoom factor (0.15x).
const MIN_ZOOM: f32 = 0.001;
/// Default speed for zoom events.
const DEFAULT_ZOOM_SPEED: f32 = 0.01;
/// Default speed for panning events.
const DEFAULT_PAN_SPEED: f32 = 1.0;



// ================
// === Settings ===
// ================

/// Enabling/disabling Navigator and changing its settings.
#[derive(Debug, Clone)]
pub struct Settings {
    is_enabled:           Cell<bool>,
    zoom_speed:           Cell<Switch<f32>>,
    pan_speed:            Cell<Switch<f32>>,
    enable_wheel_panning: Cell<bool>,
    max_zoom:             Cell<Option<f32>>,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            zoom_speed:           Cell::new(Switch::new(DEFAULT_ZOOM_SPEED, true)),
            pan_speed:            Cell::new(Switch::new(DEFAULT_PAN_SPEED, true)),
            is_enabled:           Cell::new(true),
            enable_wheel_panning: Cell::new(true),
            max_zoom:             Cell::new(None),
        }
    }
}

impl Settings {
    // === Getters ===

    pub fn zoom_speed(&self) -> f32 {
        self.zoom_speed.get().into_on().unwrap_or(0.0)
    }

    pub fn pan_speed(&self) -> f32 {
        self.pan_speed.get().into_on().unwrap_or(0.0)
    }

    pub fn max_zoom(&self) -> f32 {
        self.max_zoom.get().unwrap_or(DEFAULT_MAX_ZOOM)
    }

    pub fn is_wheel_panning_enabled(&self) -> bool {
        self.enable_wheel_panning.get()
    }

    pub fn is_enabled(&self) -> bool {
        self.is_enabled.get()
    }

    // === Setters ===

    /// Enable scene panning with mouse wheel or two-finger gesture on touchpad.
    pub fn enable_wheel_panning(&self) {
        self.enable_wheel_panning.set(true);
    }

    /// Disable scene panning with mouse wheel or two-finger gesture on touchpad.
    pub fn disable_wheel_panning(&self) {
        self.enable_wheel_panning.set(false);
    }

    /// Enable all [`Navigator`] capabilities.
    pub fn enable(&self) {
        self.is_enabled.set(true);
        self.zoom_speed.update(|switch| switch.switched(true));
        self.pan_speed.update(|switch| switch.switched(true));
    }

    /// Disable all [`Navigator`] capabilities.
    pub fn disable(&self) {
        self.is_enabled.set(false);
        self.zoom_speed.update(|switch| switch.switched(false));
        self.pan_speed.update(|switch| switch.switched(false));
    }

    /// Enable or disable the restriction of maximum zoom factor. Enabling it does change camera
    /// zoom immediately, but the restriction will be applied on the next zoom event.
    pub fn set_max_zoom(&self, value: Option<f32>) {
        self.max_zoom.set(value);
    }
}


// ======================
// === NavigatorModel ===
// ======================

/// Navigator enables camera navigation with mouse interactions.
#[derive(Debug, Deref)]
pub struct NavigatorModel {
    #[deref]
    settings:        Rc<Settings>,
    events:          NavigatorEvents,
    simulator:       physics::inertia::DynSimulator<Vector3>,
    resize_callback: callback::Handle,
}

impl NavigatorModel {
    pub fn new(scene: &Scene, camera: &Camera2d) -> Self {
        let settings = Rc::new(Settings::default());
        let (simulator, resize_callback, events) =
            Self::start_navigator_events(scene, camera, settings.clone_ref());
        Self { events, simulator, resize_callback, settings }
    }

    fn create_simulator(camera: &Camera2d) -> physics::inertia::DynSimulator<Vector3> {
        let camera_ref = camera.clone_ref();
        let on_step = Box::new(move |p: Vector3| camera_ref.set_position(p));
        let on_end = Box::new(|_| ());
        let simulator = physics::inertia::DynSimulator::new(on_step, (), on_end);
        // FIXME[WD]: This one is emitting camera position in next frame, which is not intended.
        //            Should be fixed when reworking navigator to use FRP events.
        simulator.set_value(camera.position());
        simulator.set_target_value(camera.position());
        simulator
    }

    fn start_navigator_events(
        scene: &Scene,
        camera: &Camera2d,
        settings: Rc<Settings>,
    ) -> (physics::inertia::DynSimulator<Vector3>, callback::Handle, NavigatorEvents) {
        let distance_to_zoom_factor_of_1 = |camera: &Camera2d| {
            let fovy_slope = camera.half_fovy_slope();
            camera.screen().height / 2.0 / fovy_slope
        };

        let simulator = Self::create_simulator(camera);
        let panning_callback = f!([camera,simulator,settings] (pan: PanEvent) {
            let distance = camera.position().z;
            let distance_to_zoom_factor_of_1 = distance_to_zoom_factor_of_1(&camera);
            let pan_speed = settings.pan_speed();
            let movement_scale_for_distance = distance / distance_to_zoom_factor_of_1;
            let movement = Vector3::new(pan.movement.x, pan.movement.y, 0.0);
            let diff = pan_speed * movement * movement_scale_for_distance;
            simulator.update_target_value(|p| p - diff);
        });

        let resize_callback =
            camera.add_screen_update_callback(enclose!((mut simulator,camera) move |_| {
                let position = camera.position();
                simulator.set_value(position);
                simulator.set_target_value(position);
                simulator.set_velocity(default());
            }));

        let zoom_callback = f!([camera,simulator,settings] (zoom:ZoomEvent) {
            let point = zoom.focus;
            let normalized = normalize_point2(point,camera.screen().into());
            let normalized = normalized_to_range2(normalized, -1.0, 1.0);
            let half_height = 1.0;

            // Scale X and Y to compensate aspect and fov.
            let x = -normalized.x * camera.screen().aspect();
            let y = -normalized.y;
            let z = half_height / camera.half_fovy_slope();
            let direction = Vector3(x, y, z).normalize();
            let mut position = simulator.target_value();
            let zoom_amount = zoom.amount * position.z;
            let translation = direction * zoom_amount;

            let distance_to_zoom_factor_of_1 = distance_to_zoom_factor_of_1(&camera);
            let max_zoom = settings.max_zoom();
            // Usage of min/max prefixes might be confusing here. Remember that the max zoom means
            // the maximum visible size, thus the minimal distance from the camera and vice versa.
            let min_distance = distance_to_zoom_factor_of_1 / max_zoom + camera.clipping().near;
            let max_distance = (distance_to_zoom_factor_of_1 / MIN_ZOOM).min(camera.clipping().far);

            // Smothly limit camera movements along z-axis near min/max distance.
            let positive_z_translation_limit = max_distance - position.z;
            let negative_z_translation_limit = min_distance - position.z;
            let too_far = translation.z > positive_z_translation_limit;
            let too_close = translation.z < negative_z_translation_limit;
            let z_axis_movement_limiting_factor = if too_far {
                positive_z_translation_limit / translation.z
            } else if too_close {
                negative_z_translation_limit / translation.z
            } else {
                1.0
            };
            position += translation * z_axis_movement_limiting_factor;
            simulator.set_target_value(position);
        });
        (
            simulator,
            resize_callback,
            NavigatorEvents::new(
                &scene.mouse.mouse_manager,
                panning_callback,
                zoom_callback,
                settings,
            ),
        )
    }

    /// Emit zoom event. This function could be used in the tests to simulate user interactions.
    pub fn emit_zoom_event(&self, event: ZoomEvent) {
        self.events.emit_zoom_event(event);
    }

    /// Emit pan event. This function could be used in the tests to simulate user interactions.
    pub fn emit_pan_event(&self, event: PanEvent) {
        self.events.emit_pan_event(event);
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

/// Normalize a `point` in (0..dimension.x, 0..dimension.y) to (0..1, 0..1).
fn normalize_point2(point: Vector2<f32>, dimension: Vector2<f32>) -> Vector2<f32> {
    Vector2::new(point.x / dimension.x, point.y / dimension.y)
}

/// Transforms a `point` normalized in (0..1, 0..1) to (a..b,a..b).
fn normalized_to_range2(point: Vector2<f32>, a: f32, b: f32) -> Vector2<f32> {
    let width = b - a;
    Vector2::new(point.x * width + a, point.y * width + a)
}
