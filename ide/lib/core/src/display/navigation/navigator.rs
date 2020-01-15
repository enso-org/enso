#![allow(missing_docs)]

mod events;

use events::NavigatorEvents;
use events::ZoomEvent;
use events::PanEvent;
use crate::system::web::Result;
use crate::system::web::dom::Camera;
use crate::system::web::dom::CameraType;
use crate::system::web::dom::DOMContainer;
use crate::animation::position::HasPosition;
use crate::animation::physics::inertia::PhysicsSimulator;
use crate::animation::physics::inertia::SpringProperties;
use crate::animation::physics::inertia::DragProperties;
use crate::animation::physics::inertia::PhysicsProperties;
use crate::animation::physics::inertia::KinematicsProperties;
use crate::system::web::animation_frame_loop::AnimationFrameLoop;

use nalgebra::{Vector3, zero};
use nalgebra::Vector2;
use nalgebra::clamp;



// =================
// === Navigator ===
// =================

/// Navigator enables camera navigation with mouse interactions on the specified DOM.
pub struct Navigator {
    _events    : NavigatorEvents,
    _simulator : PhysicsSimulator,
}

impl Navigator {
    // FIXME: Create a simplified constructor with dom defaulted to window.
    pub fn new
    (mut event_loop:&mut AnimationFrameLoop, dom:&DOMContainer, camera:Camera) -> Result<Self> {
        let (_simulator, properties) = Self::start_simulator(&mut event_loop, camera.clone());
        let zoom_speed             = 2.0;
        let min_zoom               = 10.0;
        let max_zoom               = 10000.0;
        let scaled_down_zoom_speed = zoom_speed / 1000.0;
        let _events = Self::start_navigator_events(
            dom,
            camera,
            min_zoom,
            max_zoom,
            scaled_down_zoom_speed,
            properties
        )?;
        Ok(Self { _events, _simulator })
    }

    fn start_simulator
    ( mut event_loop:&mut AnimationFrameLoop
    , camera:Camera) -> (PhysicsSimulator, PhysicsProperties) {
        let mass               = 30.0;
        let velocity           = zero();
        let position           = camera.position();
        let kinematics         = KinematicsProperties::new(position, velocity, zero(), mass);
        let spring_coefficient = 10000.0;
        let fixed_point        = camera.position();
        let spring             = SpringProperties::new(spring_coefficient, fixed_point);
        let drag               = DragProperties::new(1500.0);
        let properties         = PhysicsProperties::new(kinematics, spring, drag);
        let camera             = camera.object;
        let steps_per_second   = 60.0;
        let properties_clone   = properties.clone();
        let simulator          = PhysicsSimulator::new(
            &mut event_loop,
            steps_per_second,
            camera,
            properties_clone
        );
        (simulator, properties)
    }

    fn start_navigator_events
    ( dom:&DOMContainer
    , camera:Camera
    , min_zoom:f32
    , max_zoom:f32
    , zoom_speed:f32
    , mut properties:PhysicsProperties) -> Result<NavigatorEvents> {
        let dom_clone            = dom.clone();
        let camera_clone         = camera.clone();
        let mut properties_clone = properties.clone();
        let panning_callback     = move |pan: PanEvent| {
            // base_distance is a distance where the camera covers all the UI.
            let base_distance = dom_clone.dimensions().y / 2.0 * camera_clone.get_y_scale();
            // We can then scale the movement based on the camera distance. If we are closer, the
            // movement will be slower, if we are further, the movement is faster.
            let scale         = camera_clone.position().z / base_distance;

            let x = pan.movement.x * scale;
            let y = pan.movement.y * scale;
            let z = 0.0;

            properties_clone.mod_spring(|spring| { spring.fixed_point += Vector3::new(x, y, z); });
        };

        let dom_clone = dom.clone();
        let zoom_callback = move |zoom:ZoomEvent| {
            if let CameraType::Perspective(persp) = camera.camera_type() {
                let point      = zoom.focus;
                let normalized = normalize_point2(point, dom_clone.dimensions());
                let normalized = normalized_to_range2(normalized, -1.0, 1.0);

                // Scale X and Y to compensate aspect and fov.
                let x                     = -normalized.x * persp.aspect;
                let y                     =  normalized.y;
                let fov_slope             = camera.get_y_scale();
                let z                     = fov_slope;
                let direction             = Vector3::new(x, y, z).normalize();
                let mut position          = properties.spring().fixed_point;
                let zoom_amount           = zoom.amount * position.z;
                position                 += direction   * zoom_amount;
                let min_zoom              = persp.near  + min_zoom;
                position.z                = clamp(position.z, min_zoom, max_zoom);

                properties.mod_spring(|spring| spring.fixed_point = position);
            }
        };
        NavigatorEvents::new(dom, panning_callback, zoom_callback, zoom_speed)
    }
}



// =============
// === Utils ===
// =============

/// Normalize a `point` in (0..dimension.x, 0..dimension.y) to (0..1, 0..1).
fn normalize_point2
(point:Vector2<f32>, dimension:Vector2<f32>) -> Vector2<f32> {
    Vector2::new(point.x / dimension.x, point.y / dimension.y)
}

/// Transforms a `point` normalized in (0..1, 0..1) to (a..b,a..b).
fn normalized_to_range2(point:Vector2<f32>, a:f32, b:f32) -> Vector2<f32> {
    let width = b - a;
    Vector2::new(point.x * width + a, point.y * width + a)
}
