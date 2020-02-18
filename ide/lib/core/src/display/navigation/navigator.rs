mod events;

use crate::prelude::*;

use events::NavigatorEvents;
use events::ZoomEvent;
use events::PanEvent;
use crate::display::camera::Camera2d;
use crate::display::Scene;
use crate::system::web::Result;
use crate::system::web::dom::DomContainer;
use crate::animation::physics::inertia::PhysicsSimulator;
use crate::animation::physics::inertia::SpringProperties;
use crate::animation::physics::inertia::DragProperties;
use crate::animation::physics::inertia::PhysicsProperties;
use crate::animation::physics::inertia::KinematicsProperties;
use crate::system::web::dyn_into;
use crate::control::callback::CallbackHandle;

use nalgebra::{Vector3, zero};
use nalgebra::Vector2;
use nalgebra::clamp;


// =================
// === Navigator ===
// =================

/// Navigator enables camera navigation with mouse interactions.
#[derive(Debug)]
pub struct Navigator {
    _events         : NavigatorEvents,
    _simulator      : PhysicsSimulator,
    resize_callback : CallbackHandle
}

impl Navigator {
    pub fn new(scene:&Scene, camera:&Camera2d) -> Result<Self> {
        let dom                       = dyn_into(scene.canvas())?;
        let dom                       = DomContainer::from_element(dom);
        let (_simulator, properties)  = Self::start_simulator(camera.clone());
        let zoom_speed                = 2.0;
        let min_zoom                  = 10.0;
        let max_zoom                  = 10000.0;
        let scaled_down_zoom_speed    = zoom_speed / 1000.0;
        let camera                    = camera.clone();
        let (resize_callback,_events) = Self::start_navigator_events(
            &dom,
            camera,
            min_zoom,
            max_zoom,
            scaled_down_zoom_speed,
            properties
        );
        let _events = _events?;
        Ok(Self {_simulator,_events,resize_callback})
    }

    fn start_simulator(camera:Camera2d) -> (PhysicsSimulator, PhysicsProperties) {
        let mass               = 30.0;
        let velocity           = zero();
        let position           = camera.transform().position();
        let kinematics         = KinematicsProperties::new(position, velocity, zero(), mass);
        let spring_coefficient = 10000.0;
        let fixed_point        = position;
        let spring             = SpringProperties::new(spring_coefficient, fixed_point);
        let drag               = DragProperties::new(1500.0);
        let properties         = PhysicsProperties::new(kinematics, spring, drag);
        let steps_per_second   = 60.0;
        let callback           = move |position| camera.set_position(position);
        let sim = PhysicsSimulator::new(steps_per_second,&properties,callback);
        (sim,properties)
    }

    fn start_navigator_events
    ( dom:&DomContainer
    , camera:Camera2d
    , min_zoom:f32
    , max_zoom:f32
    , zoom_speed:f32
    , mut properties:PhysicsProperties) -> (CallbackHandle, Result<NavigatorEvents>) {
        let dom_clone            = dom.clone();
        let camera_clone         = camera.clone();
        let panning_callback     = enclose!((mut properties) move |pan: PanEvent| {
            let fovy_slope = camera_clone.half_fovy_slope();
            // base_distance is a distance where the camera covers all the UI.
            let base_distance = dom_clone.dimensions().y / 2.0 / fovy_slope;
            // We can then scale the movement based on the camera distance. If we are closer, the
            // movement will be slower, if we are further, the movement is faster.
            let scale         = camera_clone.transform().position().z / base_distance;

            let x = pan.movement.x * scale;
            let y = pan.movement.y * scale;
            let z = 0.0;

            properties.modify_spring(|spring| { spring.fixed_point += Vector3::new(x, y, z); });
        });

        let transform       = camera.transform();
        let resize_callback = camera.add_screen_update_callback(
            enclose!((mut properties,transform) move |_:&Vector2<f32>| {
                let position = transform.position();
                properties.modify_kinematics(|kinematics| {
                    kinematics.set_position(position);
                    kinematics.set_velocity(Vector3::new(0.0, 0.0, 0.0));
                });
                properties.modify_spring(|spring| spring.fixed_point = position);
            })
        );

        let dom_clone     = dom.clone();
        let zoom_callback = move |zoom:ZoomEvent| {
                let point       = zoom.focus;
                let normalized  = normalize_point2(point, dom_clone.dimensions());
                let normalized  = normalized_to_range2(normalized, -1.0, 1.0);
                let half_height = 1.0;

                // Scale X and Y to compensate aspect and fov.
                let x            = -normalized.x * camera.screen().aspect();
                let y            =  normalized.y;
                let z            = half_height / camera.half_fovy_slope();
                let direction    = Vector3::new(x, y, z).normalize();
                let mut position = properties.spring().fixed_point;
                let zoom_amount  = zoom.amount * position.z;
                position        += direction   * zoom_amount;
                let min_zoom     = camera.clipping().near  + min_zoom;
                position.z       = clamp(position.z, min_zoom, max_zoom);

                properties.modify_spring(|spring| spring.fixed_point = position);
        };
        (resize_callback, NavigatorEvents::new(dom, panning_callback, zoom_callback, zoom_speed))
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
