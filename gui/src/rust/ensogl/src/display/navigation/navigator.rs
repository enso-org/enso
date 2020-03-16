mod events;

use crate::prelude::*;

use crate::animation::physics::inertia::DragProperties;
use crate::animation::physics::inertia::KinematicsProperties;
use crate::animation::physics::inertia::PhysicsProperties;
use crate::animation::physics::inertia::PhysicsSimulator;
use crate::animation::physics::inertia::SpringProperties;
use crate::control::callback::CallbackHandle;
use crate::display::camera::Camera2d;
use crate::display::Scene;
use crate::system::web::dom;
use crate::system::web;
use events::NavigatorEvents;
use events::PanEvent;
use events::ZoomEvent;
use nalgebra::Vector2;
use nalgebra::Vector3;
use nalgebra::zero;



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
    pub fn new(scene:&Scene, camera:&Camera2d) -> Self {
        let dom                       = scene.dom().root;
        let (_simulator, properties)  = Self::start_simulator(camera.clone());
        let zoom_speed                = 10.0;
        let min_zoom                  = 10.0;
        let max_zoom                  = 10000.0;
        let scaled_down_zoom_speed    = zoom_speed / 1000.0;
        let camera                    = camera.clone();
        let (resize_callback,_events) = Self::start_navigator_events
            (dom,camera,min_zoom,max_zoom,scaled_down_zoom_speed,properties);
        Self {_simulator,_events,resize_callback}
    }

    fn start_simulator(camera:Camera2d) -> (PhysicsSimulator,PhysicsProperties) {
        let mass               = 30.0;
        let velocity           = zero();
        let position           = camera.transform().position();
        let kinematics         = KinematicsProperties::new(position, velocity, zero(), mass);
        let spring_coefficient = 20000.0;
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
    ( dom:dom::WithKnownShape<web::HtmlDivElement>
    , camera:Camera2d
    , min_zoom:f32
    , max_zoom:f32
    , zoom_speed:f32
    , mut properties:PhysicsProperties) -> (CallbackHandle,NavigatorEvents) {
        let dom_clone        = dom.clone();
        let camera_clone     = camera.clone();
        let panning_callback = enclose!((mut properties) move |pan: PanEvent| {
            let fovy_slope                  = camera_clone.half_fovy_slope();
            let distance                    = camera_clone.transform().position().z;
            let distance_to_show_full_ui    = dom_clone.shape().height() / 2.0 / fovy_slope;
            let movement_scale_for_distance = distance / distance_to_show_full_ui;

            let dx   = pan.movement.x * movement_scale_for_distance;
            let dy   = pan.movement.y * movement_scale_for_distance;
            let diff = Vector3::new(dx,dy,0.0);
            properties.modify_spring(|spring| { spring.fixed_point += diff; });
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
                let normalized  = normalize_point2(point,dom_clone.shape().into());
                let normalized  = normalized_to_range2(normalized, -1.0, 1.0);
                let half_height = 1.0;

                // Scale X and Y to compensate aspect and fov.
                let x              = -normalized.x * camera.screen().aspect();
                let y              =  normalized.y;
                let z              = half_height / camera.half_fovy_slope();
                let direction      = Vector3::new(x, y, z).normalize();
                let mut position   = properties.spring().fixed_point;
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
                properties.modify_spring(|spring| spring.fixed_point = position);
        };
        (resize_callback, NavigatorEvents::new(&dom, panning_callback, zoom_callback, zoom_speed))
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
