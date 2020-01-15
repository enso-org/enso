//! Test suite for the Web and headless browsers.
#![cfg(target_arch = "wasm32")]

use web_test::web_configure;
web_configure!(run_in_browser);

#[cfg(test)]
mod tests {
    use basegl::system::web::StyleSetter;
    use basegl::animation::physics::inertia::DragProperties;
    use basegl::animation::physics::inertia::SpringProperties;
    use basegl::animation::physics::inertia::KinematicsProperties;
    use basegl::animation::physics::inertia::PhysicsSimulator;
    use basegl::animation::physics::inertia::PhysicsProperties;
    use basegl::animation::animator::fixed_step::FixedStepAnimator;
    use basegl::system::web::dom::html::HTMLRenderer;
    use basegl::system::web::dom::html::HTMLObject;
    use basegl::system::web::dom::Scene;
    use basegl::system::web::dom::Camera;
    use basegl::animation::position::HasPosition;
    use web_test::*;
    use nalgebra::{zero, Vector3};
    use js_sys::Math::random;

    #[web_bench]
    fn simulator(b : &mut Bencher) {
        let renderer = HTMLRenderer::new("simulator").expect("Renderer couldn't be created");
        renderer.container.dom.set_property_or_panic("background-color", "black");

        let mut scene : Scene<HTMLObject> = Scene::new();

        let mut target = HTMLObject::new("div").unwrap();
        target.set_dimensions(1.0, 1.0);
        target.dom.set_property_or_panic("background-color", "green");
        scene.add(target.clone());

        let mut object = HTMLObject::new("div").unwrap();
        object.set_dimensions(1.0, 1.0);
        object.dom.set_property_or_panic("background-color", "red");
        scene.add(object.clone());

        let view_dim = renderer.dimensions();
        assert_eq!((view_dim.x, view_dim.y), (320.0, 240.0));

        let aspect_ratio = view_dim.x / view_dim.y;
        let mut camera = Camera::perspective(45.0, aspect_ratio, 1.0, 1000.0);
        camera.set_position(Vector3::new(0.0, 0.0, 29.0));

        let mut event_loop   = b.event_loop();
        let mass             = 2.0;
        let kinematics       = KinematicsProperties::new(zero(), zero(), zero(), mass);
        let coefficient      = 10.0;
        let fixed_point      = zero();
        let spring           = SpringProperties::new(coefficient, fixed_point);
        let drag             = DragProperties::new(0.8);
        let mut properties   = PhysicsProperties::new(kinematics, spring, drag);
        let steps_per_second = 60.0;
        let simulator        = PhysicsSimulator::new(
            &mut event_loop,
            steps_per_second,
            object,
            properties.clone()
        );

        // Updates spring's fixed point every two seconds.
        let every = 2.0;
        let animator  = FixedStepAnimator::new(&mut event_loop, 1.0 / every, move |_| {
            let x = 32.0 * (random() - 0.5) as f32;
            let y = 24.0 * (random() - 0.5) as f32;
            let z = 0.0;
            let position = Vector3::new(x, y, z);
            properties.mod_spring(|spring| spring.fixed_point = position);
            target.set_position(position);
        });

        b.iter(move || {
            let _keep_alive = &simulator;
            let _keep_alive = &animator;
            renderer.render(&mut camera, &scene);
        });
    }
}
