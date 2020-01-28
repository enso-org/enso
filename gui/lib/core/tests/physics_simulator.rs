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
    use basegl::system::web::dom::html::HtmlRenderer;
    use basegl::system::web::dom::html::HtmlObject;
    use basegl::system::web::dom::html::HtmlScene;
    use basegl::display::camera::Camera2d;
    use web_test::*;
    use nalgebra::{zero, Vector3};
    use js_sys::Math::random;
    use logger::Logger;

    #[web_bench]
    fn simulator(b : &mut Bencher) {
        let renderer = HtmlRenderer::new("simulator").expect("Renderer couldn't be created");
        renderer.container().dom.set_property_or_panic("background-color", "black");

        let logger    = Logger::new("simulator");
        let mut scene = HtmlScene::new(&logger);

        let mut target = HtmlObject::new(&logger, "div").unwrap();
        target.set_dimensions(10.0, 10.0);
        target.dom.set_property_or_panic("background-color", "green");
        scene.add_child(target.clone());

        let mut object = HtmlObject::new(&logger, "div").unwrap();
        object.set_dimensions(10.0, 10.0);
        object.dom.set_property_or_panic("background-color", "red");
        scene.add_child(object.clone());

        let view_dim = renderer.dimensions();
        assert_eq!((view_dim.x, view_dim.y), (320.0, 240.0));

        let mut camera  = Camera2d::new(logger,view_dim.x,view_dim.y);

        let mass             = 2.0;
        let position         = object.position();
        let kinematics       = KinematicsProperties::new(position, zero(), zero(), mass);
        let coefficient      = 10.0;
        let fixed_point      = zero();
        let spring           = SpringProperties::new(coefficient, fixed_point);
        let drag             = DragProperties::new(0.8);
        let mut properties   = PhysicsProperties::new(kinematics, spring, drag);
        let steps_per_second = 60.0;
        let simulator        = PhysicsSimulator::new(
            steps_per_second,
            properties.clone(),
            move |position| {
                object.set_position(position);
            }
        );

        // Updates spring's fixed point every two seconds.
        let every = 2.0;
        let animator  = FixedStepAnimator::new(1.0 / every, move |_| {
            let x = 320.0 * random() as f32;
            let y = 240.0 * random() as f32;
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
