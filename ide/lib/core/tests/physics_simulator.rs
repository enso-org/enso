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
    use basegl::system::web::dom::html::Css3dSystem;
    use web_test::*;
    use nalgebra::{zero, Vector3};
    use js_sys::Math::random;
    use basegl::display::world::WorldData;
    use basegl::display::object::DisplayObjectOps;
    use basegl::system::web::dyn_into;
    use basegl::system::web::get_element_by_id;
    use basegl::system::web::create_element;
    use web_sys::HtmlElement;
    use nalgebra::Vector2;
    use basegl::system::web::NodeInserter;
    use basegl::system::web::AttributeSetter;
    use basegl::system::web::set_stdout;
    use basegl::display::object::DisplayObject;

    #[web_test]
    fn simulator() {
        set_stdout();
        let name          = "simulator";
        let canvas_name   = format!("canvas_{}",name);
        let container     = dyn_into::<_,HtmlElement>(get_element_by_id(name).unwrap()).unwrap();
        let canvas        = create_element("canvas").unwrap();
        canvas.set_attribute_or_panic("id", &canvas_name);
        container.append_or_panic(&canvas);
        let world         = WorldData::new(&canvas_name);
        let css3d_system  = Css3dSystem::new(&world);
        world.add_child(&css3d_system);

        container.set_style_or_panic("background-color", "black");

        let mut target = css3d_system.new_instance("div").unwrap();
        target.set_dimensions(Vector2::new(10.0, 10.0));
        target.dom().set_style_or_panic("background-color", "green");

        let mut object = css3d_system.new_instance("div").unwrap();
        object.set_dimensions(Vector2::new(10.0, 10.0));
        object.dom().set_style_or_panic("background-color", "red");

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
                object.mod_position(|t| *t = position);
                world.display_object().update();
            }
        );

        // Updates spring's fixed point every two seconds.
        let every    = 2.0;
        let animator = FixedStepAnimator::new(1.0 / every, move |_| {
            let _keep_alive = &simulator;
            let _keep_alive = &css3d_system;

            let x = 320.0 * random() as f32;
            let y = 240.0 * random() as f32;
            let z = 0.0;
            let position = Vector3::new(x, y, z);
            properties.mod_spring(|spring| spring.fixed_point = position);
            target.mod_position(|t| *t = position);
        });

        std::mem::forget(animator);
    }
}
