//! Test suite for the Web and headless browsers.
#![cfg(target_arch = "wasm32")]

use web_test::web_configure;
web_configure!(run_in_browser);

use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

#[wasm_bindgen(module = "/tests/bench_test.js")]
extern "C" {
    fn set_gradient_bg(
        dom   : &JsValue,
        red   : &JsValue,
        green : &JsValue,
        blue  : &JsValue);
}

#[cfg(test)]
mod tests {
    use basegl::system::web::dom::html::Css3dObject;
    use basegl::system::web::dom::html::Css3dSystem;
    use basegl::system::web::dom::html::Css3dRenderer;
    use basegl::system::web::StyleSetter;
    use basegl::system::web::get_performance;
    use web_test::*;
    use web_sys::Performance;
    use nalgebra::Vector3;
    use logger::Logger;
    use basegl::system::web;
    use basegl::display::world::{WorldData, World};
    use basegl::display::navigation::navigator::Navigator;
    use basegl::display::object::DisplayObjectOps;
    use basegl::display::object::DisplayObject;
    use nalgebra::Vector2;
    use basegl::system::web::AttributeSetter;
    use basegl::system::web::NodeInserter;
    use basegl::system::web::dyn_into;
    use basegl::system::web::get_element_by_id;
    use basegl::system::web::create_element;
    use web_sys::HtmlElement;

    #[web_test(no_container)]
    fn invalid_container() {
        let logger   = Logger::new("invalid_container");
        let renderer = Css3dRenderer::new(&logger, "nonexistent_id");
        assert!(renderer.is_err(),"Tried to attach to a non-existent HtmlElement and succeeded.");
    }

    fn initialize_system(name:&str,color:&str) -> (World,Css3dSystem) {
        web::set_stdout();
//        let canvas_name   = format!("canvas_{}",name);
        let container     = dyn_into::<_,HtmlElement>(get_element_by_id(name).unwrap()).unwrap();
//        let canvas        = dyn_into::<_,HtmlElement>(create_element("canvas").unwrap()).unwrap();
//        canvas.set_attribute_or_panic("id", &canvas_name);
//        canvas.set_style_or_panic("width", "100%");
//        canvas.set_style_or_panic("height", "100%");
//        container.append_or_panic(&canvas);
        let world         = WorldData::new(&container);
        let css3d_system  = Css3dSystem::new(&world);
        container.set_style_or_panic("background-color", color);
        world.add_child(&css3d_system);
        (world,css3d_system)
    }

    fn create_scene(system:&Css3dSystem) -> Vec<Css3dObject> {
        let mut objects = Vec::new();
        // Iterate over 3 axes.
        for axis in vec![(1, 0, 0), (0, 1, 0), (0, 0, 1)] {
            // Creates 10 HTMLObjects per axis.
            for i in 0 .. 10 {
                let mut object = system.new_instance("div").unwrap();
                object.set_dimensions(Vector2::new(10.0, 10.0));

                // Using axis for masking.
                // For instance, the axis (0, 1, 0) creates:
                // (x, y, z) = (0, 0, 0) .. (0, 9, 0)
                let x = (i * axis.0) as f32;
                let y = (i * axis.1) as f32;
                let z = (i * axis.2) as f32;
                let factor = 120.0 / 9.0;
                let position = Vector3::new(x * factor + 160.0, y * factor + 120.0, z * factor);
                object.mod_position(|t| *t = position);

                // Creates a gradient color based on the axis.
                let r = (x * 25.5) as u8;
                let g = (y * 25.5) as u8;
                let b = (z * 25.5) as u8;
                let color = format!("rgba({}, {}, {}, {})", r, g, b, 1.0);
                object.dom().set_style_or_panic("background-color", color);
                objects.push(object);
            }
        }
        objects
    }

    #[web_test]
    fn rhs_coordinates() {
        let (world,css3d_system) = initialize_system("rhs_coordinates", "black");
        let scene         = world.scene();
        let camera        = scene.camera();
        let navigator     = Navigator::new(&scene, &camera).expect("Couldn't create navigator");

        let scene = create_scene(&css3d_system);

        world.display_object().update();

        world.on_frame(move |_| {
            let _keep_alive = &scene;
            let _keep_alive = &css3d_system;
            let _keep_alive = &navigator;
        }).forget();
        std::mem::forget(world);
    }

    fn make_sphere(mut scene : &mut Vec<Css3dObject>, performance : &Performance) {
        use super::set_gradient_bg;

        let t = (performance.now() / 1000.0) as f32;
        let length = scene.len() as f32;
        for (i, object) in (&mut scene).into_iter().enumerate() {
            let i = i as f32;
            let d = (i / length - 0.5) * 2.0;

            let mut y = d;
            let r = (1.0 - y * y).sqrt();
            let mut x = (y * 100.0 + t).cos() * r;
            let mut z = (y * 100.0 + t).sin() * r;

            x += (y * 1.25 + t * 2.50).cos() * 0.5;
            y += (z * 1.25 + t * 2.00).cos() * 0.5;
            z += (x * 1.25 + t * 3.25).cos() * 0.5;
            let x = x * 10.0;
            let y = y * 10.0;
            let z = z * 10.0;
            object.mod_position(|t| *t = Vector3::new(x, y, z));

            let faster_t = t * 100.0;
            let r = (i +   0.0 + faster_t) as u8 % 255;
            let g = (i +  85.0 + faster_t) as u8 % 255;
            let b = (i + 170.0 + faster_t) as u8 % 255;
            set_gradient_bg(&object.dom(), &r.into(), &g.into(), &b.into());
        }
    }

    #[web_bench]
    fn object_x400_update(b: &mut Bencher) {
        let (world,css3d_system) = initialize_system("object_x400_update", "black");
        let scene         = world.scene();
        let camera        = scene.camera();
        let navigator     = Navigator::new(&scene, &camera).expect("Couldn't create navigator");

        let mut objects = Vec::new();
        for _ in 0..400 {
            let mut object = css3d_system.new_instance("div").expect("Failed to create object");
            object.set_dimensions(Vector2::new(1.0, 1.0));
            object.mod_scale(|t| *t = Vector3::new(0.5, 0.5, 0.5));
            objects.push(object);
        }

        let performance = get_performance().expect("Couldn't get performance obj");
        b.iter(move || {
            let _keep_alive = &navigator;
            let _keep_alive = &css3d_system;
            make_sphere(&mut objects, &performance);
            world.display_object().set_scale(Vector3::new(5.0, 5.0, 5.0));
            world.display_object().set_position(Vector3::new(160.0, 120.0, 0.0));
            world.display_object().update();
        })
    }
}
