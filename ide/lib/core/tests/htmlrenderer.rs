//! Test suite for the Web and headless browsers.

#![cfg(target_arch = "wasm32")]

extern crate wasm_bindgen_test;
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

pub mod common;

#[cfg(test)]
mod tests {
    use crate::common::TestContainer;
    use basegl::display::rendering::*;
    use basegl::system::web::StyleSetter;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn invalid_container() {
        let scene = HTMLScene::new("nonexistent_id");
        assert!(scene.is_err(), "nonexistent_id should not exist");
    }

    #[wasm_bindgen_test]
    fn object_behind_camera() {
        TestContainer::new("object_behind_camera", 320.0, 240.0);
        let mut scene = HTMLScene::new("object_behind_camera")
                                  .expect("Failed to create HTMLScene");
        assert_eq!(scene.len(), 0, "Scene should be empty");

        let view_dim = scene.get_dimensions();
        assert_eq!((view_dim.x, view_dim.y), (320.0, 240.0));

        let mut object = HTMLObject::new("div").unwrap();
        object.set_position(0.0, 0.0, 0.0);
        object.element.set_property_or_panic("background-color", "black");
        object.set_dimensions(100.0, 100.0);
        scene.add(object);

        let aspect_ratio = view_dim.x / view_dim.y;
        let mut camera = Camera::perspective(45.0, aspect_ratio, 1.0, 1000.0);
        // We move the Camera behind the object so we don't see it.
        camera.set_position(0.0, 0.0, -100.0);

        let renderer = HTMLRenderer::new();
        renderer.render(&mut camera, &scene);
    }

    fn create_scene(dom_id : &str) -> HTMLScene {
        let mut scene = HTMLScene::new(dom_id)
                                  .expect("Failed to create HTMLScene");
        assert_eq!(scene.len(), 0);

        scene.container.set_property_or_panic("background-color", "black");

        // Iterate over 3 axes.
        for axis in vec![(1, 0, 0), (0, 1, 0), (0, 0, 1)] {
            // Creates 10 HTMLObjects per axis.
            for i in 0 .. 10 {
                let mut object = HTMLObject::new("div").unwrap();
                object.set_dimensions(1.0, 1.0);

                // Using axis for masking.
                // For instance, the axis (0, 1, 0) creates:
                // (x, y, z) = (0, 0, 0) .. (0, 9, 0)
                let x = (i * axis.0) as f32;
                let y = (i * axis.1) as f32;
                let z = (i * axis.2) as f32;
                object.set_position(x, y, z);

                // Creates a gradient color based on the axis.
                let r = (x * 25.5) as u8;
                let g = (y * 25.5) as u8;
                let b = (z * 25.5) as u8;
                let color = format!("rgba({}, {}, {}, {})", r, g, b, 1.0);

                object.element.set_property_or_panic("background-color", color);
                scene.add(object);
            }
        }
        assert_eq!(scene.len(), 30, "We should have 30 HTMLObjects");
        scene
    }

    #[wasm_bindgen_test]
    fn rhs_coordinates() {
        TestContainer::new("rhs_coordinates", 320.0, 240.0);
        let scene = create_scene("rhs_coordinates");

        let view_dim = scene.get_dimensions();
        assert_eq!((view_dim.x, view_dim.y), (320.0, 240.0));

        let aspect_ratio = view_dim.x / view_dim.y;
        let mut camera = Camera::perspective(45.0, aspect_ratio, 1.0, 1000.0);

        // We move the Camera 29 units away from the center.
        camera.set_position(0.0, 0.0, 29.0);

        let renderer = HTMLRenderer::new();
        renderer.render(&mut camera, &scene);
    }

    #[wasm_bindgen_test]
    fn rhs_coordinates_from_back() {
        use std::f32::consts::PI;

        TestContainer::new("rhs_coordinates_from_back", 320.0, 240.0);
        let scene = create_scene("rhs_coordinates_from_back");

        let view_dim = scene.get_dimensions();
        assert_eq!((view_dim.x, view_dim.y), (320.0, 240.0));

        let aspect_ratio = view_dim.x / view_dim.y;
        let mut camera = Camera::perspective(45.0, aspect_ratio, 1.0, 1000.0);

        // We move the Camera -29 units away from the center.
        camera.set_position(0.0, 0.0, -29.0);
        // We rotate it 180 degrees so we can see the center of the scene
        // from behind.
        camera.set_rotation(0.0, PI, 0.0);

        let renderer = HTMLRenderer::new();
        renderer.render(&mut camera, &scene);
    }
}
