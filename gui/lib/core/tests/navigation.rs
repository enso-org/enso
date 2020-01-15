//! Test suite for the Web and headless browsers.
#![feature(trait_alias)]
#![cfg(target_arch = "wasm32")]

use web_test::web_configure;
web_configure!(run_in_browser);

#[cfg(test)]
mod tests {
    use basegl::system::web::dom::Scene;
    use basegl::system::web::dom::Camera;
    use basegl::system::web::dom::html::HTMLObject;
    use basegl::system::web::dom::html::HTMLRenderer;
    use basegl::system::web::StyleSetter;
    use basegl::display::navigation::navigator::Navigator;
    use basegl::animation::position::HasPosition;
    use web_test::*;

    use nalgebra::Vector3;

    fn create_scene() -> Scene<HTMLObject> {
        let mut scene : Scene<HTMLObject> = Scene::new();
        assert_eq!(scene.len(), 0);

        let width  = 320.0 / 2.0;
        let height = 240.0 / 2.0;

        let positions = vec![
            (0.5, 0.5),
            (0.5, 1.5),
            (1.5, 0.5),
            (1.5, 1.5)
        ];

        let colors = vec![
            (255, 0  , 0  ),
            (0  , 255, 0  ),
            (0  ,   0, 255),
            (255, 255,   0)
        ];

        for i in 0..=3 {
            let     object = HTMLObject::new("div");
            let mut object = object.expect("Couldn't create div");
            let (x, y)     = positions[i];
            object.set_dimensions(width, height);
            object.set_position(Vector3::new(width * x, height * y, 0.0));
            let (r, g, b) = colors[i];
            let color = format!("rgb({}, {}, {})", r, g, b);
            object.dom.set_property_or_panic("background-color", color);
            scene.add(object);
        }

        scene
    }

    fn navigator_test(b: &mut Bencher, name:&str) {
        let renderer = HTMLRenderer::new(name)
            .expect("Renderer couldn't be created");
        renderer.container.dom.set_property_or_panic("background-color", "black");

        let scene = create_scene();

        let view_dim = renderer.dimensions();
        assert_eq!((view_dim.x, view_dim.y), (320.0, 240.0));

        let mut camera  = Camera::perspective(45.0, 320.0 / 240.0, 1.0, 1000.0);

        let dimensions = renderer.dimensions();
        let x = dimensions.x / 2.0;
        let y = dimensions.y / 2.0;
        let z = y * camera.get_y_scale();
        camera.set_position(Vector3::new(x, y, z));

        let mut event_loop = b.event_loop();
        let container      = &renderer.container;
        let camera_clone   = camera.clone();
        let navigator      = Navigator::new(&mut event_loop, container, camera_clone);
        let navigator      = navigator.expect("Couldn't create navigator");

        b.iter(move || {
            let _keep_alive = &navigator;
            renderer.render(&mut camera, &scene);
        })
    }

    // We create two tests to verify that each HtmlElement has its own Navigator.
    #[web_bench]
    fn navigator_test_1(b: &mut Bencher) { navigator_test(b, "navigator_test_1") }

    #[web_bench]
    fn navigator_test_2(b: &mut Bencher) { navigator_test(b, "navigator_test_2") }
}
