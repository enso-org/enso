#![allow(missing_docs)]

use wasm_bindgen::prelude::*;

use crate::system::web::dom::Scene;
use crate::system::web::dom::Camera;
use crate::system::web::dom::html::HTMLObject;
use crate::system::web::dom::html::HTMLRenderer;
use crate::control::EventLoop;
use crate::system::web::StyleSetter;
use crate::display::navigation::navigator::Navigator;

use crate::animation::animator::continuous::ContinuousAnimator;

use nalgebra::Vector2;
use nalgebra::Vector3;

fn create_scene(dim:Vector2<f32>) -> Scene<HTMLObject> {
    let mut scene : Scene<HTMLObject> = Scene::new();

    let width  = dim.x / 2.0;
    let height = dim.y / 2.0;

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
        let (p_x, p_y) = positions[i];
        object.set_dimensions(width, height);
        object.set_position(Vector3::new(width * p_x, height * p_y, 0.0));
        let (c_r, c_g, c_b) = colors[i];
        let color = format!("rgb({}, {}, {})", c_r, c_g, c_b);
        object.dom.set_property_or_panic("background-color", color);
        scene.add(object);
    }

    scene
}

#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_camera_navigation() {
    let renderer = HTMLRenderer::new("app").expect("Renderer couldn't be created");
    renderer.container.dom.set_property_or_panic("background-color", "black");

    let dimensions = renderer.dimensions();
    let scene = create_scene(dimensions);

    let mut camera  = Camera::perspective(45.0, dimensions.x / dimensions.y, 1.0, 1000.0);

    let x = dimensions.x / 2.0;
    let y = dimensions.y / 2.0;
    let z = y * camera.get_y_scale();
    camera.set_position(Vector3::new(x, y, z));

    let mut event_loop = EventLoop::new();

    let camera_clone = camera.clone();
    let navigator  = Navigator::new(&mut event_loop, &renderer.container, camera_clone);
    let navigator  = navigator.expect("Couldn't create navigator");

    let animator = ContinuousAnimator::new(&mut event_loop, move |_| {
        let _keep_alive = &navigator;
        renderer.render(&mut camera, &scene);
    });
    std::mem::forget(animator);
    std::mem::forget(event_loop);
}
