#![allow(missing_docs)]

//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

use ensogl_core::prelude::*;

use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::system::web;
use wasm_bindgen::prelude::*;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::ShapeSystem;
use ensogl_core::display::world::*;
use ensogl_core::display::shape::*;
use ensogl_core::data::color;



pub fn shape() -> AnyShape {
    let rect = Rect((20.0.px(),20.0.px()));
    let rect = rect.translate_x(1.0.px());
    let rect = rect.fill(color::Rgb::new(1.0,0.0,0.0));
    rect.into()
}

#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_shape_system() {
    web::forward_panic_hook_to_console();
    web::set_stdout();

    let world         = World::new(&web::get_html_element_by_id("root").unwrap());
    let scene         = world.scene();
    let camera        = scene.camera().clone_ref();
    let navigator     = Navigator::new(&scene,&camera);
    let sprite_system = ShapeSystem::new(&world,&shape());

    let sprite1       = sprite_system.new_instance();
    sprite1.size.set(Vector2::new(100.0, 100.0));
    sprite1.mod_position(|t| *t = Vector3::new(50.0, 50.0, 0.0));

    world.add_child(&sprite_system);
    world.keep_alive_forever();

    world.on_frame(move |_time| {
        let _keep_alive = &sprite1;
        let _keep_alive = &navigator;
    }).forget();
}
