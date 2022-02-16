//! Example scene showing simple usage of a shape system.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "1024"]

use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::ShapeSystem;
use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::system::web;
use wasm_bindgen::prelude::*;



// ==============
// === Shapes ===
// ==============

/// The shape definition.
pub fn shape() -> AnyShape {
    let circle1 = Circle(50.px());
    let circle_bg = circle1.translate_x(-(50.0.px()));
    let circle_sub = circle1.translate_y(-(50.0.px()));
    let rect = Rect((100.0.px(), 100.0.px()));
    let shape = circle_bg + rect - circle_sub;
    let shape = shape.fill(color::Rgb::new(1.0, 0.0, 0.0));
    shape.into()
}



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_shape_system() {
    web::forward_panic_hook_to_console();

    let world = World::new(&web::get_html_element_by_id("root").unwrap());
    let scene = world.scene();
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);
    let sprite_system = ShapeSystem::new(&world, &shape());
    let sprite = sprite_system.new_instance();

    sprite.size.set(Vector2::new(300.0, 300.0));
    sprite.mod_position(|t| *t = Vector3::new(50.0, 50.0, 0.0));

    world.add_child(&sprite_system);
    world.keep_alive_forever();

    world
        .on_frame(move |_time| {
            let _keep_alive = &sprite;
            let _keep_alive = &navigator;
        })
        .forget();
}
