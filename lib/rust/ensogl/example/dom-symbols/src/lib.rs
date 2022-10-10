#![recursion_limit = "1024"]
// === Features ===
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::display::world::*;
use ensogl_core::prelude::*;
use ensogl_core::system::web::traits::*;
use wasm_bindgen::prelude::*;

use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::symbol::geometry::Sprite;
use ensogl_core::display::symbol::geometry::SpriteSystem;
use ensogl_core::display::symbol::DomSymbol;
use ensogl_core::system::web;
use nalgebra::Vector2;
use nalgebra::Vector3;



#[entry_point]
#[allow(dead_code)]
#[allow(clippy::many_single_char_names)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera();
    let screen = camera.screen();
    let navigator = Navigator::new(scene, &camera);
    let sprite_system = SpriteSystem::new(&world);
    world.add_child(&sprite_system);

    let dom_front_layer = &scene.dom.layers.front;
    let dom_back_layer = &scene.dom.layers.back;

    let mut sprites: Vec<Sprite> = default();
    let mut css3d_objects: Vec<DomSymbol> = default();
    let count = 10;
    for i in 0..count {
        let x = i as f32;
        let width = screen.width * 1.5 / count as f32;
        let height = screen.height;
        let y = height / 2.0;
        if i % 2 == 0 {
            let height = height * 0.75;
            let size = Vector2::new(width, height);
            let position = Vector3::new(width / 1.5 * x + width / 2.0, y, 0.0);
            let sprite = sprite_system.new_instance();
            sprite.size.set(size);
            sprite.mod_position(|t| *t = position);
            sprites.push(sprite);
        } else {
            let div = web::document.create_div_or_panic();
            div.set_style_or_warn("width", "100%");
            div.set_style_or_warn("height", "100%");
            div.set_inner_html("top-left");

            let size = Vector2::new(width, height);
            let position = Vector3::new(width / 1.5 * x + width / 2.0, y, 0.0);
            let object = DomSymbol::new(&div);
            dom_front_layer.manage(&object);
            world.add_child(&object);
            let r = ((x + 0.0) * 16.0) as u8;
            let g = ((x + 2.0) * 32.0) as u8;
            let b = ((x + 4.0) * 64.0) as u8;
            let color = iformat!("rgb({r},{g},{b})");
            div.set_style_or_warn("background-color", color);

            object.dom().append_or_warn(&div);
            object.set_size(size);
            object.mod_position(|t| *t = position);
            css3d_objects.push(object);
        }
    }
    let layers = vec![dom_front_layer.clone_ref(), dom_back_layer.clone_ref()];

    let mut iter_to_change = 0;
    let mut i = 0;
    world.keep_alive_forever();
    world
        .on
        .before_frame
        .add(move |_| {
            let _keep_alive = &navigator;
            let _keep_alive = &sprites;
            let _keep_alive = &sprite_system;

            if iter_to_change == 0 {
                iter_to_change = 50;
                i = (i + 1) % 2;
                for (j, object) in css3d_objects.iter_mut().enumerate() {
                    layers[(i + j) % 2].manage(object);
                }
            }
            iter_to_change -= 1;
        })
        .forget();
}
