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
use wasm_bindgen::prelude::*;

use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::symbol::geometry::SpriteSystem;



#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let navigator = Navigator::new(&world.default_scene, &world.default_scene.camera());
    let sprite_system = SpriteSystem::new(&world);

    let sprite2 = sprite_system.new_instance();
    let sprite1 = sprite_system.new_instance();
    sprite1.size.set(Vector2::new(15.0, 15.0));
    sprite2.size.set(Vector2::new(15.0, 15.0));

    world.default_scene.add_child(&sprite_system);
    world.keep_alive_forever();

    let mut i = 0;
    world
        .on
        .after_frame
        .add(move |_| {
            i += 1;
            let _keep_alive = &navigator;
            let _keep_alive = &sprite1;
            let _keep_alive = &sprite2;
            let _keep_alive = &sprite_system;
            if i <= 100 {
                sprite1.mod_position(|p| p.x += 1.0);
                sprite2.mod_position(|p| p.y += 1.0);
            }
        })
        .forget();
}
