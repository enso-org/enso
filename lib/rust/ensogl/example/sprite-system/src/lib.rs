#![recursion_limit = "1024"]

// === Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

// === Non-Standard Linter Configuration ===

// === Features ===
#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]

use ensogl_core::display::world::*;
use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::symbol::geometry::SpriteSystem;



#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_sprite_system() {
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
