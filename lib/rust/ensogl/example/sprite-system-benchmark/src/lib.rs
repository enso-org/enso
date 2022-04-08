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

use ensogl_core::animation;
use ensogl_core::display::camera::Camera2d;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::symbol::geometry::Sprite;
use ensogl_core::display::symbol::geometry::SpriteSystem;
use nalgebra::Vector2;
use nalgebra::Vector3;



#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);
    let sprite_system = SpriteSystem::new(&world);

    let sprite1 = sprite_system.new_instance();
    sprite1.size.set(Vector2::new(10.0, 10.0));
    sprite1.mod_position(|t| *t = Vector3::new(5.0, 5.0, 0.0));

    scene.add_child(&sprite_system);

    let mut sprites: Vec<Sprite> = default();
    let count = 100;
    for _ in 0..count {
        let sprite = sprite_system.new_instance();
        sprite.size.set(Vector2::new(1.0, 1.0));
        sprites.push(sprite);
    }

    world.keep_alive_forever();

    let mut iter: i32 = 0;
    let mut i = 0;
    world
        .on
        .before_frame
        .add(move |time| {
            i += 1;
            if i <= 100 {
                sprite1.mod_position(|p| p.x += 1.0);
            }
            let _keep_alive = &camera;
            let _keep_alive = &iter;
            let _keep_alive = &sprite1;
            let _keep_alive = &sprites;
            let _keep_alive = &sprite_system;
            let _keep_alive = &navigator;
            // FIXME: these logs crash gui after some time!

            // println!("sprite count: {:?}",sprites.len());
            // println!("sprite_system is visible? {:?}",sprite_system.is_visible());
            // println!("sprite[5] is visible? {:?}",sprites[5].is_visible());

            on_frame(&camera, time, &mut iter, &sprite1, &mut sprites, &sprite_system)
        })
        .forget();
}

#[allow(clippy::too_many_arguments)]
#[allow(clippy::many_single_char_names)]
pub fn on_frame(
    camera: &Camera2d,
    time: animation::TimeInfo,
    iter: &mut i32,
    sprite1: &Sprite,
    sprites: &mut Vec<Sprite>,
    sprite_system: &SpriteSystem,
) {
    *iter += 1;

    let cycle_duration = 300;
    let pause_duration = 100;
    let sprite_diff_per_cycle = 100;

    let mut frozen = false;

    if *iter < cycle_duration {
        for _ in 0..sprite_diff_per_cycle {
            let sprite = sprite_system.new_instance();
            sprite.size.set(Vector2::new(1.0, 1.0));
            sprites.push(sprite);
        }
    } else if *iter < pause_duration + cycle_duration {
        sprite1.mod_position(|p| p.y += 0.5);
        frozen = true;
    } else if *iter < pause_duration + (cycle_duration * 2) {
        for _ in 0..sprite_diff_per_cycle {
            sprites.pop();
        }
    } else {
        *iter = 0;
        *sprites = default();
    }

    let screen = camera.screen();
    let half_width = screen.width / 2.0;
    let half_height = screen.height / 2.0;

    if !frozen {
        let t = time.since_animation_loop_started.unchecked_raw() / 1000.0;
        let length = sprites.len() as f32;
        for (i, sprite) in sprites.iter_mut().enumerate() {
            let i = i as f32;
            let d = (i / length - 0.5) * 2.0;

            let mut y = d;
            let r = (1.0 - y * y).sqrt();
            let mut x = (y * 100.0 + t).cos() * r;
            let mut z = (y * 100.0 + t).sin() * r;

            x += (y * 1.25 + t * 2.50).cos() * 0.5;
            y += (z * 1.25 + t * 2.00).cos() * 0.5;
            z += (x * 1.25 + t * 3.25).cos() * 0.5;

            let position = Vector3::new(
                x * 150.0 + half_width - 75.0,
                y * 150.0 + half_height - 75.0,
                z * 150.0,
            );
            sprite.set_position(position);
        }
    }
}
