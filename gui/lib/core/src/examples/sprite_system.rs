#![allow(missing_docs)]

use crate::display::object::DisplayObjectOps;
use crate::display::symbol::geometry::sprite::Sprite;
use crate::display::symbol::geometry::sprite::SpriteSystem;
use crate::display::world::*;
use crate::prelude::*;
use crate::system::web::set_stdout;
use crate::system::web::forward_panic_hook_to_console;

use nalgebra::Vector2;
use nalgebra::Vector3;
use wasm_bindgen::prelude::*;



#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_sprite_system() {
    forward_panic_hook_to_console();
    set_stdout();
    init(&WorldData::new("canvas"));
}

fn init(world: &World) {
    let sprite_system = SpriteSystem::new(world);
    let sprite1 = sprite_system.new_instance();
    sprite1.set_bbox(Vector2::new(10.0,10.0));
    sprite1.mod_position(|t| t.x += 10.0);

    let mut sprites: Vec<Sprite> = default();
    let count = 100;
    for _ in 0 .. count {
        let sprite = sprite_system.new_instance();
        sprites.push(sprite);
    }

    let mut iter:i32 = 0;
    let mut time:i32 = 0;
    world.on_frame(move |_| {
        on_frame(&mut time,&mut iter,&sprite1,&mut sprites,&sprite_system)
    }).forget();
}

#[allow(clippy::too_many_arguments)]
#[allow(clippy::many_single_char_names)]
pub fn on_frame
( time          : &mut i32
, iter          : &mut i32
, sprite1       : &Sprite
, sprites       : &mut Vec<Sprite>
, sprite_system : &SpriteSystem) {

//        camera.mod_position(|p| {
//            p.x -= 0.1;
//            p.z += 1.0
//        });

    *iter += 1;

    let cycle_duration        = 300;
    let pause_duration        = 100;
    let sprite_diff_per_cycle = 100;

    let mut frozen = false;

    if *iter < cycle_duration {
        for _ in 0..sprite_diff_per_cycle {
            let sprite = sprite_system.new_instance();
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

    if !frozen {
        *time += 1;
        let t = *time as f32 / 50.0;
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
            sprite.set_position(Vector3::new(x * 50.0 + 200.0, y * 50.0 + 100.0, z * 50.0));
        }
    }

    sprite_system.update();
}
