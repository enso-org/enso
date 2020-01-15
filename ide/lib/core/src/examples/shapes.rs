#![allow(missing_docs)]

use crate::display::object::DisplayObjectOps;
use crate::display::symbol::geometry::Sprite;
use crate::display::shape::primitive::system::ShapeSystem;
use crate::display::world::*;
use crate::system::web::set_stdout;
use crate::system::web::set_stack_trace_limit;
use crate::system::web::forward_panic_hook_to_console;

use nalgebra::Vector2;
use wasm_bindgen::prelude::*;

use crate::display::shape::primitive::def::*;


#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_shapes() {
    forward_panic_hook_to_console();
    set_stdout();
    set_stack_trace_limit();
    init(&WorldData::new("canvas"));
}

fn init(world: &World) {
    let s1 = Circle("25.0 + 20.0*sin(input_time/1000.0)");
    let s2 = s1.translate(25.0,0.0);
    let s3 = &s1 + &s2;

    let shape_system = ShapeSystem::new(world,&s3);
    let sprite = shape_system.new_instance();
    sprite.set_bbox(Vector2::new(200.0,200.0));
    sprite.mod_position(|t| {
        t.x += 250.0;
        t.y += 100.0;
    });

    let mut iter:i32 = 0;
    let mut time:i32 = 0;
    world.on_frame(move |_| {
        on_frame(&mut time,&mut iter,&sprite,&shape_system)
    }).forget();
}

#[allow(clippy::too_many_arguments)]
#[allow(clippy::many_single_char_names)]
pub fn on_frame
( _time        : &mut i32
, iter         : &mut i32
, _sprite1     : &Sprite
, shape_system : &ShapeSystem) {
    *iter += 1;
    shape_system.update();
}
