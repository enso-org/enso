#![allow(missing_docs)]

//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

use basegl::display::object::DisplayObject;
use basegl::display::object::DisplayObjectOps;
use basegl::display::symbol::geometry::Sprite;
use basegl::display::shape::primitive::system::ShapeSystem;
use basegl::display::world::*;
use basegl::system::web::set_stdout;
use basegl::system::web::set_stack_trace_limit;
use basegl::system::web::forward_panic_hook_to_console;
use basegl::display::shape::primitive::def::*;

use nalgebra::Vector2;
use wasm_bindgen::prelude::*;

//use basegl::display::navigation::navigator::Navigator;

use basegl::prelude::*;
use enso_frp::*;

use basegl::system::web;
use basegl::control::io::mouse2;
use basegl::control::io::mouse2::MouseManager;
use basegl::data::color::*;


#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_shapes() {
    forward_panic_hook_to_console();
    set_stdout();
    set_stack_trace_limit();
    init(&WorldData::new(&web::body()));
}

fn init(world: &World) {
    let scene  = world.scene();
    let camera = scene.camera();
    let screen = camera.screen();

//    let navigator = Navigator::new(&scene, &camera);
//    let navigator = navigator.expect("Couldn't create navigator");
    let node_radius = 25.0;
    let shadow_span = 10.0;
    let node   = Circle(node_radius);
    let shadow = Circle(node_radius + shadow_span);


    let node_color = Srgb::new(1.0,1.0,1.0);
    let node = node.fill(node_color);

    let shadow_color = Gradient::new()
        .add(0.0,Srgba::new(0.0,0.0,0.0,0.0).into_linear())
        .add(1.0,Srgba::new(0.0,0.0,0.0,0.1).into_linear());
    let shadow_color = DistanceGradient::new(shadow_color).max_distance(shadow_span).slope(Slope::Exponent(2.0));
    let shadow       = shadow.fill(shadow_color);

    let out = &shadow + &node;

    let shape_system = ShapeSystem::new(world,&out);
    let sprite = shape_system.new_instance();
    sprite.size().set(Vector2::new(200.0,200.0));
    sprite.mod_position(|t| {
        t.x += screen.width / 2.0;
        t.y += screen.height / 2.0;
    });

    let sprite2 = sprite.clone();


    world.add_child(&shape_system);

    let out = frp_test(Box::new(move|x:f32,y:f32| {
        sprite2.set_position(Vector3::new(x,y,0.0));
    }));

    let mut iter:i32 = 0;
    let mut time:i32 = 0;
    let mut was_rendered = false;
    let mut loader_hidden = false;
    world.on_frame(move |_| {
        let _keep_alive = &out;
        on_frame(&mut time,&mut iter,&sprite,&shape_system);
        if was_rendered && !loader_hidden {
            web::get_element_by_id("loader").map(|t| {
                t.parent_node().map(|p| {
                    p.remove_child(&t).unwrap()
                })
            }).ok();
            loader_hidden = true;
        }
        was_rendered = true;
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
    shape_system.display_object().update();
}



// ================
// === FRP Test ===
// ================

#[allow(unused_variables)]
pub fn frp_test (callback: Box<dyn Fn(f32,f32)>) -> MouseManager {
    let document        = web::document().unwrap();
    let mouse_manager   = MouseManager::new(&document);
    let mouse           = Mouse::new();

    frp! {
        mouse_down_position    = mouse.position.sample       (&mouse.on_down);
        mouse_position_if_down = mouse.position.gate         (&mouse.is_down);
        final_position_ref     = recursive::<Position>       ();
        pos_diff_on_down       = mouse_down_position.map2    (&final_position_ref,|m,f|{m-f});
        final_position         = mouse_position_if_down.map2 (&pos_diff_on_down  ,|m,f|{m-f});
        debug                  = final_position.sample       (&mouse.position);
    }
    final_position_ref.initialize(&final_position);

    // final_position.event.display_graphviz();

//    trace("X" , &debug.event);

//    final_position.map("foo",move|p| {callback(p.x as f32,-p.y as f32)});

    let target = mouse.position.event.clone_ref();
    let handle = mouse_manager.on_move.add(move |event:&mouse2::event::OnMove| {
        target.emit(Position::new(event.client_x(),event.client_y()));
    });
    handle.forget();

    let target = mouse.on_down.event.clone_ref();
    let handle = mouse_manager.on_down.add(move |event:&mouse2::event::OnDown| {
        target.emit(());
    });
    handle.forget();

    let target = mouse.on_up.event.clone_ref();
    let handle = mouse_manager.on_up.add(move |event:&mouse2::event::OnUp| {
        target.emit(());
    });
    handle.forget();

    mouse_manager
}
