#![allow(missing_docs)]

//! NOTE
//! This file is under a heavy development. It contains commented lines of code and some code may
//! be of poor quality. Expect drastic changes.

use basegl::traits::*;

use basegl::data::color::*;
use basegl::display::navigation::navigator::Navigator;
use basegl::display::shape::*;
use basegl::display::shape::primitive::system::ShapeSystem;
use basegl::display::shape::Var;
use basegl::display::symbol::geometry::Sprite;
use basegl::display::world::*;
use basegl::prelude::*;
use basegl::system::web;
use nalgebra::Vector2;
use wasm_bindgen::prelude::*;



#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_shapes() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    web::set_stack_trace_limit();
    init(&WorldData::new(&web::get_html_element_by_id("root").unwrap()));
}

pub mod icons {
    use super::*;

    pub fn history() -> AnyShape {
        let radius_diff    = 0.5.px();
        let corners_radius = 2.0.px();
        let width_diff     = &corners_radius * 3.0;
        let offset         = 2.px();
        let width          = 32.px();
        let height         = 16.px();
        let persp_diff1    = 6.px();

        let width2          = &width  - &width_diff;
        let width3          = &width2 - &width_diff;
        let corners_radius2 = &corners_radius  - &radius_diff;
        let corners_radius3 = &corners_radius2 - &radius_diff;
        let persp_diff2     = &persp_diff1 * 2.0;

        let rect1 = Rect((&width ,&height)).corners_radius(&corners_radius);
        let rect2 = Rect((&width2,&height)).corners_radius(&corners_radius2).translate_y(&persp_diff1);
        let rect3 = Rect((&width3,&height)).corners_radius(&corners_radius3).translate_y(&persp_diff2);

        let rect3 = rect3 - rect2.translate_y(&offset);
        let rect2 = rect2 - rect1.translate_y(&offset);

        let rect1 = rect1.fill(Srgba::new(0.26, 0.69, 0.99, 1.00));
        let rect2 = rect2.fill(Srgba::new(0.26, 0.69, 0.99, 0.6));
        let rect3 = rect3.fill(Srgba::new(0.26, 0.69, 0.99, 0.4));

        let icon = (rect3 + rect2 + rect1).translate_y(-persp_diff2/2.0);
        icon.into()
    }
}

fn ring_angle<R,W,A>(inner_radius:R, width:W, angle:A) -> AnyShape
where R : Into<Var<Distance<Pixels>>>,
      W : Into<Var<Distance<Pixels>>>,
      A : Into<Var<Angle<Radians>>> {
    let inner_radius = inner_radius.into();
    let width        = width.into();
    let angle        = angle.into();

    let angle2  = &angle / 2.0;
    let radius  = &width / 2.0;
    let inner   = Circle(&inner_radius);
    let outer   = Circle(&inner_radius + &width);
    let section = Plane().cut_angle(&angle);
    let corner1 = Circle(&radius).translate_y(inner_radius + radius);
    let corner2 = corner1.rotate(&angle2);
    let corner1 = corner1.rotate(-&angle2);
    let ring    = &outer - &inner;
    let pie     = &ring * &section;
    let out     = &pie + &corner1 + &corner2;
//    let out     = out.fill(Srgba::new(0.22,0.83,0.54,1.0));
    let out     = out.fill(Srgba::new(0.0,0.0,0.0,0.2));
    out.into()
}

fn nodes2() -> AnyShape {
    let node_radius = 32.0;
    let border_size = 16.0;
    let node   = Circle(node_radius.px());
//    let border = Circle((node_radius + border_size).px());
    let node   = node.fill(Srgb::new(0.97,0.96,0.95));
//    let node   = node.fill(Srgb::new(0.26,0.69,0.99));
//    let border = border.fill(Srgba::new(0.0,0.0,0.0,0.06));

    let bg   = Circle((node_radius*2.0).px());
    let bg   = bg.fill(Srgb::new(0.91,0.91,0.90));


//    let shadow1 = Circle((node_radius + border_size).px());
//    let shadow1_color = LinearGradient::new()
//        .add(0.0,Srgba::new(0.0,0.0,0.0,0.08).into_linear())
//        .add(1.0,Srgba::new(0.0,0.0,0.0,0.0).into_linear());
//    let shadow1_color = SdfSampler::new(shadow1_color).max_distance(border_size).slope(Slope::InvExponent(5.0));
//    let shadow1       = shadow1.fill(shadow1_color);

    let shadow2 = Circle((node_radius + border_size).px());
    let shadow2_color = LinearGradient::new()
        .add(0.0,Srgba::new(0.0,0.0,0.0,0.0).into_linear())
        .add(1.0,Srgba::new(0.0,0.0,0.0,0.3).into_linear());
//    let shadow2_color = ExponentSampler::new(shadow2_color);
    let shadow2_color = SdfSampler::new(shadow2_color).max_distance(border_size).slope(Slope::Exponent(4.0));
    let shadow2       = shadow2.fill(shadow2_color);


    let loader_angle : Var<Angle<Radians>> = "Radians(clamp(input_time/2000.0 - 1.0) * 1.99 * PI)".into();
    let loader_angle2 = &loader_angle / 2.0;
    let loader        = ring_angle((node_radius).px(), (border_size).px(), loader_angle);
    let loader        = loader.rotate(loader_angle2);
    let loader        = loader.rotate("Radians(input_time/200.0)");

    let icon = icons::history();


    let out = bg + loader + shadow2 + node + icon;
    out.into()
}

fn nodes3() -> AnyShape {
    nodes2().fill(Srgb::new(1.0,0.0,0.0)).into()
}


fn init(world: &World) {


    let scene  = world.scene();
    let camera = scene.camera();
    let screen = camera.screen();
    let navigator = Navigator::new(&scene,&camera);


    let node_shape =     nodes2();
    let _node_shape2 =     nodes3();

    let shape_system =     ShapeSystem::new(world,&node_shape);



    let sprite = shape_system.new_instance();
    sprite.size().set(Vector2::new(200.0,200.0));
    sprite.mod_position(|t| {
        t.x += screen.width / 2.0;
        t.y += screen.height / 2.0;
    });

    let sprite_2 = shape_system.new_instance();
    sprite_2.size().set(Vector2::new(200.0,200.0));
    sprite_2.mod_position(|t| {
        t.x += screen.width / 2.0 + 5.0;
        t.y += screen.height / 2.0 + 20.0;
    });

    let _sprite2 = sprite.clone();


    world.add_child(&shape_system);
//
//    let out = frp_test(Box::new(move|x:f32,y:f32| {
//        sprite2.set_position(Vector3::new(x,y,0.0));
//    }));

    let mut iter:i32 = 0;
    let mut time:i32 = 0;
    let mut was_rendered = false;
    let mut loader_hidden = false;
    let mut i = 10;
    world.on_frame(move |_| {
        i -= 1;
        if i == 0 {
//            shape_system.set_shape(&node_shape2);
        }
        let _keep_alive = &sprite;
        let _keep_alive = &navigator;

//        let _keep_alive = &sprite_2;
//        let _keep_alive = &out;
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



//// ================
//// === FRP Test ===
//// ================
//
//#[allow(unused_variables)]
//pub fn frp_test (callback: Box<dyn Fn(f32,f32)>) -> MouseManager {
//    let document        = web::document().unwrap();
//    let mouse_manager   = MouseManager::new(&document);
//    let mouse           = Mouse::new();
//
//    frp! {
//        mouse_down_position    = mouse.position.sample       (&mouse.on_down);
//        mouse_position_if_down = mouse.position.gate         (&mouse.is_down);
//        final_position_ref     = recursive::<Position>       ();
//        pos_diff_on_down       = mouse_down_position.map2    (&final_position_ref,|m,f|{m-f});
//        final_position         = mouse_position_if_down.map2 (&pos_diff_on_down  ,|m,f|{m-f});
//        debug                  = final_position.sample       (&mouse.position);
//    }
//    final_position_ref.initialize(&final_position);
//
//    // final_position.event.display_graphviz();
//
////    trace("X" , &debug.event);
//
////    final_position.map("foo",move|p| {callback(p.x as f32,-p.y as f32)});
//
//    let target = mouse.position.event.clone_ref();
//    let handle = mouse_manager.on_move.add(move |event:&OnMove| {
//        target.emit(Position::new(event.client_x(),event.client_y()));
//    });
//    handle.forget();
//
//    let target = mouse.on_down.event.clone_ref();
//    let handle = mouse_manager.on_down.add(move |event:&OnDown| {
//        target.emit(());
//    });
//    handle.forget();
//
//    let target = mouse.on_up.event.clone_ref();
//    let handle = mouse_manager.on_up.add(move |event:&OnUp| {
//        target.emit(());
//    });
//    handle.forget();
//
//    mouse_manager
//}
