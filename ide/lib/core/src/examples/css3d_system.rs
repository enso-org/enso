#![allow(missing_docs)]

use crate::system::web;
use web::dom::html::Css3dSystem;
use web::dom::html::Css3dObject;
use web::dom::html::Css3dOrder;
use web::StyleSetter;
use crate::display::object::DisplayObject;
use crate::display::object::DisplayObjectOps;
use crate::display::symbol::geometry::Sprite;
use crate::display::symbol::geometry::SpriteSystem;
use crate::display::world::*;
use crate::prelude::*;
use crate::animation::animator::fixed_step::FixedStepAnimator;

use nalgebra::Vector2;
use nalgebra::Vector3;
use wasm_bindgen::prelude::*;
use crate::display::navigation::navigator::Navigator;

#[wasm_bindgen]
#[allow(dead_code)]
pub fn run_example_css3d_system() {
    web::forward_panic_hook_to_console();
    web::set_stdout();
    init(WorldData::new("canvas"));
}

fn init(world:World) {
    let scene         = world.scene();
    let camera        = scene.camera();
    let screen        = camera.screen();
    let navigator     = Navigator::new(&scene, &camera).expect("Couldn't create navigator");
    let sprite_system = SpriteSystem::new(&world);
    let css3d_system  = Css3dSystem::new(&world);
    world.add_child(&sprite_system);
    world.add_child(&css3d_system);

    let mut sprites: Vec<Sprite> = default();
    let mut css3d_objects: Vec<Css3dObject> = default();
    let count = 10;
    for i in 0 .. count {
        let x          = i as f32;
        let width      = screen.width * 1.5 / count as f32;
        let height     = screen.height;
        if i % 2 == 0 {
            let height = height * 0.75;
            let dimensions = Vector2::new(width, height);
            let y          = screen.height / 2.0;
            let position   = Vector3::new(width / 1.5 * x + width / 2.0, y, 0.0);
            let sprite     = sprite_system.new_instance();
            sprite.size().set(dimensions);
            sprite.mod_position(|t| *t = position);
            sprites.push(sprite);
        } else {
            let dimensions = Vector2::new(width, height);
            let position   = Vector3::new(width / 1.5 * x + width / 2.0, height / 2.0, 0.0);
            let mut object = css3d_system.new_instance("div").expect("Couldn't create div");
            let r          = ((x + 0.0) * 16.0) as u8;
            let g          = ((x + 2.0) * 32.0) as u8;
            let b          = ((x + 4.0) * 64.0) as u8;
            let color      = iformat!("rgb({r},{g},{b})");
            object.dom().set_style_or_panic("background-color",color);
            object.set_dimensions(dimensions);
            object.mod_position(|t| *t = position);
            css3d_objects.push(object);
        }
    }
    world.display_object().update();

    let css3d_position = vec![Css3dOrder::Front, Css3dOrder::Back];
    let mut i = 0;
    let animator = FixedStepAnimator::new(2.0, move |_| {
        let _keep_alive = &world;
        let _keep_alive = &navigator;
        let _keep_alive = &sprites;
        let _keep_alive = &sprite_system;
        let _keep_alive = &css3d_system;

        i = (i + 1) % 2;
        for (j, object) in css3d_objects.iter_mut().enumerate() {
            object.set_css3d_order(css3d_position[(i + j) % 2]);
        }
    });
    std::mem::forget(animator);
}
