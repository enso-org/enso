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


// ==============
// === Export ===
// ==============

pub use ensogl_core::system::web::dom::Shape;



const ELEM_COUNT: i32 = 10;
const HTML_PADDING: f32 = 10.0;
const VERTICAL_MARGIN: f32 = 10.0;
const HEIGHT_FRACTION: f32 = 0.8;

fn update_shape(screen: Shape, sprites: &[Sprite], dom_symbols: &[DomSymbol]) {
    let side_offset = 10.0;
    let display_objects = sprites
        .iter()
        .map(|s| s.display_object())
        .interleave(dom_symbols.iter().map(|s| s.display_object()));
    let width = (screen.width - 3.0 * side_offset) / ELEM_COUNT as f32 + 2.0 * side_offset;
    let height = screen.height;
    for (i, object) in display_objects.enumerate() {
        let fi = i as f32;
        let x = -screen.width / 2.0 + width / 2.0 + (width - 2.0 * side_offset) * fi;
        object.set_position(Vector3(x, 0.0, 0.0));
    }
    for symbol in sprites {
        let size = Vector2::new(width, height * HEIGHT_FRACTION);
        symbol.size.set(size);
        symbol.update_y(|y| y - screen.height / 2.0 + size.y / 2.0 + VERTICAL_MARGIN);
    }
    for symbol in dom_symbols {
        let size = Vector2::new(width, height * HEIGHT_FRACTION - HTML_PADDING * 2.0);
        symbol.set_size(size);
        symbol.update_x(|y| y - HTML_PADDING);
        symbol.update_y(|y| y + HTML_PADDING);
        symbol.update_y(|y| {
            y + screen.height / 2.0 - (size.y + HTML_PADDING * 2.0) / 2.0 - VERTICAL_MARGIN
        });
    }
}

#[entry_point]
#[allow(dead_code)]
#[allow(clippy::many_single_char_names)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera();
    let navigator = Navigator::new(scene, &camera);
    let sprite_system = SpriteSystem::new();
    world.add_child(&sprite_system);

    let dom_front_layer = &scene.dom.layers.front;
    let dom_back_layer = &scene.dom.layers.back;

    let mut sprites: Vec<Sprite> = default();
    let mut dom_symbols: Vec<DomSymbol> = default();
    for i in 0..ELEM_COUNT {
        let fi = i as f32;
        if i % 2 == 0 {
            let sprite = sprite_system.new_instance();
            sprites.push(sprite);
        } else {
            let div = web::document.create_div_or_panic();
            div.set_style_or_warn("width", "100%");
            div.set_style_or_warn("height", "100%");
            div.set_style_or_warn("padding", format!("{HTML_PADDING}px"));
            div.set_style_or_warn(
                "font-family",
                "SF Pro Display,SF Pro Icons,Helvetica Neue,Helvetica,Arial,sans-serif",
            );
            div.set_inner_html(
                "This is a dom element.<br/>\
                Black boxes are WebGL sprites.<br/><br/>\
                Try zooming and moving the scene!",
            );

            let object = DomSymbol::new(&div);
            dom_front_layer.manage(&object);
            world.add_child(&object);
            let r = ((fi + 2.0) * 64.0 / (ELEM_COUNT as f32)) as u8;
            let g = ((fi + 4.0) * 128.0 / (ELEM_COUNT as f32)) as u8;
            let b = ((fi + 8.0) * 255.0 / (ELEM_COUNT as f32)) as u8;
            let color = iformat!("rgb({r},{g},{b})");
            div.set_style_or_warn("background-color", color);
            object.dom().append_or_warn(&div);
            dom_symbols.push(object);
        }
    }

    let network = ensogl_core::frp::Network::new("network");

    let sprites2 = sprites.clone();
    let dom_symbols2 = dom_symbols.clone();
    ensogl_core::frp::extend! { network
        eval scene.frp.shape ([] (screen) update_shape(*screen, &sprites2, &dom_symbols2));
    }


    let layers = vec![dom_front_layer.clone_ref(), dom_back_layer.clone_ref()];

    let mut iter_to_change = 0;
    let mut i = 0;
    world.keep_alive_forever();
    world
        .on
        .before_frame
        .add(move |_| {
            let _keep_alive = &network;
            let _keep_alive = &navigator;
            let _keep_alive = &sprite_system;

            if iter_to_change == 0 {
                iter_to_change = 50;
                i = (i + 1) % 2;
                for (j, object) in dom_symbols.iter_mut().enumerate() {
                    layers[(i + j) % 2].manage(object);
                }
            }
            iter_to_change -= 1;
        })
        .forget();
}
