//! Example scene showing the usage of display object auto-layout.

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::display::world::*;
use ensogl_core::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::style::theme;

use ensogl_hardcoded_theme;



// ==============
// === Shapes ===
// ==============

mod rectangle {
    use super::*;
    ensogl_core::shape! {
        (style: Style, color: Vector4<f32>) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let rect = Rect((&width, &height)).corners_radius(10.0.px());
            let shape = rect.fill(color::Rgba(0.0,0.0,0.0,0.3));
            shape.into()
        }
    }
}

const PADDING: f32 = 3.0;
const SIDES_PADDING: f32 = PADDING * 2.0;

pub mod cursor {
    use super::*;
    ensogl_core::shape! {
        pointer_events = false;
        ( style  : Style
        , press  : f32
        , radius : f32
        , color  : Vector4
        ) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let press_side_shrink = 2.px();
            let press_diff        = press_side_shrink * &press;
            let radius            = 1.px() * radius - &press_diff;
            let sides_padding     = 1.px() * SIDES_PADDING;
            let width             = &width  - &press_diff * 2.0 - &sides_padding;
            let height            = &height - &press_diff * 2.0 - &sides_padding;
            let cursor            = Rect((width,height)).corners_radius(radius);
            let cursor            = cursor.fill("srgba(input_color)");
            cursor.into()
        }
    }
}

pub mod node {
    ensogl_core::shape_old! {
        // Disable to allow interaction with the output port.
        pointer_events = true;
        (style:Style, selection:f32) {

            let width  = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let width  = width  - 4.px();
            let height = height - 4.px();

            // === Shadow ===

            // let shadow_radius = &height / 2.0;
            // let shadow_base   = Rect((&width,&height)).corners_radius(shadow_radius);
            // let shadow        = shadow::from_shape(shadow_base.into(),style);


            // === Selection ===

            // let sel_color  = style.get_color(ensogl_hardcoded_theme::graph_editor::node::selection);
            let sel_size   = style.get_number(ensogl_hardcoded_theme::graph_editor::node::selection::size);
            let sel_offset = style.get_number(ensogl_hardcoded_theme::graph_editor::node::selection::offset);

            warn!("sel_size: {:?}", sel_size);
            warn!("sel_offset: {:?}", sel_offset);

            let sel_width   = &width  - 2.px() + &sel_offset.px() * 2.0 * &selection;
            let sel_height  = &height - 2.px() + &sel_offset.px() * 2.0 * &selection;
            let sel_radius  = &sel_height / 2.0;
            warn!("select size: {:?}, {:?}", sel_width, sel_height);
            let select      = Rect((&sel_width,&sel_height)).corners_radius(sel_radius);

            let sel2_width  = &width  - 2.px() + &(sel_size + sel_offset).px() * 2.0 * &selection;
            let sel2_height = &height - 2.px() + &(sel_size + sel_offset).px() * 2.0 * &selection;
            let sel2_radius = &sel2_height / 2.0;
            warn!("select2 size: {:?}, {:?}", sel2_width, sel2_height);
            let select2     = Rect((&sel2_width,&sel2_height)).corners_radius(sel2_radius);

            let select = select2 - select;
            let select = select.fill(color::Rgba::green());


             // === Error Pattern  Alternative ===
             // TODO: Remove once the error indicator design is finalised.
             // let repeat      =  Var::<Vector2<Pixels>>::from((10.px(), 10.px()));
             // let error_width =  Var::<Pixels>::from(5.px());
             //
             // let stripe_red   = Rect((error_width, 99999.px()));
             // let pattern = stripe_red.repeat(repeat).rotate(45.0.radians());
             // let mask    = Rect((&width,&height)).corners_radius(&radius);
             // let pattern1 = mask.intersection(pattern).fill(color::Rgba::red());

             // let out =  select + shadow + shape + pattern1;

            // === Final Shape ===

            let out = select;// + shadow;
            out.into()
        }
    }
}


// ===================
// === Entry Point ===
// ===================

#[before_main]
pub fn test() {
    println!("test");
}

/// The example entry point.
/// foo bar baz
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let world = World::new().displayed_in("root");
    let scene = &world.default_scene;
    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let themes = theme::Manager::from(&scene.style_sheet);
    ensogl_hardcoded_theme::builtin::light::register(&themes);
    ensogl_hardcoded_theme::builtin::light::enable(&themes);
    themes.update();
    mem::forget(themes);

    // let cursor = cursor::View::new();
    // cursor.set_size(Vector2(20.0, 20.0));
    // cursor.radius.set(10.0);
    // cursor.color.set(color::Rgba::new(1.0, 0.0, 0.0, 0.3).into());
    // world.add_child(&cursor);

    let node = node::View::new();
    node.set_size(Vector2(200.0, 200.0));
    node.selection.set(1.0);
    world.add_child(&node);
    mem::forget(node);


    // let rect1 = rectangle::View::new();
    // rect1.set_size(Vector2::new(100.0, 100.0));
    // rect1.color.set(color::Rgba::new(0.5, 0.0, 0.0, 0.3).into());
    //
    // let rect2 = rectangle::View::new();
    // rect2.set_size(Vector2::new(100.0, 100.0));
    // rect2.color.set(color::Rgba::new(0.5, 0.0, 0.0, 0.3).into());
    //
    // let root = display::object::Instance::new();
    // root.set_size(Vector2::new(300.0, 100.0));
    // root.use_auto_layout();
    // root.add_child(&rect1);
    // root.add_child(&rect2);
    // world.add_child(&root);



    // warn!("rect1: {:?}", rect1.display_object());

    // let r = rect1.clone_ref();
    // let mut i = 0;
    // world
    //     .on
    //     .before_frame
    //     .add(move |_| {
    //         if i == 10 {
    //             warn!("rect1: {:?}", r.display_object());
    //             // warn!("rect1 sprite: {:?}", r.sprite.borrow().display_object());
    //         }
    //         i += 1;
    //     })
    //     .forget();

    world.keep_alive_forever();
    mem::forget(navigator);
    // mem::forget(cursor);
    // mem::forget(root);
    // mem::forget(rect1);
    // mem::forget(rect2);
    warn!("Hello World!");
}
