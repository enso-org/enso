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
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;
use std::mem;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::control::io::mouse;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::shape::compound::rectangle::*;
use ensogl_list_editor::ListEditor;
use ensogl_slider as slider;


// ===================
// === Entry Point ===
// ===================

pub mod glob {
    use super::*;
    ensogl_core::define_endpoints_2! {
        Input {
        }
        Output {
        }
    }
}

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    let app = Application::new("root");
    let world = app.display.clone();
    let scene = &world.default_scene;

    let camera = scene.camera().clone_ref();
    let navigator = Navigator::new(scene, &camera);

    let vector_editor = ListEditor::new(&app.cursor);


    let slider1 = app.new_view::<slider::Slider>();
    slider1.set_size((200.0, 24.0));

    let slider2 = app.new_view::<slider::Slider>();
    slider2.set_size((200.0, 24.0));

    let slider3 = app.new_view::<slider::Slider>();
    slider3.set_size((200.0, 24.0));


    // let shape1 = Circle().build(|t| {
    //     t.set_size(Vector2(60.0, 100.0))
    //         .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
    //         .set_inset_border(2.0)
    //         .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5))
    //         .keep_bottom_left_quarter();
    // });
    // let shape2 = RoundedRectangle(10.0).build(|t| {
    //     t.set_size(Vector2(120.0, 100.0))
    //         .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
    //         .set_inset_border(2.0)
    //         .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
    // });
    // let shape3 = RoundedRectangle(10.0).build(|t| {
    //     t.set_size(Vector2(240.0, 100.0))
    //         .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
    //         .set_inset_border(2.0)
    //         .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
    // });


    let glob_frp = glob::Frp::new();
    let glob_frp_network = glob_frp.network();

    // let shape1_down = shape1.on_event::<mouse::Down>();
    frp::extend! { glob_frp_network
        // trace vector_editor.request_new_item;
        vector_editor.push <+ vector_editor.request_new_item.map(move |index| {
            // let shape = RoundedRectangle(10.0).build(|t| {
            //     t.set_size(Vector2(100.0, 100.0))
            //         .set_color(color::Rgba::new(0.0, 0.0, 0.0, 0.1))
            //         .set_inset_border(2.0)
            //         .set_border_color(color::Rgba::new(0.0, 0.0, 0.0, 0.5));
            // });
            let slider = app.new_view::<slider::Slider>();
            slider.set_size((200.0, 24.0));
            // (**index, Rc::new(RefCell::new(Some(slider))))
            Rc::new(RefCell::new(Some(slider)))
        });
    }

    vector_editor.push(slider1);
    vector_editor.push(slider2);
    vector_editor.push(slider3);

    let root = display::object::Instance::new();
    root.set_size(Vector2(300.0, 100.0));
    root.add_child(&vector_editor);
    world.add_child(&root);



    world.keep_alive_forever();
    // mem::forget(app);
    mem::forget(glob_frp);
    mem::forget(navigator);
    mem::forget(root);
    mem::forget(vector_editor);
}
