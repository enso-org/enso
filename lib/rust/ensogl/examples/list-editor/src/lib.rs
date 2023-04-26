//! An example scene showing the list editor component usage.

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
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_list_editor::ListEditor;
use ensogl_slider as slider;



// ===================
// === Entry Point ===
// ===================

// A global FRP network used to handle events from the list editor.
ensogl_core::define_endpoints_2! {}

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

    let frp = Frp::new();
    let network = frp.network();

    frp::extend! { network
        vector_editor.insert <+ vector_editor.request_new_item.map(move |index| {
            let slider = app.new_view::<slider::Slider>();
            slider.set_size((200.0, 24.0));
            (**index, Rc::new(RefCell::new(Some(slider))))
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
    mem::forget(frp);
    mem::forget(navigator);
    mem::forget(root);
    mem::forget(vector_editor);
}
