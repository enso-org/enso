//! A debug scene which shows the scroll area.

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
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::Circle;
use ensogl_core::display::shape::PixelDistance;
use ensogl_core::display::shape::Rect;
use ensogl_core::display::shape::ShapeOps;
use ensogl_core::display::shape::ShapeSystem;
use ensogl_core::display::Sprite;
use ensogl_hardcoded_theme as theme;
use ensogl_scroll_area::ScrollArea;
use ensogl_text_msdf_sys::run_once_initialized;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
pub fn entry_point_scroll_area() {
    run_once_initialized(|| {
        let app = Application::new("root");
        init(&app);
        mem::forget(app);
    });
}



// ========================
// === Init Application ===
// ========================

fn init(app: &Application) {
    theme::builtin::dark::register(&app);
    theme::builtin::light::register(&app);
    theme::builtin::light::enable(&app);

    let scene = &app.display.default_scene;
    scene.camera().set_position_xy(Vector2(100.0, -100.0));


    // === Background ===

    let background_color = color::Rgba::new(0.9, 0.9, 0.9, 1.0);
    let background_size = (200.px(), 200.px());
    let background_shape = Rect(background_size).corners_radius(5.5.px()).fill(background_color);
    let background_system = ShapeSystem::new(scene, background_shape);
    let background: Sprite = background_system.new_instance();
    scene.add_child(&background_system);
    background.size.set(Vector2::new(200.0, 200.0));
    background.set_position_x(100.0);
    background.set_position_y(-100.0);
    std::mem::forget(background);


    // === Scroll Area ===

    let scroll_area = ScrollArea::new(app);
    app.display.add_child(&scroll_area);
    scroll_area.resize(Vector2(200.0, 200.0));
    scroll_area.set_content_width(300.0);
    scroll_area.set_content_height(1000.0);


    // === Content ===

    let sprite_system = ShapeSystem::new(scene, &Circle(50.px()));
    let sprite: Sprite = sprite_system.new_instance();
    scroll_area.content.add_child(&sprite_system);
    sprite.size.set(Vector2::new(100.0, 100.0));
    sprite.set_position_x(100.0);
    sprite.set_position_y(-100.0);
    std::mem::forget(sprite);


    std::mem::forget(scroll_area);
}
