//! A debug scene which shows the scroll area.

#![feature(associated_type_defaults)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![recursion_limit = "1024"]

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
use ensogl_core::system::web;
use ensogl_hardcoded_theme as theme;
use ensogl_label::Label;
use ensogl_scroll_area::ScrollArea;
use ensogl_text_msdf_sys::run_once_initialized;



// ===================
// === Entry Point ===
// ===================

/// An entry point.
#[wasm_bindgen]
pub fn entry_point_scroll_area() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();
    run_once_initialized(|| {
        let app = Application::new(&web::get_html_element_by_id("root").unwrap());
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

    let scene = app.display.scene();
    scene.camera().set_position_xy(Vector2(100.0, -100.0));


    // === Background ===

    let background_color = color::Rgba::new(0.9, 0.9, 0.9, 1.0);
    let background_size = (200.px(), 200.px());
    let background_shape = Rect(background_size).corners_radius(5.5.px()).fill(background_color);
    let background_system = ShapeSystem::new(scene, background_shape);
    let background: Sprite = background_system.new_instance();
    scene.add_child(&background);
    background.size.set(Vector2::new(300.0, 300.0));
    background.set_position_x(150.0);
    background.set_position_y(-150.0);
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
    scroll_area.content.add_child(&sprite);
    sprite.size.set(Vector2::new(100.0, 100.0));
    sprite.set_position_x(100.0);
    sprite.set_position_y(-100.0);
    std::mem::forget(sprite);

    let label0 = Label::new(app);
    scroll_area.content.add_child(&label0);
    label0.frp.set_content(
        "FOO BAR BAZ\n\
        SOME MORE CONTENT\n\
        INSIDE SCROLLBAR HOPEFULLY");
    std::mem::forget(label0);

    let label = Label::new(app);
    // TODO: 'app.display' or 'scene' ?
    app.display.add_child(&label);
    label.frp.set_content(
        "Some relatively long text with no super important meaning that should be known,\n\
        however, spanning multiple lines and hopefully also being vertically extensive enough to matter.\n\
        line 3\n\
        line 4\n\
        line 5\n\
        line 6\n\
        line 7\n\
        line 8\n\
        line 9\n\
        line 10");
    std::mem::forget(label);




    use ensogl_core::display::symbol::geometry::SpriteSystem;
    let sprite_system = SpriteSystem::new(&app.display);
    // let sprite_system = SpriteSystem::new(&world);

    let sprite2 = sprite_system.new_instance();
    let sprite1 = sprite_system.new_instance();
    sprite1.size.set(Vector2::new(15.0, 15.0));
    sprite2.size.set(Vector2::new(15.0, 15.0));
    Leak::new(sprite1);
    Leak::new(sprite2);

    scroll_area.content.add_child(&sprite_system);
    // scene.add_child(&sprite_system);



    // let sprite_system = ShapeSystem::new(scene, &Circle(50.px()));
    let shape = Circle(50.px()).fill(color::Rgb::new(1.0, 0.0, 0.0));
    let sprite_system = ShapeSystem::new(&app.display, &shape);
    // let sprite_system = ShapeSystem::new(&app.display, &Circle(50.px()));
    let sprite: Sprite = sprite_system.new_instance();
    // scene.add_child(&sprite);
    app.display.add_child(&sprite);
    sprite.size.set(Vector2::new(100.0, 100.0));
    // sprite.set_position_x(100.0);
    // sprite.set_position_y(-100.0);
    std::mem::forget(sprite);

    std::mem::forget(scroll_area);
}
