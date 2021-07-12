//! Example showing the file browser icons with animated stroke width on a grid. The grid may only
//! become visible on high zoom levels.

use ensogl_core::prelude::*;

use ensogl_gui_components::file_browser::icons::*;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::system::web;
use wasm_bindgen::prelude::*;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::world::*;
use ensogl_core::data::color;
use ensogl_core::display::DomSymbol;
use ensogl_web::StyleSetter;



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_icons() {
    web::forward_panic_hook_to_console();
    web::set_stack_trace_limit();

    let logger = Logger::new("Icons example");
    let world  = World::new(&web::get_html_element_by_id("root").unwrap());
    world.keep_alive_forever();
    let scene = world.scene();
    mem::forget(Navigator::new(&scene,&scene.camera()));


    // === Grid ===

    let grid_div = web::create_div();
    grid_div.set_style_or_panic("width",  "200px");
    grid_div.set_style_or_panic("height", "16px");
    grid_div.set_style_or_panic("background-size", "0.5px 0.5px");
    grid_div.set_style_or_panic("background-image",
                                "linear-gradient(to right,  grey 0.05px, transparent 0.05px),
                                 linear-gradient(to bottom, grey 0.05px, transparent 0.05px)");

    let grid = DomSymbol::new(&grid_div);
    scene.dom.layers.back.manage(&grid);
    world.add_child(&grid);
    grid.set_size(Vector2(200.0,ICON_SIZE));
    mem::forget(grid);


    // === Project ===

    let project_icon = project::View::new(&logger);
    world.add_child(&project_icon);
    project_icon.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    project_icon.color_rgba.set(color::Rgba::black().into());
    project_icon.set_position_x(-60.0);


    // === Home ===

    let home_icon = home::View::new(&logger);
    world.add_child(&home_icon);
    home_icon.size.set(Vector2(ICON_SIZE,ICON_SIZE));
    home_icon.color_rgba.set(color::Rgba::black().into());
    home_icon.set_position_x(-40.0);


    // === Root ===

    let root_icon = root::View::new(&logger);
    world.add_child(&root_icon);
    root_icon.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    root_icon.color_rgba.set(color::Rgba::black().into());
    root_icon.set_position_x(-20.0);


    // === Folder ===

    let folder_icon = folder::View::new(&logger);
    world.add_child(&folder_icon);
    folder_icon.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    folder_icon.color_rgba.set(color::Rgba::black().into());
    folder_icon.set_position_x(0.0);


    // === File ===

    let file_icon = file::View::new(&logger);
    world.add_child(&file_icon);
    file_icon.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    file_icon.color_rgba.set(color::Rgba::black().into());
    file_icon.set_position_x(20.0);


    // === Arrow ===

    let arrow_icon = arrow::View::new(&logger);
    world.add_child(&arrow_icon);
    arrow_icon.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    arrow_icon.color_rgba.set(color::Rgba::black().into());
    arrow_icon.set_position_x(40.0);


    // === Animation ===

    world.on_frame(move |time| {
        let stroke_width = 1.5 + 0.5 * (time.local/1000.0).sin();
        project_icon.stroke_width.set(stroke_width);
        home_icon.stroke_width.set(stroke_width);
        root_icon.stroke_width.set(stroke_width);
        folder_icon.stroke_width.set(stroke_width);
        file_icon.stroke_width.set(stroke_width);
        arrow_icon.stroke_width.set(stroke_width);
    }).forget();
}
