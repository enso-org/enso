use ide_view_component_group::prelude::*;

use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::system::web::traits::*;
use wasm_bindgen::prelude::*;

use ide_view_component_group::icon;
use ide_view_component_group::icon::*;



// =============
// === Frame ===
// =============

/// A rectangular frame to mark the edges of icon.
mod frame {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style) {
            let inner = Rect((ICON_SIZE.px(),ICON_SIZE.px()));
            let outer = inner.grow(0.2.px());
            let shape = (outer - inner).fill(color::Rgba::black());
            shape.shrink(SHRINK_AMOUNT.px()).into()
        }
    }
}



// ===================
// === Entry Point ===
// ===================

/// An entry point that displays all icon on a grid.
#[wasm_bindgen]
#[allow(dead_code)]
pub fn entry_point_searcher_icons() {
    let logger = Logger::new("Icons example");
    let app = Application::new("root");
    ensogl_hardcoded_theme::builtin::dark::register(&app);
    ensogl_hardcoded_theme::builtin::light::register(&app);
    ensogl_hardcoded_theme::builtin::light::enable(&app);
    let world = app.display.clone();
    mem::forget(app);
    let scene = &world.default_scene;
    mem::forget(Navigator::new(scene, &scene.camera()));


    // === Grid ===

    let grid_div = web::document.create_div_or_panic();
    grid_div.set_style_or_warn("width", "1000px");
    grid_div.set_style_or_warn("height", "16px");
    grid_div.set_style_or_warn("background-size", "1.0px 1.0px");
    grid_div.set_style_or_warn(
        "background-image",
        "linear-gradient(to right,  grey 0.05px, transparent 0.05px),
                                 linear-gradient(to bottom, grey 0.05px, transparent 0.05px)",
    );

    let grid = DomSymbol::new(&grid_div);
    scene.dom.layers.back.manage(&grid);
    world.add_child(&grid);
    grid.set_size(Vector2(1000.0, ICON_SIZE));
    mem::forget(grid);


    // === Frame ===

    let frame = frame::View::new(&logger);
    world.add_child(&frame);
    frame.size.set(Vector2(ICON_SIZE + 20.0, ICON_SIZE + 20.0));
    mem::forget(frame);


    // === Star ===

    let star = Id::Star.create_shape(&logger, Vector2(ICON_SIZE, ICON_SIZE));
    world.add_child(&*star);
    star.set_position_x(20.0);
    mem::forget(star);


    // === Data Input ===

    // let data_input = data_input::View::new(&logger);
    let data_input = Id::DataInput.create_shape(&logger, Vector2(ICON_SIZE, ICON_SIZE));
    world.add_child(&*data_input);
    // data_input.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    data_input.set_position_x(40.0);
    mem::forget(data_input);


    // === Data Output ===

    // let data_output = data_output::View::new(&logger);
    let data_output = Id::DataOutput.create_shape(&logger, Vector2(ICON_SIZE, ICON_SIZE));
    world.add_child(&*data_output);
    // data_output.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    data_output.set_position_x(60.0);
    mem::forget(data_output);


    // === Text Input ===

    let text_input = text_input::View::new(&logger);
    world.add_child(&text_input);
    text_input.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    text_input.set_position_x(80.0);
    mem::forget(text_input);


    // === Number Input ===

    let number_input = number_input::View::new(&logger);
    world.add_child(&number_input);
    number_input.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    number_input.set_position_x(100.0);
    mem::forget(number_input);


    // === Table Edit ===

    let table_edit = table_edit::View::new(&logger);
    world.add_child(&table_edit);
    table_edit.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    table_edit.set_position_x(120.0);
    mem::forget(table_edit);


    // === Convert ===

    let convert = convert::View::new(&logger);
    world.add_child(&convert);
    convert.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    convert.set_position_x(140.0);
    mem::forget(convert);


    // === Dataframe Clean ===

    let dataframe_clean = dataframe_clean::View::new(&logger);
    world.add_child(&dataframe_clean);
    dataframe_clean.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframe_clean.set_position_x(160.0);
    mem::forget(dataframe_clean);


    // === Add Column ===

    let add_column = add_column::View::new(&logger);
    world.add_child(&add_column);
    add_column.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    add_column.set_position_x(180.0);
    mem::forget(add_column);


    // === Add Row ===

    let add_row = add_row::View::new(&logger);
    world.add_child(&add_row);
    add_row.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    add_row.set_position_x(200.0);
    mem::forget(add_row);


    // === Select Column ===

    let select_column = select_column::View::new(&logger);
    world.add_child(&select_column);
    select_column.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    select_column.set_position_x(220.0);
    mem::forget(select_column);


    // === Select Row ===

    let select_row = select_row::View::new(&logger);
    world.add_child(&select_row);
    select_row.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    select_row.set_position_x(240.0);
    mem::forget(select_row);


    // === Dataframe Map Column ===

    let dataframe_map_column = dataframe_map_column::View::new(&logger);
    world.add_child(&dataframe_map_column);
    dataframe_map_column.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframe_map_column.set_position_x(260.0);
    mem::forget(dataframe_map_column);


    // === Dataframe Map Row ===

    let dataframe_map_row = dataframe_map_row::View::new(&logger);
    world.add_child(&dataframe_map_row);
    dataframe_map_row.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframe_map_row.set_position_x(280.0);
    mem::forget(dataframe_map_row);


    // === Dataframes Join ===

    let dataframes_join = dataframes_join::View::new(&logger);
    world.add_child(&dataframes_join);
    dataframes_join.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframes_join.set_position_x(300.0);
    mem::forget(dataframes_join);


    // === Dataframes Union ===

    let dataframes_union = dataframes_union::View::new(&logger);
    world.add_child(&dataframes_union);
    dataframes_union.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    dataframes_union.set_position_x(320.0);
    mem::forget(dataframes_union);


    // === Sigma ===

    let sigma = sigma::View::new(&logger);
    world.add_child(&sigma);
    sigma.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    sigma.set_position_x(340.0);
    mem::forget(sigma);


    // === Split Text ===

    let split_text = split_text::View::new(&logger);
    world.add_child(&split_text);
    split_text.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    split_text.set_position_x(360.0);
    mem::forget(split_text);


    // === Data Science ===

    let data_science = data_science::View::new(&logger);
    world.add_child(&data_science);
    data_science.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    data_science.set_position_x(380.0);
    mem::forget(data_science);


    // === Network ===

    let network = network::View::new(&logger);
    world.add_child(&network);
    network.size.set(Vector2(ICON_SIZE + 1.0, ICON_SIZE));
    network.set_position_x(400.0);
    mem::forget(network);


    // === System ===

    let system = icon::system::View::new(&logger);
    world.add_child(&system);
    system.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    system.set_position_x(420.0);
    mem::forget(system);


    // === Libraries ===

    let libraries = libraries::View::new(&logger);
    world.add_child(&libraries);
    libraries.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    libraries.set_position_x(440.0);
    mem::forget(libraries);


    // === Marketplace ===

    let marketplace = marketplace::View::new(&logger);
    world.add_child(&marketplace);
    marketplace.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    marketplace.set_position_x(460.0);
    mem::forget(marketplace);


    // === IO ===

    let io = io::View::new(&logger);
    world.add_child(&io);
    io.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    io.set_position_x(480.0);
    mem::forget(io);


    // === Preparation ===

    let preparation = preparation::View::new(&logger);
    world.add_child(&preparation);
    preparation.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    preparation.set_position_x(500.0);
    mem::forget(preparation);


    // === Join ===

    let join = join::View::new(&logger);
    world.add_child(&join);
    join.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    join.set_position_x(520.0);
    mem::forget(join);


    // === Text ===

    let text = text::View::new(&logger);
    world.add_child(&text);
    text.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    text.set_position_x(540.0);
    mem::forget(text);


    // === Date and Time ===

    let date_and_time = date_and_time::View::new(&logger);
    world.add_child(&date_and_time);
    date_and_time.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    date_and_time.set_position_x(560.0);
    mem::forget(date_and_time);


    // === Spatial ===

    let spatial = spatial::View::new(&logger);
    world.add_child(&spatial);
    spatial.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    spatial.set_position_x(580.0);
    mem::forget(spatial);


    // === Predictive ===

    let predictive = predictive::View::new(&logger);
    world.add_child(&predictive);
    predictive.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    predictive.set_position_x(600.0);
    mem::forget(predictive);


    // === Machine Learning ===

    let machine_learning = machine_learning::View::new(&logger);
    world.add_child(&machine_learning);
    machine_learning.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    machine_learning.set_position_x(620.0);
    mem::forget(machine_learning);


    // === Computer Vision ===

    let computer_vision = computer_vision::View::new(&logger);
    world.add_child(&computer_vision);
    computer_vision.size.set(Vector2(ICON_SIZE, ICON_SIZE));
    computer_vision.set_position_x(640.0);
    mem::forget(computer_vision);
}
