//! A single block component that is used to build up a flame graph.

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::data::color::Lcha;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::frp;
use ensogl_grid_view as grid_view;
use ensogl_gui_component::component;
use ensogl_gui_component::component::ComponentView;

// mod entry;

// =================
// === Constants ===
// =================

// Size of single entry in pixels.
const ENTRY_HEIGHT: f32 = 25.0;
// Default maximum external size of the whole dropdown.
const DEFAULT_SIZE: Vector2 = Vector2(150.0, 400.0);
// Total height of search bar element.
const SEARCH_HEIGHT: f32 = ENTRY_HEIGHT; // TODO: add to FRP and default
                                         // Dropdown corner radius.
const CORNER_RADIUS: f32 = 12.0;



// =========================
// === Shape Definition ===
// =========================

mod background {
    use super::*;
    ensogl_core::shape! {
        (style:Style, color_rgba: Vector4<f32>) {
            let size          = Var::canvas_size();
            let color         = Var::<color::Rgba>::from(color_rgba);
            let overlay       = Rect(size).corners_radius(CORNER_RADIUS.px());
            let overlay       = overlay.fill(color);
            let out           = overlay;
            out.into()
        }
    }
}


// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        model_for_entry(usize, EntryModel),
        set_number_of_entries(usize),
        set_max_size(Vector2),
        set_color(Lcha),
        request_model_for_visible_entries(),


    }
    Output {
        model_for_entry_needed(usize),
        selected(usize)
    }
}


impl component::Frp<Model> for Frp {
    fn init(
        network: &frp::Network,
        api: &Self::Private,
        _app: &Application,
        model: &Model,
        _style: &StyleWatchFrp,
    ) {
        frp::extend! { network
            init <- source_();
            default_size <- init.constant(DEFAULT_SIZE);
            max_size <- any(default_size, api.input.set_max_size);
            dimensions <- all(api.input.set_number_of_entries, max_size);

            eval dimensions((&(num_entries, max_size)) model.set_dimensions(num_entries, max_size));
            eval api.input.set_color((color) model.set_color(*color));

            api.output.model_for_entry_needed <+ model.grid.model_for_entry_needed._0();
            model.grid.model_for_entry <+ api.input.model_for_entry.map(|(row, model)| (*row, 0, model.clone_ref()));
            trace model.grid.model_for_entry;
            model.grid.request_model_for_visible_entries <+ api.input.request_model_for_visible_entries;
        }

        // initialize
        init.emit(());
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    background:     background::View,
    pub grid:       Grid,
    display_object: display::object::Instance,
    // TODO: search
}

impl component::Model for Model {
    fn label() -> &'static str {
        "Dropdown"
    }

    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();

        let background = background::View::new();
        let grid = Grid::new(app);

        display_object.add_child(&background);
        display_object.add_child(&grid);

        Model { background, grid, display_object }
    }
}


impl Model {
    fn set_dimensions(&self, num_entries: usize, max_size: Vector2) {
        let grid_inner_height = num_entries as f32 * ENTRY_HEIGHT;

        let height = (grid_inner_height + SEARCH_HEIGHT).min(max_size.y);
        let width = max_size.x;

        self.background.size.set(Vector2(width, height));
        self.background.set_position_xy(Vector2(width, -height) / 2.0);
        self.grid.set_position_y(-SEARCH_HEIGHT);
        self.grid.scroll_frp().resize(Vector2(width, height - SEARCH_HEIGHT));
        self.grid.scroll_frp().set_corner_radius_bottom_right(CORNER_RADIUS);
        self.grid.scroll_frp().set_corner_radius_bottom_left(CORNER_RADIUS);
        self.grid.set_entries_size(Vector2(width, ENTRY_HEIGHT));
        self.grid.resize_grid(num_entries, 1);
    }


    fn set_color(&self, color: Lcha) {
        self.background.color_rgba.set(color::Rgba::from(color).into());
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =================
// === Component ===
// =================

#[allow(missing_docs)]
pub type Dropdown = ComponentView<Model, Frp>;

// GRID
pub type DropdownEntry = grid_view::simple::Entry;
pub type EntryModel = <DropdownEntry as grid_view::entry::Entry>::Model;
pub type Grid = grid_view::scrollable::SelectableGridView<grid_view::simple::Entry>;
