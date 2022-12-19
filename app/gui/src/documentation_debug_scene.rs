//! Example scene showing a documentation panel of the component browser.

use ensogl::prelude::*;
use wasm_bindgen::prelude::*;

use crate::doc_section;
use crate::mock_suggestion_database;
use crate::model::suggestion_database;
use crate::model::suggestion_database::Entry;
use crate::model::SuggestionDatabase;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display;
use ensogl::display::navigation::navigator::Navigator;
use ensogl::display::shape::StyleWatchFrp;
use ensogl::display::shape::*;
use ensogl::frp;
use ensogl::shape;
use ensogl::system::web;
use ensogl_hardcoded_theme::application::component_browser as theme;
use ide_view::documentation;
use ide_view::graph_editor::component::visualization::Registry;
use std::f32::consts::PI;



// =======================
// === DatabaseWrapper ===
// =======================

/// A wrapper for the suggestion database that allows iterating over entries and displaying their
/// documentation.
#[derive(Debug, Clone, CloneRef)]
struct DatabaseWrapper {
    database:      Rc<SuggestionDatabase>,
    current_entry: Rc<Cell<suggestion_database::entry::Id>>,
}

impl DatabaseWrapper {
    fn from_db(database: SuggestionDatabase) -> Self {
        Self { database: database.into(), current_entry: default() }
    }

    /// Switch to the next entry in the database.
    fn switch_to_next(&self) {
        let max = self.database.keys().len();
        let current = self.current_entry.get();
        let next = (current + 1) % max;
        self.current_entry.set(next);
    }

    /// Switch to the previous entry in the database.
    fn switch_to_previous(&self) {
        let max = self.database.keys().len();
        let current = self.current_entry.get();
        let previous = if current == 0 { max - 1 } else { current - 1 };
        self.current_entry.set(previous);
    }

    /// Documentation for the currently selected entry.
    fn documentation(&self) -> String {
        let index = self.current_entry.get();
        let ids = self.database.keys();
        let id = ids[index];
        self.database.lookup(id).map(entry_documentation).unwrap_or_default()
    }
}

fn entry_documentation(entry: Rc<Entry>) -> String {
    format!("{:#?}", entry.documentation)
}

fn database() -> SuggestionDatabase {
    mock_suggestion_database! {
        #[with_doc_section(doc_section!("This is a test documentation."))]
        Standard.Base {
            #[with_doc_section(doc_section!("Maybe type."))]
            #[with_doc_section(doc_section!(@ "Annotated", ""))]
            type Maybe {
                #[with_doc_section(doc_section!("Some constructor."))]
                #[with_doc_section(doc_section!(> "Example", "Some 1"))]
                #[with_doc_section(doc_section!("Documentation for the Some(a) constructor."))]
                Some (a);
                #[with_doc_section(doc_section!("Documentation for the None constructor."))]
                None;

                #[with_doc_section(doc_section!("Documentation for the is_some() method."))]
                #[with_doc_section(doc_section!(! "Important", "This method is important."))]
                fn is_some() -> Standard.Base.Boolean;
            }
        }
    }
}



// ==============
// === Button ===
// ==============

const BUTTON_SIZE: f32 = 40.0;
const BUTTON_BACKGROUND_COLOR: color::Rgba = color::Rgba(0.87, 0.87, 0.87, 1.0);

mod button {
    use super::*;
    shape! {
        (style: Style) {
            let background = Rect((BUTTON_SIZE.px(), BUTTON_SIZE.px()));
            let background = background.corners_radius(10.0.px());
            let background = background.fill(BUTTON_BACKGROUND_COLOR);
            let icon = Triangle(10.0, 10.0);
            let icon = icon.rotate((PI/2.0).radians());
            let icon = icon.fill(color::Rgba::red());
            let shape = background + icon;
            shape.into()
        }
    }
}



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point(documentation)]
#[allow(dead_code)]
pub fn main() {
    ensogl_text_msdf::run_once_initialized(|| {
        let app = Application::new("root");
        ensogl_hardcoded_theme::builtin::light::register(&app);
        ensogl_hardcoded_theme::builtin::light::enable(&app);
        let _registry = Registry::with_default_visualizations();

        let wrapper = DatabaseWrapper::from_db(database());

        let world = &app.display;
        let scene = &world.default_scene;
        let navigator = Navigator::new(scene, &scene.layers.node_searcher.camera());
        let panel = documentation::View::new(&app);
        scene.add_child(&panel);
        scene.layers.node_searcher.add(&panel);

        let buttons = display::object::Instance::new();
        scene.add_child(&buttons);
        scene.layers.below_main.add(&buttons);

        let previous = button::View::new();
        previous.set_rotation_z(PI);
        previous.set_size(Vector2(BUTTON_SIZE, BUTTON_SIZE));
        buttons.add_child(&previous);
        previous.set_x(-BUTTON_SIZE);

        let next = button::View::new();
        next.set_size(Vector2(BUTTON_SIZE, BUTTON_SIZE));
        buttons.add_child(&next);
        next.set_x(BUTTON_SIZE);

        let network = frp::Network::new("documentation");
        let style = StyleWatchFrp::new(&scene.style_sheet);
        let width = style.get_number(theme::documentation::width);
        let grid_height = style.get_number(theme::component_list_panel::grid::height);
        let menu_height = style.get_number(theme::component_list_panel::menu_height);
        frp::extend! { network
            init <- source_();

            height <- all_with3(&init, &grid_height, &menu_height, |_, grid_height, menu_height| {
                grid_height + menu_height
            });
            size <- all_with(&width, &height, |w, h| Vector2(*w, *h));
            eval size((size) panel.visualization_frp.inputs.set_size.emit(*size));


            // === Next/Previous buttons ===

            let scene_shape = scene.shape();
            buttons_x <- all_with(&init, scene_shape, |_, shape| {
                shape.width / 2.0 - BUTTON_SIZE * 3.0
            });
            buttons_y <- all_with(&init, scene_shape, |_, shape| {
                shape.height / 2.0 - BUTTON_SIZE
            });
            eval buttons_x((x) buttons.set_x(*x));
            eval buttons_y((y) buttons.set_y(*y));

            eval_ next.events.mouse_down(wrapper.switch_to_next());
            eval_ previous.events.mouse_down(wrapper.switch_to_previous());
            button_pressed <- any(&next.events.mouse_down, &previous.events.mouse_down).constant(());
            update_docs <- any(&button_pressed, &init);
            panel.frp.display_documentation <+ update_docs.map(f_!(wrapper.documentation()));


            // === Disable navigator on hover ===

            navigator.frp.set_enabled <+ panel.frp.is_hovered.not();
        }
        init.emit(());
        web::document
            .get_element_by_id("loader")
            .map(|t| t.parent_node().map(|p| p.remove_child(&t).unwrap()));

        mem::forget(app);
        mem::forget(panel);
        mem::forget(navigator);
        mem::forget(network);
        mem::forget(previous);
        mem::forget(next);
        mem::forget(buttons);
    })
}
