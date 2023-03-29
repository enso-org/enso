//! UI component that allows selecting a execution mode.

#![recursion_limit = "512"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
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

use enso_prelude::*;
use ensogl::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::data::color::Rgba;
use ensogl::display;
use ensogl::display::camera::Camera2d;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::graph_editor::execution_mode_selector as theme;



// ================
// === Constants ===
// ================

const PLAY_BUTTON_SIZE: f32 = 10.0;
const PLAY_BUTTON_OFFSET: f32 = 15.0;
const PLAY_BUTTON_PADDING: f32 = 10.0;
const DIVIDER_OFFSET: f32 = 32.5;
const DIVIDER_PADDING: f32 = 10.0;
const DROPDOWN_WIDTH: f32 = 95.0;
/// Width of the execution mode selector.
pub const OVERALL_WIDTH: f32 =
    DROPDOWN_WIDTH + 2.0 * DIVIDER_PADDING + PLAY_BUTTON_SIZE + PLAY_BUTTON_PADDING;
/// Height of the execution mode selector.
pub const HEIGHT: f32 = 24.0;


// ==============
// === Shapes ===
// ==============

mod background {
    use super::*;

    ensogl::shape! {
        below = [play_icon];
        (style:Style) {
            let size = Var::canvas_size();
            let height = size.y();
            let width = size.x();
            let color = style.get_color(theme::background);
            let base_rect = Rect(size).corners_radius(&height/2.px()).fill(color);

            let divider = Rect((1.px(), &height)).fill(style.get_color(theme::divider));
            let divider = divider.translate_x(width/2.0 - DIVIDER_OFFSET.px());

            (base_rect + divider).into()
        }
    }
}

mod play_icon {
    use super::*;

    use std::f32::consts::PI;
    ensogl::shape! {
        above = [background];
        (style:Style) {
           let size = Var::canvas_size();
           let height = size.y();
           let width = size.x();
           let triangle = Triangle(width, height).rotate((PI/2.0).radians());
           let color = style.get_color(theme::triangle);
            triangle.fill(color).into()
        }
    }
}



// ===========
// === FRP ===
// ===========

/// An identifier of a execution mode.
pub type ExecutionMode = String;
/// A list of execution modes.
pub type ExecutionModes = Rc<Vec<ExecutionMode>>;

ensogl::define_endpoints_2! {
    Input {
        set_execution_modes      (ExecutionModes),
    }
    Output {
        selected_ecxecution_mode (ExecutionMode),
        play_press(),
    }
}


// =============
// === Model ===
// =============

/// The model of the execution mode selector.
#[derive(Debug, Clone, CloneRef)]
pub struct Model {
    display_object: display::object::Instance,
    inner_root:     display::object::Instance,
    background:     background::View,
    play_icon:      play_icon::View,
    dropdown:       ensogl_drop_down_menu::DropDownMenu,
}

impl Model {
    fn init_dropdown(dropdown: &ensogl_drop_down_menu::DropDownMenu) {
        dropdown.set_menu_offset_y(20.0);
        dropdown.set_x(OVERALL_WIDTH / 2.0 - DIVIDER_OFFSET);
        dropdown.set_label_color(Rgba::white());
        dropdown.set_icon_size(Vector2::new(1.0, 1.0));
        dropdown.set_menu_alignment(ensogl_drop_down_menu::Alignment::Right);
        dropdown.set_label_alignment(ensogl_drop_down_menu::Alignment::Left);
    }

    fn init_background(max_width: f32, background: &background::View) {
        background.set_size(Vector2::new(max_width, HEIGHT));
    }

    fn init_play_icon(max_width: f32, shape: &play_icon::View) {
        shape.set_size(Vector2::new(10.0, 11.0));
        shape.set_x(max_width / 2.0 - PLAY_BUTTON_OFFSET);
    }

    fn update_position(&self, camera: &Camera2d) {
        let screen = camera.screen();
        let x = -screen.width / 2.0 + OVERALL_WIDTH / 2.0;
        let y = screen.height / 2.0 - HEIGHT / 2.0;
        self.inner_root.set_x(x.round());
        self.inner_root.set_y(y.round());
        error!("update_position x: {}, y: {}", x, y);
    }

    fn set_entries(&self, entries: Rc<Vec<ExecutionMode>>) {
        let provider = ensogl_list_view::entry::AnyModelProvider::from(entries.clone_ref());
        self.dropdown.set_entries(provider);
        self.dropdown.set_selected(0);
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}


// =============================
// === ExecutionModeDropdown ===
// =============================

impl component::Model for Model {
    fn label() -> &'static str {
        "ExecutionModeDropdown"
    }

    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;

        let display_object = display::object::Instance::new();
        let inner_root = display::object::Instance::new();
        let background = background::View::new();
        let play_icon = play_icon::View::new();
        let dropdown = ensogl_drop_down_menu::DropDownMenu::new(app);

        display_object.add_child(&inner_root);
        inner_root.add_child(&dropdown);
        inner_root.add_child(&play_icon);
        inner_root.add_child(&background);

        scene.layers.panel.add(&inner_root);
        scene.layers.panel.add(&dropdown);
        scene.layers.panel.add(&play_icon);

        dropdown.set_label_layer(&scene.layers.panel_text);

        Self::init_dropdown(&dropdown);
        Self::init_background(OVERALL_WIDTH, &background);
        Self::init_play_icon(OVERALL_WIDTH, &play_icon);

        Self { display_object, background, play_icon, dropdown, inner_root }
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        network: &enso_frp::Network,
        frp: &<Self as ensogl::application::frp::API>::Private,
        app: &Application,
        model: &Model,
        _style_watch: &ensogl::display::shape::StyleWatchFrp,
    ) {
        let scene = &app.display.default_scene;
        let camera = scene.camera();
        let dropdown = &model.dropdown;
        let play_icon = &model.play_icon;
        let input = &frp.input;
        let output = &frp.output;

        frp::extend! { network
            init <- source();

            // Layout
            let camera_changed = scene.frp.camera_changed.clone_ref();
            update_position <- any(init, camera_changed);
            eval_ update_position ([model, camera] {
                model.update_position(&camera);
            });

            // Inputs

            eval input.set_execution_modes ((entries) model.set_entries(entries.clone()));

            selected_id <- dropdown.frp.chosen_entry.unwrap();
            selection <- all(input.set_execution_modes, selected_id);
            selected_entry <- selection.map(|(entries, entry_id)| entries[*entry_id].clone());
            output.selected_ecxecution_mode <+ selected_entry;

            // Outputs
            output.play_press <+ play_icon.events.mouse_down.constant(());
        }
        init.emit(());
    }
}

/// ExecutionModeSelector is a component that allows the user to select the execution mode of the
/// graph.
pub type ExecutionModeSelector = component::ComponentView<Model, Frp>;
