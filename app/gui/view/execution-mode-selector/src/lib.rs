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
use ensogl::display::shape::StyleWatchFrp;
use ensogl_derive_theme::FromTheme;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::graph_editor::execution_mode_selector as theme;



// =============
// === Style ===
// ==============

/// Theme specification for the execution mode selector.
#[derive(Debug, Clone, Copy, Default, FromTheme)]
#[base_path = "ensogl_hardcoded_theme::graph_editor::execution_mode_selector"]
pub struct Style {
    play_button_size:    f32,
    play_button_offset:  f32,
    play_button_padding: f32,
    divider_offset:      f32,
    divider_padding:     f32,
    dropdown_width:      f32,
    height:              f32,
    background:          Rgba,
    divider:             Rgba,
    menu_offset:         f32,
}

impl Style {
    fn overall_width(&self) -> f32 {
        self.dropdown_width
            + 2.0 * self.divider_padding
            + self.play_button_size
            + self.play_button_padding
    }
}


// ==============
// === Shapes ===
// ==============

mod play_icon {
    use super::*;

    use std::f32::consts::PI;
    ensogl::shape! {
        above = [display::shape::compound::rectangle::shape];
        (style:Style) {
            let triangle_size = style.get_number(theme::play_button_size);
            let color = style.get_color(theme::triangle);
            let triangle = Triangle(triangle_size, triangle_size).rotate((PI/2.0).radians());
            let triangle = triangle.fill(color);
            let bg_size = Var::canvas_size();
            let bg = Rect(bg_size).fill(INVISIBLE_HOVER_COLOR);
            (bg + triangle).into()
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
        set_available_execution_modes      (ExecutionModes),
    }
    Output {
        selected_execution_mode (ExecutionMode),
        play_press(),
        size                    (Vector2),
    }
}


// =============
// === Model ===
// =============

/// The model of the execution mode selector.
#[derive(Debug, Clone, CloneRef)]
pub struct Model {
    /// Main root object for the execution mode selector exposed for external positioning.
    display_object: display::object::Instance,
    /// Inner root that will be used for positioning the execution mode selector relative to the
    /// window
    inner_root:     display::object::Instance,
    background:     display::shape::compound::rectangle::Rectangle,
    divider:        display::shape::compound::rectangle::Rectangle,
    play_icon:      play_icon::View,
    dropdown:       ensogl_drop_down_menu::DropDownMenu,
}

impl Model {
    fn update_dropdown_style(&self, style: &Style) {
        self.dropdown.set_menu_offset_y(style.menu_offset);
        self.dropdown.set_x(style.overall_width() / 2.0 - style.divider_offset);
        self.dropdown.set_label_color(Rgba::white());
        self.dropdown.set_icon_size(Vector2::new(1.0, 1.0));
        self.dropdown.set_menu_alignment(ensogl_drop_down_menu::Alignment::Right);
        self.dropdown.set_label_alignment(ensogl_drop_down_menu::Alignment::Left);
    }

    fn update_background_style(&self, style: &Style) {
        let width = style.overall_width();
        let Style { height, background, .. } = *style;
        let size = Vector2::new(width, height);
        self.background.set_size(size);
        self.background.set_xy(-size / 2.0);
        self.background.set_corner_radius(height / 2.0);
        self.background.set_color(background);

        self.divider.set_size(Vector2::new(1.0, height));
        self.divider.set_xy(Vector2::new(width / 2.0 - style.divider_offset, -height / 2.0));
        self.divider.set_color(style.divider);
    }

    fn update_play_icon_style(&self, style: &Style) {
        let width = style.overall_width();
        let size = Vector2::new(style.play_button_size + style.play_button_padding, style.height);
        self.play_icon.set_size(size);
        self.play_icon.set_x(width / 2.0 - style.play_button_offset - size.x / 2.0);
        self.play_icon.set_y(-size.y / 2.0);
    }

    fn update_position(&self, style: &Style, camera: &Camera2d) {
        let screen = camera.screen();
        let x = -screen.width / 2.0 + style.overall_width() / 2.0;
        let y = screen.height / 2.0 - style.height / 2.0;
        self.inner_root.set_x(x.round());
        self.inner_root.set_y(y.round());
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
        let background = default();
        let divider = default();
        let play_icon = play_icon::View::new();
        let dropdown = ensogl_drop_down_menu::DropDownMenu::new(app);

        display_object.add_child(&inner_root);
        inner_root.add_child(&dropdown);
        inner_root.add_child(&play_icon);
        inner_root.add_child(&background);
        inner_root.add_child(&divider);

        scene.layers.panel.add(&inner_root);
        scene.layers.panel.add(&dropdown);
        scene.layers.panel.add(&divider);

        dropdown.set_label_layer(&scene.layers.panel_text);
        dropdown.restore_shape_constraints(app);


        Self { display_object, background, play_icon, dropdown, inner_root, divider }
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        network: &enso_frp::Network,
        frp: &<Self as ensogl::application::frp::API>::Private,
        app: &Application,
        model: &Model,
        style_watch: &StyleWatchFrp,
    ) {
        let scene = &app.display.default_scene;
        let camera = scene.camera();
        let dropdown = &model.dropdown;
        let play_icon = &model.play_icon;
        let input = &frp.input;
        let output = &frp.output;

        let style = Style::from_theme(network, style_watch);
        let style_update = style.update;

        frp::extend! { network

            // == Layout ==

            let camera_changed = scene.frp.camera_changed.clone_ref();
            update_position <- all(camera_changed, style_update)._1();
            eval update_position ([model, camera] (style){
                model.update_position(style, &camera);
            });

            eval style_update((style) {
               model.update_dropdown_style(style);
               model.update_background_style(style);
               model.update_play_icon_style(style);
            });

            // == Inputs ==

            eval input.set_available_execution_modes ((entries) model.set_entries(entries.clone()));

            selected_id <- dropdown.frp.chosen_entry.unwrap();
            selection <- all(input.set_available_execution_modes, selected_id);
            selected_entry <- selection.map(|(entries, entry_id)| entries[*entry_id].clone());
            output.selected_execution_mode <+ selected_entry;

            // == Outputs ==

            output.play_press <+ play_icon.events_deprecated.mouse_down.constant(());
            output.size <+ style_update.map(|style| {
                Vector2::new(style.overall_width(),style.height)
            }).on_change();
        }
        style.init.emit(());
    }
}

/// ExecutionModeSelector is a component that allows the user to select the execution mode of the
/// graph.
pub type ExecutionModeSelector = component::ComponentView<Model, Frp>;
