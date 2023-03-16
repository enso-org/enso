//! A crate with Component Browser View.
//!
//! Currently, this crate gathers the related panels. The Component Browser View itself is defined
//! in `ide_view` crate, because the Documentation panel is used by old node searcher as well, and
//! we need to avoid crates' circular dependencies.

#![recursion_limit = "256"]
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

use ide_view_documentation as documentation;
use ide_view_graph_editor::component::node::HEIGHT as NODE_HEIGHT;

use enso_frp as frp;
use ensogl::application::frp::API;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::shape::StyleWatchFrp;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser as theme;



// ==============
// === Export ===
// ==============

pub use ide_view_component_list_panel as component_list_panel;
pub use ide_view_component_list_panel_breadcrumbs as breadcrumbs;



/// The Model of Component Browser View.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object:    display::object::Instance,
    pub list:          component_list_panel::View,
    pub documentation: documentation::View,
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentBrowser"
    }

    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let list = app.new_view::<component_list_panel::View>();
        let documentation = documentation::View::new(app);
        app.display.default_scene.layers.node_searcher.add(&display_object);
        display_object.add_child(&list);
        display_object.add_child(&documentation);

        // We need to set the shapes order for the documentation panel overlay to be on top of the
        // CB background; otherwise, the background would block some mouse events. The CB background
        // is big because of the surrounding shadow and partially overlaps the left side of the
        // documentation panel.
        let scene = &app.display.default_scene;
        shapes_order_dependencies! {
            scene => {
                component_list_panel::background -> documentation::overlay;
            }
        }

        Self { display_object, list, documentation }
    }
}

impl Model {
    fn expression_input_position(
        size: &Vector2,
        vertical_gap: &f32,
        snap_to_pixel_offset: &Vector2,
    ) -> Vector2 {
        let half_node_height = NODE_HEIGHT / 2.0;
        let panel_left = -size.x / 2.0;
        let panel_bottom = -size.y / 2.0;
        let x = panel_left;
        let y = panel_bottom - vertical_gap - half_node_height;
        Vector2(x, y) + snap_to_pixel_offset
    }

    fn snap_to_pixel_offset(size: Vector2, scene_shape: &display::scene::Shape) -> Vector2 {
        let device_size = scene_shape.device_pixels();
        let origin_left_top_pos = Vector2(device_size.width, device_size.height) / 2.0;
        let origin_snapped = Vector2(origin_left_top_pos.x.floor(), origin_left_top_pos.y.floor());
        let origin_offset = origin_snapped - origin_left_top_pos;
        let panel_left_top_pos = (size * scene_shape.pixel_ratio) / 2.0;
        let panel_snapped = Vector2(panel_left_top_pos.x.floor(), panel_left_top_pos.y.floor());
        let panel_offset = panel_snapped - panel_left_top_pos;
        origin_offset - panel_offset
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

ensogl::define_endpoints_2! {
    Input {
        show(),
        hide(),
    }
    Output {
        is_visible(bool),
        size(Vector2),
        expression_input_position(Vector2),
        is_hovered(bool),
        editing_committed(),
        is_empty(bool),
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        network: &frp::Network,
        frp_api: &<Self as API>::Private,
        app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
    ) {
        let scene = &app.display.default_scene;
        let input = &frp_api.input;
        let out = &frp_api.output;
        let list_panel = &model.list.output;
        let documentation = &model.documentation;
        let grid = &model.list.model().grid;

        let gap = style.get_number(theme::panels_gap);
        let doc_width = style.get_number(theme::documentation::width);
        let doc_height = style.get_number(theme::documentation::height);
        frp::extend! { network
            init <- source_();

            doc_size <- all_with3(&init, &doc_width, &doc_height, |_, w, h| Vector2(*w, *h));
            size <- all_with4(&init, &list_panel.size, &doc_size, &gap, |(), list_size, doc_size, gap| {
                let width = list_size.x + gap + doc_size.x;
                let height = max(list_size.y, doc_size.y);
                Vector2(width, height)
            });
            snap <- all_with(&size, &scene.frp.shape, |sz, sh| Model::snap_to_pixel_offset(*sz, sh));
            list_position_x <-
                all_with3(&size, &list_panel.size, &snap, |sz, list_sz, snap| list_sz.x / 2.0 - sz.x / 2.0 + snap.x);
            doc_position_x <- all_with3(&size, &doc_size, &snap, |sz, doc_sz, snap| sz.x / 2.0 - doc_sz.x / 2.0 + snap.x);
            eval list_position_x ((x) model.list.set_x(*x));
            eval doc_position_x ((x) model.documentation.set_x(*x));

            model.list.input.show <+ input.show;
            model.list.input.hide <+ input.hide;
            out.is_visible <+ bool(&input.hide, &input.show);
            out.size <+ size;
            out.expression_input_position <+ all_with3(
                &size,
                &gap,
                &snap,
                Model::expression_input_position
            );

            out.is_hovered <+ list_panel.is_hovered || documentation.frp.is_hovered;
            out.is_hovered <+ input.hide.constant(false);

            out.editing_committed <+ grid.expression_accepted.constant(());
        }
        init.emit(());
    }
}

/// Component Browser View.
///
/// The Component Browser is a panel where user searches for types, variables and methods which can
/// be used to construct new nodes. The components are arranged in sections and groups, and
/// displayed in Component List Panel. The Component Browser View contains also Documentation Panel,
/// displaying documentation of currently selected or hovered component.
pub type View = component::ComponentView<Model, Frp>;
