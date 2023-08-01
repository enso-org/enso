//! This module defines the [Component Browser Panel](View), sub-content of the Component Browser,
//! that shows the available components grouped by categories, with navigator and breadcrumbs.
//!
//! To learn more about the Component Browser and its components, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

#![recursion_limit = "4096"]
// === Features ===
#![allow(incomplete_features)]
#![feature(negative_impls)]
#![feature(associated_type_defaults)]
#![feature(cell_update)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(marker_trait_attr)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(trace_macros)]
#![feature(const_trait_impl)]
#![feature(slice_as_chunks)]
#![feature(option_result_contains)]
#![feature(int_roundings)]
#![feature(array_methods)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use crate::prelude::*;
use ensogl_core::display::shape::*;

use crate::button_panel::View as ButtonPanel;

use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::control::io::mouse;
use ensogl_core::data::bounding_box::BoundingBox;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::compound::rectangle::Rectangle;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::style::FromTheme;
use ensogl_grid_view as grid_view;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as theme;
use theme::button_panel as button_panel_theme;



// ==============
// === Export ===
// ==============

mod button_panel;

pub use ensogl_core::prelude;
pub use ensogl_icons::icon;
pub use ide_view_component_list_panel_grid as grid;



// ==============
// === Shapes ===
// ==============

// === Layout Constants ===

/// Extra space around shape to allow for shadows.
const SHADOW_PADDING: f32 = 25.0;
const INFINITE: f32 = 999999.0;


// === Style ===

/// The style values for the Component List Panel.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width: f32,
    pub height: f32,
    pub background_color: color::Rgba,
    pub corners_radius: f32,
    pub padding_bottom: f32,
    #[theme_path = "button_panel_theme::margin_bottom"]
    pub button_panel_margin_bottom: f32,
}

/// The combined style values for Component List Panel and its content.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default)]
pub struct AllStyles {
    pub panel: Style,
    pub grid:  grid::Style,
}

impl AllStyles {
    fn size(&self) -> Vector2 {
        Vector2(self.panel.width, self.panel.height)
    }

    fn grid_size(&self) -> Vector2 {
        Vector2(self.grid.width, self.grid.height)
    }
}



// =============
// === Model ===
// =============

/// The Model of Select Component.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct Model {
    display_object:   display::object::Instance,
    /// This display object moves the origin of the grid to the bottom left corner, to support
    /// auto-layout system.
    grid_adapter:     display::object::Instance,
    background:       Rectangle,
    #[focus_receiver]
    pub grid:         grid::View,
    pub button_panel: ButtonPanel,
}

impl Model {
    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let grid_adapter = display::object::Instance::new();

        let background = Rectangle::new();
        display_object.add_child(&background);
        background
            .use_auto_layout()
            .set_column_flow()
            .set_children_alignment_center()
            .justify_content_center();

        let grid = app.new_view::<grid::View>();
        background.add_child(&grid_adapter);
        grid_adapter.add_child(&grid);

        let button_panel = ButtonPanel::new(app);
        background.add_child(&button_panel);

        shapes_order_dependencies! {
            scene => {
                grid_view::selectable::highlight::shape -> ensogl_icons::icon::any;
            }
        }

        Self { display_object, background, grid, grid_adapter, button_panel }
    }

    /// Access to FRP of the buttons.
    pub fn buttons(&self) -> &button_panel::Frp {
        &self.button_panel
    }

    fn update_style(&self, style: &AllStyles) {
        self.grid_adapter.set_size(style.grid_size());
        self.grid.set_y(style.grid_size().y);
        let style = &style.panel;
        self.background.set_color(style.background_color);
        self.background.set_size(Vector2(style.width, style.height));
        self.background.set_corner_radius(style.corners_radius);
        self.background.set_padding_bottom(style.padding_bottom);
        self.button_panel.set_margin_bottom(style.button_panel_margin_bottom);
    }

    // We need to know if the mouse is over the panel, but cannot do it via a shape, as
    // sub-components still need to receive all of the mouse events, too.
    //
    // The `pos` is mouse position in Component List Panel space (the origin is in the middle of
    // the panel).
    fn is_hovered(&self, pos: Vector2, style: &AllStyles) -> bool {
        let size = style.size();
        let viewport = BoundingBox::from_center_and_size(size / 2.0, size);
        viewport.contains(pos)
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentBrowserPanel"
    }

    fn new(app: &Application) -> Self {
        Self::new(app)
    }
}



// ===========
// === FRP ===
// ===========

define_endpoints_2! {
    Input{
        /// The component browser is displayed on screen.
        show(),
        /// The component browser is hidden from screen.
        hide(),
    }
    Output{
        size(Vector2),
        is_hovered(bool),
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
        let output = &frp_api.output;

        frp::extend! { network
            // === Style ===

            let panel_style = Style::from_theme(network, style);
            let grid_style = grid::Style::from_theme(network, style);
            style <- all_with(&panel_style, &grid_style, |&panel, &grid| AllStyles {panel, grid});
            eval style ((style) model.update_style(style));
            output.size <+ style.map(|style| style.size());

            model.grid.set_top_margin <+ all_with(&model.button_panel.height, &style, |buttons_h, style| {
                (buttons_h - (style.panel.height - style.grid.height - style.panel.padding_bottom)).max(0.0)
            });


            // === Hover & Focus ===

            is_visible <- bool(&input.hide, &input.show);
            is_hovered <- app.cursor.frp.screen_position.map2(&style, f!([model,scene](pos, style) {
                let pos = scene.screen_to_object_space(&model, pos.xy());
                model.is_hovered(pos, style)
            })).gate(&is_visible).on_change();
            output.is_hovered <+ is_hovered;

            let mouse_down = model.on_event::<mouse::Down>();
            eval_ mouse_down (model.focus());
            eval_ input.show (model.focus());
            eval_ input.hide (model.blur());

        }
    }
}

/// A sub-content of the Component Browser, that shows the available Component List Sections.
/// Each Component List Section contains named tiles called Component List Groups. To learn more
/// see the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
pub type View = component::ComponentView<Model, Frp>;
