//! This module defines the Component Browser Panel, as sub-content of the Searcher 2.0, that shows
//! the available components grouped by categories. It also defines the shape that the Component
//! Browser Menu will be placed on, as this will appear as a single continuous shape.
//!
//! The widget is defined by the [`SearcherListPanel`].
//!
//! To learn more about component groups, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

#![recursion_limit = "512"]
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
use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_gui_component::component;
use ensogl_list_view as list_view;
use ensogl_scroll_area::ScrollArea;
use ensogl_shadow as shadow;
use ide_view_component_group as component_group;



// ==============
// === Shapes ===
// ==============


// === Layout Constants ===

/// Scale required to covert from figma pixels measures to Enso pixel numbers.
const DPI_SCALE: f32 = 0.5;

/// Extra space around shape to allow for shadows.
const PADDING: f32 = 50.0 * DPI_SCALE;

/// Width of the Component List Panel.
const CONTENT_WIDTH: f32 = 799.0 * DPI_SCALE;
/// Height of the Component List Panel.
const CONTENT_HEIGHT: f32 = 797.0 * DPI_SCALE;

/// Width of the whole shape (Component List Panel + Component Browser Panel Menu) without padding.
const WIDTH_INNER: f32 = CONTENT_WIDTH;
/// Height of the whole shape (Component List Panel + Component Browser Panel Menu) without padding.
const HEIGHT_INNER: f32 = MENU_HEIGHT + CONTENT_HEIGHT;

/// Width of the whole shape (Component List Panel + Component Browser Panel Menu) including
/// padding.
const WIDTH: f32 = WIDTH_INNER * 2.0 * PADDING;
/// Height of the whole shape (Component List Panel + Component Browser Panel Menu) including
/// padding.
const HEIGHT: f32 = HEIGHT_INNER + (2.0 * PADDING);

/// Height of the area reserved for Component Browser Panel Menu.
const MENU_HEIGHT: f32 = 70.0 * DPI_SCALE;

/// Radius of the rounded corners.
const CORNER_RADIUS: f32 = 30.0 * DPI_SCALE;

/// Thickness of the line that divides the Component List Panel from the Component Browser Panel
/// Menu.
const DIVIDER_HEIGHT: f32 = 1.0 * DPI_SCALE;

/// Y-position of the Divider within the shape.
const DIVIDER_Y_POS: f32 = (HEIGHT_INNER / 2.0) - MENU_HEIGHT;


// === Color Constants ===

/// Color used for the Divider.
const DIVIDER_COLOR: color::Rgb = color::Rgb::new(0.7804, 0.7804, 0.7804);
/// Color used for the panel background.
const BACKGROUND_COLOR: color::Rgba =
    color::Rgba::new(252.0 / 256.0, 254.0 / 255.0, 255.0 / 255.0, 1.0);


// === Shape Definition ===

mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,bg_color:Vector4) {
            let alpha = Var::<f32>::from(format!("({0}.w)",bg_color));
            let bg_color = &Var::<color::Rgba>::from(bg_color.clone());

            let left_width = &(WIDTH_INNER/2.0).px();
            let left = Rect((left_width,HEIGHT_INNER.px())).translate_x(-left_width/2.0);

            let right_width = &(WIDTH_INNER/2.0 + 2.0 * CORNER_RADIUS).px();
            let right = Rect((right_width,HEIGHT_INNER.px())).corners_radius(CORNER_RADIUS.px());
            let right = right.translate_x((WIDTH_INNER/4.0-CORNER_RADIUS).px());

            let divider = Rect((WIDTH_INNER.px(),DIVIDER_HEIGHT.px()));
            let divider = divider.fill(DIVIDER_COLOR);
            let divider = divider.translate_y(DIVIDER_Y_POS.px());

            let base_shape = &(left + right);
            let background = base_shape.fill(bg_color);
            let shadow     = shadow::from_shape_with_alpha(base_shape.into(),&alpha,style);
            // TODO: double check shadow against FIGMA reference. This uses our themes default shadows.

            (shadow + background + divider).into()
        }
    }
}



// =============
// === Model ===
// =============


/// The Model of Select Component.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    app:                Application,
    display_object:     display::object::Instance,
    background:         background::View,
    scroll_area:        ScrollArea,
    favourites_section: component_group::wide::View,
}


impl Model {
    fn new(app: &Application) -> Self {
        let logger = Logger::new("ComponentBrowserPanel");
        let app = app.clone_ref();
        let display_object = display::object::Instance::new(&logger);

        let background = background::View::new(&logger);
        background.bg_color.set(BACKGROUND_COLOR.into());
        background.size.set(Vector2::new(WIDTH, HEIGHT));
        display_object.add_child(&background);

        let favourites_section = app.new_view::<component_group::wide::View>();
        favourites_section.set_width(WIDTH_INNER);
        favourites_section.set_no_items_label_text("No Favorites.");

        let scroll_area = ScrollArea::new(&app);
        display_object.add_child(&scroll_area);
        scroll_area.resize(Vector2::new(CONTENT_WIDTH, CONTENT_HEIGHT));
        scroll_area.set_position_xy(Vector2::new(
            -CONTENT_WIDTH / 2.0,
            CONTENT_HEIGHT / 2.0 - MENU_HEIGHT / 2.0,
        ));

        scroll_area.content().add_child(&favourites_section);
        // Required for correct clipping.
        favourites_section.model().set_layer(scroll_area.content_layer());

        Self { app, display_object, background, scroll_area, favourites_section }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentBrowserPanel"
    }

    fn new(app: &Application, _logger: &DefaultWarningLogger) -> Self {
        Self::new(app)
    }
}



// ===========
// === FRP ===
// ===========


define_endpoints_2! {
    Input{
        set_local_scope_section(()), // TODO define proper API. This is for testing and illustrative purposes only.
        set_favourites_section(list_view::entry::AnyModelProvider<component_group::Entry>), // TODO define proper API. This is for testing and illustrative purposes only.
        set_sub_modules_section(()), // TODO define proper API. This is for testing and illustrative purposes only.
    }
    Output{
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        frp_api: &<Self as API>::Private,
        _app: &Application,
        model: &Model,
        _style: &StyleWatchFrp,
    ) {
        let network = &frp_api.network;
        frp::extend! { network
            model.favourites_section.set_entries <+ frp_api.input.set_favourites_section;

            eval model.favourites_section.size ((size) {
                model.favourites_section.set_position_x(size.x/2.0);
                model.favourites_section.set_position_y(-size.y/2.0);
                model.scroll_area.set_content_height(size.y);
                model.scroll_area.set_content_width(size.x);
            });
        }
    }
}

/// A sub-content of the Searcher 2.0, that shows the available components grouped by categories.
///
/// To learn more about Component Groups, see the [Component Browser Design
/// Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
pub type ComponentBrowserPanel = component::ComponentView<Model, Frp>;
