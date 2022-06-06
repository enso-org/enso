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

mod column_grid;

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_gui_component::component;
use ensogl_list_view as list_view;
use ensogl_scroll_area::ScrollArea;
use ensogl_shadow as shadow;
use ensogl_text as text;
use ide_view_component_group as component_group;
use ide_view_component_group::Layers;


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
const SECTION_DIVIDER_HEIGHT: f32 = 4.0 * DPI_SCALE;

/// Y-position of the Divider within the shape.
const DIVIDER_Y_POS: f32 = (HEIGHT_INNER / 2.0) - MENU_HEIGHT;

const PADDING_INNER: f32 = 6.0 * DPI_SCALE;

const SECTION_HEADING_SIZE: text::style::Size = text::style::Size::new(16.0);

const SECTION_HEADING_FONT: &str = "Causten-Semibold";
const SECTION_HEADING_COLOR_HEX: &str = "737373";
const LABEL_OFFSET: f32 = 1.0 * SECTION_HEADING_SIZE.raw;
const INFINITE: f32 = 999999.0;

// === Color Constants ===

/// Color used for the Divider.
const DIVIDER_COLOR: color::Rgb = color::Rgb::new(0.7804, 0.7804, 0.7804);
/// Color used for the panel background.
const BACKGROUND_COLOR: color::Rgba = color::Rgba::new(252.0 / 256.0, 254.0 / 255.0, 1.0, 1.0);
const FAVOURITES_SECTION_BASE_COLOR: color::Rgba = color::Rgba::new(0.0, 0.42, 0.64, 1.0);
const FAVOURITES_SECTION_HEADING_LABEL: &str = "Favorite Data Science Tools";


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

mod hline {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style) {
            let width            = Var::<Pixels>::from("input_size.x");
            let height           = Var::<Pixels>::from("input_size.y");

            let rect = Rect((width,height));
            let rect = rect.fill(color::Rgb::from_hex(SECTION_HEADING_COLOR_HEX).unwrap());
            rect.into()
        }
    }
}



// =============
// === Model ===
// =============


/// The Model of Select Component.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    app:                 Application,
    logger:              Logger,
    display_object:      display::object::Instance,
    background:          background::View,
    scroll_area:         ScrollArea,
    favourites_section:  ColumnSection,
    local_scope_section: WideSection,
    sub_modules_section: ColumnSection,
    layers:              Layers,
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
        app.display.default_scene.layers.below_main.add_exclusive(&background);

        let favourites_section = Self::init_favorite_section(&app);
        let local_scope_section = Self::init_local_scope_section(&app);
        let sub_modules_section = Self::init_sub_modules_section(&app);

        let scroll_area = ScrollArea::new(&app);
        display_object.add_child(&scroll_area);
        scroll_area.resize(Vector2::new(
            CONTENT_WIDTH - PADDING_INNER,
            CONTENT_HEIGHT - 2.0 * PADDING_INNER,
        ));
        scroll_area.set_position_xy(Vector2::new(
            -CONTENT_WIDTH / 2.0,
            CONTENT_HEIGHT / 2.0 - MENU_HEIGHT / 2.0,
        ));
        scroll_area.set_corner_radius_bottom_right(CORNER_RADIUS);

        let camera = &scroll_area.content_layer().camera();
        let parent_layer = scroll_area.content_layer();
        let layers = component_group::Layers::new(&app.logger, camera, parent_layer);

        favourites_section.set_parent(scroll_area.content());
        local_scope_section.set_parent(scroll_area.content());
        sub_modules_section.set_parent(scroll_area.content());

        // Required for correct clipping.
        favourites_section.set_layers(&layers, scroll_area.content_layer());
        local_scope_section.set_layers(&layers, scroll_area.content_layer());
        sub_modules_section.set_layers(&layers, scroll_area.content_layer());

        Self {
            app,
            display_object,
            background,
            scroll_area,
            favourites_section,
            local_scope_section,
            sub_modules_section,
            layers,
            logger,
        }
    }

    fn update_layers(&self) {
        self.favourites_section.set_layers(&self.layers, self.scroll_area.content_layer());
        self.local_scope_section.set_layers(&self.layers, self.scroll_area.content_layer());
        self.sub_modules_section.set_layers(&self.layers, self.scroll_area.content_layer());
    }

    fn init_favorite_section(app: &Application) -> ColumnSection {
        let content = app.new_view::<column_grid::ColumnGrid>();
        content.set_position_x(PADDING_INNER);
        let section = LabeledSection::new(content, app);
        section.label.set_content(FAVOURITES_SECTION_HEADING_LABEL);
        section
    }

    fn init_local_scope_section(app: &Application) -> WideSection {
        let content = app.new_view::<component_group::wide::View>();
        content.set_no_items_label_text("No Entries.");
        content.set_position_x(PADDING_INNER + CONTENT_WIDTH / 2.0);

        content.set_width(WIDTH_INNER - 2.0 * PADDING_INNER);
        content.set_color(FAVOURITES_SECTION_BASE_COLOR);
        let section = LabeledSection::new(content, app);
        section.label.set_content("Local Scope");
        section
    }

    fn init_sub_modules_section(app: &Application) -> ColumnSection {
        let content = app.new_view::<column_grid::ColumnGrid>();
        content.set_position_x(PADDING_INNER);
        let section = LabeledSection::new(content, app);
        section.label.set_content("Sub Modules");
        section
    }

    fn recompute_layout(&self) {
        self.update_layers();

        let favourites_section_height = self.favourites_section.height();
        let local_scope_height = self.local_scope_section.height();
        let sub_modules_height = self.sub_modules_section.height();

        self.favourites_section.set_base_position_y(0.0);
        self.local_scope_section.set_base_position_y(-favourites_section_height);
        self.sub_modules_section
            .set_base_position_y(-favourites_section_height - local_scope_height);

        self.scroll_area.set_content_height(
            favourites_section_height + local_scope_height + sub_modules_height,
        );
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



// =======================
// === Labeled Section ===
// =======================

#[derive(Clone, Debug, CloneRef)]
struct LabeledSection<T: CloneRef> {
    pub label:   text::Area,
    pub divider: hline::View,
    pub content: T,
}

type WideSection = LabeledSection<component_group::wide::View>;
type ColumnSection = LabeledSection<column_grid::ColumnGrid>;

impl<T: CloneRef> LabeledSection<T> {
    pub fn new(content: T, app: &Application) -> Self {
        let logger = Logger::new("LabeledSection");
        let label = text::Area::new(app);
        let divider = hline::View::new(logger);
        divider.size.set(Vector2(INFINITE, SECTION_DIVIDER_HEIGHT));

        Self { label, divider, content }.init_label()
    }

    fn init_label(self) -> Self {
        let label = &self.label;
        let section_header_color: color::Rgba =
            color::Rgb::from_hex(SECTION_HEADING_COLOR_HEX).unwrap().into();
        label.set_default_color(section_header_color);
        label.set_default_text_size(SECTION_HEADING_SIZE);
        label.set_font(SECTION_HEADING_FONT.to_string()); // TODO double check the correct font is used.
        label.set_position_y(-0.75 * SECTION_HEADING_SIZE.raw);
        label.set_position_x(3.0 + PADDING_INNER);
        self
    }

    fn label_height(&self) -> f32 {
        let label_height = SECTION_HEADING_SIZE.raw;
        label_height + LABEL_OFFSET
    }
}

impl<T: ObjectOps + CloneRef> LabeledSection<T> {
    fn set_parent(&self, parent: impl ObjectOps) {
        parent.add_child(&self.content);
        parent.add_child(&self.label);
        parent.add_child(&self.divider);
    }
}

impl WideSection {
    fn set_layers(&self, layers: &Layers, scroll_layer: &ensogl_core::display::scene::Layer) {
        self.content.model().set_layers(layers);
        scroll_layer.add_exclusive(&self.label);
        self.label.add_to_scene_layer(scroll_layer);
        scroll_layer.add_exclusive(&self.divider);
    }

    fn height(&self) -> f32 {
        let label_height = self.label_height();
        let body_height = self.content.size.value().y;
        let next_section_offset = 17.0; // TODO this should not be needed.
        body_height + label_height + next_section_offset
    }

    fn set_base_position_y(&self, position_y: f32) {
        self.label.set_position_y(position_y - self.label.height.value() / 1.5);
        self.divider.set_position_y(position_y);
        self.content.set_position_y(
            position_y - LABEL_OFFSET - self.label_height() - self.content.size.value().y / 2.0,
        );
    }
}

impl ColumnSection {
    fn set_layers(&self, layers: &Layers, scroll_layer: &display::scene::Layer) {
        self.content.model().set_layers(layers, scroll_layer);
        scroll_layer.add_exclusive(&self.label);
        self.label.add_to_scene_layer(scroll_layer);
        scroll_layer.add_exclusive(&self.divider);
    }

    fn height(&self) -> f32 {
        let label_height = self.label_height();
        let body_height = self.content.size.value().y;
        let next_section_offset = 17.0; // TODO this should not be needed.
        body_height + label_height + next_section_offset
    }

    fn set_base_position_y(&self, position_y: f32) {
        self.label.set_position_y(position_y - self.label.height.value() / 1.5);
        self.divider.set_position_y(position_y);
        self.content.set_position_y(position_y - LABEL_OFFSET - self.label_height());
    }
}


// ===========
// === FRP ===
// ===========

type ColumnContent = list_view::entry::AnyModelProvider<component_group::Entry>;

define_endpoints_2! {
    Input{
        set_local_scope_section(ColumnContent),
        set_favourites_section(Vec<ColumnContent>),
        set_sub_modules_section(Vec<ColumnContent>),
    }
    Output{}
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
            model.favourites_section.content.set_content <+ frp_api.input.set_favourites_section;
            model.local_scope_section.content.set_entries <+ frp_api.input.set_local_scope_section;
            model.sub_modules_section.content.set_content <+ frp_api.input.set_sub_modules_section;

            content_update <- any3_(
                &frp_api.input.set_favourites_section,
                &frp_api.input.set_local_scope_section,
                &frp_api.input.set_sub_modules_section,
            );

            eval_ content_update( model.recompute_layout() );
        }
    }
}

/// A sub-content of the Searcher 2.0, that shows the available components grouped by categories.
///
/// To learn more about Component Groups, see the [Component Browser Design
/// Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
pub type ComponentBrowserPanel = component::ComponentView<Model, Frp>;
