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

use crate::navigator::Navigator as SectionNavigator;

use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::bounding_box::BoundingBox;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_derive_theme::FromTheme;
use ensogl_grid_view as grid_view;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as theme;
use ensogl_shadow as shadow;



// ==============
// === Export ===
// ==============

mod navigator;

pub use breadcrumbs::BreadcrumbId;
pub use breadcrumbs::SECTION_NAME_CRUMB_INDEX;
pub use ensogl_core::prelude;
pub use ide_view_component_list_panel_breadcrumbs as breadcrumbs;
pub use ide_view_component_list_panel_grid as grid;
pub use ide_view_component_list_panel_grid::entry::icon;



// =================
// === Constants ===
// =================

const INITIAL_SECTION_NAME: &str = "Popular";



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
    pub background_color:       color::Rgba,
    pub corners_radius:         f32,
    #[theme_path = "theme::menu::breadcrumbs::crop_left"]
    pub breadcrumbs_crop_left:  f32,
    #[theme_path = "theme::menu::breadcrumbs::crop_right"]
    pub breadcrumbs_crop_right: f32,
    pub menu_height:            f32,
    pub menu_divider_color:     color::Rgba,
    pub menu_divider_height:    f32,
}

/// The combined style values for Component List Panel and its content.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default)]
pub struct AllStyles {
    pub panel:     Style,
    pub grid:      grid::Style,
    pub navigator: navigator::Style,
}

impl AllStyles {
    fn size(&self) -> Vector2 {
        let width = self.grid.width + self.navigator.width;
        let height = self.grid.height + self.panel.menu_height;
        Vector2::new(width, height)
    }

    fn background_sprite_size(&self) -> Vector2 {
        self.size().map(|value| value + 2.0 * SHADOW_PADDING)
    }

    fn menu_divider_y_pos(&self) -> f32 {
        self.size().y / 2.0 - self.panel.menu_height
    }

    fn breadcrumbs_pos(&self) -> Vector2 {
        let crop_left = self.panel.breadcrumbs_crop_left;
        let x = -self.grid.width / 2.0 + self.navigator.width / 2.0 + crop_left;
        let y = self.size().y / 2.0;
        Vector2(x, y)
    }

    fn breadcrumbs_size(&self) -> Vector2 {
        let crop_left = self.panel.breadcrumbs_crop_left;
        let crop_right = self.panel.breadcrumbs_crop_right;
        let width = self.grid.width - crop_left - crop_right;
        Vector2(width, self.panel.menu_height)
    }

    fn grid_pos(&self) -> Vector2 {
        let grid_x = -self.grid.content_size().x / 2.0 + self.navigator.width / 2.0;
        let grid_y = self.grid.content_size().y / 2.0 - self.panel.menu_height / 2.0;
        Vector2(grid_x, grid_y)
    }
}



// ========================
// === Shape Definition ===
// ========================


// === Background ===

#[allow(missing_docs)]
pub mod background {
    use super::*;

    ensogl_core::shape! {
        below = [grid::entry::background, grid_view::entry::overlay, grid_view::selectable::highlight::shape];
        (style:Style,bg_color:Vector4) {
            let alpha = Var::<f32>::from(format!("({bg_color}.w)"));
            let bg_color = &Var::<color::Rgba>::from(bg_color.clone());

            let grid_padding = style.get_number(theme::grid::padding);
            let grid_width = style.get_number(theme::grid::width);
            let grid_height = style.get_number(theme::grid::height);
            let corners_radius = style.get_number(theme::corners_radius);
            let menu_divider_color = style.get_color(theme::menu_divider_color);
            let navigator_divider_color = style.get_color(theme::navigator_divider_color);
            let menu_divider_width = grid_width - grid_padding * 2.0;
            let menu_divider_height = style.get_number(theme::menu_divider_height);
            let navigator_divider_width = style.get_number(theme::navigator_divider_width);
            let menu_height = style.get_number(theme::menu_height);
            let navigator_width = style.get_number(theme::navigator::width);

            let width = grid_width + navigator_width;
            let height = grid_height + menu_height;

            let menu_divider_x_pos = navigator_width / 2.0;
            let menu_divider_y_pos = height / 2.0 - menu_height + menu_divider_height;
            let navigator_divider_x = -width / 2.0 + navigator_width - navigator_divider_width / 2.0;
            let navigator_divider_y = 0.0;

            let menu_divider = Rect((menu_divider_width.px(),menu_divider_height.px()));
            let menu_divider = menu_divider.fill(menu_divider_color);
            let menu_divider = menu_divider.translate_x(menu_divider_x_pos.px());
            let menu_divider = menu_divider.translate_y(menu_divider_y_pos.px());

            let navigator_divider = Rect((navigator_divider_width.px(), height.px()));
            let navigator_divider = navigator_divider.fill(navigator_divider_color);
            let navigator_divider = navigator_divider.translate_x(navigator_divider_x.px());
            let navigator_divider = navigator_divider.translate_y(navigator_divider_y.px());

            let base_shape = Rect((width.px(), height.px()));
            let base_shape = base_shape.corners_radius(corners_radius.px());
            let background = base_shape.fill(bg_color);
            let shadow     = shadow::from_shape_with_alpha(base_shape.into(),&alpha,style);

            (shadow + background + menu_divider + navigator_divider).into()
        }
    }
}



// =============
// === Model ===
// =============

/// The Model of Select Component.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object:        display::object::Instance,
    background:            background::View,
    pub grid:              grid::View,
    pub section_navigator: SectionNavigator,
    pub breadcrumbs:       breadcrumbs::Breadcrumbs,
}

impl Model {
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();

        let background = background::View::new();
        display_object.add_child(&background);

        let grid = app.new_view::<grid::View>();
        display_object.add_child(&grid);

        let section_navigator = SectionNavigator::new(&app);
        display_object.add_child(&section_navigator);

        let breadcrumbs = app.new_view::<breadcrumbs::Breadcrumbs>();
        breadcrumbs.set_base_layer(&app.display.default_scene.layers.node_searcher);
        display_object.add_child(&breadcrumbs);

        Self { display_object, background, grid, section_navigator, breadcrumbs }
    }

    fn set_initial_breadcrumbs(&self) {
        let breadcrumb = breadcrumbs::Breadcrumb::new(INITIAL_SECTION_NAME);
        self.breadcrumbs.set_entries_from((vec![breadcrumb], 0));
        self.breadcrumbs.show_ellipsis(true);
    }

    fn update_style(&self, style: &AllStyles) {
        self.background.bg_color.set(style.panel.background_color.into());
        self.background.set_size(style.background_sprite_size());
        self.section_navigator.update_layout(style);

        self.breadcrumbs.set_xy(style.breadcrumbs_pos());
        self.breadcrumbs.frp().set_size(style.breadcrumbs_size());
        self.grid.set_xy(style.grid_pos());
    }

    // We need to know if the mouse is over the panel, but cannot do it via a shape, as
    // sub-components still need to receive all of the mouse events, too.
    //
    // The `pos` is mouse position in Component List Panel space (the origin is in the middle of
    // the panel).
    fn is_hovered(&self, pos: Vector2, style: &AllStyles) -> bool {
        let size = style.size();
        let viewport = BoundingBox::from_center_and_size(default(), size);
        viewport.contains(pos)
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
            // === Section navigator ===

            model.grid.switch_section <+ model.section_navigator.chosen_section.filter_map(|s| *s);
            model.section_navigator.select_section <+ model.grid.active_section.on_change();


            // === Breadcrumbs ===

            eval_ input.show(model.set_initial_breadcrumbs());


            // === Style ===

            let panel_style = Style::from_theme(network, style);
            let grid_style = grid::Style::from_theme(network, style);
            let navigator_style = navigator::Style::from_theme(network, style);
            style <- all_with3(&panel_style.update, &grid_style.update, &navigator_style.update, |&panel, &grid, &navigator| AllStyles {panel, grid, navigator});
            model.section_navigator.style <+ style;
            eval style ((style) model.update_style(style));
            output.size <+ style.map(|style| style.size());


            // === Hover & Focus ===

            is_visible <- bool(&input.hide, &input.show);
            is_hovered <- app.cursor.frp.screen_position.map2(&style, f!([model,scene](pos, style) {
                let pos = scene.screen_to_object_space(&model, pos.xy());
                model.is_hovered(pos, style)
            })).gate(&is_visible).on_change();
            output.is_hovered <+ is_hovered;
            // TODO[ib] Temporary solution for focus, we grab keyboard events if the
            //   component browser is visible. The proper implementation is tracked in
            //   https://www.pivotaltracker.com/story/show/180872763
            model.grid.deprecated_set_focus <+ is_visible;

            on_hover_end <- is_hovered.on_false();
            model.grid.unhover_element <+ on_hover_end;
        }
        panel_style.init.emit(());
        grid_style.init.emit(());
        navigator_style.init.emit(());
    }
}

/// A sub-content of the Component Browser, that shows the available Component List Sections.
/// Each Component List Section contains named tiles called Component List Groups. To learn more
/// see the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
pub type View = component::ComponentView<Model, Frp>;
