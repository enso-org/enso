//! This module defines the [`ComponentBrowserPanel`], sub-content of the Component Browser, that
//! shows the available components grouped by categories. It also defines the shape that the
//! Component Browser Menu will be placed on, as this will appear as a single continuous shape.
//!
//! To learn more about the Component Browser and its components, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

#![recursion_limit = "512"]
// === Features ===
#![allow(incomplete_features)]
#![feature(negative_impls)]
#![feature(associated_type_defaults)]
#![feature(bool_to_option)]
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
#![feature(derive_default_enum)]
#![feature(slice_as_chunks)]
#![feature(option_result_contains)]
#![feature(int_roundings)]
#![feature(array_methods)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
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

use crate::navigator::navigator_shadow;
use crate::navigator::Navigator as SectionNavigator;
use crate::navigator::Section;

use component_group::icon;
use enso_frp as frp;
use ensogl_core::animation::physics::inertia;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::bounding_box::BoundingBox;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::navigation::navigator::Navigator;
use ensogl_core::display::object::ObjectOps;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::style;
use ensogl_core::Animation;
use ensogl_derive_theme::FromTheme;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_group as component_group_theme;
use ensogl_hardcoded_theme::application::component_browser::searcher as searcher_theme;
use ensogl_list_view as list_view;
use ensogl_scroll_area::ScrollArea;
use ensogl_scroll_area::Viewport;
use ensogl_shadow as shadow;
use ensogl_text as text;
use ide_view_component_group as component_group;
use ide_view_component_group::set::Group;
use ide_view_component_group::set::SectionId;
use ide_view_component_group::Layers as GroupLayers;
use searcher_theme::list_panel as list_panel_theme;


// ==============
// === Export ===
// ==============

pub mod column_grid;
pub mod layout;



mod layouting;
mod navigator;

pub use column_grid::LabeledAnyModelProvider;
pub use component_group::set::GroupId;
pub use ensogl_core::prelude;



// =================
// === Constants ===
// =================

/// The selection animation is faster than the default one because of the increased spring force.
const SELECTION_ANIMATION_SPRING_FORCE_MULTIPLIER: f32 = 1.5;



// ==============
// === Layers ===
// ==============

#[derive(Debug, Clone, CloneRef)]
struct Layers {
    groups:          GroupLayers,
    base:            Layer,
    navigator:       Layer,
    selection:       Layer,
    selection_mask:  Layer,
    scroll_layer:    Layer,
    scrollbar_layer: Layer,
}

impl Layers {
    fn new(app: &Application, scroll_area: &ScrollArea) -> Self {
        let camera = app.display.default_scene.layers.node_searcher.camera();
        let base = Layer::new_with_cam(app.logger.sub("component_groups"), &camera);
        let selection = Layer::new_with_cam(app.logger.sub("selection"), &camera);
        let navigator = Layer::new_with_cam(app.logger.sub("navigator"), &camera);
        let scrollbar_layer = Layer::new_with_cam(app.logger.sub("scroll_bar"), &camera);
        let selection_mask = Layer::new_with_cam(app.logger.sub("selection_mask"), &camera);
        selection.set_mask(&selection_mask);
        app.display.default_scene.layers.node_searcher.add_sublayer(&base);
        app.display.default_scene.layers.node_searcher.add_sublayer(&selection);
        app.display.default_scene.layers.node_searcher.add_sublayer(&navigator);
        app.display.default_scene.layers.node_searcher.add_sublayer(&scrollbar_layer);
        let content = &scroll_area.content_layer();
        let groups = GroupLayers::new(&app.logger, content, &selection);
        let scroll_layer = scroll_area.content_layer().clone_ref();
        Self { base, selection, groups, selection_mask, navigator, scroll_layer, scrollbar_layer }
    }
}



// ==============
// === Shapes ===
// ==============

// === Layout Constants ===

/// Extra space around shape to allow for shadows.
const SHADOW_PADDING: f32 = 25.0;

const FAVOURITES_SECTION_HEADING_LABEL: &str = "Favorite Data Science Tools";
const LOCAL_SCOPE_SECTION_HEADING_LABEL: &str = "Local Scope";
const SUB_MODULES_SECTION_HEADING_LABEL: &str = "Sub Modules";

const INFINITE: f32 = 999999.0;


// === Style ===


#[derive(Clone, Debug, Default, FromTheme)]
#[base_path = "list_panel_theme"]
struct Style {
    content_width:            f32,
    content_height:           f32,
    content_padding:          f32,
    content_corner_radius:    f32,
    content_background_color: color::Rgba,

    section_divider_height:      f32,
    section_heading_size:        f32,
    section_heading_offset:      f32,
    section_heading_text_offset: f32,
    section_heading_font:        String,
    section_heading_color:       color::Rgba,
    section_divider_color:       color::Rgba,

    menu_height:         f32,
    menu_divider_color:  color::Rgba,
    menu_divider_height: f32,

    favourites_section_base_color: color::Rgba,

    navigator_width:             f32,
    navigator_list_view_width:   f32,
    navigator_icon_strong_color: color::Rgba,
    navigator_icon_weak_color:   color::Rgba,
    navigator_top_padding:       f32,
    navigator_bottom_padding:    f32,
}

impl Style {
    fn size_inner(&self) -> Vector2 {
        let width = self.content_width + self.navigator_width;
        let height = self.content_height + self.menu_height;
        Vector2::new(width, height)
    }

    fn size(&self) -> Vector2 {
        self.size_inner().map(|value| value + 2.0 * SHADOW_PADDING)
    }

    fn scroll_area_size(&self) -> Vector2 {
        let width = self.content_width - 2.0 * self.content_padding;
        let height = self.content_height - self.content_padding;
        Vector2(width, height)
    }

    fn menu_divider_y_pos(&self) -> f32 {
        self.size_inner().y / 2.0 - self.menu_height
    }

    fn section_heading_height(&self) -> f32 {
        self.section_heading_size + self.section_heading_offset
    }
}

/// Enum to indicate whether sections headers should be placed above or below a section. Dead code
/// is allowed as only one option is used at compile time.
#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
enum SectionHeaderPlacement {
    Top,
    Bottom,
}

/// Indicates whether sections headers should be placed above or below a section.
const SECTION_HEADER_PLACEMENT: SectionHeaderPlacement = SectionHeaderPlacement::Bottom;



// ========================
// === Shape Definition ===
// ========================


// === Selection ===

/// A shape of a selection box. It is used as a mask to show only specific parts of the selection
/// layers.
///
/// The selection box highlights a selected entry in the component list panel view. We render the
/// selection above the background of the component group (so it is visible) but below the entry's
/// text (so the text is not covered). Headers of the component groups complicate matters. The
/// header is displayed on top of entries when scrolling the group, so there are cases when
/// selection covers the header background (the header is considered the entry) and cases when the
/// header covers selection, i.e., we select a partially visible entry below the header. To allow
/// such behavior, we render a highlighted version of each component group on top of a normal one
/// and use a layer masking to show only a part of it. See
/// [component_group](component_group#selection) documentation to learn more about how the component
/// groups are implemented.
///
/// The selection box should never leave the borders of the component list panel. We can't use
/// [layers masking][] as our renderer does not support hierarchical masks, so instead, we build a
/// shape of the selection box as an intersection of the `mask` rounded rectangle and the `area`
/// shape. The `area` covers the entire component list panel and thus limits the area in which the
/// selection box is visible.
///
/// The `mask` represents the selection box itself and moves in borders of `area` shape. `pos`
/// and `selection_size` parameters represent its position and size, respectively. The shape of the
/// component list panel determines the position and the size of the `area`. One can adjust them
/// using a standard display object's API. (e.g. `set_position` method)
///
/// The `margin_top` parameter controls the variable height of the `area`. In a scrolled
/// component group, the user can select a partially visible entry behind the group's header. We
/// make the selection box appear covered by the header by reducing the height of the `area`. It's
/// enough because headers are always on top of the component list panel in a scrolled group.
///
/// We have only a single `corners_radius` parameter for the bottom corners of the `area` and the
/// corners of the selection `mask`. In the current [design][], the corner radius of the selection
/// box depends on whether it covers the group's header or the entries. When entries are selected, a
/// single `corners_radius` makes total sense - both `mask` and `area` have the same roundness.
/// When the headers are highlighted, the `corners_radius` parameter no longer matches the roundness
/// of the component list panel. It is ok because the header sticks to the top and is never
/// displayed near the bottom part of the component list panel, where the rounded corners are.
///
/// [design]: https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md
/// [layers masking]: ensogl::display::scene::layer::Layer#masking-layers-with-arbitrary-shapes
pub mod selection_box {
    use super::*;

    ensogl_core::define_shape_system! {
        pointer_events = false;
        (style:Style,corners_radius:f32,pos:Vector2,selection_size:Vector2,margin_top:f32) {
            let area_width = Var::<Pixels>::from("input_size.x");
            let area_height = Var::<Pixels>::from("input_size.y");
            let area_height = area_height - margin_top.px();
            let half_height = area_height.clone() / 2.0;
            let quater_height = area_height.clone() / 4.0;
            let area_top = Rect((area_width.clone(),half_height.clone()));
            let area_top = area_top.translate_y(quater_height.clone());
            // Additional padding so that the two halves of the area overlap without gaps.
            // The precise size is unimportant, so we use 1/8 of the height.
            let padding = area_height / 8.0;
            let area_bottom = Rect((area_width, half_height + padding.clone()));
            let area_bottom = area_bottom.corners_radius(corners_radius.px());
            let area_bottom = area_bottom.translate_y(-quater_height + padding / 2.0);
            let area = area_top + area_bottom;
            let area = area.translate_y(-margin_top.px() / 2.0);

            let size = selection_size;
            let mask = Rect(size.px()).corners_radius(corners_radius.px());
            let mask = mask.translate(pos.px());
            let mask = &mask * &area;
            mask.fill(color::Rgba::black()).into()
        }
    }
}


// === Background ===

mod background {
    use super::*;

    ensogl_core::define_shape_system! {
        below = [component_group::background];
        (style:Style,bg_color:Vector4) {
            let theme_path: style::Path = list_panel_theme::HERE.into();

            let alpha = Var::<f32>::from(format!("({0}.w)",bg_color));
            let bg_color = &Var::<color::Rgba>::from(bg_color.clone());

            let content_width = style.get_number(theme_path.sub("content_width"));
            let content_height = style.get_number(theme_path.sub("content_height"));
            let content_corner_radius = style.get_number(theme_path.sub("content_corner_radius"));
            let menu_divider_color = style.get_color(theme_path.sub("menu_divider_color"));
            let menu_divider_height = style.get_number(theme_path.sub("menu_divider_height"));
            let menu_height = style.get_number(theme_path.sub("menu_height"));
            let navigator_width = style.get_number(theme_path.sub("navigator_width"));

            let width = content_width + navigator_width;
            let height = content_height + menu_height;

            let divider_x_pos = navigator_width / 2.0;
            let divider_y_pos = height / 2.0 - menu_height + menu_divider_height ;

            let divider = Rect((content_width.px(),menu_divider_height.px()));
            let divider = divider.fill(menu_divider_color);
            let divider = divider.translate_x(divider_x_pos.px());
            let divider = divider.translate_y(divider_y_pos.px());

            let base_shape = Rect((width.px(), height.px()));
            let base_shape = base_shape.corners_radius(content_corner_radius.px());
            let background = base_shape.fill(bg_color);
            let shadow     = shadow::from_shape_with_alpha(base_shape.into(),&alpha,style);

            (shadow + background + divider).into()
        }
    }
}


// === Section divider ===

mod hline {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style) {
            let theme_path: style::Path = list_panel_theme::HERE.into();

            let width  = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let section_divider_color = style.get_color(theme_path.sub("section_divider_color"));
            let rect = Rect((width,height));
            let rect = rect.fill(section_divider_color);
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
    // FIXME[#182593513]: This separate shape for navigator shadow can be removed and replaced
    //   with a shadow embedded into the [`background`] shape when the
    //   [issue](https://www.pivotaltracker.com/story/show/182593513) is fixed.
    //   To display the shadow correctly it needs to be clipped to the [`background`] shape, but
    //   we can't do that because of a bug in the renderer. So instead we add the shadow as a
    //   separate shape and clip it using `size.set(...)`.
    navigator_shadow:    navigator_shadow::View,
    scroll_area:         ScrollArea,
    favourites_section:  ColumnSection,
    local_scope_section: WideSection,
    sub_modules_section: ColumnSection,
    section_navigator:   SectionNavigator,
    layers:              Layers,
    groups_wrapper:      component_group::set::Wrapper,
    navigator:           Rc<RefCell<Option<Navigator>>>,
    selection:           selection_box::View,
}

impl Model {
    fn new(app: &Application) -> Self {
        let logger = Logger::new("ComponentBrowserPanel");
        let app = app.clone_ref();
        let display_object = display::object::Instance::new(&logger);
        let navigator = default();
        let groups_wrapper = component_group::set::Wrapper::new();

        let background = background::View::new(&logger);
        display_object.add_child(&background);
        let navigator_shadow = navigator_shadow::View::new(&logger);
        display_object.add_child(&navigator_shadow);

        let favourites_section = Self::init_column_section(&app);
        let local_scope_section = Self::init_wide_section(&app);
        let sub_modules_section = Self::init_column_section(&app);

        use SectionId::*;
        favourites_section.content.set_group_wrapper(&(Favorites, groups_wrapper.clone_ref()));
        let local_scope_id = GroupId::local_scope_group();
        groups_wrapper.add(local_scope_id, Group::Wide(local_scope_section.content.clone_ref()));
        sub_modules_section.content.set_group_wrapper(&(SubModules, groups_wrapper.clone_ref()));

        let scroll_area = ScrollArea::new(&app);
        scroll_area.set_camera(app.display.default_scene.layers.node_searcher.camera());
        display_object.add_child(&scroll_area);
        let layers = Layers::new(&app, &scroll_area);
        layers.base.add_exclusive(&scroll_area);

        let section_navigator = SectionNavigator::new(&app);
        display_object.add_child(&section_navigator);
        layers.navigator.add_exclusive(&section_navigator);

        let selection = selection_box::View::new(&app.logger);
        scroll_area.add_child(&selection);
        layers.selection_mask.add_exclusive(&selection);

        scroll_area.set_scrollbars_layer(&layers.scrollbar_layer);
        layers.scrollbar_layer.set_mask(scroll_area.mask_layer());

        favourites_section.set_parent(scroll_area.content());
        local_scope_section.set_parent(scroll_area.content());
        sub_modules_section.set_parent(scroll_area.content());

        // Required for correct clipping. The components need to be set up with the
        // `scroll_area.content_layer` to be masked correctly by the [`ScrollArea`].
        favourites_section.set_layers(&layers);
        local_scope_section.set_layers(&layers);
        sub_modules_section.set_layers(&layers);

        Self {
            app,
            display_object,
            background,
            navigator_shadow,
            scroll_area,
            favourites_section,
            local_scope_section,
            sub_modules_section,
            layers,
            section_navigator,
            logger,
            groups_wrapper,
            navigator,
            selection,
        }
    }

    fn init_column_section(app: &Application) -> ColumnSection {
        let content = app.new_view::<column_grid::ColumnGrid>();
        LabeledSection::new(content, app)
    }

    fn init_wide_section(app: &Application) -> WideSection {
        let content = app.new_view::<component_group::wide::View>();
        content.set_no_items_label_text("No Entries.");
        LabeledSection::new(content, app)
    }

    fn update_style(&self, style: &Style) {
        // Background

        self.background.bg_color.set(style.content_background_color.into());
        self.background.size.set(style.size());
        self.section_navigator.update_layout(style.clone());

        let navigator_shadow_x = -style.content_width / 2.0;
        self.navigator_shadow.set_position_x(navigator_shadow_x);
        let section_navigator_shadow_size = Vector2(style.navigator_width, style.size_inner().y);
        self.navigator_shadow.size.set(section_navigator_shadow_size);

        // Sections

        self.sub_modules_section.set_style(style);
        self.local_scope_section.set_style(style);
        self.favourites_section.set_style(style);

        self.local_scope_section.content.set_position_x(style.content_width / 2.0);
        self.local_scope_section.content.set_width(style.content_width);
        self.local_scope_section.content.set_color(style.favourites_section_base_color);
        self.local_scope_section.label.set_content(LOCAL_SCOPE_SECTION_HEADING_LABEL);

        self.favourites_section.content.set_position_x(0.0);
        self.favourites_section.label.set_content(FAVOURITES_SECTION_HEADING_LABEL);

        self.sub_modules_section.content.set_position_x(0.0);
        self.sub_modules_section.label.set_content(SUB_MODULES_SECTION_HEADING_LABEL);

        // Scroll Area

        let scroll_area_size = style.scroll_area_size();
        self.scroll_area.resize(scroll_area_size);
        self.scroll_area.set_position_xy(Vector2::new(
            -style.content_width / 2.0 + style.content_padding + style.navigator_width / 2.0,
            style.content_height / 2.0 - style.menu_height / 2.0,
        ));
        self.scroll_area.set_corner_radius_bottom_right(style.content_corner_radius);
        self.selection.size.set(scroll_area_size);
        let selection_area_pos = Vector2(scroll_area_size.x / 2.0, -scroll_area_size.y / 2.0);
        self.selection.set_position_xy(selection_area_pos);
    }

    fn recompute_layout(&self, style: &Style) {
        self.update_style(style);

        let favourites_section_height = self.favourites_section.height(style);
        let local_scope_height = self.local_scope_section.height(style);
        let sub_modules_height = self.sub_modules_section.height(style);
        let full_height = favourites_section_height + local_scope_height + sub_modules_height;

        self.sub_modules_section.set_base_position_y(0.0, style);
        let local_scope_y = -sub_modules_height;
        self.local_scope_section.set_base_position_y(local_scope_y, style);
        let favourites_section_y = -local_scope_height - sub_modules_height;
        self.favourites_section.set_base_position_y(favourites_section_y, style);


        self.scroll_area.set_content_height(full_height);
        self.scroll_area.jump_to_y(full_height);
    }

    /// Scroll to the bottom of the [`section`].
    fn scroll_to(&self, section: Section, style: &Style) {
        let sub_modules_height = self.sub_modules_section.height(style);
        let favourites_section_height = self.favourites_section.height(style);
        let local_scope_height = self.local_scope_section.height(style);
        use crate::navigator::Section::*;
        let section_bottom_y = match section {
            SubModules => sub_modules_height,
            LocalScope => sub_modules_height + local_scope_height,
            Favorites => sub_modules_height + local_scope_height + favourites_section_height,
        };
        let target_y = section_bottom_y - style.size_inner().y;
        self.scroll_area.scroll_to_y(target_y);
    }

    fn update_scroll_viewport(&self) {
        self.favourites_section.update_scroll_viewport(&self.scroll_area);
        self.sub_modules_section.update_scroll_viewport(&self.scroll_area);
        self.local_scope_section.update_scroll_viewport(&self.scroll_area);
    }

    /// Returns the bottom-most visible section inside the scroll area.
    fn bottom_most_visible_section(&self) -> Option<Section> {
        // We built a viewport that is similar to `scroll_area.viewport` but which uses
        // `scroll_position_target_y` instead of `scroll_position_y`. We use it to avoid akward
        // jumps of the selection box animation when clicking on section navigator buttons.
        let scroll_y = -self.scroll_area.scroll_position_target_y.value();
        let viewport = Viewport {
            top:    scroll_y,
            bottom: scroll_y - self.scroll_area.scroll_area_height.value(),
            // We don't care about the left and right edges because the sections are positioned
            // vertically.
            left:   0.0,
            right:  0.0,
        };
        use Section::*;
        let sections: &[(&dyn WithinViewport, Section)] = &[
            (&self.favourites_section, Favorites),
            (&self.local_scope_section, LocalScope),
            (&self.sub_modules_section, SubModules),
        ];
        let section = sections.iter().find(|(s, _)| s.within_viewport(&viewport));
        section.map(|(_, name)| *name)
    }

    /// Set the navigator so it can be disabled on hover.
    pub fn set_navigator(&self, navigator: Option<Navigator>) {
        *self.navigator.borrow_mut() = navigator
    }

    // Note that this is a workaround for lack of hierarchical mouse over events.
    // We need to know if the mouse is over the panel, but cannot do it via a shape, as
    // sub-components still need to receive all of the mouse events, too.
    fn is_hovered(&self, pos: Vector2) -> bool {
        let center = self.display_object.position().xy();
        let size = self.background.size().get();
        let viewport = BoundingBox::from_center_and_size(center, size);
        viewport.contains(pos)
    }

    /// Clamp the Y-coordinate of [`pos`] inside the boundaries of the scroll area.
    fn clamp_y(&self, pos: Vector2) -> Vector2 {
        let top_y = self.scroll_area.position().y;
        let height = self.scroll_area.scroll_area_height.value();
        let selection_height = self.selection.size.get().y;
        let half_selection_height = selection_height / 2.0;
        let y = pos.y.clamp(top_y - height + half_selection_height, top_y - half_selection_height);
        Vector2(pos.x, y)
    }

    /// Calculate the view-local position of the selection from the group-local one.
    ///
    /// A group-local position of the selection box is provided by the
    /// [`component_group::set::Wrapper`]. We need to convert it to a view-local position to display
    /// the selection box correctly. We do this by manually going through the hierarchy of display
    /// objects from the scroll area to the component group that is currently selected and adding
    /// the positions of these objects.
    fn selection_position(&self, id: GroupId, group_local_pos: Vector2, style: &Style) -> Vector2 {
        let scroll_area = &self.scroll_area;
        let scroll_area_size = style.scroll_area_size();
        let scroll_area_center = Vector2(-scroll_area_size.x / 2.0, scroll_area_size.y / 2.0);
        let scroll_area_pos = scroll_area_center + scroll_area.content().position().xy();
        let section_pos = match id.section {
            SectionId::Favorites => self.favourites_section.content.position(),
            SectionId::LocalScope => default(),
            SectionId::SubModules => self.sub_modules_section.content.position(),
        };
        let group_pos = self.groups_wrapper.get(&id).map(|g| g.position()).unwrap_or_default();
        scroll_area_pos + (section_pos + group_pos).xy() + group_local_pos
    }

    fn on_hover(&self) {
        if let Some(navigator) = self.navigator.borrow().as_ref() {
            navigator.disable()
        } else {
            tracing::warn!(
                "Navigator was not initialised on ComponentBrowserPanel. \
            Scroll events will not be handled correctly."
            )
        }
    }

    fn on_hover_end(&self) {
        if let Some(navigator) = self.navigator.borrow().as_ref() {
            navigator.enable()
        }
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

/// Struct that contains the components contained in a section of the Component Browser Panel. Also
/// provides some utility functions for shape and layout handling.
#[derive(Clone, Debug)]
struct LabeledSection<T> {
    pub label:   text::Area,
    pub divider: hline::View,
    pub content: T,
}

impl<T: CloneRef> CloneRef for LabeledSection<T> {
    fn clone_ref(&self) -> Self {
        LabeledSection {
            label:   self.label.clone_ref(),
            divider: self.divider.clone_ref(),
            content: self.content.clone_ref(),
        }
    }
}

type WideSection = LabeledSection<component_group::wide::View>;
type ColumnSection = LabeledSection<column_grid::ColumnGrid>;

impl<T: CloneRef> LabeledSection<T> {
    pub fn new(content: T, app: &Application) -> Self {
        let logger = Logger::new("LabeledSection");
        let label = text::Area::new(app);
        let divider = hline::View::new(logger);
        Self { label, divider, content }
    }

    fn set_style(&self, style: &Style) {
        self.divider.size.set(Vector2(INFINITE, style.section_divider_height));
        self.label.set_default_color(style.section_heading_color);
        self.label.set_default_text_size(text::Size(style.section_heading_size));
        self.label.set_font(style.section_heading_font.clone());
        self.label.set_position_x(style.content_padding);
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
    fn update_scroll_viewport(&self, scroll_area: &ScrollArea) {
        let viewport = scroll_area.viewport.value();
        if self.content.within_viewport(&viewport) {
            scroll_area.content().add_child(&self.content);
        } else {
            self.content.unset_parent();
        }
    }
}

impl ColumnSection {
    fn update_scroll_viewport(&self, scroll_area: &ScrollArea) {
        let viewport = scroll_area.viewport.value();
        if self.content.within_viewport(&viewport) {
            self.content.set_scroll_viewport(viewport);
            scroll_area.content().add_child(&self.content);
        } else {
            self.content.unset_parent();
        }
    }
}

/// Helper trait that exposes `within_viewport` method for all sections.
trait WithinViewport {
    fn within_viewport(&self, viewport: &Viewport) -> bool;
}

impl<T: SectionContent> WithinViewport for LabeledSection<T> {
    fn within_viewport(&self, viewport: &Viewport) -> bool {
        self.content.within_viewport(viewport)
    }
}

/// Trait that provides functionality for layouting and layer setting for structs used in the
/// `LabeledSection`.
trait SectionContent: display::Object {
    fn set_layers(&self, layers: &Layers);
    fn set_position_top_y(&self, position_y: f32);
    fn size(&self) -> Vector2;
    fn width(&self) -> f32 {
        self.size().x
    }
    fn height(&self) -> f32 {
        self.size().y
    }
    fn center(&self) -> Vector2 {
        self.position().xy()
    }
    fn within_viewport(&self, viewport: &Viewport) -> bool {
        viewport.intersects(self.center(), self.size())
    }
}

impl SectionContent for component_group::wide::View {
    fn set_layers(&self, layers: &Layers) {
        self.model().set_layers(&layers.groups);
    }

    fn set_position_top_y(&self, position_y: f32) {
        self.set_position_y(position_y - self.height() / 2.0);
    }

    fn size(&self) -> Vector2 {
        self.size.value()
    }

    fn center(&self) -> Vector2 {
        self.position().xy() + Vector2::new(-self.size().x, self.size().y) / 2.0
    }
}

impl SectionContent for column_grid::ColumnGrid {
    fn set_layers(&self, layers: &Layers) {
        self.model().set_layers(layers);
    }

    fn set_position_top_y(&self, position_y: f32) {
        self.set_position_y(position_y);
    }

    fn size(&self) -> Vector2 {
        self.size.value()
    }
}

impl<T: SectionContent + CloneRef> LabeledSection<T> {
    fn set_layers(&self, layers: &Layers) {
        self.content.set_layers(layers);
        layers.scroll_layer.add_exclusive(&self.label);
        self.label.add_to_scene_layer(&layers.scroll_layer);
        layers.scroll_layer.add_exclusive(&self.divider);
    }

    /// Full height of the section including header.
    fn height(&self, style: &Style) -> f32 {
        let label_height = style.section_heading_height();
        let body_height = self.content.height();
        body_height + label_height
    }

    /// Set the top y position of the section.
    fn set_base_position_y(&self, position_y: f32, style: &Style) {
        match SECTION_HEADER_PLACEMENT {
            SectionHeaderPlacement::Top => {
                let label_pos = position_y - style.section_heading_text_offset;
                self.label.set_position_y(label_pos);
                self.divider.set_position_y(position_y);
                let offset_from_top = style.section_heading_offset + style.section_heading_height();
                let content_position_y = position_y - offset_from_top;
                self.content.set_position_top_y(content_position_y);
            }
            SectionHeaderPlacement::Bottom => {
                let label_offset = self.content.height() + style.section_heading_text_offset;
                let label_pos = position_y - label_offset;
                self.label.set_position_y(label_pos);
                let divider_offset = self.content.height();
                let divider_pos = position_y - divider_offset - style.section_divider_height / 2.0;
                self.divider.set_position_y(divider_pos);
                self.content.set_position_top_y(position_y);
            }
        }
    }
}



// ===========
// === FRP ===
// ===========

/// An identifier of Component Entry in Component List.
///
/// The component is identified by its group id and its number on the component list.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct EntryId {
    pub group:    GroupId,
    pub entry_id: component_group::entry::Id,
}

impl EntryId {
    fn from_wrapper_event(&(group, entry_id): &(GroupId, component_group::entry::Id)) -> Self {
        Self { group, entry_id }
    }
}

/// The selected part of the component group and the identifier of this group.
///
/// Similar to [`component_group::Selected`], but also contains a group id.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Selected {
    Header(GroupId),
    Entry(GroupId, component_group::entry::Id),
}

impl Selected {
    /// Extract an [`EntryId`] if the selected part is an entry.
    pub fn as_entry_id(&self) -> Option<EntryId> {
        match *self {
            Selected::Entry(group, entry_id) => Some(EntryId { group, entry_id }),
            _ => None,
        }
    }

    fn from_wrapper_event(&(group, selected): &(GroupId, component_group::Selected)) -> Self {
        use component_group::Selected::*;
        match selected {
            Header => Self::Header(group),
            Entry(entry_id) => Self::Entry(group, entry_id),
        }
    }
}

define_endpoints_2! {
    Input{
        set_local_scope_section(list_view::entry::AnyModelProvider<component_group::Entry>),
        set_favourites_section(Vec<LabeledAnyModelProvider>),
        set_sub_modules_section(Vec<LabeledAnyModelProvider>),
        /// The component browser is displayed on screen.
        show(),
        /// The component browser is hidden from screen.
        hide(),
    }
    Output{
        selected(Option<Selected>),
        suggestion_accepted(EntryId),
        expression_accepted(EntryId),
        header_accepted(GroupId),
        size(Vector2),
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        frp_api: &<Self as API>::Private,
        app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
    ) {
        let network = &frp_api.network;
        let header_height = style.get_number(component_group_theme::header::height);
        let layout_frp = Style::from_theme(network, style);
        let scene = &app.display.default_scene;
        let input = &frp_api.input;
        let output = &frp_api.output;
        let groups = &model.groups_wrapper;
        let selection = &model.selection;

        let selection_animation = Animation::<Vector2>::new(network);
        let selection_size_animation = Animation::<Vector2>::new(network);
        let selection_corners_animation = Animation::<f32>::new(network);
        let spring = inertia::Spring::default() * SELECTION_ANIMATION_SPRING_FORCE_MULTIPLIER;
        selection_animation.set_spring.emit(spring);

        frp::extend! { network
            model.favourites_section.content.set_content <+ frp_api.input.set_favourites_section;
            model.local_scope_section.content.set_entries <+ frp_api.input.set_local_scope_section;
            model.sub_modules_section.content.set_content <+ frp_api.input.set_sub_modules_section;
            content_update <- any3_(
                &frp_api.input.set_favourites_section,
                &frp_api.input.set_local_scope_section,
                &frp_api.input.set_sub_modules_section,
            );
            recompute_layout <- all(&content_update,&layout_frp.update);
            eval recompute_layout(((_,layout)) model.recompute_layout(layout) );

            eval_ model.scroll_area.viewport( model.update_scroll_viewport() );

            is_visible <- bool(&input.hide, &input.show);
            is_hovered <- app.cursor.frp.screen_position.map(f!([model,scene](pos) {
                let pos = scene.screen_to_object_space(&model, pos.xy());
                model.is_hovered(pos.xy())
            })).gate(&is_visible).on_change();

            on_hover <- is_hovered.on_true();
            on_hover_end <- is_hovered.on_false();
            eval_ on_hover ( model.on_hover() );
            eval_ on_hover_end ( model.on_hover_end() );

            output.selected <+ groups.selected.map(|op| op.as_ref().map(Selected::from_wrapper_event));
            output.suggestion_accepted <+ groups.suggestion_accepted.map(EntryId::from_wrapper_event);
            output.expression_accepted <+ groups.expression_accepted.map(EntryId::from_wrapper_event);
            output.header_accepted <+ groups.header_accepted;

            output.size <+ layout_frp.update.map(|style| style.size_inner());


            // === Selection ===

            selection_size_animation.target <+ groups.selection_size._1();
            selection_animation.target <+ groups.selection_position_target.all_with3(
                &model.scroll_area.scroll_position_y,
                &layout_frp.update,
                f!(((id, pos), _, style) model.selection_position(*id, *pos, style))
            );
            selection_corners_animation.target <+ groups.selection_corners_radius._1();
            eval selection_animation.value ((pos) selection.pos.set(*pos));
            eval selection_size_animation.value ((pos) selection.selection_size.set(*pos));
            eval selection_corners_animation.value ((r) selection.corners_radius.set(*r));
            eval_ model.scroll_area.scroll_position_y(selection_animation.skip.emit(()));
            // When the selection highlights entries near the header of the component group, we use
            // the `margin_top` parameter of the [`selection_box`] shape to clip the selection shape
            // and make it appear covered by the header. When we select the header, the `margin_top`
            // parameter is reset to zero to avoid clipping the selection shape.
            // See the documentation of the [`selection_box`](selection_box) module for more
            // details.
            let is_header = |s: &Option<Selected>| matches!(*s, Some(Selected::Header(_)));
            is_any_header_selected <- output.selected.map(is_header).on_change();
            on_any_header_selected <- is_any_header_selected.on_true();
            on_any_header_deselected <- is_any_header_selected.on_false();
            // The local scope section does not have a header and we must reset the selection area
            // margin when hovering it.
            let is_local_scope = |s: &Option<(GroupId, _)>| {
                matches!(*s, Some((id, _)) if id == GroupId::local_scope_group())
            };
            is_local_scope_section_selected <- groups.selected.map(is_local_scope).on_change();
            should_reset_area <- any(...);
            should_reset_area <+ on_any_header_selected.constant(true);
            should_reset_area <+ on_any_header_deselected.constant(false);
            should_reset_area <- or(&should_reset_area, &is_local_scope_section_selected);
            reset_selection_area <- should_reset_area.on_true();
            restrict_selection_area <- should_reset_area.on_false();
            eval_ reset_selection_area(selection.margin_top.set(0.0));
            _eval <- all_with(&header_height, &restrict_selection_area,
                f!((height, _) selection.margin_top.set(*height))
            );


            // === Section navigator ===

            eval_ input.show(model.section_navigator.select_section(Section::Favorites));

            chosen_section <- model.section_navigator.chosen_section.filter_map(|s| *s);
            scroll_to_section <- all(&chosen_section, &layout_frp.update);
            eval scroll_to_section(((section, layout)) model.scroll_to(*section, layout));

            visible_section <- model.scroll_area.viewport.filter_map(
                f_!(model.bottom_most_visible_section())
            ).on_change();
            eval visible_section((section) model.section_navigator.select_section(*section));


            // === Navigator icons colors ===

            let strong_color = style.get_color(list_panel_theme::navigator_icon_strong_color);
            let weak_color = style.get_color(list_panel_theme::navigator_icon_weak_color);
            let params = icon::Params { strong_color, weak_color };
            model.section_navigator.set_bottom_buttons_entry_params(params);
        }
        layout_frp.init.emit(());
        selection_animation.skip.emit(());
    }
}

/// A sub-content of the Component Browser, that shows the available Component List Sections.
/// Each Component List Section contains named tiles called Component List Groups. To learn more
/// see the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
pub type ComponentBrowserPanel = component::ComponentView<Model, Frp>;
