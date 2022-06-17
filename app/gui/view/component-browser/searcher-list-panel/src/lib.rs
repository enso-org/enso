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
#![feature(slice_as_chunks)]
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

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
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
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::searcher as searcher_theme;
use ensogl_list_view as list_view;
use ensogl_scroll_area::ScrollArea;
use ensogl_scroll_area::Viewport;
use ensogl_shadow as shadow;
use ensogl_text as text;
use ide_view_component_group as component_group;
use ide_view_component_group::set::Group;
use ide_view_component_group::set::SectionId;
use ide_view_component_group::Layers;
use searcher_theme::list_panel as list_panel_theme;


// ==============
// === Export ===
// ==============

pub mod column_grid;

pub use column_grid::LabeledAnyModelProvider;
pub use component_group::set::GroupId;



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

#[derive(Clone, Debug, Default)]
struct Style {
    content: ContentStyle,
    section: SectionStyle,
    menu:    MenuStyle,
}

impl Style {
    fn size_inner(&self) -> Vector2 {
        let width = self.content.size.x;
        let height = self.content.size.y + self.menu.height;
        Vector2::new(width, height)
    }

    fn size(&self) -> Vector2 {
        self.size_inner().map(|value| value + 2.0 * SHADOW_PADDING)
    }

    fn menu_divider_y_pos(&self) -> f32 {
        self.size_inner().y / 2.0 - self.menu.height
    }
}

#[derive(Clone, Debug, Default)]
struct ContentStyle {
    /// Size of the Component List Panel content area.
    size:             Vector2,
    /// Radius of the rounded corners.
    corner_radius:    f32,
    /// Extra space between scroll area and backgound shape edge.
    padding:          f32,
    /// Color used for the panel background.
    background_color: color::Rgba,
}

#[derive(Clone, Debug, Default)]
struct SectionHeadingStyle {
    /// Font size used for section headers.
    size:   text::style::Size,
    /// Font used for section headers.
    font:   String,
    /// Color used for section headers.
    color:  color::Rgba,
    /// Distance between the section header and the section content.
    offset: f32,
}

impl SectionHeadingStyle {
    /// Returns the size of the header, which consists of the header text and the padding between
    /// text and content.
    fn height(&self) -> f32 {
        self.size.raw + self.offset
    }
}

#[derive(Clone, Debug, Default)]
struct SectionStyle {
    heading: SectionHeadingStyle,
    /// Thickness of the line that divides the sections within the Component List Panel.
    divider_height: f32,
    /// Color used for the Divider.
    divider_color: color::Rgba,
    favourites_section_base_color: color::Rgba,
}

#[derive(Clone, Debug, Default)]
struct MenuStyle {
    /// Height of the area reserved for Component Browser Panel Menu.
    height:         f32,
    /// Thickness of the line that divides the Component List Panel from the Component Browser
    /// Panel Menu.
    divider_height: f32,
    /// Color used for the Divider.
    divider_color:  color::Rgba,
}

impl Style {
    fn from_theme(
        network: &enso_frp::Network,
        style: &StyleWatchFrp,
    ) -> (enso_frp::Stream<Style>, enso_frp::stream::WeakNode<enso_frp::SourceData>) {
        let theme_path: style::Path = list_panel_theme::HERE.into();

        let content_width = style.get_number(theme_path.sub("content_width"));
        let content_height = style.get_number(theme_path.sub("content_height"));
        let content_corner_radius = style.get_number(theme_path.sub("content_corner_radius"));
        let content_padding = style.get_number(theme_path.sub("content_padding"));
        let content_background_color = style.get_color(theme_path.sub("content_background_color"));

        let menu_height = style.get_number(theme_path.sub("menu_height"));
        let menu_divider_color = style.get_color(theme_path.sub("menu_divider_color"));
        let menu_divider_height = style.get_number(theme_path.sub("menu_divider_height"));

        let section_divider_height = style.get_number(theme_path.sub("section_divider_height"));
        let section_heading_size = style.get_number(theme_path.sub("section_heading_size"));
        let section_heading_font = style.get_text(theme_path.sub("section_heading_font"));
        let section_heading_color = style.get_color(theme_path.sub("section_heading_color"));
        let section_divider_color = style.get_color(theme_path.sub("section_divider_color"));

        let favourites_section_base_color =
            style.get_color(theme_path.sub("favourites_section_base_color"));

        frp::extend! { network
            init <- source_();

            content_size <- all3(&init,&content_width, &content_height).map(|(_,x,y)|Vector2::new(*x,*y));

            section_heading_layout_data <- all4(
                &init,
                &section_heading_size,
                &section_heading_font,
                &section_heading_color
            );
            section_heading_layout <- section_heading_layout_data.map(|(_,size,font,color)| {
                SectionHeadingStyle{
                    size:text::style::Size(*size),
                    font:font.clone(),
                    color:*color,
                    offset:*size
                }
            });
            section_layout_data <- all5(
                &init,
                &section_heading_layout,
                &section_divider_height,
                &section_divider_color,
                &favourites_section_base_color
            );
            section_layout <- section_layout_data.map(
                |(_,heading,divider_height,divider_color,favourites_section_base_color)|{
                SectionStyle{
                        heading:heading.clone(),
                        divider_height:*divider_height,
                        divider_color:*divider_color,
                        favourites_section_base_color:*favourites_section_base_color
                    }
            });
            content_layout_data <- all5(
                &init,
                &content_size,
                &content_corner_radius,
                &content_padding,
                &content_background_color
            );
            content_layout <- content_layout_data.map(
                |(_,size,corner_radius,padding,background_color)|{
                ContentStyle {
                    size:*size,
                    corner_radius:*corner_radius,
                    padding:*padding,
                    background_color:*background_color,
                }
            });

            menu_layout_data <- all4(&init,&menu_height,&menu_divider_color,&menu_divider_height);
            menu_layou <- menu_layout_data.map(|(_,height,divider_color,divider_height)|{
                MenuStyle {
                    height:*height,
                    divider_height:*divider_height,
                    divider_color:*divider_color,
                }
            });

            layout_data <- all4(&init,&content_layout,&section_layout,&menu_layou);
            layout <- layout_data.map(|(_,content,section,menu)| {
                Style {
                    content:content.clone(),
                    section:section.clone(),
                    menu:menu.clone(),
                }
            });

        }
        (layout, init)
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


// === Shape Definition ===

mod background {
    use super::*;

    ensogl_core::define_shape_system! {
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

            let width = content_width;
            let height = content_height + menu_height;

            let divider_y_pos = height / 2.0 - menu_height;

            let left_width = &(width/2.0).px();
            let left = Rect((left_width,height.px())).translate_x(-left_width/2.0);

            let right_width = &(width/2.0 + 2.0 * content_corner_radius).px();
            let right = Rect((right_width,height.px())).corners_radius(content_corner_radius.px());
            let right = right.translate_x((width/4.0-content_corner_radius).px());

            let divider = Rect((width.px(),menu_divider_height.px()));
            let divider = divider.fill(menu_divider_color);
            let divider = divider.translate_y(divider_y_pos.px());

            let base_shape = &(left + right);
            let background = base_shape.fill(bg_color);
            let shadow     = shadow::from_shape_with_alpha(base_shape.into(),&alpha,style);

            (shadow + background + divider).into()
        }
    }
}

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
    scroll_area:         ScrollArea,
    favourites_section:  ColumnSection,
    local_scope_section: WideSection,
    sub_modules_section: ColumnSection,
    layers:              Layers,
    groups_wrapper:      component_group::set::Wrapper,
    navigator:           Rc<RefCell<Option<Navigator>>>,
}

impl Model {
    fn new(app: &Application) -> Self {
        let logger = Logger::new("ComponentBrowserPanel");
        let app = app.clone_ref();
        let display_object = display::object::Instance::new(&logger);
        let groups_wrapper = component_group::set::Wrapper::new();

        let background = background::View::new(&logger);
        display_object.add_child(&background);
        app.display.default_scene.layers.below_main.add_exclusive(&background);

        let favourites_section = Self::init_column_section(&app);
        let local_scope_section = Self::init_wide_section(&app);
        let sub_modules_section = Self::init_column_section(&app);

        favourites_section
            .content
            .set_group_wrapper(&(SectionId::Favorites, groups_wrapper.clone_ref()));
        groups_wrapper.add(
            GroupId::local_scope_group(),
            Group::Wide(local_scope_section.content.clone_ref()),
        );
        sub_modules_section
            .content
            .set_group_wrapper(&(SectionId::SubModules, groups_wrapper.clone_ref()));

        let scroll_area = ScrollArea::new(&app);
        display_object.add_child(&scroll_area);

        let camera = &scroll_area.content_layer().camera();
        let parent_layer = scroll_area.content_layer();
        let layers = Layers::new(&app.logger, camera, parent_layer);

        favourites_section.set_parent(scroll_area.content());
        local_scope_section.set_parent(scroll_area.content());
        sub_modules_section.set_parent(scroll_area.content());

        let navigator = default();
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
            groups_wrapper,
            navigator,
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

    fn update_layers(&self) {
        self.favourites_section.set_layers(&self.layers, self.scroll_area.content_layer());
        self.local_scope_section.set_layers(&self.layers, self.scroll_area.content_layer());
        self.sub_modules_section.set_layers(&self.layers, self.scroll_area.content_layer());
    }

    fn update_style(&self, style: &Style) {
        self.sub_modules_section.set_style(style);
        self.local_scope_section.set_style(style);
        self.favourites_section.set_style(style);

        self.background.bg_color.set(style.content.background_color.into());
        self.background.size.set(style.size());

        self.local_scope_section
            .content
            .set_position_x(style.content.padding + style.content.size.x / 2.0);
        self.local_scope_section
            .content
            .set_width(style.size_inner().x - 2.0 * style.content.padding);
        self.local_scope_section.content.set_color(style.section.favourites_section_base_color);
        self.local_scope_section.label.set_content(LOCAL_SCOPE_SECTION_HEADING_LABEL);

        self.favourites_section.content.set_position_x(style.content.padding);
        self.favourites_section.label.set_content(SUB_MODULES_SECTION_HEADING_LABEL);

        self.sub_modules_section.content.set_position_x(style.content.padding);
        self.sub_modules_section.label.set_content(SUB_MODULES_SECTION_HEADING_LABEL);

        self.scroll_area.resize(Vector2::new(
            style.content.size.x - style.content.padding,
            style.content.size.y - style.content.padding,
        ));
        self.scroll_area.set_position_xy(Vector2::new(
            -style.content.size.x / 2.0,
            style.content.size.y / 2.0 - style.menu.height / 2.0,
        ));
        self.scroll_area.set_corner_radius_bottom_right(style.content.corner_radius);
    }

    fn recompute_layout(&self, style: &Style) {
        self.update_style(style);
        self.update_layers();

        let favourites_section_height = self.favourites_section.height(style);
        let local_scope_height = self.local_scope_section.height(style);
        let sub_modules_height = self.sub_modules_section.height(style);
        let full_height = favourites_section_height + local_scope_height + sub_modules_height;

        self.sub_modules_section.set_base_position_y(0.0, style);
        self.favourites_section.set_base_position_y(-sub_modules_height, style);
        self.local_scope_section
            .set_base_position_y(-favourites_section_height - sub_modules_height, style);

        self.scroll_area.set_content_height(full_height);
        self.scroll_area.jump_to_y(full_height);
    }

    fn update_scroll_viewport(&self) {
        self.favourites_section.update_scroll_viewport(&self.scroll_area);
        self.sub_modules_section.update_scroll_viewport(&self.scroll_area);
        self.local_scope_section.update_scroll_viewport(&self.scroll_area);
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

    fn on_hover(&self) {
        if let Some(navigator) = self.navigator.borrow().as_ref() {
            navigator.disable()
        } else {
            WARNING!(
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
        self.divider.size.set(Vector2(INFINITE, style.section.divider_height));
        self.label.set_default_color(style.section.heading.color);
        self.label.set_default_text_size(style.section.heading.size);
        self.label.set_font(style.section.heading.font.clone());
        self.label.set_position_y(-0.75 * style.section.heading.size.raw);
        self.label.set_position_x(3.0 + style.content.padding);
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

/// Trait that provides functionality for layouting and layer setting for structs used in the
/// `LabeledSection`.
trait SectionContent: display::Object {
    fn set_layers(&self, layers: &Layers, scroll_layer: &Layer);
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
    fn set_layers(&self, layers: &Layers, _scroll_layer: &Layer) {
        self.model().set_layers(layers);
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
    fn set_layers(&self, layers: &Layers, scroll_layer: &Layer) {
        self.model().set_layers(layers, scroll_layer);
    }

    fn set_position_top_y(&self, position_y: f32) {
        self.set_position_y(position_y);
    }

    fn size(&self) -> Vector2 {
        self.size.value()
    }
}

impl<T: SectionContent + CloneRef> LabeledSection<T> {
    fn set_layers(&self, layers: &Layers, scroll_layer: &Layer) {
        self.content.set_layers(layers, scroll_layer);
        scroll_layer.add_exclusive(&self.label);
        self.label.add_to_scene_layer(scroll_layer);
        scroll_layer.add_exclusive(&self.divider);
    }

    /// Full height of the section including header.
    fn height(&self, style: &Style) -> f32 {
        let label_height = style.section.heading.height();
        let body_height = self.content.height();
        let next_section_offset = 17.0; // TODO[MM] this should not be needed.
        body_height + label_height + next_section_offset
    }

    /// Set the top y position of the section.
    fn set_base_position_y(&self, position_y: f32, style: &Style) {
        match SECTION_HEADER_PLACEMENT {
            SectionHeaderPlacement::Top => {
                let label_pos = position_y - self.label.height.value() / 1.5;
                self.label.set_position_y(label_pos);
                self.divider.set_position_y(position_y);
                let content_position_y =
                    position_y - style.section.heading.offset - style.section.heading.height();
                self.content.set_position_top_y(content_position_y);
            }
            SectionHeaderPlacement::Bottom => {
                let label_pos =
                    position_y - self.content.height() - self.label.height.value() / 1.5;
                self.label.set_position_y(label_pos);
                let divider_pos =
                    position_y - self.content.height() - style.section.divider_height + 1.0;
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

define_endpoints_2! {
    Input{
        set_local_scope_section(list_view::entry::AnyModelProvider<component_group::Entry>),
        set_favourites_section(Vec<LabeledAnyModelProvider>),
        set_sub_modules_section(Vec<LabeledAnyModelProvider>),
    }
    Output{
        selected_entry(Option<EntryId>),
        suggestion_accepted(EntryId),
        expression_accepted(EntryId),
        is_header_selected(GroupId, bool),
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
        let (layout_update, init_layout) = Style::from_theme(network, style);
        let output = &frp_api.output;
        let groups = &model.groups_wrapper;
        frp::extend! { network
            model.favourites_section.content.set_content <+ frp_api.input.set_favourites_section;
            model.local_scope_section.content.set_entries <+ frp_api.input.set_local_scope_section;
            model.sub_modules_section.content.set_content <+ frp_api.input.set_sub_modules_section;
            content_update <- any3_(
                &frp_api.input.set_favourites_section,
                &frp_api.input.set_local_scope_section,
                &frp_api.input.set_sub_modules_section,
            );
            recompute_layout <- all(&content_update,&layout_update);
            eval recompute_layout(((_,layout)) model.recompute_layout(layout) );

            eval_ model.scroll_area.viewport( model.update_scroll_viewport() );

            is_hovered <- app.cursor.frp.scene_position.map(f!((pos) model.is_hovered(pos.xy())) ).on_change();

            on_hover <- is_hovered.on_true();
            on_hover_end <- is_hovered.on_false();
            eval_ on_hover ( model.on_hover() );
            eval_ on_hover_end ( model.on_hover_end() );

            output.selected_entry <+ groups.selected_entry.map(|op| op.as_ref().map(EntryId::from_wrapper_event));
            output.suggestion_accepted <+ groups.suggestion_accepted.map(EntryId::from_wrapper_event);
            output.expression_accepted <+ groups.expression_accepted.map(EntryId::from_wrapper_event);
            output.is_header_selected <+ groups.is_header_selected;
            output.header_accepted <+ groups.header_accepted;

            output.size <+ layout_update.map(|style| style.content.size);
        }
        init_layout.emit(())
    }
}

/// A sub-content of the Component Browser, that shows the available Component List Sections.
/// Each Component List Section contains named tiles called Component List Groups. To learn more
/// see the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
pub type ComponentBrowserPanel = component::ComponentView<Model, Frp>;
