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


pub mod column_grid;

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::component_group::icon;
pub use column_grid::LabeledAnyModelProvider;
use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::define_endpoints_2;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::style;
use ensogl_core::display::style::Path;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::searcher as searcher_theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::AnyModelProvider;
use ensogl_scroll_area::ScrollArea;
use ensogl_shadow as shadow;
use ensogl_text as text;
use ide_view_component_group as component_group;
use ide_view_component_group::Layers as GroupLayers;
use searcher_theme::list_panel as list_panel_theme;



// ==============
// === Layers ===
// ==============

#[derive(Debug, Clone, CloneRef)]
struct Layers {
    groups:         GroupLayers,
    base:           Layer,
    selection:      Layer,
    selection_mask: Layer,
    scroll_layer:   Layer,
}

impl Layers {
    fn new(app: &Application, scroll_area: &ScrollArea) -> Self {
        let main_camera = app.display.default_scene.layers.main.camera();
        let base = Layer::new_with_cam(app.logger.sub("component_groups"), &main_camera);
        let selection = Layer::new_with_cam(app.logger.sub("selection"), &main_camera);
        let selection_mask = Layer::new_with_cam(app.logger.sub("selection_mask"), &main_camera);
        selection.set_mask(&selection_mask);
        app.display.default_scene.layers.main.add_sublayer(&base);
        app.display.default_scene.layers.main.add_sublayer(&selection);
        let content = &scroll_area.content_layer();
        let groups = GroupLayers::new(&app.logger, content, &selection);
        let scroll_layer = scroll_area.content_layer().clone_ref();
        Self { base, selection, groups, selection_mask, scroll_layer }
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

#[derive(Clone, Debug, Default)]
struct Style {
    content:   ContentStyle,
    section:   SectionStyle,
    menu:      MenuStyle,
    navigator: NavigatorStyle,
}

impl Style {
    fn size_inner(&self) -> Vector2 {
        let width = self.content.size.x + self.navigator.width;
        let height = self.content.size.y + self.menu.height;
        Vector2::new(width, height)
    }

    fn size(&self) -> Vector2 {
        self.size_inner().map(|value| value + 2.0 * SHADOW_PADDING)
    }

    fn navigator_size(&self) -> Vector2 {
        self.size_inner() + Vector2(self.navigator.width, 0.0)
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

#[derive(Clone, Debug, Default)]
struct NavigatorStyle {
    width:  f32,
    height: f32,
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

        let navigator_width = style.get_number(theme_path.sub("section_navigator_width"));

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
            menu_layout <- menu_layout_data.map(|(_,height,divider_color,divider_height)|{
                MenuStyle {
                    height:*height,
                    divider_height:*divider_height,
                    divider_color:*divider_color,
                }
            });

            navigator_layout_data <- all4(&init, &navigator_width, &content_height, &menu_height);
            navigator_layout <- navigator_layout_data.map(|(_,width,content_height,menu_height)| {
                NavigatorStyle {
                    width: *width,
                    height: *content_height + *menu_height,
                }
            });

            layout_data <- all5(&init,&content_layout,&section_layout,&menu_layout,
                &navigator_layout);
            layout <- layout_data.map(|(_,content,section,menu,navigator)| {
                Style {
                    content:content.clone(),
                    section:section.clone(),
                    menu:menu.clone(),
                    navigator:navigator.clone(),
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
            let navigator_width = style.get_number(theme_path.sub("section_navigator_width"));

            let width = content_width + navigator_width;
            let height = content_height + menu_height;

            let divider_y_pos = height / 2.0 - menu_height;

            let divider = Rect((content_width.px(),menu_divider_height.px()));
            let divider = divider.fill(menu_divider_color);
            let divider = divider.translate_y(divider_y_pos.px());

            let base_shape = Rect((width.px(), height.px())).corners_radius(content_corner_radius
                .px()).translate_x(-navigator_width.px() / 2.0);
            let background = base_shape.fill(bg_color);
            let shadow     = shadow::from_shape_with_alpha(base_shape.into(),&alpha,style);

            (shadow + background + divider).into()
        }
    }
}

mod section_navigator_shadow {
    use super::*;

    ensogl_core::define_shape_system! {
        above = [background];
        (style:Style) {
            let theme_path: style::Path = list_panel_theme::HERE.into();
            let content_height = style.get_number(theme_path.sub("content_height"));
            let menu_height = style.get_number(theme_path.sub("menu_height"));
            let navigator_width = style.get_number(theme_path.sub("section_navigator_width"));
            let height = content_height + menu_height;
            let width = navigator_width;
            let base_shape = Rect((width.px(), height.px() * 2.0)).translate_x(width.px());
            let shadow = shadow::from_shape(base_shape.into(), style);
            shadow.into()
        }
    }
}

mod hline {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style) {
            let theme_path: style::Path = list_panel_theme::HERE.into();
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let section_divider_color = style.get_color(theme_path.sub("section_divider_color"));
            let rect = Rect((width,height));
            let rect = rect.fill(section_divider_color);
            rect.into()
        }
    }
}

#[derive(Debug, Clone, CloneRef)]
struct Icon {
    display_object: display::object::Instance,
    logger:         Logger,
    icon:           Rc<RefCell<Option<icon::Any>>>,
}

impl display::Object for Icon {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl list_view::Entry for Icon {
    type Model = icon::Id;
    type Params = ();

    fn new(app: &Application, style_prefix: &Path, params: &Self::Params) -> Self {
        let logger = app.logger.sub("NavigatorIcon");
        let display_object = display::object::Instance::new(&logger);
        let icon = default();
        Self { display_object, logger, icon }
    }

    fn update(&self, model: &Self::Model) {
        // todo: cache icon?
        let size = Vector2(icon::SIZE, icon::SIZE);
        let icon = model.create_shape(&self.logger, size);
        icon.strong_color.set(color::Rgba::black().into());
        self.display_object.add_child(&icon);
        *self.icon.borrow_mut() = Some(icon);
    }

    fn set_max_width(&self, max_width_px: f32) {}

    fn set_label_layer(&self, label_layer: &Layer) {}
}

#[derive(Debug, Copy, Clone)]
enum Section {
    SubModules,
    LocalScope,
    Favourites,
}

impl Default for Section {
    fn default() -> Self {
        Section::Favourites
    }
}

/// TODO: docs
#[derive(Debug, Clone, CloneRef)]
pub struct Navigator {
    display_object: display::object::Instance,
    network:        frp::Network,
    bottom_buttons: list_view::ListView<Icon>,
    top_buttons:    list_view::ListView<Icon>,
    chosen_section: frp::Source<Option<Section>>,
}

const TOP_BUTTONS: [icon::Id; 2] = [icon::Id::Libraries, icon::Id::Marketplace];
const BOTTOM_BUTTONS: [icon::Id; 3] = [icon::Id::SubModules, icon::Id::LocalScope, icon::Id::Star];

impl Navigator {
    pub fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new(&app.logger);
        let top_buttons = app.new_view::<list_view::ListView<Icon>>();
        let bottom_buttons = app.new_view::<list_view::ListView<Icon>>();
        top_buttons.set_background_color(HOVER_COLOR);
        bottom_buttons.set_background_color(HOVER_COLOR);
        top_buttons.show_background_shadow(false);
        bottom_buttons.show_background_shadow(false);
        top_buttons.resize(Vector2(39.0, list_view::entry::HEIGHT * TOP_BUTTONS.len() as f32));
        bottom_buttons
            .resize(Vector2(39.0, list_view::entry::HEIGHT * BOTTOM_BUTTONS.len() as f32));
        display_object.add_child(&top_buttons);
        display_object.add_child(&bottom_buttons);
        top_buttons.hide_selection();
        top_buttons.set_style_prefix(list_panel_theme::navigator::HERE.str);
        bottom_buttons.set_style_prefix(list_panel_theme::navigator::HERE.str);

        top_buttons.set_entries(AnyModelProvider::new(TOP_BUTTONS.to_vec()));
        bottom_buttons.set_entries(AnyModelProvider::new(BOTTOM_BUTTONS.to_vec()));

        let network = frp::Network::new("ComponentBrowser.Navigator");
        frp::extend! { network
            chosen_section <- source();
            eval bottom_buttons.chosen_entry([chosen_section](id) match id {
                Some(0) => chosen_section.emit(Some(Section::SubModules)),
                Some(1) => chosen_section.emit(Some(Section::LocalScope)),
                Some(2) => chosen_section.emit(Some(Section::Favourites)),
                _ => {}
            });
        }
        bottom_buttons.select_entry(Some(2));

        Self { display_object, top_buttons, bottom_buttons, network, chosen_section }
    }

    fn update_layout(&self, style: Style) {
        let top = style.navigator.height / 2.0;
        let bottom = -style.navigator.height / 2.0;
        let top_buttons_height = TOP_BUTTONS.len() as f32 * list_view::entry::HEIGHT;
        let bottom_buttons_height = BOTTOM_BUTTONS.len() as f32 * list_view::entry::HEIGHT;
        let top_padding = -3.0;
        let bottom_padding = 7.0;
        let x_pos = -style.content.size.x / 2.0 - style.navigator.width / 2.0;
        self.top_buttons
            .set_position_xy(Vector2(x_pos, top - top_buttons_height / 2.0 - top_padding));
        self.bottom_buttons
            .set_position_xy(Vector2(x_pos, bottom + bottom_buttons_height / 2.0 + bottom_padding));
    }
}

impl display::Object for Navigator {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =============
// === Model ===
// =============

/// The Model of Select Component.
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    app: Application,
    logger: Logger,
    display_object: display::object::Instance,
    background: background::View,
    // FIXME[TODO]: this should be embedded into a background shape
    section_navigator_shadow: section_navigator_shadow::View,
    scroll_area: ScrollArea,
    favourites_section: ColumnSection,
    local_scope_section: WideSection,
    sub_modules_section: ColumnSection,
    section_navigator: Navigator,
    layers: Layers,
}

impl Model {
    fn new(app: &Application) -> Self {
        let logger = Logger::new("ComponentBrowserPanel");
        let app = app.clone_ref();
        let display_object = display::object::Instance::new(&logger);

        let background = background::View::new(&logger);
        display_object.add_child(&background);
        app.display.default_scene.layers.below_main.add_exclusive(&background);
        let section_navigator_shadow = section_navigator_shadow::View::new(&logger);
        display_object.add_child(&section_navigator_shadow);
        app.display.default_scene.layers.below_main.add_exclusive(&section_navigator_shadow);

        let favourites_section = Self::init_column_section(&app);
        let local_scope_section = Self::init_wide_section(&app);
        let sub_modules_section = Self::init_column_section(&app);

        let section_navigator = Navigator::new(&app);
        display_object.add_child(&section_navigator);

        let scroll_area = ScrollArea::new(&app);
        display_object.add_child(&scroll_area);

        let layers = Layers::new(&app, &scroll_area);
        layers.base.add_exclusive(&scroll_area);

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
            section_navigator_shadow,
            scroll_area,
            favourites_section,
            local_scope_section,
            sub_modules_section,
            layers,
            section_navigator,
            logger,
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
        self.sub_modules_section.set_style(style);
        self.local_scope_section.set_style(style);
        self.favourites_section.set_style(style);

        self.background.bg_color.set(style.content.background_color.into());
        self.background.size.set(style.size());
        self.section_navigator.update_layout(style.clone());

        self.section_navigator_shadow
            .set_position_x(-style.content.size.x / 2.0 - style.navigator.width / 2.0);
        let section_navigator_shadow_size = Vector2(style.navigator.width, style.size_inner().y);
        self.section_navigator_shadow.size.set(section_navigator_shadow_size);

        let local_scope_content = &self.local_scope_section.content;
        local_scope_content.set_position_x(style.content.padding + style.content.size.x / 2.0);
        local_scope_content.set_width(style.size_inner().x - 2.0 * style.content.padding);
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

        let favourites_section_height = self.favourites_section.height(style);
        let local_scope_height = self.local_scope_section.height(style);
        let sub_modules_height = self.sub_modules_section.height(style);

        self.favourites_section.set_base_position_y(0.0, style);
        self.local_scope_section.set_base_position_y(-favourites_section_height, style);
        let sub_modules_position = -favourites_section_height - local_scope_height;
        self.sub_modules_section.set_base_position_y(sub_modules_position, style);

        let full_height = favourites_section_height + local_scope_height + sub_modules_height;
        self.scroll_area.set_content_height(full_height);
        self.scroll_area.jump_to_y(full_height);
    }

    fn show_section(&self, section: Section, style: &Style) {
        let sub_modules_height = self.sub_modules_section.height(style);
        let favourites_section_height = self.favourites_section.height(style);
        let local_scope_height = self.local_scope_section.height(style);
        use Section::*;
        let target_y = match section {
            SubModules => sub_modules_height,
            LocalScope => sub_modules_height + local_scope_height,
            Favourites => sub_modules_height + local_scope_height + favourites_section_height,
        };
        let target_y = target_y - style.size_inner().y;
        self.scroll_area.scroll_to_y(target_y);
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
        // TODO[MM]: These magic numbers will be removed with https://github.com/enso-org/enso/pull/3537
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

/// Trait that provides functionality for layouting and layer setting for structs used in the
/// `LabeledSection`.
trait SectionContent {
    fn set_layers(&self, layers: &Layers);
    fn height(&self) -> f32;
    fn set_position_top_y(&self, position_y: f32);
}

impl SectionContent for component_group::wide::View {
    fn set_layers(&self, layers: &Layers) {
        self.model().set_layers(&layers.groups);
    }

    fn height(&self) -> f32 {
        self.size.value().y
    }

    fn set_position_top_y(&self, position_y: f32) {
        self.set_position_y(position_y - self.height() / 2.0);
    }
}

impl SectionContent for column_grid::ColumnGrid {
    fn set_layers(&self, layers: &Layers) {
        self.model().set_layers(layers);
    }

    fn height(&self) -> f32 {
        self.size.value().y
    }

    fn set_position_top_y(&self, position_y: f32) {
        self.set_position_y(position_y);
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
        let label_height = style.section.heading.height();
        let body_height = self.content.height();
        // TODO[MM]: This magic number will be removed with https://github.com/enso-org/enso/pull/3537
        let next_section_offset = 29.0;
        body_height + label_height + next_section_offset
    }

    /// Set the top y position of the section.
    fn set_base_position_y(&self, position_y: f32, style: &Style) {
        match SECTION_HEADER_PLACEMENT {
            SectionHeaderPlacement::Top => {
                // TODO[MM] This magic number will be removed with https://github.com/enso-org/enso/pull/3537
                let label_pos = position_y - self.label.height.value() / 1.5;
                self.label.set_position_y(label_pos);
                self.divider.set_position_y(position_y);
                let offset_from_top = style.section.heading.offset + style.section.heading.height();
                let content_position_y = position_y - offset_from_top;
                self.content.set_position_top_y(content_position_y);
            }
            SectionHeaderPlacement::Bottom => {
                // TODO[MM]: This magic number will be removed with https://github.com/enso-org/enso/pull/3537
                let offset_from_top = 1.0;
                let label_offset = self.content.height() + self.label.height.value() / 1.5;
                let label_pos = position_y - label_offset - offset_from_top;
                self.label.set_position_y(label_pos);
                let divider_offset = self.content.height() - style.section.divider_height + 2.0;
                // TODO[MM]: This magic number will be removed with https://github.com/enso-org/enso/pull/3537
                let divider_pos = position_y - divider_offset - offset_from_top;
                self.divider.set_position_y(divider_pos);
                self.content.set_position_top_y(position_y);
            }
        }
    }
}



// ===========
// === FRP ===
// ===========

define_endpoints_2! {
    Input{
        set_local_scope_section(list_view::entry::AnyModelProvider<component_group::Entry>),
        set_favourites_section(Vec<LabeledAnyModelProvider>),
        set_sub_modules_section(Vec<LabeledAnyModelProvider>),
    }
    Output{}
}

impl component::Frp<Model> for Frp {
    fn init(
        frp_api: &<Self as API>::Private,
        _app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
    ) {
        let network = &frp_api.network;
        let (layout_update, init_layout) = Style::from_theme(network, style);
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

            chosen_section <- model.section_navigator.chosen_section.filter_map(|s| *s);
            show_section <- all(&chosen_section, &layout_update);
            eval show_section(((section, layout)) model.show_section(*section, layout));
        }
        init_layout.emit(())
    }
}

/// A sub-content of the Component Browser, that shows the available Component List Sections.
/// Each Component List Section contains named tiles called Component List Groups. To learn more
/// see the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
pub type ComponentBrowserPanel = component::ComponentView<Model, Frp>;
