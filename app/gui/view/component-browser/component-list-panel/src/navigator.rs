//! The [section navigator bar](Navigator). This is a narrow bar on the left of the Searcher List
//! Panel that contains two sets of navigation buttons.
//!
//! See the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::grid::entry::icon;
use crate::AllStyles;

use enso_frp as frp;
use ensogl_core::animation::animation::delayed::DelayedAnimation;
use ensogl_core::application;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::compound::rectangle::Rectangle;
use ensogl_derive_theme::FromTheme;
use ensogl_grid_view as grid;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as list_panel_theme;
use ensogl_toggle_button::ColorableShape;
use ensogl_toggle_button::ToggleButton;
use ensogl_tooltip::Tooltip;
use grid::Col;
use grid::Row;
use ide_view_component_list_panel_grid::SectionId;
use list_panel_theme::navigator as theme;



mod entry;

type Grid = grid::selectable::GridView<entry::View>;



// =================
// === Constants ===
// =================

const MARKETPLACE_BUTTON_INDEX: usize = 1;
const MARKETPLACE_TOOLTIP_TEXT: &str = "Marketplace will be available soon.";
const MARKETPLACE_TOOLTIP_HIDE_DELAY_MS: f32 = 3000.0;
const MARKETPLACE_TOOLTIP_PLACEMENT: tooltip::Placement = tooltip::Placement::Bottom;
const TOP_BUTTONS: [icon::Id; 2] = [icon::Id::Libraries, icon::Id::Marketplace];
const TOP_BUTTONS_COUNT: usize = TOP_BUTTONS.len();
/// This is the minmum number of bottom buttons available, when no namespace sections are present.
const MIN_BOTTOM_BUTTONS_COUNT: usize = 2;


// =============
// === Style ===
// =============

#[derive(Copy, Clone, Debug, Default, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width:                    f32,
    pub button_size:              f32,
    pub top_padding:              f32,
    pub bottom_padding:           f32,
    pub hover_color:              color::Rgba,
    #[theme_path = "theme::highlight::color"]
    pub highlight_color:          color::Rgba,
    #[theme_path = "theme::highlight::size"]
    pub highlight_size:           f32,
    #[theme_path = "theme::highlight::corners_radius"]
    pub highlight_corners_radius: f32,
}

impl From<Style> for entry::Params {
    fn from(style: Style) -> Self {
        Self {
            hover_color:              style.hover_color.into(),
            selection_color:          style.highlight_color.into(),
            selection_size:           style.highlight_size,
            selection_corners_radius: style.highlight_corners_radius,
        }
    }
}

/// Colors of the buttons of the section navigator.
/// Each of the section buttons can have a different "active" color, but they all share the same
/// "inactive" color. "active" color is used when the button is highlighted, and the "inactive" is
/// used as default.
#[derive(Debug, Clone, Copy, Default, FromTheme)]
#[base_path = "theme::buttons"]
#[allow(missing_docs)]
pub struct Colors {
    pub inactive:    color::Rgba,
    #[theme_path = "theme::buttons::active::popular"]
    pub popular:     color::Rgba,
    #[theme_path = "theme::buttons::active::local_scope"]
    pub local_scope: color::Rgba,
    #[theme_path = "theme::buttons::active::submodules"]
    pub submodules:  color::Rgba,
}

impl Colors {
    fn get(&self, section: SectionId) -> color::Rgba {
        match section {
            SectionId::Popular => self.popular,
            SectionId::LocalScope => self.local_scope,
            SectionId::Namespace(_) => self.submodules,
        }
    }
}

/// Convert [`SectionId`] to the displayed icon id.
fn section_id_to_icon_id(section: SectionId) -> icon::Id {
    match section {
        SectionId::Popular => icon::Id::Star,
        SectionId::LocalScope => icon::Id::LocalScope,
        SectionId::Namespace(_) => icon::Id::SubModules,
    }
}



// ============================================================
// === Conversions Between SectionId and Grid View Location ===
// ============================================================

/// Convert [`SectionId`] to location on [`Navigator::bottom_buttons`].
fn section_id_to_grid_loc(id: SectionId, sections_count: usize) -> (Row, Col) {
    const COLUMN: Col = 0;
    let namespace_section_offset = sections_count - MIN_BOTTOM_BUTTONS_COUNT;
    match id {
        SectionId::Popular => (namespace_section_offset, COLUMN),
        SectionId::LocalScope => (namespace_section_offset + 1, COLUMN),
        SectionId::Namespace(n) if n < namespace_section_offset =>
            (namespace_section_offset - n - 1, COLUMN),
        SectionId::Namespace(_) => (namespace_section_offset, COLUMN),
    }
}

/// Convert the location on [`Navigator::bottom_buttons`] to [`SectionId`]. Prints error on invalid
/// index and returns the id of topmost section.
fn loc_to_section_id(&(row, _): &(Row, Col), sections_count: usize) -> SectionId {
    let namespace_section_offset = sections_count - MIN_BOTTOM_BUTTONS_COUNT;
    match row {
        n if n == namespace_section_offset => SectionId::Popular,
        n if n == namespace_section_offset + 1 => SectionId::LocalScope,
        n if n < namespace_section_offset => SectionId::Namespace(namespace_section_offset - n - 1),
        _ => {
            error!("Tried to create SectionId from too high Navigator List row ({}).", row);
            SectionId::Popular
        }
    }
}



// =============================
// === Bottom Buttons Layout ===
// =============================

fn get_bottom_buttons_entries_size(style: &AllStyles) -> Vector2<f32> {
    let size = style.navigator.button_size;
    Vector2(size, size)
}

fn get_bottom_buttons_entries_params(style: &AllStyles) -> entry::Params {
    entry::Params::from(style.navigator)
}

fn get_bottom_buttons_viewport((buttons_count, style): &(usize, AllStyles)) -> grid::Viewport {
    let size = style.navigator.button_size;
    let bottom = -size * (*buttons_count as f32);
    grid::Viewport { top: 0.0, bottom, left: 0.0, right: size }
}

fn get_bottom_buttons_pos((buttons_count, style): &(usize, AllStyles)) -> Vector2<f32> {
    let size = style.navigator.button_size;
    let width = style.navigator.width;
    let height = style.grid.height + style.panel.menu_height;
    let padding = style.navigator.bottom_padding;
    let buttons_height = size * (*buttons_count as f32);
    let bottom = (-height / 2.0).floor();
    let left = -style.grid.width / 2.0 - width / 2.0;
    let x_pos = left + (width / 2.0).floor() - size / 2.0;
    let y_pos = bottom + buttons_height + padding;
    Vector2(x_pos, y_pos)
}


ensogl_core::define_endpoints_2! {
    Input {
        set_local_scope_mode(bool),
        set_show_shortcuts(bool),
        set_search_unstable(bool),
        set_side_panel(bool),
    }
    Output {
        local_scope_mode(bool),
        show_shortcuts(bool),
        search_unstable(bool),
        side_panel(bool),
    }
}

const SIZE: f32 = 16.0;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid::entry::icon::dull_color_alpha;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid::entry::special_icons;
use ide_view_component_list_panel_icons::common_part::*;
use std::f32::consts::PI;

/// Local scope section button. A dot inside a circle.
pub mod local_scope {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
            let dull_color = vivid_color.clone().multiply_alpha(&dull_alpha);
            let dot = Circle(&unit * 3.0);
            let dot = dot.fill(&vivid_color);
            let outer = Circle(&unit * 8.0) - Circle(&unit * 5.0);
            let outer = outer.fill(dull_color);
            let shape = outer + dot;
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }

    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

pub mod command_key {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            let center = Rect((6.0.px(), 6.0.px()));
            let center = center - Rect((2.0.px(), 2.0.px()));
            let circle_tl = Circle(3.0.px());
            let hole = Circle(1.0.px()) + Rect((1.0.px(), 1.0.px())).translate((0.5.px(), (-0.5).px()));
            let circle_tl = circle_tl - hole;
            let circle_tr = circle_tl.rotate((PI/2.0).radians());
            let circle_bl = circle_tl.flip_y();
            let circle_br = circle_tr.flip_y();
            let circle_tl = circle_tl.translate(((-4.0).px(), 4.0.px()));
            let circle_tr = circle_tr.translate((4.0.px(), 4.0.px()));
            let circle_bl = circle_bl.translate(((-4.0).px(), (-4.0).px()));
            let circle_br = circle_br.translate((4.0.px(), (-4.0).px()));

            let shape = center + circle_tl + circle_tr + circle_bl + circle_br;
            let shape = shape.fill(vivid_color);
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }
    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}
pub mod unstable {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let canvas_width = Var::<Pixels>::from("input_size.x");
            let canvas_height = Var::<Pixels>::from("input_size.y");
            let unit = &canvas_width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            let x_radius = 8.0;
            let y_radius = x_radius * 0.465;
            let bottom = Ellipse(x_radius.px(), y_radius.px());
            let bottom_pos_y = -SIZE / 2.0 + y_radius;
            let bottom = bottom.translate_y(bottom_pos_y.px());
            let bottom_center = Ellipse(5.0.px(), 2.0.px()).translate_y((-3.0).px());
            let height = 12.5;
            let y_offset = height / 2.0 - 3.0;
            let bottom_triangle = Triangle(10.0.px(), height.px()).translate_y(y_offset.px());
            let bottom_mask = bottom_center + bottom_triangle;
            let bottom = bottom - bottom_mask;

            let x_radius = 4.0;
            let y_radius = 1.5;
            let middle = Ellipse(x_radius.px(), y_radius.px());
            let middle_pos_y = -0.5;
            let middle = middle.translate_y(middle_pos_y.px());
            let height = 10.0;
            let y_offset = middle_pos_y + height / 2.0;
            let middle_triangle = Triangle(8.0.px(), height.px()).translate_y(y_offset.px());
            let middle = middle + middle_triangle;
            let mask_y = middle_pos_y + 2.5;
            let mask = HalfPlane().translate_y(mask_y.px());
            let mask_ellipse = Ellipse(3.0.px(), 1.0.px()).translate_y(mask_y.px());
            let mask = mask + mask_ellipse;
            let middle = middle - mask;

            let height = 5.5;
            let top_pos_y = middle_pos_y + 4.5;
            let y_offset = top_pos_y + height / 2.0;
            let top_triangle = Triangle(4.4.px(), height.px()).translate_y(y_offset.px());
            let triangle_mask = HalfPlane().translate_y((SIZE / 2.0 - 1.0).px());
            let top_triangle = top_triangle - triangle_mask;
            let top_ellipse = Ellipse(2.2.px(), 0.4.px()).translate_y(top_pos_y.px());
            let top = top_triangle + top_ellipse;

            let shape = bottom + middle + top;
            let shape = shape.fill(vivid_color);
            let hover_area = Rect((&canvas_width, &canvas_height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }
    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}
pub mod marketplace {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            use special_icons::marketplace as theme;
            let dull_alpha: Var<f32> = style.get_number(theme::dull_alpha).into();
            let secondary_alpha: Var<f32> = style.get_number(theme::secondary_alpha).into();
            let tertiary_alpha: Var<f32> = style.get_number(theme::tertiary_alpha).into();
            let dull_color = vivid_color.clone().multiply_alpha(&dull_alpha);
            let secondary_color = vivid_color.clone().multiply_alpha(&secondary_alpha);
            let tertiary_color = vivid_color.clone().multiply_alpha(&tertiary_alpha);
            let size = 7.0;
            let half = size / 2.0;
            let corners_radius = 1.0;
            let plus = plus(size,1.5);
            let plus = plus.fill(vivid_color);
            let plus = plus.translate(((-half - 0.5).px(),(half + 0.5).px()));

            let rect1 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
            let rect1 = rect1.fill(secondary_color);
            let rect1 = rect1.translate(((-half - 0.5).px(),(-half - 0.5).px()));

            let rect2 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
            let rect2 = rect2.fill(tertiary_color);
            let rect2 = rect2.translate(((half + 0.5).px(),(-half - 0.5).px()));

            let rect3 = Rect((size.px(),size.px())).corners_radius(corners_radius.px());
            let rect3 = rect3.fill(dull_color);
            let rect3 = rect3.translate(((half + 0.5).px(),(half + 0.5).px()));

            let shape = plus + rect1 + rect2 + rect3;
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }
    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}
pub mod right_side_panel {
    use super::*;

    ensogl_core::shape! {
        alignment = left_bottom;
        (style: Style, vivid_color: Vector4) {
            let width = Var::<Pixels>::from("input_size.x");
            let height = Var::<Pixels>::from("input_size.y");
            let unit = &width / SIZE;
            let vivid_color: Var<color::Rgba> = vivid_color.into();
            let dull_alpha: Var<f32> = style.get_number(dull_color_alpha).into();
            let dull_color = vivid_color.clone().multiply_alpha(&dull_alpha);
            let left = Rect((8.0.px(), 16.0.px())).corners_radiuses(3.0.px(), 0.0.px(), 3.0.px(), 0.0.px());
            let left = left.translate_x((-4.0).px());
            let left = left.fill(dull_color);

            let right = Rect((7.0.px(), 16.0.px())).corners_radiuses(0.0.px(), 3.0.px(), 0.0.px(), 3.0.px());
            let right = right.translate_x(4.5.px());
            let button = Rect((3.0.px(), 1.0.px())).translate((4.5.px(), (-0.5).px()));
            let button2 = button.translate_y(2.0.px());
            let button3 = button2.translate_y(2.0.px());
            let right = right - button - button2 - button3;
            let right = right.fill(vivid_color);

            let shape = left + right;
            let hover_area = Rect((&width, &height)).fill(INVISIBLE_HOVER_COLOR);
            (shape + hover_area).into()
        }
    }
    impl ColorableShape for Shape {
        fn set_color(&self, color: color::Rgba) {
            self.vivid_color.set(Vector4::new(color.red, color.green, color.blue, color.alpha));
        }
    }
}

// =================
// === Navigator ===
// =================

/// A section navigator bar. Contains two sets of buttons placed on the left of the Searcher List
/// Panel.
///
/// The first set on top of the bar contains "Libraries" and "Marketplace" buttons. The second
/// set on the bottom contains section navigation buttons used to quickly scroll to a specific
/// section.
#[derive(Debug, Clone, CloneRef)]
struct Navigator {
    background: Rectangle,
    network: frp::Network,
    bottom_buttons: Grid,
    top_buttons: Grid,
    tooltip: Tooltip,
    local_scope: ToggleButton<local_scope::Shape>,
    shortcuts: ToggleButton<command_key::Shape>,
    unstable: ToggleButton<unstable::Shape>,
    marketplace: ToggleButton<marketplace::Shape>,
    doc_panel: ToggleButton<right_side_panel::Shape>,
    pub set_namespace_section_count: frp::Any<usize>,
    pub style: frp::Any<AllStyles>,
    pub select_section: frp::Any<Option<SectionId>>,
    pub chosen_section: frp::Stream<Option<SectionId>>,
}

impl Navigator {
    pub fn new(app: &Application) -> Self {
        let background = Rectangle::new();
        background.use_auto_layout();
        background.set_size(Vector2(182.0, 32.0));
        background.set_corner_radius(16.0);
        background.set_color(color::Rgba::transparent());
        background.set_border_and_inset(0.5);
        background.set_border_color(color::Rgba(0.76, 0.76, 0.76, 1.0));
        background
            .set_gap((12.0, 0.0))
            .set_padding_all(8.0)
            .set_children_alignment_left_center()
            .justify_content_center_y();
        let top_buttons = Grid::new(app);
        let bottom_buttons = Grid::new(app);
        let tooltip = Tooltip::new(app);
        app.display.default_scene.add_child(&tooltip);
        let style_sheet = &app.display.default_scene.style_sheet;
        let local_scope = ToggleButton::new(app, default());
        local_scope.set_size(Vector2(16.0, 16.0));
        background.add_child(&local_scope);
        let shortcuts = ToggleButton::new(app, default());
        shortcuts.set_size(Vector2(16.0, 16.0));
        background.add_child(&shortcuts);
        let unstable = ToggleButton::new(app, default());
        unstable.set_size(Vector2(16.0, 16.0));
        background.add_child(&unstable);
        let marketplace = ToggleButton::new(app, default());
        marketplace.set_size(Vector2(16.0, 16.0));
        background.add_child(&marketplace);
        let doc_panel = ToggleButton::new(app, default());
        doc_panel.set_size(Vector2(16.0, 16.0));
        background.add_child(&doc_panel);
        doc_panel.set_margin_left(38.0);

        let network = frp::Network::new("ComponentBrowser.Navigator");
        let style_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let colors = Colors::from_theme(&network, &style_frp);
        let tooltip_hide_timer = DelayedAnimation::new(&network);
        tooltip_hide_timer.set_delay(MARKETPLACE_TOOLTIP_HIDE_DELAY_MS);
        tooltip_hide_timer.set_duration(0.0);
        frp::extend! { network
            style <- any(...);
            set_namespace_section_count <- any(...);
            section_count <- set_namespace_section_count.map(
                |&n: &usize| n + MIN_BOTTOM_BUTTONS_COUNT
            );
            section_count <- section_count.on_change();
            bottom_buttons_shape <- section_count.map(|n| (*n, 1));
            bottom_buttons_params <- all2(&section_count, &style);
            bottom_buttons_viewport <- bottom_buttons_params.map(get_bottom_buttons_viewport);
            bottom_buttons_pos <- bottom_buttons_params.map(get_bottom_buttons_pos);
            bottom_buttons.set_entries_params <+ style.map(get_bottom_buttons_entries_params);
            bottom_buttons.set_entries_size <+ style.map(get_bottom_buttons_entries_size);
            bottom_buttons.reset_entries <+ bottom_buttons_shape;
            bottom_buttons.set_viewport <+ bottom_buttons_viewport;
            eval bottom_buttons_pos ((pos) bottom_buttons.set_xy(*pos));

            select_section <- any(...);
            user_selected_section <- all2(&select_section, &section_count);
            user_selected_section <- user_selected_section.map(
                |(s, max): &(Option<SectionId>, _)| s.map(|s| section_id_to_grid_loc(s, *max))
            );
            bottom_buttons.select_entry <+ user_selected_section;
            selected_button_and_section <- all(&bottom_buttons.entry_selected, &user_selected_section);
            different_section_chosen <- selected_button_and_section.filter(|(e, u)| *e != *u)._0();
            chosen_section <- all2(&different_section_chosen, &section_count);
            chosen_section <- chosen_section.map(
                |(loc, max)| loc.as_ref().map(|loc| loc_to_section_id(loc, *max))
            );

            model <- bottom_buttons.model_for_entry_needed.map3(&colors.update, &section_count, f!([]
                ((row, col), colors, section_count) {
                    let section_id = loc_to_section_id(&(*row, *col), *section_count);
                    let icon_id = section_id_to_icon_id(section_id);
                    let active_colors = colors.get(section_id).into();
                    let inactive_colors = colors.inactive.into();
                    let model = entry::Model::new(icon_id, active_colors, inactive_colors);
                    (*row, *col, model)
                }
            ));
            bottom_buttons.model_for_entry <+ model;

            model <- top_buttons.model_for_entry_needed.map2(&colors.update, f!([]
                ((row, col), colors) {
                    let icon_id = TOP_BUTTONS.get(*row).cloned().unwrap_or_default();
                    let model = entry::Model::new(icon_id, colors.inactive.into(), colors.inactive.into());
                    (*row, *col, model)
                }
            ));
            top_buttons.model_for_entry <+ model;

            // === Show tooltip when hovering the Marketplace button

            let idx_of_marketplace_btn = |loc: &Option<(Row, Col)>| matches!(loc, Some((row, _)) if *row == MARKETPLACE_BUTTON_INDEX);
            marketplace_button_hovered <- top_buttons.entry_hovered.map(idx_of_marketplace_btn);
            marketplace_button_hovered <- marketplace_button_hovered.on_change();
            tooltip_hide_timer.start <+ marketplace_button_hovered.on_true();
            tooltip_hide_timer.reset <+ marketplace_button_hovered.on_false();
            tooltip_not_hidden <- bool(&tooltip_hide_timer.on_end, &tooltip_hide_timer.on_reset);
            showing_tooltip <- marketplace_button_hovered && tooltip_not_hidden;
            tooltip.frp.set_style <+ showing_tooltip.map(|showing| if *showing {
                    let style = tooltip::Style::set_label(MARKETPLACE_TOOLTIP_TEXT.into());
                    style.with_placement(MARKETPLACE_TOOLTIP_PLACEMENT)
                } else {
                    tooltip::Style::unset_label()
                }
            );
        }
        colors.init.emit(());
        tooltip_hide_timer.reset();
        top_buttons.reset_entries(TOP_BUTTONS_COUNT, 1);
        set_namespace_section_count.emit(0);

        Self {
            background,
            top_buttons,
            bottom_buttons,
            tooltip,
            network,
            set_namespace_section_count,
            style,
            select_section,
            chosen_section,
            local_scope,
            shortcuts,
            unstable,
            marketplace,
            doc_panel,
        }
    }

    pub(crate) fn update_layout(&self, style: &AllStyles) {
        let size = style.navigator.button_size;
        let top_buttons_height = size * TOP_BUTTONS_COUNT as f32;
        self.top_buttons.set_entries_size(Vector2(size, size));
        let (top, left, right) = (0.0, 0.0, size);
        let viewport = grid::Viewport { top, bottom: -top_buttons_height, left, right };
        self.top_buttons.set_viewport(viewport);
        let disabled_params = entry::Params {
            hover_color:              color::Lcha::transparent(),
            selection_color:          color::Lcha::transparent(),
            selection_size:           0.0,
            selection_corners_radius: 0.0,
        };
        self.top_buttons.set_entries_params(disabled_params);

        let width = style.navigator.width;
        let height = style.grid.height + style.panel.menu_height;
        let top = height / 2.0;
        let left = -style.grid.width / 2.0 - width / 2.0;
        let top_padding = style.navigator.top_padding;
        let x_pos = left + (width / 2.0).floor() - size / 2.0;
        let top_buttons_y = top - top_padding;
        self.top_buttons.set_xy(Vector2(x_pos, top_buttons_y));
    }
}

#[derive(Debug, Clone, CloneRef, Deref)]
pub struct View {
    model: Navigator,
    #[deref]
    frp:   Frp,
}

impl View {
    pub fn new(app: &Application) -> Self {
        let model = Navigator::new(app);
        let frp = Frp::new();
        Self { model, frp }
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        self.model.background.display_object()
    }
}
