//! The [section navigator bar](Navigator). This is a narrow bar on the left of the Searcher List
//! Panel that contains two sets of navigation buttons.
//!
//! See the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;
use ensogl_tooltip::tooltip::Placement;

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

const MARKETPLACE_TOOLTIP: &str = "Marketplace will be available soon.";
const LOCAL_SCOPE_TOOLTIP: &str = "Search local scope";
const SHOW_SHORTCUTS_TOOLTIP: &str = "Show shortcuts";
const UNSTABLE_TOOLTIP: &str = "Search unstable/advanced components";
const DOC_PANEL_TOOLTIP: &str = "Show/hide documentation";
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
    local_scope: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    shortcuts: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    unstable: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    marketplace: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    doc_panel: ToggleButton<ensogl_toggle_button::any_cached::Shape>,
    pub set_namespace_section_count: frp::Any<usize>,
    pub style: frp::Any<AllStyles>,
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
        let local_scope_tooltip =
            tooltip::Style::set_label(LOCAL_SCOPE_TOOLTIP).with_placement(Placement::Top);
        let local_scope =
            ToggleButton::new_from_cached::<icon::local_scope::Shape>(app, local_scope_tooltip);
        let shortcuts_tooltip =
            tooltip::Style::set_label(SHOW_SHORTCUTS_TOOLTIP).with_placement(Placement::Top);
        let shortcuts =
            ToggleButton::new_from_cached::<icon::command_key::Shape>(app, shortcuts_tooltip);
        let unstable_tooltip =
            tooltip::Style::set_label(UNSTABLE_TOOLTIP).with_placement(Placement::Top);
        let unstable =
            ToggleButton::new_from_cached::<icon::unstable::Shape>(app, unstable_tooltip);
        let marketplace_tooltip =
            tooltip::Style::set_label(MARKETPLACE_TOOLTIP).with_placement(Placement::Top);
        let marketplace =
            ToggleButton::new_from_cached::<icon::marketplace::Shape>(app, marketplace_tooltip);
        let doc_panel_tooltip =
            tooltip::Style::set_label(DOC_PANEL_TOOLTIP).with_placement(Placement::Top);
        let doc_panel =
            ToggleButton::new_from_cached::<icon::right_side_panel::Shape>(app, doc_panel_tooltip);
        let size = Vector2(icon::SIZE, icon::SIZE);
        local_scope.set_size(size);
        shortcuts.set_size(size);
        unstable.set_size(size);
        marketplace.set_size(size);
        doc_panel.set_size(size);
        background.add_child(&local_scope);
        background.add_child(&shortcuts);
        background.add_child(&unstable);
        background.add_child(&marketplace);
        background.add_child(&doc_panel);
        doc_panel.set_margin_left(38.0);

        let network = frp::Network::new("ComponentBrowser.Navigator");
        let style_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
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
        }
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
