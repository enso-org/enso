//! The [section navigator bar](Navigator). This is a narrow bar on the left of the Searcher List
//! Panel that contains two sets of navigation buttons.
//!
//! See the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::AllStyles;

use crate::grid::entry::icon;
use enso_frp as frp;
use ensogl_core::animation::animation::delayed::DelayedAnimation;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_derive_theme::FromTheme;
use ensogl_grid_view as grid;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as list_panel_theme;
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
const BOTTOM_BUTTONS_COUNT: usize = 3;


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
            SectionId::SubModules => self.submodules,
        }
    }
}

/// Convert [`SectionId`] to the displayed icon id.
fn section_id_to_icon_id(section: SectionId) -> icon::Id {
    match section {
        SectionId::Popular => icon::Id::Star,
        SectionId::LocalScope => icon::Id::LocalScope,
        SectionId::SubModules => icon::Id::SubModules,
    }
}



// ============================================================
// === Conversions Between SectionId and Grid View Location ===
// ============================================================

/// Convert [`SectionId`] to location on [`Navigator::bottom_buttons`].
fn section_id_to_grid_loc(id: SectionId) -> (Row, Col) {
    const COLUMN: Col = 0;
    match id {
        SectionId::Popular => (1, COLUMN),
        SectionId::LocalScope => (2, COLUMN),
        SectionId::SubModules => (0, COLUMN),
    }
}

/// Convert the location on [`Navigator::bottom_buttons`] to [`SectionId`]. Prints error on invalid
/// index and returns the id of topmost section.
fn loc_to_section_id(&(row, _): &(Row, Col)) -> SectionId {
    let highest = SectionId::SubModules;
    match row {
        0 => highest,
        1 => SectionId::Popular,
        2 => SectionId::LocalScope,
        _ => {
            error!("Tried to create SectionId from too high Navigator List row ({}).", row);
            highest
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
pub struct Navigator {
    display_object:     display::object::Instance,
    network:            frp::Network,
    bottom_buttons:     Grid,
    top_buttons:        Grid,
    tooltip:            Tooltip,
    pub select_section: frp::Any<Option<SectionId>>,
    pub chosen_section: frp::Stream<Option<SectionId>>,
}

impl Navigator {
    pub fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let top_buttons = Grid::new(app);
        let bottom_buttons = Grid::new(app);
        display_object.add_child(&top_buttons);
        display_object.add_child(&bottom_buttons);
        let tooltip = Tooltip::new(app);
        app.display.default_scene.add_child(&tooltip);

        let network = frp::Network::new("ComponentBrowser.Navigator");
        let style_frp = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let colors = Colors::from_theme(&network, &style_frp);
        let tooltip_hide_timer = DelayedAnimation::new(&network);
        tooltip_hide_timer.set_delay(MARKETPLACE_TOOLTIP_HIDE_DELAY_MS);
        tooltip_hide_timer.set_duration(0.0);
        frp::extend! { network
            select_section <- any(...);
            user_selected_section <- select_section.map(|&s:&Option<SectionId>| s.map(section_id_to_grid_loc));
            bottom_buttons.select_entry <+ user_selected_section;
            selected_button_and_section <- all(&bottom_buttons.entry_selected, &user_selected_section);
            different_section_chosen <- selected_button_and_section.filter(|(e, u)| *e != *u);
            chosen_section <- different_section_chosen._1().map(|loc| loc.as_ref().map(loc_to_section_id));

            model <- bottom_buttons.model_for_entry_needed.map2(&colors.update, f!([]
                ((row, col), colors) {
                    let section_id = loc_to_section_id(&(*row, *col));
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
        bottom_buttons.reset_entries(BOTTOM_BUTTONS_COUNT, 1);
        top_buttons.reset_entries(TOP_BUTTONS_COUNT, 1);

        Self {
            display_object,
            top_buttons,
            bottom_buttons,
            tooltip,
            network,
            select_section,
            chosen_section,
        }
    }

    pub(crate) fn update_layout(&self, style: &AllStyles) {
        let size = style.navigator.button_size;
        let top_buttons_height = size * TOP_BUTTONS_COUNT as f32;
        let bottom_buttons_height = size * BOTTOM_BUTTONS_COUNT as f32;
        self.bottom_buttons.set_entries_size(Vector2(size, size));
        self.top_buttons.set_entries_size(Vector2(size, size));
        let (top, left, right) = (0.0, 0.0, size);
        let viewport = grid::Viewport { top, bottom: 0.0, left, right };
        let top_buttons_viewport = grid::Viewport { bottom: -top_buttons_height, ..viewport };
        let bottom_buttons_viewport = grid::Viewport { bottom: -bottom_buttons_height, ..viewport };
        self.top_buttons.set_viewport(top_buttons_viewport);
        self.bottom_buttons.set_viewport(bottom_buttons_viewport);
        let buttons_params = entry::Params::from(style.navigator);
        self.bottom_buttons.set_entries_params(buttons_params);
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
        let bottom = -height / 2.0;
        let left = -style.grid.width / 2.0 - width / 2.0;
        let top_padding = style.navigator.top_padding;
        let bottom_padding = style.navigator.bottom_padding;
        let x_pos = left + width / 2.0 - size / 2.0;
        let top_buttons_y = top - top_padding;
        let bottom_buttons_y = bottom + bottom_buttons_height + bottom_padding;
        self.top_buttons.set_position_xy(Vector2(x_pos, top_buttons_y));
        self.bottom_buttons.set_position_xy(Vector2(x_pos, bottom_buttons_y));
    }
}

impl display::Object for Navigator {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
