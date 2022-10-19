//! The [section navigator bar](Navigator). This is a narrow bar on the left of the Searcher List
//! Panel that contains two sets of navigation buttons.
//!
//! See the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::AllStyles;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_derive_theme::FromTheme;
use ensogl_grid_view as grid;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as list_panel_theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::AnyModelProvider;
use ensogl_shadow as shadow;
use grid::Col;
use grid::Row;
use ide_view_component_list_panel_grid::entry::icon;
use ide_view_component_list_panel_grid::SectionId;
use list_panel_theme::navigator as theme;

type Grid = grid::selectable::GridView<icon::View>;



// =============
// === Style ===
// =============

#[derive(Copy, Clone, Debug, Default, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width:                    f32,
    pub button_size:              f32,
    pub icon_strong_color:        color::Rgba,
    pub icon_weak_color:          color::Rgba,
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

impl From<Style> for icon::Params {
    fn from(style: Style) -> Self {
        Self {
            strong_color:             style.icon_strong_color,
            weak_color:               style.icon_weak_color,
            hover_color:              style.hover_color.into(),
            selection_color:          style.highlight_color.into(),
            selection_size:           style.highlight_size,
            selection_corners_radius: style.highlight_corners_radius,
        }
    }
}



// =========================================================
// === Conversions Between SectionId and List View Index ===
// =========================================================

/// Convert [`SectionId`] to index on [`Navigator::bottom_buttons`].
fn section_id_to_list_index(id: SectionId) -> (Row, Col) {
    match id {
        SectionId::Popular => (1, 0),
        SectionId::LocalScope => (2, 0),
        SectionId::SubModules => (0, 0),
    }
}

/// Convert the index on [`Navigator::bottom_buttons`] to [`SectionId`]. Prints error on invalid
/// index and returns the id of topmost section.
fn index_to_section_id(&(row, col): &(Row, Col)) -> SectionId {
    let highest = SectionId::SubModules;
    match row {
        0 => highest,
        1 => SectionId::Popular,
        2 => SectionId::LocalScope,
        index => {
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
    pub select_section: frp::Any<Option<SectionId>>,
    pub chosen_section: frp::Stream<Option<SectionId>>,
}

const TOP_BUTTONS: [icon::Id; 2] = [icon::Id::Libraries, icon::Id::Marketplace];
const BOTTOM_BUTTONS: [icon::Id; 3] = [icon::Id::SubModules, icon::Id::Star, icon::Id::LocalScope];

impl Navigator {
    pub fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let top_buttons = Grid::new(app);
        let bottom_buttons = Grid::new(app);
        display_object.add_child(&top_buttons);
        display_object.add_child(&bottom_buttons);
        // Top buttons are disabled until https://www.pivotaltracker.com/story/show/182613789.
        //top_buttons.hide_selection();

        //top_buttons.set_entries(AnyModelProvider::new(TOP_BUTTONS.to_vec()));
        //bottom_buttons.set_entries(AnyModelProvider::new(BOTTOM_BUTTONS.to_vec()));
        //bottom_buttons.select_entry(Some(section_id_to_list_index(SectionId::Popular)));

        let network = frp::Network::new("ComponentBrowser.Navigator");
        frp::extend! { network
            select_section <- any(...);
            bottom_buttons.select_entry <+
                select_section.map(|&s:&Option<SectionId>| s.map(section_id_to_list_index));
            chosen_section <-
                bottom_buttons.entry_selected.map(|loc| loc.as_ref().map(index_to_section_id));

            model <- bottom_buttons.model_for_entry_needed.map(f!([]
                ((row, col)) {
                    DEBUG!("Model for entry: {row}, {BOTTOM_BUTTONS.get(*row):?}");
                    BOTTOM_BUTTONS.get(*row).map(|model| (*row, *col, *model))
                }
            )).filter_map(|m| *m);
            bottom_buttons.model_for_entry <+ model;

            model <- top_buttons.model_for_entry_needed.map(f!([]
                ((row, col)) {
                    TOP_BUTTONS.get(*row).map(|model| (*row, *col, *model))
                }
            )).filter_map(|m| *m);
            top_buttons.model_for_entry <+ model;
        }

        bottom_buttons.reset_entries(BOTTOM_BUTTONS.len(), 1);
        top_buttons.reset_entries(TOP_BUTTONS.len(), 1);

        Self {
            display_object,
            top_buttons,
            bottom_buttons,
            network,
            select_section,
            chosen_section,
        }
    }

    pub(crate) fn update_layout(&self, style: &AllStyles) {
        let size = style.navigator.button_size;
        let top_buttons_height = size * TOP_BUTTONS.len() as f32;
        let bottom_buttons_height = size * BOTTOM_BUTTONS.len() as f32;
        self.bottom_buttons.set_entries_size(Vector2(size, size));
        self.top_buttons.set_entries_size(Vector2(size, size));
        let top_buttons_viewport =
            grid::Viewport { top: 0.0, bottom: -top_buttons_height, left: 0.0, right: size };
        self.top_buttons.set_viewport(top_buttons_viewport);
        let bottom_buttons_viewport = grid::Viewport {
            top:    0.0,
            bottom: -bottom_buttons_height,
            left:   0.0,
            right:  size,
        };
        self.bottom_buttons.set_viewport(bottom_buttons_viewport);
        let buttons_params = icon::Params::from(style.navigator.clone());
        self.bottom_buttons.set_entries_params(buttons_params.clone());
        self.top_buttons.set_entries_params(buttons_params);

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
