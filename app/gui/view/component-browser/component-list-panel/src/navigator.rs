//! The [section navigator bar](Navigator). This is a narrow bar on the left of the Searcher List
//! Panel that contains two sets of navigation buttons.
//!
//! See the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::AllStyles;

use enso_frp as frp;
use ensogl_core::animation::animation::delayed::DelayedAnimation;
use ensogl_core::application::tooltip;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_derive_theme::FromTheme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as list_panel_theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::AnyModelProvider;
use ensogl_shadow as shadow;
use ensogl_tooltip::Tooltip;
use ide_view_component_list_panel_grid::entry::icon;
use ide_view_component_list_panel_grid::SectionId;
use list_panel_theme::navigator as theme;



// =================
// === Constants ===
// =================

const MARKETPLACE_TOOLTIP_TEXT: &str = "Marketplace will be available soon.";
const MARKETPLACE_TOOLTIP_HIDE_DELAY_MS: f32 = 3000.0;
const MARKETPLACE_TOOLTIP_PLACEMENT: tooltip::Placement = tooltip::Placement::Bottom;
const TOP_BUTTONS: [icon::Id; 2] = [icon::Id::Libraries, icon::Id::Marketplace];
const MARKETPLACE_BUTTON_INDEX: usize = 1;
const BOTTOM_BUTTONS: [icon::Id; 3] = [icon::Id::SubModules, icon::Id::Star, icon::Id::LocalScope];



// ==============
// === Shadow ===
// ==============

/// A shadow between the navigator bar and the main part of the Searcher List Panel.
///
/// We should have this shape embedded into the background shape, but we use a separate object
/// because of https://www.pivotaltracker.com/story/show/182593513.
pub mod navigator_shadow {
    use super::*;

    ensogl_core::define_shape_system! {
        above = [crate::background];
        below = [list_view::overlay, list_view::selection];
        pointer_events = false;
        (style:Style) {
            let grid_height = style.get_number(list_panel_theme::grid::height);
            let menu_height = style.get_number(list_panel_theme::menu_height);
            let navigator_width = style.get_number(theme::width);
            let height = grid_height + menu_height;
            let width = navigator_width;
            let base_shape = Rect((width.px(), height.px() * 2.0)).translate_x(width.px());
            shadow::from_shape(base_shape.into(), style)
        }
    }
}



// =============
// === Style ===
// =============

#[derive(Copy, Clone, Debug, Default, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width:             f32,
    pub list_view_width:   f32,
    pub icon_strong_color: color::Rgba,
    pub icon_weak_color:   color::Rgba,
    pub top_padding:       f32,
    pub bottom_padding:    f32,
}



// =========================================================
// === Conversions Between SectionId and List View Index ===
// =========================================================

/// Convert [`SectionId`] to index on [`Navigator::bottom_buttons`].
fn section_id_to_list_index(id: SectionId) -> usize {
    match id {
        SectionId::Popular => 1,
        SectionId::LocalScope => 2,
        SectionId::SubModules => 0,
    }
}

/// Convert the index on [`Navigator::bottom_buttons`] to [`SectionId`]. Prints error on invalid
/// index and returns the id of topmost section.
fn index_to_section_id(&index: &usize) -> SectionId {
    let highest = SectionId::SubModules;
    match index {
        0 => highest,
        1 => SectionId::Popular,
        2 => SectionId::LocalScope,
        index => {
            error!("Tried to create SectionId from too high Navigator List index ({}).", index);
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
    bottom_buttons:     list_view::ListView<icon::Entry>,
    top_buttons:        list_view::ListView<icon::Entry>,
    tooltip:            Tooltip,
    pub select_section: frp::Any<Option<SectionId>>,
    pub chosen_section: frp::Stream<Option<SectionId>>,
}

impl Navigator {
    pub fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let top_buttons = app.new_view::<list_view::ListView<icon::Entry>>();
        let bottom_buttons = app.new_view::<list_view::ListView<icon::Entry>>();
        let tooltip = Tooltip::new(app);
        top_buttons.set_style_prefix(list_panel_theme::navigator::list_view::HERE.str);
        bottom_buttons.set_style_prefix(list_panel_theme::navigator::list_view::HERE.str);
        top_buttons.show_background_shadow(false);
        bottom_buttons.show_background_shadow(false);
        bottom_buttons.disable_selecting_entries_with_mouse();
        display_object.add_child(&top_buttons);
        display_object.add_child(&bottom_buttons);
        app.display.default_scene.add_child(&tooltip);
        top_buttons.hide_selection();

        top_buttons.set_entries(AnyModelProvider::new(TOP_BUTTONS.to_vec()));
        bottom_buttons.set_entries(AnyModelProvider::new(BOTTOM_BUTTONS.to_vec()));
        bottom_buttons.select_entry(Some(section_id_to_list_index(SectionId::Popular)));

        let network = frp::Network::new("ComponentBrowser.Navigator");
        let tooltip_hide_timer = DelayedAnimation::new(&network);
        tooltip_hide_timer.set_delay(MARKETPLACE_TOOLTIP_HIDE_DELAY_MS);
        tooltip_hide_timer.set_duration(0.0);
        frp::extend! { network
            select_section <- any(...);
            bottom_buttons.select_entry <+
                select_section.map(|&s:&Option<SectionId>| s.map(section_id_to_list_index));
            chosen_section <-
                bottom_buttons.chosen_entry.map(|&id| id.as_ref().map(index_to_section_id));


            // === Show tooltip when hovering the Marketplace button

            let idx_of_marketplace_btn = |idx: &Option<_>| *idx == Some(MARKETPLACE_BUTTON_INDEX);
            marketplace_button_selected <- top_buttons.selected_entry.map(idx_of_marketplace_btn);
            marketplace_button_hovered <- marketplace_button_selected && top_buttons.is_mouse_over;
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
        tooltip_hide_timer.reset();

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

    pub(crate) fn set_bottom_buttons_entry_params(&self, params: icon::Params) {
        self.bottom_buttons.set_entry_params_and_recreate_entries(params);
    }

    pub(crate) fn update_layout(&self, style: &AllStyles) {
        let list_view_width = style.navigator.list_view_width;
        let top_buttons_height = list_view::entry::HEIGHT * TOP_BUTTONS.len() as f32;
        let bottom_buttons_height = list_view::entry::HEIGHT * BOTTOM_BUTTONS.len() as f32;
        self.top_buttons.resize(Vector2(list_view_width, top_buttons_height));
        self.bottom_buttons.resize(Vector2(list_view_width, bottom_buttons_height));

        let height = style.grid.height + style.panel.menu_height;
        let top = height / 2.0;
        let bottom = -height / 2.0;
        let top_buttons_height = TOP_BUTTONS.len() as f32 * list_view::entry::HEIGHT;
        let bottom_buttons_height = BOTTOM_BUTTONS.len() as f32 * list_view::entry::HEIGHT;
        let top_padding = style.navigator.top_padding;
        let bottom_padding = style.navigator.bottom_padding;
        let x_pos = -style.grid.width / 2.0;
        let top_buttons_y = top - top_buttons_height / 2.0 - top_padding;
        let bottom_buttons_y = bottom + bottom_buttons_height / 2.0 + bottom_padding;
        self.top_buttons.set_position_xy(Vector2(x_pos, top_buttons_y));
        self.bottom_buttons.set_position_xy(Vector2(x_pos, bottom_buttons_y));
    }
}

impl display::Object for Navigator {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
