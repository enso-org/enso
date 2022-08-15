//! The [section navigator bar](Navigator). This is a narrow bar on the left of the Searcher List
//! Panel that contains two sets of navigation buttons.
//!
//! See the [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::component_group::icon;
use crate::Style;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::style;
use ensogl_hardcoded_theme::application::component_browser::searcher as searcher_theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::AnyModelProvider;
use ensogl_shadow as shadow;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;
use searcher_theme::list_panel as list_panel_theme;



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
        (style:Style) {
            let theme_path: style::Path = list_panel_theme::HERE.into();
            let content_height = style.get_number(theme_path.sub("content_height"));
            let menu_height = style.get_number(theme_path.sub("menu_height"));
            let navigator_width = style.get_number(theme_path.sub("navigator_width"));
            let height = content_height + menu_height;
            let width = navigator_width;
            let base_shape = Rect((width.px(), height.px() * 2.0)).translate_x(width.px());
            shadow::from_shape(base_shape.into(), style)
        }
    }
}



// =================
// === Navigator ===
// =================


// === Section enum ===

/// Three sections of the Searcher List Panel. See the
/// [Component Browser Design Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, IntoPrimitive, TryFromPrimitive)]
#[repr(usize)]
pub enum Section {
    SubModules = 0,
    LocalScope = 1,
    #[default]
    Favorites  = 2,
}


// === Navigator ===

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
    pub chosen_section: frp::Source<Option<Section>>,
}

const TOP_BUTTONS: [icon::Id; 2] = [icon::Id::Libraries, icon::Id::Marketplace];
const BOTTOM_BUTTONS: [icon::Id; 3] = [icon::Id::SubModules, icon::Id::LocalScope, icon::Id::Star];

impl Navigator {
    pub fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new(&app.logger);
        let top_buttons = app.new_view::<list_view::ListView<icon::Entry>>();
        let bottom_buttons = app.new_view::<list_view::ListView<icon::Entry>>();
        top_buttons.set_style_prefix(list_panel_theme::navigator_list_view::HERE.str);
        bottom_buttons.set_style_prefix(list_panel_theme::navigator_list_view::HERE.str);
        top_buttons.show_background_shadow(false);
        bottom_buttons.show_background_shadow(false);
        top_buttons.disable_selecting_entries_with_mouse();
        bottom_buttons.disable_selecting_entries_with_mouse();
        display_object.add_child(&top_buttons);
        display_object.add_child(&bottom_buttons);
        // Top buttons are disabled until https://www.pivotaltracker.com/story/show/182613789.
        top_buttons.hide_selection();

        top_buttons.set_entries(AnyModelProvider::new(TOP_BUTTONS.to_vec()));
        bottom_buttons.set_entries(AnyModelProvider::new(BOTTOM_BUTTONS.to_vec()));

        let network = frp::Network::new("ComponentBrowser.Navigator");
        frp::extend! { network
            chosen_section <- source();
            eval bottom_buttons.chosen_entry([chosen_section](id) match id {
                Some(id) => chosen_section.emit(Section::try_from(*id).ok()),
                None => {},
            });
        }
        bottom_buttons.select_entry(Some(2));

        Self { display_object, top_buttons, bottom_buttons, network, chosen_section }
    }

    pub(crate) fn select_section(&self, section: Section) {
        self.bottom_buttons.select_entry(Some(section.into()));
    }

    pub(crate) fn set_bottom_buttons_entry_params(&self, params: icon::Params) {
        self.bottom_buttons.set_entry_params_and_recreate_entries(params);
    }

    pub(crate) fn update_layout(&self, style: Style) {
        let list_view_width = style.navigator_list_view_width;
        let top_buttons_height = list_view::entry::HEIGHT * TOP_BUTTONS.len() as f32;
        let bottom_buttons_height = list_view::entry::HEIGHT * BOTTOM_BUTTONS.len() as f32;
        self.top_buttons.resize(Vector2(list_view_width, top_buttons_height));
        self.bottom_buttons.resize(Vector2(list_view_width, bottom_buttons_height));

        let height = style.content_height + style.menu_height;
        let top = height / 2.0;
        let bottom = -height / 2.0;
        let top_buttons_height = TOP_BUTTONS.len() as f32 * list_view::entry::HEIGHT;
        let bottom_buttons_height = BOTTOM_BUTTONS.len() as f32 * list_view::entry::HEIGHT;
        let top_padding = style.navigator_top_padding;
        let bottom_padding = style.navigator_bottom_padding;
        let x_pos = -style.content_width / 2.0;
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
