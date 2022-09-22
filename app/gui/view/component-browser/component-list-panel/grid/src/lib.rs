#![recursion_limit = "1024"]
// === Features ===
#![feature(array_methods)]
#![feature(option_result_contains)]
#![feature(derive_default_enum)]
#![feature(trait_alias)]
#![feature(hash_drain_filter)]
#![feature(bool_to_option)]
#![feature(int_roundings)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use crate::prelude::*;

use crate::entry::icon;
use crate::layout::Layout;
use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::shortcut::Shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_derive_theme::FromTheme;
use ensogl_grid_view as grid_view;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid as theme;
use ensogl_text as text;

pub mod content;
pub mod entry;
pub mod layout;
pub mod layouting;
pub use content::ElementId;
pub use content::GroupEntryId;
pub use content::GroupId;
pub use content::SectionId;

/// A module containing common imports.
pub mod prelude {
    pub use enso_frp as frp;
    pub use ensogl_core::application::traits::*;
    pub use ensogl_core::display::shape::*;
    pub use ensogl_core::prelude::*;
    pub use ensogl_grid_view as grid_view;
    pub use ensogl_text as text;
}

pub const COLUMN_COUNT: usize = 3;

#[derive(Clone, Debug, Default)]
pub struct HeaderModel {
    pub caption: ImString,
}

#[derive(Clone, Debug, Default)]
pub struct EntryModel {
    pub caption:     ImString,
    pub highlighted: Rc<Vec<text::Range<text::Bytes>>>,
    pub icon:        Option<icon::Id>,
}

ensogl_core::define_endpoints_2! {
    Input {
        reset(content::Info),
        model_for_header(GroupId, HeaderModel),
        model_for_entry(GroupEntryId, EntryModel),
        set_active_element()
    }
    Output {
        active(ElementId),
        model_for_header_needed(GroupId),
        model_for_entry_needed(EntryModel),
        suggestion_accepted(GroupEntryId),
        expression_accepted(GroupEntryId),
        module_entered(ElementId),
    }
}


// ============
// === Grid ===
// ============

pub type Grid = grid_view::scrollable::SelectableGridViewWithHeaders<entry::View, entry::View>;


#[derive(Clone, Copy, Default, Debug, PartialEq, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width:      f32,
    pub height:     f32,
    pub padding:    f32,
    pub column_gap: f32,
}

impl Style {
    pub fn content_size(&self) -> Vector2 {
        let size = Vector2(self.width, self.height);
        let padding = Vector2(self.padding, self.padding) * 2.0;
        size - padding
    }

    pub fn column_width(&self) -> f32 {
        let column_gaps = self.column_gap * ((COLUMN_COUNT - 1) as f32);
        (self.content_size().x - column_gaps) / (COLUMN_COUNT as f32)
    }
}

#[derive(Clone, Debug)]
pub struct Model {
    grid:   Grid,
    layout: RefCell<Layout>,
    colors: RefCell<HashMap<GroupId, color::Rgba>>,
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        self.grid.display_object()
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
        frp::extend! { network


        }
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        default()
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentListPanelGrid"
    }

    fn new(app: &Application, _logger: &DefaultWarningLogger) -> Self {
        let grid = Grid::new(app);
        let layout = default();
        let colors = default();

        Self { grid, layout, colors }
    }
}

pub type View = component::ComponentView<Model, Frp>;

// /// Scroll to the bottom of the [`section`].
//     fn scroll_to(&self, section: Section, style: &Style) {
//         let sub_modules_height = self.sub_modules_section.height(style);
//         let favourites_section_height = self.favourites_section.height(style);
//         let local_scope_height = self.local_scope_section.height(style);
//         use crate::navigator::Section::*;
//         let section_bottom_y = match section {
//             SubModules => sub_modules_height,
//             LocalScope => sub_modules_height + local_scope_height,
//             Favorites => sub_modules_height + local_scope_height + favourites_section_height,
//         };
//         let target_y = section_bottom_y - style.size_inner().y;
//         self.scroll_area.scroll_to_y(target_y);
//     }
//
//     /// Returns the bottom-most visible section inside the scroll area.
//     fn bottom_most_visible_section(&self) -> Option<Section> {
//         // We built a viewport that is similar to `scroll_area.viewport` but which uses
//         // `scroll_position_target_y` instead of `scroll_position_y`. We use it to avoid akward
//         // jumps of the selection box animation when clicking on section navigator buttons.
//         let scroll_y = -self.scroll_area.scroll_position_target_y.value();
//         let viewport = Viewport {
//             top:    scroll_y,
//             bottom: scroll_y - self.scroll_area.scroll_area_height.value(),
//             // We don't care about the left and right edges because the sections are positioned
//             // vertically.
//             left:   0.0,
//             right:  0.0,
//         };
//         use Section::*;
//         let sections: &[(&dyn WithinViewport, Section)] = &[
//             (&self.favourites_section, Favorites),
//             (&self.local_scope_section, LocalScope),
//             (&self.sub_modules_section, SubModules),
//         ];
//         let section = sections.iter().find(|(s, _)| s.within_viewport(&viewport));
//         section.map(|(_, name)| *name)
//     }
