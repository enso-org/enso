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
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as panel_theme;
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
use ensogl_grid_view::Col;
use ensogl_grid_view::Row;

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
    pub icon:        icon::Id,
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
        model_for_entry_needed(GroupEntryId),
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
    pub width:        f32,
    pub height:       f32,
    pub padding:      f32,
    pub column_gap:   f32,
    pub entry_height: f32,
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

    pub fn middle_column_width(&self) -> f32 {
        self.column_width() + self.column_gap * 2.0
    }

    pub fn entry_size(&self) -> Vector2 {
        Vector2(self.column_width(), self.entry_height)
    }
}

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    grid:   Grid,
    layout: Rc<RefCell<Layout>>,
    colors: Rc<RefCell<HashMap<GroupId, color::Rgba>>>,
}

impl Model {
    fn reset(&self, content: &content::Info) -> (Row, Col) {
        let layouter = layouting::Layouter::new(content.groups.iter().copied());
        let layout = layouter.create_layout(content.local_scope_size);
        let rows_and_cols = (layout.row_count(), layout.column_count());
        *self.layout.borrow_mut() = layout;
        let colors =
            content.groups.iter().filter_map(|g| Some((g.id, color::Rgba::from(g.color?))));
        *self.colors.borrow_mut() = colors.collect();
        rows_and_cols
    }

    fn location_to_headers_group_id(&self, &(row, col): &(Row, Col)) -> Option<GroupId> {
        let element = self.layout.borrow().element_at_location(row, col)?;
        element.header_group()
    }

    fn location_to_entry_id(&self, &(row, col): &(Row, Col)) -> Option<GroupEntryId> {
        let element = self.layout.borrow().element_at_location(row, col)?;
        element.as_entry_id()
    }

    fn group_id_to_header_location(&self, group: GroupId) -> Option<(Row, Col)> {
        let element = content::ElementInGroup::Header;
        self.layout.borrow().location_of_element(ElementId { group, element })
    }

    fn entry_id_to_location(
        &self,
        GroupEntryId { group, entry }: GroupEntryId,
    ) -> Option<(Row, Col)> {
        let element = content::ElementInGroup::Entry(entry);
        self.layout.borrow().location_of_element(ElementId { group, element })
    }

    fn model_for_header(
        &self,
        (group, model): &(GroupId, HeaderModel),
    ) -> Option<(Row, Col, entry::Model)> {
        let (row, col) = self.group_id_to_header_location(*group)?;
        let entry_model = entry::Model {
            kind:        entry::Kind::Header,
            color:       self.colors.borrow().get(&group).copied().unwrap_or_default(),
            caption:     model.caption.clone_ref(),
            highlighted: default(),
            icon:        None,
            group_id:    *group,
        };
        Some((row, col, entry_model))
    }

    fn model_for_entry(
        &self,
        (entry, model): &(GroupEntryId, EntryModel),
    ) -> Option<(Row, Col, entry::Model)> {
        let (row, col) = self.entry_id_to_location(*entry)?;
        let kind = if entry.group.section == SectionId::LocalScope {
            entry::Kind::LocalScopeEntry
        } else {
            entry::Kind::Entry
        };
        let entry_model = entry::Model {
            kind,
            color: self.colors.borrow().get(&entry.group).copied().unwrap_or_default(),
            caption: model.caption.clone_ref(),
            highlighted: model.highlighted.clone_ref(),
            icon: Some(model.icon),
            group_id: entry.group,
        };
        Some((row, col, entry_model))
    }

    fn entries_params(
        &self,
        (style, entry_style, color_intensities): &(
            Style,
            entry::Style,
            entry::style::ColorIntensities,
        ),
    ) -> entry::Params {
        entry::Params {
            style:             entry_style.clone(),
            grid_style:        *style,
            color_intensities: *color_intensities,
            dimmed_groups:     entry::DimmedGroups::None,
        }
    }

    fn selection_entries_params(
        &self,
        (base_params, color_intensities): &(entry::Params, entry::style::SelectionColorIntensities),
    ) -> entry::Params {
        entry::Params { color_intensities: (*color_intensities).into(), ..base_params.clone() }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        self.grid.display_object()
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        frp_api: &<Self as API>::Private,
        _app: &Application,
        model: &Model,
        style: &StyleWatchFrp,
    ) {
        let network = &frp_api.network;
        let input = &frp_api.input;
        let out = &frp_api.output;
        let grid = &model.grid;
        let grid_scroll_frp = grid.scroll_frp();
        let grid_selection_frp = grid.selection_highlight_frp();
        let corners_radius = style.get_number(panel_theme::corners_radius);
        let style = Style::from_theme(network, style);
        frp::extend! { network
            grid.reset_entries <+ input.reset.map(f!((content) model.reset(content)));
            grid.model_for_entry <+ input.model_for_header.filter_map(f!((input) model.model_for_header(input)));
            grid.model_for_entry <+ input.model_for_entry.filter_map(f!((input) model.model_for_entry(input)));
            out.model_for_header_needed <+ grid.model_for_entry_needed.filter_map(f!((loc) model.location_to_headers_group_id(loc)));
            out.model_for_entry_needed <+ grid.model_for_entry_needed.filter_map(f!((loc) model.location_to_entry_id(loc)));

            entry_style <- source::<entry::Style>();
            color_intensities <- source::<entry::style::ColorIntensities>();
            selection_color_intensities <- source::<entry::style::SelectionColorIntensities>();
            entries_style <- all(style.update, entry_style, color_intensities);
            entries_params <- entries_style.map(f!((input) model.entries_params(input)));
            selection_entries_style <- all(entries_params, selection_color_intensities);
            selection_entries_params <- selection_entries_style.map(f!((input) model.selection_entries_params(input)));

            grid_scroll_frp.resize <+ style.update.map(|s| s.content_size());
            grid_scroll_frp.set_corner_radius_bottom_right <+ all(&corners_radius, &style.init)._0();
            grid.set_entries_size <+ style.update.map(|s| s.entry_size());
            grid.set_column_width <+ style.update.map(|s| (1, s.middle_column_width()));
            grid.set_entries_params <+ entries_params;
            grid_selection_frp.set_entries_params <+ selection_entries_params;
        }

        // Set the proper number of columns so we can set column widths.
        grid.resize_grid(0, COLUMN_COUNT);
        tracing::warn!("X");
        style.init.emit(());
        tracing::warn!("XX");
        //TODO[ao] fix FromTheme and use it to get those values (without using source nodes).
        entry_style.emit(entry::Style {
            padding:                  17.0,
            icon_size:                16.0,
            text_size:                text::Size(12.0),
            icon_text_padding:        8.0,
            font:                     "default".into(),
            selection_corners_radius: 12.0,
            highlight_bold:           0.04,
            header_shadow_size:       27.0,
        });
        tracing::warn!("XXX");
        color_intensities.emit(entry::style::ColorIntensities {
            text:            1.0,
            background:      0.2,
            hover_highlight: 0.4,
            dimmed:          0.5,
            icon_strong:     1.0,
            icon_weak:       0.5,
        });
        tracing::warn!("XXXX");
        selection_color_intensities.emit(entry::style::SelectionColorIntensities {
            text:        0.2,
            background:  1.0,
            icon_strong: 0.2,
            icon_weak:   0.5,
        });
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
