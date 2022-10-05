#![recursion_limit = "1024"]
// === Features ===
#![feature(array_methods)]
#![feature(drain_filter)]
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
use ensogl_core::display::scene::Layer;
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

pub mod column {
    use ensogl_grid_view::Col;

    pub const COUNT: usize = 3;
    pub const LEFT: Col = 0;
    pub const CENTER: Col = 1;
    pub const RIGHT: Col = 2;

    pub const SECTION_SELECTION_PRIORITY: [Col; COUNT] = [CENTER, LEFT, RIGHT];
}
pub const GROUP_COLOR_VARIANT_COUNT: usize = 6;


#[derive(Clone, Debug, Default)]
pub struct HeaderModel {
    pub caption:        ImString,
    pub can_be_entered: bool,
}

#[derive(Clone, Debug, Default)]
pub struct EntryModel {
    pub caption:        ImString,
    pub highlighted:    Rc<Vec<text::Range<text::Byte>>>,
    pub icon:           icon::Id,
    pub can_be_entered: bool,
}

ensogl_core::define_endpoints_2! {
    Input {
        reset(content::Info),
        model_for_header(GroupId, HeaderModel),
        model_for_entry(GroupEntryId, EntryModel),
        switch_section(SectionId),
        accept_suggestion(),
        jump_group_up(),
        jump_group_down(),
    }
    Output {
        active(ElementId),
        active_section(SectionId),
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

#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub struct GroupColors {
    variants:          [color::Rgba; GROUP_COLOR_VARIANT_COUNT],
    local_scope_group: color::Rgba,
}

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
        let column_gaps = self.column_gap * ((column::COUNT - 1) as f32);
        (self.content_size().x - column_gaps) / (column::COUNT as f32)
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
    grid:                   Grid,
    grid_layer:             Layer,
    selection_layer:        Layer,
    layout:                 Rc<RefCell<Layout>>,
    enterable_elements:     Rc<RefCell<HashSet<ElementId>>>,
    colors:                 Rc<RefCell<HashMap<GroupId, entry::MainColor>>>,
    requested_section_info: Rc<RefCell<[Row; column::COUNT]>>,
}

impl Model {
    fn reset(&self, content: &content::Info) -> (Row, Col) {
        let layouter = layouting::Layouter::new(content.groups.iter().copied());
        let layout = layouter.create_layout(content.local_scope_size);
        let rows_and_cols = (layout.row_count(), layout.column_count());
        *self.layout.borrow_mut() = layout;
        *self.colors.borrow_mut() = Self::collect_colors(content);
        self.enterable_elements.borrow_mut().clear();
        rows_and_cols
    }

    fn collect_colors(content: &content::Info) -> HashMap<GroupId, entry::MainColor> {
        let variants = (0..).map(|i| i % GROUP_COLOR_VARIANT_COUNT);
        content
            .groups
            .iter()
            .zip(variants)
            .map(|(group, variant)| {
                let color = match (group.color, group.id.section) {
                    (Some(color), _) => entry::MainColor::Custom(color.into()),
                    (None, SectionId::LocalScope) => entry::MainColor::LocalScope,
                    _ => entry::MainColor::Predefined { variant },
                };
                (group.id, color)
            })
            .collect()
    }

    fn can_be_entered(&self, element_id: ElementId) -> bool {
        self.enterable_elements.borrow().contains(&element_id)
    }

    fn location_to_element_id(&self, &(row, col): &(Row, Col)) -> Option<ElementId> {
        self.layout.borrow().element_at_location(row, col)
    }

    fn location_to_headers_group_id(&self, location: &(Row, Col)) -> Option<GroupId> {
        let element = self.location_to_element_id(location)?;
        element.header_group()
    }

    fn location_to_entry_id(&self, location: &(Row, Col)) -> Option<GroupEntryId> {
        let element = self.location_to_element_id(location)?;
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

    fn section_info_requested(&self, &(row, col): &(Row, Col)) -> Option<GroupId> {
        *self.requested_section_info.borrow_mut().get_mut(col)? = row;
        Some(self.layout.borrow().group_at_location(row, col)?.group.id)
    }

    fn is_requested_section(&self, (rows, col, _): &(Range<Row>, Col, entry::Model)) -> bool {
        self.requested_section_info.borrow().get(*col).map_or(false, |r| rows.contains(r))
    }

    fn make_section_info(
        &self,
        (group, model): &(GroupId, HeaderModel),
    ) -> Option<(Range<Row>, Col, entry::Model)> {
        if model.can_be_entered {
            self.enterable_elements.borrow_mut().insert(ElementId::from(*group));
        }
        let (rows, col) = self.layout.borrow().location_of_group(*group)?;
        let entry_model = entry::Model {
            kind:        entry::Kind::Header,
            color:       self.colors.borrow().get(&group).copied().unwrap_or_default(),
            caption:     model.caption.clone_ref(),
            highlighted: default(),
            icon:        None,
            group_id:    *group,
        };
        Some((rows, col, entry_model))
    }

    fn model_for_entry(
        &self,
        (entry, model): &(GroupEntryId, EntryModel),
    ) -> Option<(Row, Col, entry::Model)> {
        if model.can_be_entered {
            let element_id = ElementId::from(*entry);
            self.enterable_elements.borrow_mut().insert(element_id);
        }
        let (row, col) = self.entry_id_to_location(*entry)?;
        let kind = if entry.group.section == SectionId::LocalScope {
            let first_line = row == self.layout.borrow().local_scope_rows().start;
            entry::Kind::LocalScopeEntry { first_line }
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

    fn entry_to_select_when_switching_to_section(&self, section: SectionId) -> Option<(Row, Col)> {
        let layout = self.layout.borrow();
        let pick = |row, col| layout.element_at_location(row, col).map(|_| (row, col));
        if section == SectionId::LocalScope {
            let row = layout.local_scope_rows().start;
            column::SECTION_SELECTION_PRIORITY.iter().filter_map(|col| pick(row, *col)).next()
        } else {
            let column_priority = column::SECTION_SELECTION_PRIORITY.iter();
            let bottommost_per_column = column_priority.filter_map(|&col| {
                let section_rows = layout.section_rows_at_column(section, col)?;
                let last_row = section_rows.last()?;
                pick(last_row, col)
            });
            bottommost_per_column.rev().max_by_key(|(row, _)| *row)
        }
    }

    fn entry_to_select_after_reset(&self) -> Option<(Row, Col)> {
        let sections = [SectionId::Popular, SectionId::SubModules, SectionId::LocalScope].iter();
        let pick_location = |s: &SectionId| self.entry_to_select_when_switching_to_section(*s);
        sections.filter_map(pick_location).next()
    }

    fn entries_params(
        &self,
        (style, entry_style, color_intensities, group_colors): &(
            Style,
            entry::Style,
            entry::style::ColorIntensities,
            GroupColors,
        ),
        dimmed_groups: entry::DimmedGroups,
    ) -> entry::Params {
        entry::Params {
            style: entry_style.clone(),
            grid_style: *style,
            group_colors: *group_colors,
            color_intensities: *color_intensities,
            dimmed_groups,
        }
    }

    fn selection_entries_params(
        &self,
        (base_params, color_intensities): &(entry::Params, entry::style::SelectionColorIntensities),
    ) -> entry::Params {
        entry::Params { color_intensities: (*color_intensities).into(), ..base_params.clone() }
    }

    fn navigation_scroll_margins(
        &self,
        active_section: SectionId,
        style: &Style,
    ) -> grid_view::Margins {
        let vertical_margin = style.content_size().y - style.entry_height;
        if active_section == SectionId::LocalScope {
            grid_view::Margins { bottom: vertical_margin + style.column_gap, ..default() }
        } else {
            grid_view::Margins { top: vertical_margin, ..default() }
        }
    }

    fn selection_after_jump_group_up(
        &self,
        &(current_row, col): &(Row, Col),
    ) -> Option<(Row, Col)> {
        let layout = self.layout.borrow();
        if let Some(group_above) = layout.group_above_location(current_row, col) {
            Some((group_above.rows().last()?, col))
        } else {
            let current_group = layout.group_at_location(current_row, col)?;
            Some((current_group.rows().next()?, col))
        }
    }

    fn selection_after_jump_group_down(
        &self,
        &(current_row, col): &(Row, Col),
    ) -> Option<(Row, Col)> {
        let layout = self.layout.borrow();
        if let Some(group_below) = layout.group_below_location(current_row, col) {
            Some((group_below.rows().last()?, col))
        } else {
            let current_group = layout.group_at_location(current_row, col)?;
            Some((current_group.rows().last()?, col))
        }
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        self.grid.display_object()
    }
}

/// The action after selecting the entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Action {
    EnterModule(ElementId),
    Accept(GroupEntryId),
}

impl Default for Action {
    fn default() -> Self {
        Self::EnterModule(default())
    }
}

impl component::Frp<Model> for Frp {
    fn init(
        network: &frp::Network,
        frp_api: &<Self as API>::Private,
        _app: &Application,
        model: &Model,
        style_frp: &StyleWatchFrp,
    ) {
        let input = &frp_api.input;
        let out = &frp_api.output;
        let grid = &model.grid;
        let grid_scroll_frp = grid.scroll_frp();
        let grid_extra_scroll_frp = grid.extra_scroll_frp();
        let grid_selection_frp = grid.selection_highlight_frp();
        let grid_header_frp = grid.header_frp();
        let corners_radius = style_frp.get_number(panel_theme::corners_radius);
        let style = Style::from_theme(network, style_frp);
        let entry_style = entry::Style::from_theme(network, style_frp);
        let color_intensities = entry::style::ColorIntensities::from_theme(network, style_frp);
        let selection_color_intensities =
            entry::style::SelectionColorIntensities::from_theme(network, style_frp);
        frp::extend! { network
            // === Active Entry ===

            out.active <+ grid.entry_selected.filter_map(f!([model](loc) loc.as_ref().and_then(|l| model.location_to_element_id(l))));
            out.active_section <+ out.active.map(|e| e.group.section).on_change();


            // === Accepting Suggestion and Expression ===

            action <- grid.entry_accepted.map(f!([model](loc) {
                let element_id = model.location_to_element_id(loc)?;
                if model.can_be_entered(element_id) {
                    Some(Action::EnterModule(element_id))
                } else {
                    Some(Action::Accept(model.location_to_entry_id(loc)?))
                }
            })).filter_map(|a| *a);
            out.module_entered <+ action.filter_map(|m| if let Action::EnterModule(m) = m {Some(*m)} else {None});
            out.expression_accepted <+ action.filter_map(|e| if let Action::Accept(e) = e {Some(*e)} else {None});
            out.suggestion_accepted <+ out.active.sample(&input.accept_suggestion).filter_map(|e| e.as_entry_id());


            // === Style and Entries Params ===

            group_colors <- source::<GroupColors>();
            dimmed_groups <- out.active_section.map(|section| entry::DimmedGroups::AllExceptSection(*section));
            entries_style <- all4(&style.update, &entry_style.update, &color_intensities.update, &group_colors);
            entries_params <- all_with(&entries_style, &dimmed_groups, f!((style, dimmed) model.entries_params(style, *dimmed)));
            selection_entries_style <- all(entries_params, selection_color_intensities.update);
            selection_entries_style <- all(entries_params, selection_color_intensities.update);
            selection_entries_params <- selection_entries_style.map(f!((input) model.selection_entries_params(input)));
            grid_scroll_frp.resize <+ style.update.map(|s| s.content_size());
            grid_scroll_frp.set_corner_radius_bottom_right <+ all(&corners_radius, &style.init)._0();
            grid.set_entries_size <+ style.update.map(|s| s.entry_size());
            grid.set_column_width <+ style.update.map(|s| (1, s.middle_column_width()));
            grid.set_entries_params <+ entries_params;
            grid_selection_frp.set_entries_params <+ selection_entries_params;



            // === Header and Entries Models ===

            grid.reset_entries <+ input.reset.map(f!((content) model.reset(content)));
            section_info <- input.model_for_header.filter_map(f!((input) model.make_section_info(input)));
            grid.model_for_entry <+ section_info.map(|(rows, col, model)| (rows.start, *col, model.clone()));
            grid.model_for_entry <+ input.model_for_entry.filter_map(f!((input) model.model_for_entry(input)));
            grid_header_frp.section_info <+ section_info.filter(f!((input) model.is_requested_section(input)));
            out.model_for_header_needed <+ grid_header_frp.section_info_needed.filter_map(f!((loc) model.section_info_requested(loc)));
            out.model_for_header_needed <+ grid.model_for_entry_needed.filter_map(f!((loc) model.location_to_headers_group_id(loc)));
            out.model_for_entry_needed <+ grid.model_for_entry_needed.filter_map(f!((loc) model.location_to_entry_id(loc)));


            // === Scrolling and Jumping to Section ===

            grid_extra_scroll_frp.select_and_scroll_to_entry <+ input.reset.filter_map(f_!(model.entry_to_select_after_reset()));
            grid_extra_scroll_frp.set_preferred_margins_around_entry <+ all_with(&out.active_section, &style.update, f!((section, style) model.navigation_scroll_margins(*section, style)));
            grid_extra_scroll_frp.select_and_scroll_to_entry <+ input.switch_section.filter_map(f!((section) model.entry_to_select_when_switching_to_section(*section)));
            // The content area is higher than just height of all entries, as we add also a gap
            // between all groups and local scope section.
            grid_scroll_frp.set_content_height <+ all_with(&grid.content_size, &style.update, |c, s| c.y + s.column_gap);


            // === Jumping by Groups ===

            grid_extra_scroll_frp.select_and_scroll_to_entry <+ grid.entry_selected.sample(&input.jump_group_up).filter_map(f!((loc) model.selection_after_jump_group_up(loc.as_ref()?)));
            grid_extra_scroll_frp.select_and_scroll_to_entry <+ grid.entry_selected.sample(&input.jump_group_down).filter_map(f!((loc) model.selection_after_jump_group_down(loc.as_ref()?)));


            // === Focus propagation ===

            grid.focus <+ input.focus;
            grid.defocus <+ input.defocus;
            grid.set_focus <+ input.set_focus;
        }

        // Set the proper number of columns so we can set column widths.
        grid.resize_grid(0, column::COUNT);
        style.init.emit(());
        entry_style.init.emit(());
        color_intensities.init.emit(());
        selection_color_intensities.init.emit(());
        group_colors.emit(GroupColors {
            variants:          [
                color::Rgba(43.0 / 255.0, 117.0 / 255.0, 239.0 / 255.0, 1.0),
                color::Rgba(62.0 / 255.0, 139.0 / 255.0, 41.0 / 255.0, 1.0),
                color::Rgba(192.0 / 255.0, 71.0 / 255.0, 171.0 / 255.0, 1.0),
                color::Rgba(121.0 / 255.0, 126.0 / 255.0, 37.0 / 255.0, 1.0),
                color::Rgba(181.0 / 255.0, 97.0 / 255.0, 35.0 / 255.0, 1.0),
                color::Rgba(61.0 / 255.0, 146.0 / 255.0, 206.0 / 255.0, 1.0),
            ],
            local_scope_group: color::Rgba(0.0, 0.42, 0.64, 1.0),
        });
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        use ensogl_core::application::shortcut::ActionType::*;
        (&[
            (Press, "tab", "accept_suggestion"),
            (Press, "cmd up", "jump_group_up"),
            (Press, "cmd down", "jump_group_down"),
        ])
            .iter()
            .map(|(a, b, c)| View::self_shortcut(*a, *b, *c))
            .collect()
    }
}

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentListPanelGrid"
    }

    fn new(app: &Application) -> Self {
        let grid = Grid::new(app);
        let layout = default();
        let enterable_elements = default();
        let colors = default();
        let requested_section_info = default();
        let base_layer = &app.display.default_scene.layers.node_searcher_text;
        let grid_layer = base_layer.create_sublayer();
        let selection_layer = base_layer.create_sublayer();
        grid_layer.add_exclusive(&grid);
        grid.selection_highlight_frp().setup_masked_layer(selection_layer.downgrade());
        Self {
            grid,
            layout,
            enterable_elements,
            colors,
            grid_layer,
            selection_layer,
            requested_section_info,
        }
    }
}

pub type View = component::ComponentView<Model, Frp>;
