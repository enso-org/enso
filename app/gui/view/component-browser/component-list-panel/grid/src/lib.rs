//! A crate defining the grid inside the Component List Panel showing all components. For more
//! information see docs of [`View`].
//!
//! In the Component List Panel Grid we make distinction between entries (which are normal entries
//! inside groups) and elements (which are entries or group's headers). See also structures defined
//! in [`content`] module.
//!
//! To learn more about the Component Browser and its components, see the [Component Browser Design
//! Document](https://github.com/enso-org/design/blob/e6cffec2dd6d16688164f04a4ef0d9dff998c3e7/epics/component-browser/design.md).

#![recursion_limit = "1024"]
// === Features ===
#![feature(array_methods)]
#![feature(drain_filter)]
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(hash_drain_filter)]
#![feature(int_roundings)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
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
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_derive_theme::FromTheme;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::Col;
use ensogl_grid_view::Row;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as panel_theme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid as theme;
use ensogl_text as text;


// ==============
// === Export ===
// ==============

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



// =================
// === Constants ===
// =================

/// A set of constants related to grid columns.
pub mod column {
    use ensogl_grid_view::Col;

    /// The priority telling which column will be selected first when switching to section having
    /// many lowest elements (with maximum row index).
    pub const SECTION_SELECTION_PRIORITY: [Col; COUNT] = [CENTER, LEFT, RIGHT];

    // The constants below plays informative role, and should not be easily changed, as the
    // layouter algorithm rely strongly on assumption that there are 3 columns.

    /// Number of columns in Component List Panel Grid.
    pub const COUNT: usize = 3;
    /// The index of left column.
    pub const LEFT: Col = 0;
    /// The index of center column.
    pub const CENTER: Col = 1;
    /// The index of right column.
    pub const RIGHT: Col = 2;
}
/// The number of color variants taken from the style sheet, used to coloring group without color
/// specified by library author.
pub const GROUP_COLOR_VARIANT_COUNT: usize = 6;



// ===========
// === FRP ===
// ===========

// === Models ===

/// The model for group's headers.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct HeaderModel {
    pub caption:        ImString,
    pub can_be_entered: bool,
}

/// The model for entries.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct EntryModel {
    pub caption:        ImString,
    pub highlighted:    Rc<Vec<text::Range<text::Byte>>>,
    pub icon:           icon::Id,
    pub can_be_entered: bool,
}


// === API ===

ensogl_core::define_endpoints_2! {
    Input {
        reset(content::Info),
        model_for_header(GroupId, HeaderModel),
        model_for_entry(GroupEntryId, EntryModel),
        switch_section(SectionId),
        switch_section_no_animation(SectionId),
        accept_suggestion(),
        jump_group_up(),
        jump_group_down(),
        unhover_element(),
    }
    Output {
        active(Option<ElementId>),
        active_section(Option<SectionId>),
        hovered(Option<ElementId>),
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

/// A variant of the [component's](View) underlying [EnsoGL grid component](grid_view).
pub type Grid = grid_view::scrollable::SelectableGridViewWithHeaders<entry::View, entry::View>;



// =============
// === Style ===
// =============

// === GroupColors ===

/// Default groups colors.
#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub struct GroupColors {
    /// Variants to be used in groups in column layout.
    variants:          [color::Lcha; GROUP_COLOR_VARIANT_COUNT],
    /// The color of the group in Local Scope section.
    local_scope_group: color::Lcha,
}


// === Style ===

/// The grid's style structure.
#[allow(missing_docs)]
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
    /// Compute the size of grid without external paddings.
    pub fn content_size(&self) -> Vector2 {
        let size = Vector2(self.width, self.height);
        let padding = Vector2(self.padding, self.padding) * 2.0;
        size - padding
    }

    /// Compute the single column width.
    pub fn column_width(&self) -> f32 {
        let column_gaps = self.column_gap * ((column::COUNT - 1) as f32);
        (self.content_size().x - column_gaps) / (column::COUNT as f32)
    }

    /// Compute the middle column width to be set in underlying [Grid component](Grid). It includes
    /// the gaps between columns.
    pub fn middle_column_width(&self) -> f32 {
        self.column_width() + self.column_gap * 2.0
    }

    /// Get the single entry size.
    pub fn entry_size(&self) -> Vector2 {
        Vector2(self.column_width(), self.entry_height)
    }
}



// =============
// === Model ===
// =============

// === Model ===

/// A [Model](component::Model) of [Component List Panel Grid View](View).
#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    display_object:         display::object::Instance,
    grid:                   Grid,
    grid_layer:             Layer,
    selection_layer:        Layer,
    layout:                 Rc<RefCell<Layout>>,
    enterable_elements:     Rc<RefCell<HashSet<ElementId>>>,
    colors:                 Rc<RefCell<HashMap<GroupId, entry::MainColor>>>,
    requested_section_info: Rc<RefCell<[Row; column::COUNT]>>,
}


// === component::Model Trait; Constructor ===

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentListPanelGrid"
    }

    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let grid = Grid::new(app);
        let layout = default();
        let enterable_elements = default();
        let colors = default();
        let requested_section_info = default();
        let base_layer = &app.display.default_scene.layers.node_searcher_text;
        let grid_layer = base_layer.create_sublayer("grid_layer");
        let selection_layer = base_layer.create_sublayer("selection_layer");
        display_object.add_child(&grid);
        grid_layer.add(&grid);
        grid.selection_highlight_frp().setup_masked_layer(selection_layer.downgrade());
        Self {
            display_object,
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


// === Resetting Grid ===

impl Model {
    /// The grid is resetting: remove all data regarding existing entries and return new grid size.
    fn reset(&self, content: &content::Info) -> (Row, Col) {
        let layouter = layouting::Layouter::new(content.groups.iter().copied());
        let layout = layouter.create_layout(content.local_scope_entry_count);
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
                    _ => entry::MainColor::Predefined { variant_index: variant },
                };
                (group.id, color)
            })
            .collect()
    }
}


// === Action ===

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

impl Action {
    fn enter_module(self) -> Option<ElementId> {
        if let Self::EnterModule(element) = self {
            Some(element)
        } else {
            None
        }
    }

    fn accept(self) -> Option<GroupEntryId> {
        if let Self::Accept(entry) = self {
            Some(entry)
        } else {
            None
        }
    }
}

impl Model {
    fn can_be_entered(&self, element_id: ElementId) -> bool {
        self.enterable_elements.borrow().contains(&element_id)
    }

    fn action_after_accepting_entry(&self, loc: &(Row, Col)) -> Option<Action> {
        let element_id = self.location_to_element_id(loc)?;
        if self.can_be_entered(element_id) {
            Some(Action::EnterModule(element_id))
        } else {
            Some(Action::Accept(self.location_to_entry_id(loc)?))
        }
    }
}


// === Conversion From and To Grid's Location ===

impl Model {
    fn location_to_element_id(&self, &(row, col): &(Row, Col)) -> Option<ElementId> {
        self.layout.borrow().element_at_location(row, col)
    }

    fn location_to_headers_group_id(&self, location: &(Row, Col)) -> Option<GroupId> {
        let element = self.location_to_element_id(location)?;
        element.as_header()
    }

    fn location_to_entry_id(&self, location: &(Row, Col)) -> Option<GroupEntryId> {
        let element = self.location_to_element_id(location)?;
        element.as_entry_id()
    }

    fn entry_id_to_location(
        &self,
        GroupEntryId { group, entry }: GroupEntryId,
    ) -> Option<(Row, Col)> {
        let element = content::ElementInGroup::Entry(entry);
        self.layout.borrow().location_of_element(ElementId { group, element })
    }
}


// === Entries' Models ===

impl Model {
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
            color:       self.colors.borrow().get(group).copied().unwrap_or_default(),
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
}


// === Navigation ===

impl Model {
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


// === Updating Style ===

impl Model {
    fn grid_size((style, content_size): &(Style, Vector2)) -> Vector2 {
        let x = min(style.content_size().x, content_size.x);
        let y = min(style.content_size().y, content_size.y);
        Vector2(x, y)
    }

    fn grid_position(input: &(Style, Vector2)) -> Vector2 {
        let (style, _) = input;
        let y = -style.content_size().y + Self::grid_size(input).y;
        Vector2(0.0, y)
    }

    fn entries_params(
        &self,
        (style, entry_style, colors, group_colors): &(
            Style,
            entry::Style,
            entry::style::Colors,
            GroupColors,
        ),
        dimmed_groups: entry::DimmedGroups,
    ) -> entry::Params {
        entry::Params {
            style: entry_style.clone(),
            grid_style: *style,
            group_colors: *group_colors,
            colors: *colors,
            dimmed_groups,
        }
    }

    fn selection_entries_params(
        &self,
        (base_params, colors): &(entry::Params, entry::style::SelectionColors),
    ) -> entry::Params {
        entry::Params { colors: (*colors).into(), ..base_params.clone() }
    }

    fn navigation_scroll_margins(
        &self,
        active_section: Option<SectionId>,
        style: &Style,
    ) -> grid_view::Margins {
        let vertical_margin = style.content_size().y - style.entry_height;
        if active_section == Some(SectionId::LocalScope) {
            grid_view::Margins {
                bottom: vertical_margin + style.column_gap,
                top: -style.column_gap,
                ..default()
            }
        } else {
            grid_view::Margins { top: vertical_margin, ..default() }
        }
    }
}


// === display::Object Implementation ===

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =================
// === FRP Logic ===
// =================

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
        let colors = entry::style::Colors::from_theme(network, style_frp);
        let selection_colors = entry::style::SelectionColors::from_theme(network, style_frp);
        frp::extend! { network
            // === Active and Hovered Entry ===

            out.active <+ grid.entry_selected.map(f!((loc)
                model.location_to_element_id(loc.as_ref()?)
            ));
            out.active_section <+ out.active.map(|&e| Some(e?.group.section)).on_change();
            out.hovered <+ grid.entry_hovered.map(f!((loc)
                model.location_to_element_id(loc.as_ref()?)
            ));
            grid.hover_entry <+ input.unhover_element.constant(None);


            // === Accepting Suggestion and Expression ===

            action <- grid.entry_accepted.filter_map(f!((loc) model.action_after_accepting_entry(loc)));
            out.module_entered <+ action.filter_map(|m| m.enter_module());
            out.expression_accepted <+ action.filter_map(|e| e.accept());
            element_on_suggestion_accept <- out.active.sample(&input.accept_suggestion);
            out.suggestion_accepted <+ element_on_suggestion_accept.filter_map(|&e| e?.as_entry_id());


            // === Groups colors ===

            let group0 = style_frp.get_color(theme::group_colors::group_0);
            let group1 = style_frp.get_color(theme::group_colors::group_1);
            let group2 = style_frp.get_color(theme::group_colors::group_2);
            let group3 = style_frp.get_color(theme::group_colors::group_3);
            let group4 = style_frp.get_color(theme::group_colors::group_4);
            let group5 = style_frp.get_color(theme::group_colors::group_5);
            groups <- all7(&style.init, &group0, &group1, &group2, &group3, &group4, &group5);
            let local_scope_group = style_frp.get_color(theme::group_colors::local_scope_group);
            group_colors <- all_with(&groups, &local_scope_group, |&((), g0, g1, g2, g3, g4, g5), ls| {
                GroupColors {
                    variants: [g0, g1, g2, g3, g4, g5].map(color::Lcha::from),
                    local_scope_group: ls.into(),
                }
            });


            // === Style and Entries Params ===

            style_and_content_size <- all(&style.update, &grid.content_size);
            dimmed_groups <- out.active_section.map(|opt_section| match opt_section {
                Some(section) => entry::DimmedGroups::AllExceptSection(*section),
                None => entry::DimmedGroups::None,
            });
            entries_style <-
                all4(&style.update, &entry_style.update, &colors.update, &group_colors);
            entries_params <-
                all_with(&entries_style, &dimmed_groups, f!((s, d) model.entries_params(s, *d)));
            selection_entries_style <- all(entries_params, selection_colors.update);
            selection_entries_params <-
                selection_entries_style.map(f!((input) model.selection_entries_params(input)));
            grid_scroll_frp.resize <+ style_and_content_size.map(Model::grid_size);
            grid_position <- style_and_content_size.map(Model::grid_position);
            eval grid_position ((pos) model.grid.set_xy(*pos));
            grid_scroll_frp.set_corner_radius_bottom_right <+ all(&corners_radius, &style.init)._0();
            grid.set_entries_size <+ style.update.map(|s| s.entry_size());
            grid.set_column_width <+ style.update.map(|s| (column::CENTER, s.middle_column_width()));
            grid.set_entries_params <+ entries_params;
            grid_selection_frp.set_entries_params <+ selection_entries_params;


            // === Header and Entries Models ===

            grid.reset_entries <+ input.reset.map(f!((content) model.reset(content)));
            section_info <- input.model_for_header.filter_map(f!((m) model.make_section_info(m)));
            grid.model_for_entry <+ section_info.map(|(rs, c, m)| (rs.start, *c, m.clone()));
            grid.model_for_entry <+
                input.model_for_entry.filter_map(f!((input) model.model_for_entry(input)));
            grid_header_frp.section_info <+
                section_info.filter(f!((input) model.is_requested_section(input)));
            out.model_for_header_needed <+ grid_header_frp.section_info_needed.filter_map(
                f!((loc) model.section_info_requested(loc))
            );
            out.model_for_header_needed <+ grid.model_for_entry_needed.filter_map(
                f!((loc) model.location_to_headers_group_id(loc))
            );
            out.model_for_entry_needed <+
                grid.model_for_entry_needed.filter_map(f!((loc) model.location_to_entry_id(loc)));


            // === Scrolling and Jumping to Section ===

            grid_extra_scroll_frp.set_preferred_margins_around_entry <+ all_with(
                &out.active_section,
                &style.update,
                f!((section, style) model.navigation_scroll_margins(*section, style))
            );
            select_after_reset <- input.reset.map(f_!(model.entry_to_select_after_reset()));
            grid_extra_scroll_frp.select_and_jump_to_entry <+ select_after_reset.filter_map(|e| *e);
            grid.select_entry <+ select_after_reset.filter(|e| e.is_none()).constant(None);
            grid_selection_frp.skip_animations <+ input.reset.constant(());
            grid_extra_scroll_frp.select_and_scroll_to_entry <+ input.switch_section.filter_map(
                f!((section) model.entry_to_select_when_switching_to_section(*section))
            );
            grid_extra_scroll_frp.select_and_jump_to_entry <+
                input.switch_section_no_animation.filter_map(
                    f!((section) model.entry_to_select_when_switching_to_section(*section))
                );
            // The content area is higher than just height of all entries, because there is a gap
            // between all groups and local scope section.
            grid_scroll_frp.set_content_height <+
                style_and_content_size.map(|(s, c)| c.y + s.column_gap);


            // === Jumping by Groups ===

            entry_on_jump_down <- grid.entry_selected.sample(&input.jump_group_up);
            entry_on_jump_up <- grid.entry_selected.sample(&input.jump_group_down);
            grid_extra_scroll_frp.select_and_scroll_to_entry <+ entry_on_jump_down.filter_map(
                f!((loc) model.selection_after_jump_group_up(loc.as_ref()?))
            );
            grid_extra_scroll_frp.select_and_scroll_to_entry <+ entry_on_jump_up.filter_map(
                f!((loc) model.selection_after_jump_group_down(loc.as_ref()?))
            );


            // === Focus propagation ===

            grid.deprecated_focus <+ input.deprecated_focus;
            grid.deprecated_defocus <+ input.deprecated_defocus;
            grid.deprecated_set_focus <+ input.deprecated_set_focus;
        }

        // Set the proper number of columns so we can set column widths.
        grid.resize_grid(0, column::COUNT);
        style.init.emit(());
        entry_style.init.emit(());
        colors.init.emit(());
        selection_colors.init.emit(());
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        use ensogl_core::application::shortcut::ActionType::*;
        [
            (Press, "tab", "accept_suggestion"),
            (Press, "cmd up", "jump_group_up"),
            (Press, "cmd down", "jump_group_down"),
        ]
        .iter()
        .map(|(a, b, c)| View::self_shortcut(*a, *b, *c))
        .collect()
    }
}

/// The Component List Panel Grid View.
///
/// This component displays the scrollable grid with Compnent List Panel Content. It uses the
/// efficient [EnsoGL Grid View Component](grid_view). The entries are instantiated lazily.
///
/// # Setup
///
/// As a reaction for `model_for_entry_needed` and `model_for_header_needed` events, the proper
/// models should be send to `model_for_entry` and `model_for_header` inputs. Once you set up
/// FRP connections properly, you should emit `reset` event with information about group sizes and
/// colors.
pub type View = component::ComponentView<Model, Frp>;
