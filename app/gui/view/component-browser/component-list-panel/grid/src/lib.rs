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

use enso_frp as frp;
use ensogl_core::application::frp::API;
use ensogl_core::application::shortcut::Shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::display::style::FromTheme;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::Col;
use ensogl_grid_view::Row;
use ensogl_gui_component::component;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel as panel_theme;
use ensogl_hardcoded_theme::application::component_browser::component_list_panel::grid as theme;
use ensogl_icons::icon;
use ensogl_text as text;


// ==============
// === Export ===
// ==============

pub mod content;
pub mod entry;



/// A module containing common imports.
pub mod prelude {
    pub use enso_frp as frp;
    pub use ensogl_core::application::traits::*;
    pub use ensogl_core::display::shape::*;
    pub use ensogl_core::prelude::*;
    pub use ensogl_grid_view as grid_view;
    pub use ensogl_text as text;
}

pub use content::EntryId;
pub use content::GroupId;


// =================
// === Constants ===
// =================

/// The number of color variants taken from the style sheet, used to coloring group without color
/// specified by library author.
pub const GROUP_COLOR_VARIANT_COUNT: usize = 6;
/// The index of the column in the grid view where entries will be displayed.
pub const COLUMN: usize = 0;
/// The number of columns in components grid.
pub const COLUMN_COUNT: usize = 1;



// ===========
// === FRP ===
// ===========

// === Models ===

/// The model for entries.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default)]
pub struct EntryModel {
    pub caption:        ImString,
    pub highlighted:    Rc<Vec<text::Range<text::Byte>>>,
    pub icon:           icon::Id,
    pub can_be_entered: bool,
    pub group:          Option<GroupId>,
}


// === API ===

ensogl_core::define_endpoints_2! {
    Input {
        /// Set top margin inside scroll area. We need to leave some margin so the button panel
        /// will not cover the topmost entry when maximally scrolled up.
        set_top_margin(f32),
        reset(content::Info),
        select_first_entry(),
        model_for_entry(EntryId, EntryModel),
        /// Accept suggestion and continue editing.
        accept_suggestion(),
        /// Accept current input as expression, ignoring any active suggestion.
        accept_current_input_expression(),
        focus(),
    }
    Output {
        active(Option<EntryId>),
        is_active(bool),
        model_for_entry_needed(EntryId),
        suggestion_accepted(EntryId),
        expression_accepted(Option<EntryId>),
        module_entered(EntryId),
    }
}



// ============
// === Grid ===
// ============

/// A variant of the [component's](View) underlying [EnsoGL grid component](grid_view).
pub type Grid = grid_view::scrollable::SelectableGridView<entry::View>;



// =============
// === Style ===
// =============

// === GroupColors ===

/// The grid's style structure.
#[allow(missing_docs)]
#[derive(Clone, Copy, Default, Debug, PartialEq, FromTheme)]
#[base_path = "theme::group_colors"]
struct GroupColorsTheme {
    group_0:           color::Lcha,
    group_1:           color::Lcha,
    group_2:           color::Lcha,
    group_3:           color::Lcha,
    group_4:           color::Lcha,
    group_5:           color::Lcha,
    local_scope_group: color::Lcha,
}

/// Default groups colors.
#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub struct GroupColors {
    /// Variants to be used in groups in column layout.
    variants:      [color::Lcha; GROUP_COLOR_VARIANT_COUNT],
    /// The color of the group outside any defined component group.
    outside_group: color::Lcha,
}

impl From<GroupColorsTheme> for GroupColors {
    fn from(theme: GroupColorsTheme) -> Self {
        Self {
            variants:      [
                theme.group_0,
                theme.group_1,
                theme.group_2,
                theme.group_3,
                theme.group_4,
                theme.group_5,
            ],
            outside_group: theme.local_scope_group,
        }
    }
}



// === Style ===

/// The grid's style structure.
#[allow(missing_docs)]
#[derive(Clone, Copy, Default, Debug, PartialEq, FromTheme)]
#[base_path = "theme"]
pub struct Style {
    pub width:          f32,
    pub height:         f32,
    pub padding_x:      f32,
    pub padding_y:      f32,
    pub column_gap:     f32,
    pub entry_height:   f32,
    #[theme_path = "panel_theme::corners_radius"]
    pub corners_radius: f32,
}

impl Style {
    /// Compute the size of grid without external paddings.
    pub fn content_size(&self) -> Vector2 {
        let size = Vector2(self.width, self.height);
        let padding = Vector2(self.padding_x, self.padding_y) * 2.0;
        size - padding
    }

    /// Get the single entry size.
    pub fn entry_size(&self) -> Vector2 {
        Vector2(self.content_size().x, self.entry_height)
    }
}



// =============
// === Model ===
// =============

// === Model ===

/// A [Model](component::Model) of [Component List Panel Grid View](View).
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct Model {
    display_object:     display::object::Instance,
    #[focus_receiver]
    grid:               Grid,
    grid_layer:         Layer,
    selection_layer:    Layer,
    scroll_bars_layer:  Layer,
    enterable_elements: Rc<RefCell<HashSet<EntryId>>>,
    colors:             Rc<RefCell<HashMap<GroupId, entry::MainColor>>>,
}


// === component::Model Trait; Constructor ===

impl component::Model for Model {
    fn label() -> &'static str {
        "ComponentListPanelGrid"
    }

    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let grid = Grid::new(app);
        let enterable_elements = default();
        let colors = default();
        let base_layer = &app.display.default_scene.layers.node_searcher;
        let grid_layer = base_layer.create_sublayer("grid_layer");
        let selection_layer = base_layer.create_sublayer("selection_layer");
        let scroll_bars_layer = base_layer.create_sublayer("scrollbars_layer");
        display_object.add_child(&grid);
        grid_layer.add(&grid);
        grid.selection_highlight_frp().setup_masked_layer(selection_layer.downgrade());
        grid.set_scrollbars_layer(&scroll_bars_layer);
        Self {
            display_object,
            grid,
            enterable_elements,
            colors,
            grid_layer,
            selection_layer,
            scroll_bars_layer,
        }
    }
}


// === Resetting Grid ===

impl Model {
    /// The grid is resetting: remove all data regarding existing entries and return new grid size.
    fn reset(&self, content: &content::Info) -> (Row, Col) {
        *self.colors.borrow_mut() = Self::collect_colors(content);
        self.enterable_elements.borrow_mut().clear();
        (content.entry_count, COLUMN_COUNT)
    }

    fn collect_colors(content: &content::Info) -> HashMap<GroupId, entry::MainColor> {
        let variants = (0..).map(|i| i % GROUP_COLOR_VARIANT_COUNT);
        let groups_with_variants = content.groups.iter().zip(variants);
        groups_with_variants
            .map(|(group, variant)| {
                let color = match group.color {
                    Some(color) => entry::MainColor::Custom(color.into()),
                    None => entry::MainColor::Predefined { variant_index: variant },
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
    EnterModule(EntryId),
    Accept(EntryId),
}

impl Default for Action {
    fn default() -> Self {
        Self::EnterModule(default())
    }
}

impl Action {
    fn enter_module(self) -> Option<EntryId> {
        if let Self::EnterModule(element) = self {
            Some(element)
        } else {
            None
        }
    }

    fn accept(self) -> Option<EntryId> {
        if let Self::Accept(entry) = self {
            Some(entry)
        } else {
            None
        }
    }
}

impl Model {
    fn can_be_entered(&self, entry: EntryId) -> bool {
        self.enterable_elements.borrow().contains(&entry)
    }

    fn action_after_accepting_entry(
        &self,
        loc: &(Row, Col),
        content: &content::Info,
    ) -> Option<Action> {
        let entry_id = self.location_to_entry_id(loc, content)?;
        if self.can_be_entered(entry_id) {
            Some(Action::EnterModule(entry_id))
        } else {
            Some(Action::Accept(entry_id))
        }
    }
}


// === Conversion From and To Grid's Location ===

impl Model {
    fn location_to_entry_id(
        &self,
        &(row, _): &(Row, Col),
        content: &content::Info,
    ) -> Option<EntryId> {
        content.entry_count.checked_sub(row + 1)
    }

    fn entry_id_to_location(
        &self,
        entry_id: EntryId,
        content: &content::Info,
    ) -> Option<(Row, Col)> {
        content.entry_count.checked_sub(entry_id + 1).map(|row| (row, COLUMN))
    }
}


// === Entries' Models ===

impl Model {
    fn model_for_entry(
        &self,
        (entry, model): &(EntryId, EntryModel),
        content: &content::Info,
    ) -> Option<(Row, Col, entry::Model)> {
        if model.can_be_entered {
            self.enterable_elements.borrow_mut().insert(*entry);
        }
        let (row, col) = self.entry_id_to_location(*entry, content)?;
        let entry_model = entry::Model {
            color:       self.color_of_entry(model),
            caption:     model.caption.clone_ref(),
            highlighted: model.highlighted.clone_ref(),
            icon:        Some(model.icon),
        };
        Some((row, col, entry_model))
    }

    fn color_of_entry(&self, model: &EntryModel) -> entry::MainColor {
        let group_color = model.group.and_then(|group| self.colors.borrow().get(&group).copied());
        group_color.unwrap_or(entry::MainColor::OutsideGroup)
    }
}


// === Navigation ===

impl Model {
    fn first_entry_to_select(&self, info: &content::Info) -> Option<(Row, Col)> {
        info.entry_count.checked_sub(1).map(|row| (row, COLUMN))
    }
}


// === Updating Style ===

impl Model {
    fn grid_size((style, content_size, top_margin): &(Style, Vector2, f32)) -> Vector2 {
        let x = min(style.content_size().x, content_size.x);
        let y = min(style.content_size().y, content_size.y + top_margin);
        Vector2(x, y)
    }

    fn grid_position(input: &(Style, Vector2, f32)) -> Vector2 {
        let (style, _, _) = input;
        let y = -style.content_size().y + Self::grid_size(input).y;
        Vector2(style.padding_x, y)
    }

    fn entries_params(
        &self,
        (style, entry_style, colors, group_colors): &(
            Style,
            entry::Style,
            entry::style::Colors,
            GroupColors,
        ),
    ) -> entry::Params {
        entry::Params {
            style:        entry_style.clone(),
            grid_style:   *style,
            group_colors: *group_colors,
            colors:       *colors,
        }
    }

    fn selection_entries_params(
        &self,
        (base_params, colors): &(entry::Params, entry::style::SelectionColors),
    ) -> entry::Params {
        entry::Params { colors: (*colors).into(), ..base_params.clone() }
    }

    fn navigation_scroll_margins(&self, style: &Style) -> grid_view::Margins {
        let vertical_margin = style.content_size().y - style.entry_height;
        grid_view::Margins { top: vertical_margin, ..default() }
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
        let style = Style::from_theme(network, style_frp);
        let entry_style = entry::Style::from_theme(network, style_frp);
        let colors = entry::style::Colors::from_theme(network, style_frp);
        let selection_colors = entry::style::SelectionColors::from_theme(network, style_frp);
        frp::extend! { network

            // === Active and Hovered Entry ===

            grid.select_entry <+ grid.entry_hovered;
            out.active <+ grid.entry_selected.map2(&input.reset, f!((loc, content)
                model.location_to_entry_id(loc.as_ref()?, content)
            ));
            out.is_active <+ out.active.is_some();


            // === Accepting Suggestion and Expression ===

            action <- grid.entry_accepted.map2(&input.reset, f!((loc, content) model.action_after_accepting_entry(loc, content))).unwrap();
            out.module_entered <+ action.filter_map(|m| m.enter_module());
            out.expression_accepted <+ action.filter_map(|e| e.accept()).some();
            out.expression_accepted <+ input.accept_current_input_expression.constant(None);
            element_on_suggestion_accept <- out.active.sample(&input.accept_suggestion);
            out.suggestion_accepted <+ element_on_suggestion_accept.unwrap();


            // === Groups colors ===

            let group_colors_theme = GroupColorsTheme::from_theme(network, style_frp);
            group_colors <- group_colors_theme.map(|t| GroupColors::from(*t));


            // === Style and Entries Params ===

            size_and_position_factors <- all(style, grid.content_size, input.set_top_margin);
            entries_style <-
                all4(&style, &entry_style, &colors, &group_colors);
            entries_params <- entries_style.map(f!((s) model.entries_params(s)));
            selection_entries_style <- all(entries_params, selection_colors);
            selection_entries_params <-
                selection_entries_style.map(f!((input) model.selection_entries_params(input)));
            grid_scroll_frp.resize <+ size_and_position_factors.map(Model::grid_size);
            grid_position <- size_and_position_factors.map(Model::grid_position);
            eval grid_position ((pos) model.grid.set_xy(*pos));
            corners_radius <- style.map(|s| s.corners_radius);
            grid_scroll_frp.set_corner_radius_bottom_right <+ corners_radius;
            grid_scroll_frp.set_corner_radius_bottom_right <+ corners_radius;
            grid.set_entries_size <+ style.map(|s| s.entry_size());
            grid.set_entries_params <+ entries_params;
            grid_selection_frp.set_entries_params <+ selection_entries_params;
            grid_extra_scroll_frp.set_margins <+
                input.set_top_margin.map(|&top| grid_view::Margins { top, ..default() });


            // === Header and Entries Models ===

            grid.reset_entries <+ input.reset.map(f!((content) model.reset(content)));
            grid.model_for_entry <+
                input.model_for_entry.map2(&input.reset, f!((input, content) model.model_for_entry(input, content))).unwrap();
            out.model_for_entry_needed <+
                grid.model_for_entry_needed.map2(&input.reset, f!((loc, content) model.location_to_entry_id(loc, content))).unwrap();


            // === Initial Selection ===

            grid.select_entry <+ input.reset.constant(None);
            first_entry_to_select <- input.reset.map(
                f!((info) model.first_entry_to_select(info))
            );
            grid_extra_scroll_frp.jump_to_entry <+ first_entry_to_select.filter_map(|e| *e);
            grid.select_entry <+ first_entry_to_select.sample(&input.select_first_entry);
            grid_selection_frp.skip_animations <+ input.reset.constant(());


            // === Scrolling ===

            grid_extra_scroll_frp.set_preferred_margins_around_entry <+ style.map(
                f!((style) model.navigation_scroll_margins(style))
            );


            // === Focus ===

            eval_ input.focus (model.focus());
            let focused = model.on_event::<ensogl_core::event::FocusIn>();
            let defocused = model.on_event::<ensogl_core::event::FocusOut>();
            grid.disable_selection <+ bool(&focused, &defocused);
        }

        grid.resize_grid(0, COLUMN_COUNT);
    }

    fn default_shortcuts() -> Vec<Shortcut> {
        use ensogl_core::application::shortcut::ActionType::*;
        [
            (Press, "is_active", "tab", "accept_suggestion"),
            // The shortcut for accepting expression with active suggestion is handled by
            // underlying Grid View.
            (Press, "!is_active", "enter", "accept_current_input_expression"),
            (Press, "", "cmd enter", "accept_current_input_expression"),
            (Press, "!is_active", "up", "select_first_entry"),
            (Press, "!is_active", "up", "focus"),
        ]
        .iter()
        .map(|(a, b, c, d)| View::self_shortcut_when(*a, *c, *d, *b))
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
