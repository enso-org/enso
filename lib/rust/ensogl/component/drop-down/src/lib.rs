//! Dropdown component based on grid-view. Supports displaying static and dynamic list of selectable
//! entries.

#![recursion_limit = "512"]
// === Features ===
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;
use model::*;

use ensogl_core::animation::Animation;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::application::View;
use ensogl_core::data::color::Lcha;
use ensogl_core::display::shape::StyleWatchFrp;
use ensogl_core::frp;
use ensogl_gui_component::component;
use ensogl_gui_component::component::ComponentView;


// ==============
// === Export ===
// ==============

pub mod entry;
pub mod model;



// =================
// === Constants ===
// =================

/// Default minimum outer size of the dropdown. Defines the minimum width of the background shape.
const DEFAULT_MIN_WIDTH: f32 = 40.0;
/// Default maximum outer size of the dropdown. Defines the maximum size of the background shape.
/// Entries exceeding this size will have added.
const DEFAULT_MAX_SIZE: Vector2 = Vector2(160.0, 300.0);
/// Default color of the dropdown background.
const DEFAULT_COLOR: Lcha = Lcha::new(0.56708, 0.23249, 0.71372, 1.0);
/// Default maximum number of entries that can be cached at once.
const DEFAULT_MAX_ENTRIES: usize = 128;



// ===========
// === FRP ===
// ===========

pub trait DropdownValue: Debug + Clone + PartialEq + Eq + Hash + 'static {
    fn label(&self) -> ImString;
}

impl<T> DropdownValue for T
where
    T: Debug + Clone + PartialEq + Eq + Hash + 'static,
    for<'a> &'a T: Into<ImString>,
{
    fn label(&self) -> ImString {
        self.into()
    }
}

ensogl_core::define_endpoints_2! { <T: (DropdownValue)>
    Input {
        /// Set background color of the dropdown.
        set_color(Lcha),
        /// The dropdown initially is not open. This event can open or close the dropdown.
        set_open(bool),
        /// Set maximum width and height of the open dropdown. If the content is larger, it will be
        /// scrollable and/or ellipsis will be applied to labels.
        set_max_open_size(Vector2),
        /// Set minimum width of the open dropdown. If the content is smaller, the dropdown will
        /// expand to the minimum width anyway adding padding to the right of the labels.
        set_min_open_width(f32),
        /// Provide a list of entries to be displayed. The list is assumed to be complete. No
        /// `entries_in_range_needed` event will be emitted after this call.
        ///
        /// This is equivalent to setting `set_number_of_entries` and `set_max_cached_entries`
        /// matching the list size, then `provide_entries_at_range` with full range of entries.
        set_all_entries(Vec<T>),
        /// Provide a list of entries that are selected.
        set_selected_entries(HashSet<T>),
        /// Set the total number of entries that the list contains. All not yet loaded entries will
        /// be displayed as placeholders. Needs to be set before any entries are provided, otherwise
        /// the provided entries will be discarded. The default value is 0.
        set_number_of_entries(usize),
        /// Set the ability to select multiple entries at once.
        set_multiselect(bool),
        /// Set the ability to deselect all entries. Note that this is only enforced when selection
        /// was already set. If selection is cleared with `set_selected_entries`, this flag will be
        /// ignored.
        allow_deselect_all(bool),

        /// Update a list of entries at specified range. Provided entries are not guaranteed to be
        /// always kept in memory. If the entry was unloaded due to memory constraints, it can be
        /// requested again.
        ///
        /// Entries that are provided past currently set `set_number_of_entries` limit will be
        /// discarded. Make sure to set the correct number of entries before providing entries.
        provide_entries_at_range(Range<usize>, Vec<T>),

        /// Set the maximum number of entries that can be kept in memory at once. If entry buffer
        /// exceeds this size, some unused entries will be unloaded and can be requested again. The
        /// pattern in which entries are unloaded is not defined and should not be relied upon.
        set_max_cached_entries(usize),

        /// Move the focus to the entry above current focus. If there is no entry focused,
        /// the first currently visible entry will be focused.
        focus_previous_entry(),
        /// Move the focus to the entry below current focus. If there is no entry focused,
        /// the last currently visible entry will be focused.
        focus_next_entry(),
        /// Toggle currently focused entry. If only one entry can be selected, this will deselect
        /// other entry.
        toggle_focused_entry(),
    }
    Output {
        /// Emitted when the dropdown needs a list of entries in a specified range to be loaded.
        /// Will be emitted for the first time after `set_number_of_entries` is set to non-zero
        /// value. The side handling this event should provide the requested entries using
        /// `provide_entries_at_range`. Until the requested entries are provided, a loader
        /// placeholder will be displayed when those entries are in view.
        entries_in_range_needed(Range<usize>),
        /// The range of entry indices that are currently in view. This range is different from the
        /// range of entries that are currently loaded or requested to be loaded.
        currently_visible_range(Range<usize>),
        /// Currently selected entries. Changes each time the user selects or deselects an entry.
        selected_entries(HashSet<T>),
        /// Currently selected single entry. Is `None` when more than one entry is selected. When
        /// working with multiselect dropdown, use `selected_entries` instead.
        single_selected_entry(Option<T>),

        /// Emitted when the user actually selects an entry. This is different from
        /// `selected_entries`, which is emitted each time the selection changes, including when the
        /// selection is changed by the user or programmatically by setting `set_selected_entries`.
        user_select_action(),

        /// Whether or not the dropdown is currently open.
        is_open(bool),
    }
}

impl<T: DropdownValue> Frp<T> {
    #[profile(Debug)]
    fn init(network: &frp::Network, api: &api::Private<T>, model: &Model<T>) {
        let input = &api.input;
        let output = &api.output;

        let open_anim = Animation::new(network);

        frp::extend! { network
            // === Static entries support ===
            static_number_of_entries <- input.set_all_entries.map(|entries| entries.len());
            static_entries <- input.set_all_entries.map(|entries|
                (0..entries.len(), entries.clone())
            );
            max_cache_size <- any(input.set_max_cached_entries, static_number_of_entries);
            number_of_entries <- any(input.set_number_of_entries, static_number_of_entries);
            provided_entries <- any(input.provide_entries_at_range, static_entries);


            // === Layout and animation ===
            open_anim.target <+ input.set_open.map(|open| if *open { 1.0 } else { 0.0 });
            output.is_open <+ input.set_open;

            grid_width <- model.grid.content_size.map(|s| s.x).on_change();
            max_height <- input.set_max_open_size.map(|s| s.y);
            max_width <- input.set_max_open_size.map(|s| s.x);
            width_bounds <- all(input.set_min_open_width, max_width).on_change();
            eval width_bounds(((min, max)) model.set_outer_width_bounds(*min, *max));

            dimensions <- all(number_of_entries, max_height, grid_width, open_anim.value);
            eval dimensions((&(num_entries, max_height, grid_width, anim_progress))
                model.set_dimensions(num_entries, max_height, grid_width, anim_progress));
            eval input.set_color((color) model.set_color(*color));


            // === Entry update and dynamic entries support ===
            requested_index <- model.grid.model_for_entry_needed._0();
            requested_batch <- requested_index.batch();
            ready_and_request_ranges <- requested_batch.map(
                f!((batch) model.get_ready_and_request_ranges(batch))
            );
            requested_range_ready <- ready_and_request_ranges._0().iter();
            requested_range_needed <- ready_and_request_ranges._1().iter();
            eval requested_range_needed ((range) model.expect_update_for_range(range.clone()));

            output.entries_in_range_needed <+ requested_range_needed;

            visible_range <- model.grid.viewport.map(|viewport| {
                let start = (-viewport.top / ENTRY_HEIGHT).floor() as usize;
                let end = (-viewport.bottom / ENTRY_HEIGHT).ceil() as usize;
                start..end
            });
            output.currently_visible_range <+ visible_range;

            requested_ranges_received <- provided_entries.map4(
                &visible_range, &max_cache_size, &number_of_entries,
                f!([model]((range, entries), visible, max_size, num_entries) {
                    let range = range.clone();
                    let visible = visible.clone();
                    model.insert_entries_in_range(range, entries, visible, *max_size, *num_entries)
                })
            );

            ranges_to_update <- any(...);
            ranges_to_update <+ requested_range_ready.map(f!([](range) vec![range.clone()]));
            ranges_to_update <+ requested_ranges_received;
            model.grid.model_for_entry <+ ranges_to_update.map(f!([model](ranges) {
                let mut models = vec![];
                for range in ranges {
                    let models_with_index = model.entry_models_for_range(range.clone());
                    let models_with_cell_pos =
                        models_with_index.map(|(idx, entry)| (idx, 0, entry));
                    models.extend(models_with_cell_pos);
                }
                models
            })).iter();


            // === Selection ===
            selection_pruned <- input.set_multiselect.map(
                f!((multi) model.set_multiselect(*multi))
            ).on_true();
            selection_accepted <- model.grid.entry_accepted.map3(
                &input.set_multiselect, &input.allow_deselect_all,
                f!(((row, _), multi, allow) model.accept_entry_at_index(*row, *multi, *allow)));
            selection_set <- input.set_selected_entries.map2(&input.set_multiselect,
                f!((values, max) model.set_selection(values, *max)));
            selection_changed <- any3(&selection_accepted, &selection_set, &selection_pruned);

            model.grid.request_model_for_visible_entries <+ selection_changed;
            output.selected_entries <+ selection_changed.map(
                f!((()) model.get_selected_entries())
            ).on_change();
            output.single_selected_entry <+ selection_changed.map(
                f!((()) model.get_single_selected_entry())
            ).on_change();
            output.user_select_action <+_ model.grid.entry_accepted;

            // === Keyboard navigation ===
            model.grid.accept_selected_entry <+ input.toggle_focused_entry;
            model.grid.move_selection_up <+ input.focus_previous_entry;
            model.grid.move_selection_down <+ input.focus_next_entry;
            model.grid.select_entry <+ model.grid.entry_hovered;

            has_focused_entry <- model.grid.entry_selected.map(|entry| entry.is_some());
            model.grid.select_entry <+ input.focus_previous_entry.gate_not(&has_focused_entry)
                .map2(&visible_range, |_, range| Some(((range.end - 1).max(range.start), 0)));
            model.grid.select_entry <+ input.focus_next_entry.gate_not(&has_focused_entry)
                .map2(&visible_range, |_, range| Some((range.start, 0)));


            // === Initialization ===
            // request initial batch of entries after creating the dropdown
            init <- source_();
            run_once <- init.debounce();
            init.emit(());
            model.grid.request_model_for_visible_entries <+ run_once;
        }
    }
}


impl<T: DropdownValue> component::Frp<Model<T>> for Frp<T> {
    fn init_inputs(frp: &Self::Public) {
        frp.set_min_open_width(DEFAULT_MIN_WIDTH);
        frp.set_max_open_size(DEFAULT_MAX_SIZE);
        frp.set_color(DEFAULT_COLOR);
        frp.set_multiselect(false);
        frp.set_max_cached_entries(DEFAULT_MAX_ENTRIES);
        frp.set_open(false);
        frp.allow_deselect_all(false);
    }

    fn init(
        network: &frp::Network,
        api: &Self::Private,
        _app: &Application,
        model: &Model<T>,
        _style: &StyleWatchFrp,
    ) {
        Frp::init(network, api, model);
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::*;
        [
            (Press, "is_open", "down", "focus_next_entry"),
            (Press, "is_open", "up", "focus_previous_entry"),
            (Press, "is_open", "enter", "toggle_focused_entry"),
        ]
        .iter()
        .map(|(a, b, c, d)| Dropdown::<T>::self_shortcut_when(*a, *c, *d, *b))
        .collect()
    }
}

// =================
// === Component ===
// =================

#[allow(missing_docs)]
pub type Dropdown<T> = ComponentView<Model<T>, Frp<T>>;
