//! Dropdown component based on grid-view. Supports displaying static and dynamic list of selectable
//! entries.

#![recursion_limit = "512"]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;
use model::*;

use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::application::View;
use ensogl_core::data::color::Lcha;
use ensogl_core::display::shape::StyleWatchFrp;
// use ensogl_core::event::FocusIn;
// use ensogl_core::event::FocusOut;
use ensogl_core::animation::Animation;
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

/// Size of single entry in pixels.
const ENTRY_HEIGHT: f32 = 24.0;
/// Default maximum external size of the whole dropdown.
const DEFAULT_SIZE: Vector2 = Vector2(80.0, 300.0);
// const DEFAULT_COLOR: Lcha = Lcha::new(0.44682, 0.30307, 0.96682, 0.7);

// BASE COLOR
// const DEFAULT_COLOR: Lcha = Lcha::new(0.49112, 0.27123, 0.72659, 1.0);
// MIXED COLOR (15% white on top)
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
        set_max_size(Vector2),
        set_color(Lcha),
        /// The dropdown initially is not opened. This event can open or close the dropdown.
        set_opened(bool),

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

        /// Currently set internal buffer memory limit.
        max_entry_memory(usize),
    }
}


impl<T: DropdownValue> component::Frp<Model<T>> for Frp<T> {
    fn init_inputs(frp: &Self::Public) {
        frp.set_max_size(DEFAULT_SIZE);
        frp.set_color(DEFAULT_COLOR);
        frp.set_multiselect(false);
        frp.set_max_cached_entries(DEFAULT_MAX_ENTRIES);
        frp.set_opened(false);
        frp.allow_deselect_all(false);
    }

    fn init(
        network: &frp::Network,
        api: &Self::Private,
        _app: &Application,
        model: &Model<T>,
        _style: &StyleWatchFrp,
    ) {
        let input = &api.input;
        let output = &api.output;

        let open_anim = Animation::new(network);

        // static entries support
        frp::extend! { network
            static_number_of_entries <- input.set_all_entries.map(|entries| entries.len());
            static_entries <- input.set_all_entries.map(|entries| (0..entries.len(), entries.clone()));

            max_cache_size <- any(input.set_max_cached_entries, static_number_of_entries);
            number_of_entries <- any(input.set_number_of_entries, static_number_of_entries);
            provided_entries <- any(input.provide_entries_at_range, static_entries);
        }


        frp::extend! { network
            open_anim.target <+ input.set_opened.map(|open| if *open { 1.0 } else { 0.0 });
            dimensions <- all(number_of_entries, input.set_max_size, open_anim.value);
            eval dimensions((&(num_entries, max_size, anim_progress))
                model.set_dimensions(num_entries, max_size, ENTRY_HEIGHT, anim_progress));
            eval input.set_color((color) model.set_color(*color));
        }


        frp::extend! { network
            requested_index <- model.grid.model_for_entry_needed._0();
            missing_ranges <- requested_index.filter_map(f!([model] (&row) {
                match model.cache.borrow().get(row) {
                    Some(_) => None,
                    // TODO: batch requests
                    None => Some(row..row + 1),
                }
            }));
            output.entries_in_range_needed <+ missing_ranges;

            // request initial batch of entries after creating the dropdown
            let after_animations = ensogl_core::animation::on_after_animations();
            once_after_animations <- after_animations.constant(true).on_change().constant(());
            model.grid.request_model_for_visible_entries <+ once_after_animations;

        }
        frp::extend! { network

            visible_range <- model.grid.viewport.map(|viewport| {
                let start = (-viewport.top / ENTRY_HEIGHT).floor() as usize;
                let end = (-viewport.bottom / ENTRY_HEIGHT).ceil() as usize;
                start..(end + 1)
            });
            output.currently_visible_range <+ visible_range;

            provided_range <- provided_entries.map4(
                &visible_range, &max_cache_size, &number_of_entries,
                f!([model] ((updated_range, updated_entries), visible_range, max_cache_size, num_entries) {
                    let update_start = updated_range.start.min(*num_entries);
                    let update_end = updated_range.end.min(*num_entries);
                    let truncated_range = update_start..update_end;
                    let truncated_entries = &updated_entries[0..(update_end - update_start)];

                    let mut cache = model.cache.borrow_mut();
                    cache.insert(truncated_range, truncated_entries, visible_range.clone(), *max_cache_size);
                    updated_range.clone()
                })
            );
        }
        frp::extend! { network
            // TODO: avoid unnecessary wasteful range allocation
            provided_index <- provided_range.map(|range| range.clone().collect_vec()).iter();
            index_to_update <- any(provided_index, requested_index);
        }
        frp::extend! { network

            model.grid.model_for_entry <+ index_to_update.filter_map(f!([model] (index) {
                let cache = model.cache.borrow();
                let entry = cache.get(*index)?;
                let model = model_for_entry(entry, model.is_selected(entry));
                Some((*index, 0, model))
            }));
        }
        frp::extend! { network
            model.grid.accept_selected_entry <+ input.toggle_focused_entry;
            model.grid.move_selection_up <+ input.focus_previous_entry;
            model.grid.move_selection_down <+ input.focus_next_entry;
            model.grid.select_entry <+ model.grid.entry_hovered;
        }

        // selection
        frp::extend! { network
            selection_pruned <- input.set_multiselect.map(f!((multi) model.set_multiselect(*multi))).on_true();
            selection_accepted <- model.grid.entry_accepted.map3(
                &input.set_multiselect, &input.allow_deselect_all,
                f!(((row, _), multi, allow_deselect_all)
                model.accept_entry_at_index(*row, *multi, *allow_deselect_all)));

            selection_set <- input.set_selected_entries.map2(&input.set_multiselect,
                f!((values, max) model.set_selection(values, *max)));
            selection_changed <- any3(&selection_accepted, &selection_set, &selection_pruned);
            output.selected_entries <+ selection_changed.map(f!((()) model.get_selected_entries())).on_change();
            output.single_selected_entry <+ selection_changed.map(f!((()) model.get_single_selected_entry())).on_change();
        }

        frp::extend! { network
            viewport_needs_update <- any(selection_changed);
            viewport_entries_to_update <- visible_range.sample(&viewport_needs_update);
            viewport_entries_to_update <- viewport_entries_to_update.map(f!([model] (range) {
                let cache = model.cache.borrow();
                range.clone().filter_map(|index| {
                    let entry = cache.get(index)?;
                    let entry = model_for_entry(entry, model.is_selected(entry));
                    Some((index, 0, entry))
                }).collect_vec()
            }));
            model.grid.model_for_entry <+ viewport_entries_to_update.iter();
        }
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::*;
        [
            (Press, "set_opened", "down", "focus_next_entry"),
            (Press, "set_opened", "up", "focus_previous_entry"),
            (Press, "set_opened", "enter", "toggle_focused_entry"),
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

fn model_for_entry<T: DropdownValue>(input: &T, selected: bool) -> entry::EntryModel {
    entry::EntryModel::new(input.label(), selected)
}
