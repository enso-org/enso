use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use crate::entry::Entry;
use crate::entry::EntryModel;
use crate::entry::EntryParams;
use crate::DropdownValue;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::data::color::Lcha;
use ensogl_core::display;
use ensogl_grid_view as grid_view;
use ensogl_gui_component::component;



// =================
// === Constants ===
// =================

/// Dropdown corner radius.
const CORNER_RADIUS: f32 = 8.0;
/// Dropdown padding. This is the padding between the dropdown border and the entry hover highlight.
const CLIP_PADDING: f32 = 3.0;
/// Size of single entry in pixels.
pub(crate) const ENTRY_HEIGHT: f32 = 24.0;
/// Open/close animation scale and offset factors. The animation is scaled and offset by these
/// factors to avoid the animation showing a tiny sliver of the dropdown for too long. The values
/// were chosen empirically to look good and not cause issues with dropdown scroll area.
const OPEN_ANIMATION_SCALE: f32 = 1.05;
/// The offset of scaled animation is applied to move the scaling pivot point towards the "fully
/// open dropdown" end of the animation. A small extra epsilon is added to cancel out rounding
/// errors that cause the dropdown scroll area to be slightly too small for its internal content,
/// causing a scrollbar to appear when it is not necessary.
const OPEN_ANIMATION_OFFSET: f32 = OPEN_ANIMATION_SCALE - 1.001;



// =========================
// === Shape Definition ===
// =========================

mod rounded_rect {
    use super::*;
    ensogl_core::shape! {
        (style:Style, color_rgba: Vector4<f32>, corner_radius: f32) {
            let color = Var::<color::Rgba>::from(color_rgba);
            let rect  = Rect(Var::canvas_size()).corners_radius(corner_radius.px());
            let out   = rect.fill(color);
            out.into()
        }
    }
}

pub type RoundedRect = rounded_rect::View;



// =============
// === Model ===
// =============

#[derive(Derivative, CloneRef, Debug)]
#[derivative(Clone(bound = ""))]
pub struct Model<T> {
    display_object:   display::object::Instance,
    background:       RoundedRect,
    pub grid:         Grid,
    selected_entries: Rc<RefCell<HashSet<T>>>,
    cache:            Rc<RefCell<EntryCache<T>>>,
}

impl<T> component::Model for Model<T> {
    fn label() -> &'static str {
        "Dropdown"
    }

    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();

        let background = RoundedRect::new();
        let grid = Grid::new(app);
        display_object.add_child(&background);
        display_object.add_child(&grid);

        let inner_corners_radius = CORNER_RADIUS - CLIP_PADDING;
        let entries_params = EntryParams { corners_radius: inner_corners_radius, ..default() };
        let min_width = entries_params.min_width;
        grid.set_entries_params(entries_params);
        grid.scroll_frp().set_corner_radius(inner_corners_radius);
        grid.set_entries_size(Vector2(min_width, ENTRY_HEIGHT));

        Model { background, grid, display_object, selected_entries: default(), cache: default() }
    }
}

impl<T: DropdownValue> Model<T> {
    /// Set the minimum and maximum allowed inner width of an entry.
    pub fn set_outer_width_bounds(&self, min_outer_width: f32, max_outer_width: f32) {
        let corners_radius = CORNER_RADIUS - CLIP_PADDING;
        let max_width = max_outer_width - CLIP_PADDING * 2.0;
        let min_width = min_outer_width.min(max_width);
        let params = EntryParams { corners_radius, min_width, max_width, ..default() };
        self.grid.set_entries_params(params);
        self.grid.set_entries_size(Vector2(min_width, ENTRY_HEIGHT));
    }

    /// Set the dimensions of all ui elements of the dropdown.
    pub fn set_dimensions(
        &self,
        num_entries: usize,
        max_height: f32,
        grid_width: f32,
        anim_progress: f32,
    ) {
        // Limit animation near almost closed state to avoid slow animation on very thin dropdown.
        let anim_progress = anim_progress * OPEN_ANIMATION_SCALE - OPEN_ANIMATION_OFFSET;
        let anim_progress = anim_progress.clamp(0.0, 1.0);
        let total_grid_height = num_entries as f32 * ENTRY_HEIGHT;
        let limited_grid_height = total_grid_height.min(max_height - CLIP_PADDING * 2.0);
        let outer_height = (limited_grid_height + CLIP_PADDING * 2.0) * anim_progress;
        let inner_width = grid_width;
        let outer_width = inner_width + CLIP_PADDING * 2.0;
        let inner_height = outer_height - CLIP_PADDING * 2.0;
        let inner_size = Vector2(inner_width, inner_height);
        let outer_size = Vector2(outer_width, outer_height);

        self.background.set_size(outer_size);
        // align the dropdown origin to its top left corner
        self.background.set_xy(Vector2(outer_width, -outer_height) / 2.0);
        self.background.corner_radius.set(CORNER_RADIUS);

        self.grid.set_xy(Vector2(CLIP_PADDING, -CLIP_PADDING));
        self.grid.scroll_frp().resize(inner_size);
        self.grid.resize_grid(num_entries, 1);
    }

    pub fn set_selection(&self, selected: &HashSet<T>, allow_multiselect: bool) {
        let mut entries = self.selected_entries.borrow_mut();
        entries.clear();
        if allow_multiselect {
            entries.extend(selected.iter().cloned());
        } else {
            entries.extend(selected.iter().take(1).cloned());
        }
    }

    /// Convert provided list of indices onto sets of index ranges. One set of ranges is for indices
    /// that are already in cache, and the other set is for indices that need to be requested.
    pub fn get_ready_and_request_ranges(
        &self,
        requested_indices: &[usize],
    ) -> (Vec<Range<usize>>, Vec<Range<usize>>) {
        let cache = self.cache.borrow();

        let mut request_ranges: Vec<Range<usize>> = Vec::new();
        let mut ready_ranges: Vec<Range<usize>> = Vec::new();
        for &index in requested_indices {
            let modify_ranges = match cache.contains_key(index) {
                true => &mut ready_ranges,
                false => &mut request_ranges,
            };

            let mut new_range = Range { start: index, end: index + 1 };
            modify_ranges.retain(|range| {
                let ranges_overlap = new_range.start <= range.end && range.start <= new_range.end;
                if ranges_overlap {
                    new_range.start = range.start.min(new_range.start);
                    new_range.end = range.end.max(new_range.end);
                }
                !ranges_overlap
            });
            modify_ranges.push(new_range);
        }

        (ready_ranges, request_ranges)
    }

    /// Accepts entry at given index, modifying selection. If entry is already selected, it will be
    /// unselected, unless it is the last selected entry and `allow_empty` is false. For
    /// single-select dropdowns, previously selected entry will be unselected.
    pub fn accept_entry_at_index(&self, index: usize, allow_multiselect: bool, allow_empty: bool) {
        let cache = self.cache.borrow();
        let Some(entry) = cache.get(index) else { return };
        let mut selected = self.selected_entries.borrow_mut();
        if selected.contains(entry) {
            if allow_empty || selected.len() > 1 {
                selected.remove(entry);
            }
        } else if allow_multiselect || selected.len() == 0 {
            selected.insert(entry.clone());
        } else {
            selected.clear();
            selected.insert(entry.clone());
        }
    }

    /// Returns an iterator over entry models in given range. Only iterates over models for entries
    /// that are currently in cache.
    ///
    /// Note: The iterator borrows cache and selection. Make sure to drop it before calling any
    /// methods that need to borrow them mutably.
    pub fn entry_models_for_range(
        &self,
        range: Range<usize>,
    ) -> impl Iterator<Item = (usize, EntryModel)> + '_ {
        let cache = self.cache.borrow();
        let selection = self.selected_entries.borrow();
        range.filter_map(move |index| {
            let entry = cache.get(index)?;
            let selected = Immutable(selection.contains(entry));
            let text = entry.label();
            Some((index, EntryModel { text, selected }))
        })
    }

    /// Update cache with new entries at given range. Returns range of indices that were updated.
    pub fn insert_entries_in_range(
        &self,
        updated_range: Range<usize>,
        updated_entries: &[T],
        visible_range: Range<usize>,
        max_cache_size: usize,
        num_entries: usize,
    ) -> Range<usize> {
        let update_start = updated_range.start.min(num_entries);
        let update_end = updated_range.end.min(num_entries);
        let truncated_range = update_start..update_end;
        let truncated_entries = &updated_entries[0..(update_end - update_start)];

        let mut cache = self.cache.borrow_mut();
        cache.insert(truncated_range.clone(), truncated_entries, visible_range, max_cache_size);
        truncated_range
    }

    /// Prune selection according to changed multiselect mode. Returns true if the selection was
    /// changed.
    pub fn set_multiselect(&self, multiselect: bool) -> bool {
        let mut entries = self.selected_entries.borrow_mut();
        if !multiselect && entries.len() > 1 {
            let first = entries.drain().next();
            let first = first.expect("Set should not be empty after checking size");
            entries.insert(first);
            true
        } else {
            false
        }
    }

    /// Check if given entry is currently selected.
    pub fn is_selected(&self, entry: &T) -> bool {
        self.selected_entries.borrow().contains(entry)
    }

    /// Get a set of all currently selected entries.
    pub fn get_selected_entries(&self) -> HashSet<T> {
        self.selected_entries.borrow().clone()
    }

    /// Get currently selected entry, if and only if there is exactly one.
    pub fn get_single_selected_entry(&self) -> Option<T> {
        let entries = self.selected_entries.borrow();
        if entries.len() == 1 {
            entries.iter().next().cloned()
        } else {
            None
        }
    }

    /// Set the background color of the dropdown.
    pub fn set_color(&self, color: Lcha) {
        self.background.color_rgba.set(color::Rgba::from(color).into());
    }
}

impl<T: PartialEq + Hash> display::Object for Model<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

type Grid = grid_view::scrollable::SelectableGridView<Entry>;



// ===================
// === Entry Cache ===
// ===================

/// A cache for the entries of the dropdown. It stores the entries in a `HashMap` and keeps track of
/// the position of each entry. It allows the dropdown to manage the grid-view internally and
/// provide simplified external APIs for providing the dropdown entries.
#[derive(Debug)]
struct EntryCache<T> {
    position_to_entry: HashMap<usize, T>,
}

impl<T> Default for EntryCache<T> {
    fn default() -> Self {
        Self { position_to_entry: default() }
    }
}

impl<T> EntryCache<T> {
    fn insert(
        &mut self,
        update_range: Range<usize>,
        new_entries: &[T],
        visible_range: Range<usize>,
        max_cache_size: usize,
    ) where
        T: Clone + Hash + Eq,
    {
        let max_cache_size = max_cache_size.max(visible_range.end - visible_range.start);

        if self.position_to_entry.len() + update_range.len() > max_cache_size {
            self.prune(visible_range);
        }

        for (position, entry) in update_range.zip(new_entries) {
            self.position_to_entry.insert(position, entry.clone());
        }
    }

    /// Remove cache entries that are not in the given range.
    fn prune(&mut self, retain_range: Range<usize>) {
        self.position_to_entry.retain(|k, _| retain_range.contains(k));
    }

    /// Get entry value at position.
    fn get(&self, position: usize) -> Option<&T>
    where T: Clone {
        self.position_to_entry.get(&position)
    }

    fn contains_key(&self, position: usize) -> bool {
        self.position_to_entry.contains_key(&position)
    }
}
