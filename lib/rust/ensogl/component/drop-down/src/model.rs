use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::data::color::Lcha;
use ensogl_core::display;
use ensogl_grid_view as grid_view;
use ensogl_gui_component::component;

use crate::entry::Entry;
use crate::entry::EntryParams;
use crate::DropdownValue;


// =================
// === Constants ===
// =================

/// Dropdown corner radius.
const CORNER_RADIUS: f32 = 8.0;
/// Dropdown padding.
const CLIP_PADDING: f32 = 3.0;



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
    display_object:       display::object::Instance,
    pub background:       RoundedRect,
    pub grid:             Grid,
    pub selected_entries: Rc<RefCell<HashSet<T>>>,
    pub cache:            Rc<RefCell<EntryCache<T>>>,
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
        grid.set_entries_params(EntryParams { corners_radius: inner_corners_radius, ..default() });
        grid.scroll_frp().set_corner_radius(inner_corners_radius);

        Model { background, grid, display_object, selected_entries: default(), cache: default() }
    }
}

impl<T: DropdownValue> Model<T> {
    pub fn set_dimensions(
        &self,
        num_entries: usize,
        max_size: Vector2,
        entry_height: f32,
        anim_progress: f32,
    ) {
        // Limit animation near almost closed state to avoid slow animation on very thin dropdown.
        let anim_progress = anim_progress * 1.05 - 0.05;
        let total_grid_height = num_entries as f32 * entry_height;
        let limited_grid_height = total_grid_height.min(max_size.y - CLIP_PADDING * 2.0);
        let outer_width = max_size.x;
        let outer_height = (limited_grid_height + CLIP_PADDING * 2.0) * anim_progress;
        let inner_width = (outer_width - CLIP_PADDING * 2.0).max(0.0);
        let inner_height = (outer_height - CLIP_PADDING * 2.0).max(0.0001);
        let inner_size = Vector2(inner_width, inner_height);
        let outer_size = Vector2(outer_width, outer_height);

        self.background.size.set(outer_size);
        // align the dropdown origin to its top left corner
        self.background.set_xy(Vector2(outer_width, -outer_height) / 2.0);
        self.background.corner_radius.set(CORNER_RADIUS);

        self.grid.set_xy(Vector2(CLIP_PADDING, -CLIP_PADDING));
        self.grid.scroll_frp().resize(inner_size);
        self.grid.set_entries_size(Vector2(inner_width, entry_height));
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

    pub fn accept_entry_at_index(&self, index: usize, allow_multiselect: bool, allow_empty: bool) {
        let cache = self.cache.borrow();
        let Some(entry) = cache.get(index) else { return };
        let mut selected = self.selected_entries.borrow_mut();
        if selected.contains(&entry) {
            if allow_empty || selected.len() > 1 {
                selected.remove(&entry);
            }
        } else if allow_multiselect || selected.len() == 0 {
            selected.insert(entry.clone());
        } else {
            selected.clear();
            selected.insert(entry.clone());
        }
    }

    /// Prune selection according to changed multiselect mode. Returns true if the selection was
    /// changed.
    pub fn set_multiselect(&self, multiselect: bool) -> bool {
        let mut entries = self.selected_entries.borrow_mut();
        if !multiselect && entries.len() > 1 {
            let mut to_drop = entries.len() - 1;
            entries.retain(|_| {
                let retain = to_drop == 0;
                to_drop = to_drop.saturating_sub(1);
                retain
            });
            true
        } else {
            false
        }
    }

    pub fn is_selected(&self, entry: &T) -> bool {
        self.selected_entries.borrow().contains(entry)
    }

    pub fn get_selected_entries(&self) -> HashSet<T> {
        self.selected_entries.borrow().clone()
    }

    pub fn get_single_selected_entry(&self) -> Option<T> {
        let entries = self.selected_entries.borrow();
        if entries.len() == 1 {
            entries.iter().next().cloned()
        } else {
            None
        }
    }

    pub fn set_color(&self, color: Lcha) {
        self.background.color_rgba.set(color::Rgba::from(color).into());
    }
}

impl<T: PartialEq + Hash> display::Object for Model<T> {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

type DropdownGridEntry = Entry;
type Grid = grid_view::scrollable::SelectableGridView<DropdownGridEntry>; // selectable is probably wrong, need multiselect


// ===================
// === Entry Cache ===
// ===================

/// A cache for the entries of the dropdown. It stores the entries in a `HashMap` and keeps track of
/// the position of each entry. It allows the dropdown to manage the grid-view internally and
/// provide simplified external APIs for providing the dropdown entries.
#[derive(Debug)]
pub struct EntryCache<T> {
    position_to_entry: HashMap<usize, T>,
}

impl<T> Default for EntryCache<T> {
    fn default() -> Self {
        Self { position_to_entry: default() }
    }
}

impl<T> EntryCache<T> {
    pub fn insert(
        &mut self,
        update_range: Range<usize>,
        new_entries: &[T],
        visible_range: Range<usize>,
        max_cache_size: usize,
    ) where
        T: Clone + Hash + Eq,
    {
        let max_cache_size = max_cache_size.max(visible_range.end - visible_range.start);

        if self.position_to_entry.len() > max_cache_size {
            self.prune_cache(visible_range);
        }

        for (position, entry) in update_range.zip(new_entries) {
            self.position_to_entry.insert(position, entry.clone());
        }
    }

    pub fn prune_cache(&mut self, retain_range: Range<usize>) {
        self.position_to_entry.retain(|k, _| retain_range.contains(k));
    }

    /// Get entry value at position.
    pub fn get(&self, position: usize) -> Option<&T>
    where T: Clone {
        self.position_to_entry.get(&position)
    }
}
