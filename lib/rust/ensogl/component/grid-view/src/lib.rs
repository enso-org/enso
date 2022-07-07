//! ListView EnsoGL Component.
//!
//! ListView a displayed list of entries with possibility of selecting one and "choosing" by
//! clicking or pressing enter - similar to the HTML `<select>`.

#![recursion_limit = "1024"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
#![feature(hash_drain_filter)]
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


// ==============
// === Export ===
// ==============

pub mod entry;



/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_core::gui::Widget;
use ensogl_hardcoded_theme as theme;

pub use entry::Entry;



// =================
// === Constants ===
// =================

const DEFAULT_STYLE_PATH: &str = theme::widget::list_view::HERE.str;



// ===========
// === FRP ===
// ===========

pub type Row = usize;
pub type Col = usize;

#[derive(Copy, Clone, Debug, Default)]
pub struct VisibleArea {
    left_top: Vector2,
    size:     Vector2,
}

impl VisibleArea {
    fn right_bottom(&self) -> Vector2 {
        self.left_top + Vector2(self.size.x, -self.size.y)
    }

    fn all_visible_locations(&self, entry_size: Vector2) -> impl Iterator<Item = (Row, Col)> {
        itertools::iproduct!(self.visible_rows(entry_size), self.visible_columns(entry_size))
    }

    fn visible_rows(&self, entry_size: Vector2) -> RangeInclusive<Row> {
        let right_bottom = self.right_bottom();
        let min_row: Row = (self.left_top.y / -entry_size.y).floor() as Row;
        let max_row: Row = (right_bottom.y / -entry_size.y).ceil() as Row;
        min_row..=max_row
    }

    fn visible_columns(&self, entry_size: Vector2) -> RangeInclusive<Col> {
        let right_bottom = self.right_bottom();
        let min_col: Col = (self.left_top.x / entry_size.x).floor() as Col;
        let max_col: Col = (right_bottom.x / entry_size.x).ceil() as Col;
        min_col..=max_col
    }
}

ensogl_core::define_endpoints! {
    <EntryModel: (frp::node::Data), EntryParams: (frp::node::Data)>
    Input {
        set_visible_area(VisibleArea),
        reset_entries(Row, Col),
        model_for_entry(Row, Col, EntryModel),
        set_entries_size(Vector2),
        set_entries_params(EntryParams),
    }

    Output {
        size(Vector2<f32>),
        model_for_entry_needed(Row, Col),
    }
}


// =============
// === Model ===
// =============

struct EntryCreationCtx {
    app:              Application,
    network:          frp::WeakNetwork,
    set_entry_size:   frp::Stream<Vector2>,
    set_entry_params: frp::Stream<Vector2>,
}

impl EntryCreationCtx {
    fn create_entry<E: Entry>(&self) -> E {
        let entry = E::new(&self.app);
        match self.network.upgrade() {
            Some(network) => {
                let entry_frp = entry.frp();
                let entry_network = entry_frp.network();
                frp::new_bridge_network! { [network, entry_network] grid_view_entry_bridge
                    init <- source_();
                    entry_frp.set_size <+ self.set_entry_size;
                    entry_frp.set_params <+ self.set_entry_params;
                }
                init.emit(());
            }
            None => {
                tracing::warn!("Tried to connect entry FRP when the GridView network is dropped");
            }
        }
        entry
    }
}

fn set_entry_position<E: display::Object>(entry: &E, row: Row, col: Col, entry_size: Vector2) {
    let x = (col as f32 + 0.5) * entry_size.x;
    let y = (row as f32 + 0.5) * -entry_size.y;
    entry.set_position_xy(Vector2(x, y));
}

/// The Model of Select Component.
#[derive(Clone, Debug)]
struct Model<E> {
    display_object:     display::object::Instance,
    visible_entries:    RefCell<HashMap<(Row, Col), E>>,
    free_entries:       RefCell<Vec<E>>,
    entry_creation_ctx: EntryCreationCtx,
}

impl<E> Model<E> {
    fn new(entry_creation_ctx: EntryCreationCtx) -> Self {
        let logger = Logger::new("GridView");
        let display_object = display::object::Instance::new(&logger);
        let visible_entries = default();
        let free_entries = default();
        Model { display_object, visible_entries, free_entries, entry_creation_ctx }
    }

    fn update_entries_positions(&self, entry_size: Vector2) {
        for entry in self.visible_entries.borrow() {
            entry.update_position(entry_size)
        }
    }
}

impl<E: display::Object> Model<E> {
    fn update_entries_visibility(
        &self,
        visible_area: VisibleArea,
        entry_size: Vector2,
    ) -> impl IntoIterator<Item = (Row, Col)> {
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let visible_rows = visible_area.visible_rows(entry_size);
        let visible_cols = visible_area.visible_columns(entry_size);
        let no_longer_visible = visible_entries.drain_filter(|(row, col), _| {
            !visible_rows.contains(row) || !visible_cols.contains(col)
        });
        let detached = no_longer_visible.map(|(_, entry)| {
            entry.unset_parent();
            entry
        });
        free_entries.extend(detached);
        let uncovered = visible_area
            .all_visible_locations(entry_size)
            .filter(|loc| !visible_entries.contains_key(loc));
        uncovered.collect_vec()
    }

    fn update_after_entry_size_change(
        &self,
        entry_size: Vector2,
        visible_area: VisibleArea,
    ) -> impl IntoIterator<Item = (Row, Col)> {
        let to_model_request = self.update_entries_visibility(visible_area, entry_size);
        for ((row, col), visible_entry) in &*self.visible_entries.borrow() {
            set_entry_position(visible_entry, *row, *col, entry_size);
        }
        to_model_request
    }

    fn reset_entries(
        &self,
        visible_area: VisibleArea,
        entry_size: Vector2,
    ) -> impl IntoIterator<Item = (Row, Col)> {
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let detached = visible_entries.drain().map(|(_, entry)| {
            entry.unset_parent();
            entry
        });
        free_entries.extend(detached);
        visible_area.all_visible_locations(entry_size)
    }
}

impl<E: Entry> Model<E> {
    fn update_entry(&self, row: Row, col: Col, model: E::Model) -> E {
        use std::collections::hash_map::Entry::*;
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let entry = match visible_entries.entry((row, col)) {
            Occupied(entry) => entry.get(),
            Vacant(lack_of_entry) => {
                let new_entry =
                    free_entries.pop().unwrap_or_else(|| self.entry_creation_ctx.create_entry());
                set_entry_position(&new_entry, row, col);
                self.display_object.add_child(&new_entry);
                lack_of_entry.insert(new_entry)
            }
        };
        entry.frp().set_model(model)
    }
}



// ================
// === GridView ===
// ================


#[allow(missing_docs)]
#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
struct GridViewTemplate<E: 'static, M: frp::node::Data, P: frp::node::Data> {
    widget: Widget<Frp<M, P>, Model<E>>,
}

pub type GridView<E> = GridViewTemplate<E, <E as Entry>::Model, <E as Entry>::Params>;

impl<E: Entry> GridView<E> {
    fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let entry_creation_ctx = EntryCreationCtx {
            app:              app.clone_ref(),
            network:          network.downgrade(),
            set_entry_size:   frp.set_entries_size.into(),
            set_entry_params: frp.set_entries_params.into(),
        };
        let model = Rc::new(Model::new(entry_creation_ctx));
        let out = &frp.output;
        frp::extend! { network
            params <- all(frp.set_entry_size, frp.set_visible_area);
            request_models_after_vis_area_change <= frp.set_visible_area.map2(
                &frp.set_entry_size,
                f!((area, entry_size) model.update_entries_visibility(area, entry_size))
            );
            request_models_after_entry_size_change <= frp.set_entry_size.map2(
                &frp.set_visible_area,
                f!((entry_size, area) model.update_after_entry_size_change(entry_size, area))
            );
            request_models_after_reset <= frp.reset.map3(
                &frp.entry_size,
                &frp.visible_area,
                f!(((), entry_size, area) model.reset_entries(area, entry_size))
            );
            out.model_for_entry_needed <+ request_models_after_vis_area_change;
            out.model_for_entry_needed <+ request_models_after_entry_size_change;
            out.model_for_entry_needed <+ request_models_after_reset;
        }
    }
}

impl<E, M: frp::node::Data, P: frp::node::Data> display::Object for GridViewTemplate<E, M, P> {
    fn display_object(&self) -> &display::object::Instance {
        &self.widget.display_object()
    }
}
