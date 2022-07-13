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

pub mod basic;
pub mod entry;
pub mod visible_area;

pub use visible_area::VisibleArea;


/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::gui::Widget;
use ensogl_hardcoded_theme as theme;

use crate::entry::EntryFrp;
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

ensogl_core::define_endpoints_2! {
    <EntryModel: (frp::node::Data), EntryParams: (frp::node::Data)>
    Input {
        set_visible_area(VisibleArea),
        reset_entries(Row, Col),
        model_for_entry(Row, Col, EntryModel),
        set_entries_size(Vector2),
        set_entries_params(EntryParams),
    }

    Output {
        row_count(usize),
        column_count(usize),
        visible_area(VisibleArea),
        entries_size(Vector2),
        entries_params(EntryParams),
        viewport_size(Vector2),
        model_for_entry_needed(Row, Col),
    }
}


// =============
// === Model ===
// =============

#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
struct EntryCreationCtx<P> {
    app:              Application,
    network:          frp::WeakNetwork,
    set_entry_size:   frp::Stream<Vector2>,
    set_entry_params: frp::Stream<P>,
}

impl<P: frp::node::Data> EntryCreationCtx<P> {
    fn create_entry<E: Entry<Params = P>>(&self) -> E {
        let entry = E::new(&self.app);
        match self.network.upgrade() {
            Some(network) => {
                let entry_frp = entry.frp();
                let entry_network = entry_frp.network();
                frp::new_bridge_network! { [network, entry_network] grid_view_entry_bridge
                    init <- source_();
                    size <- all(init, self.set_entry_size)._1();
                    entry_frp.set_size <+ all(init, self.set_entry_size)._1();
                    entry_frp.set_params <+ all(init, self.set_entry_params)._1();
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

#[derive(Copy, Clone, Debug, Default)]
struct Properties {
    row_count:    usize,
    col_count:    usize,
    visible_area: VisibleArea,
    entries_size: Vector2,
}

impl Properties {
    pub fn viewport_size(&self) -> Vector2 {
        let x = self.col_count as f32 * self.entries_size.x;
        let y = self.row_count as f32 * self.entries_size.y;
        Vector2(x, y)
    }
}

/// The Model of Select Component.
#[derive(Clone, Debug)]
pub struct Model<E, P> {
    display_object:     display::object::Instance,
    visible_entries:    RefCell<HashMap<(Row, Col), E>>,
    free_entries:       RefCell<Vec<E>>,
    entry_creation_ctx: EntryCreationCtx<P>,
}

impl<E, P> Model<E, P> {
    fn new(entry_creation_ctx: EntryCreationCtx<P>) -> Self {
        let logger = Logger::new("GridView");
        let display_object = display::object::Instance::new(&logger);
        let visible_entries = default();
        let free_entries = default();
        Model { display_object, visible_entries, free_entries, entry_creation_ctx }
    }
}

impl<E: display::Object, P> Model<E, P> {
    fn update_entries_visibility(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { visible_area, entries_size, row_count, col_count } = properties;
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let visible_rows = visible_area.visible_rows(entries_size, row_count);
        let visible_cols = visible_area.visible_columns(entries_size, col_count);
        let no_longer_visible = visible_entries.drain_filter(|(row, col), _| {
            !visible_rows.contains(row) || !visible_cols.contains(col)
        });
        let detached = no_longer_visible.map(|(_, entry)| {
            entry.unset_parent();
            entry
        });
        free_entries.extend(detached);
        let uncovered = visible_area
            .all_visible_locations(entries_size, row_count, col_count)
            .filter(|loc| !visible_entries.contains_key(loc));
        uncovered.collect_vec()
    }

    fn update_after_entries_size_change(&self, properties: Properties) -> Vec<(Row, Col)> {
        let to_model_request = self.update_entries_visibility(properties);
        for ((row, col), visible_entry) in &*self.visible_entries.borrow() {
            set_entry_position(visible_entry, *row, *col, properties.entries_size);
        }
        to_model_request
    }

    fn reset_entries(&self, properties: Properties) -> Vec<(Row, Col)> {
        let Properties { visible_area, entries_size, row_count, col_count } = properties;
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let detached = visible_entries.drain().map(|(_, entry)| {
            entry.unset_parent();
            entry
        });
        free_entries.extend(detached);
        visible_area.all_visible_locations(entries_size, row_count, col_count).collect_vec()
    }
}

impl<E: Entry> Model<E, E::Params> {
    fn update_entry(&self, row: Row, col: Col, model: E::Model, entry_size: Vector2) {
        use std::collections::hash_map::Entry::*;
        let mut visible_entries = self.visible_entries.borrow_mut();
        let mut free_entries = self.free_entries.borrow_mut();
        let entry = match visible_entries.entry((row, col)) {
            Occupied(entry) => entry.into_mut(),
            Vacant(lack_of_entry) => {
                let new_entry =
                    free_entries.pop().unwrap_or_else(|| self.entry_creation_ctx.create_entry());
                set_entry_position(&new_entry, row, col, entry_size);
                self.display_object.add_child(&new_entry);
                lack_of_entry.insert(new_entry)
            }
        };
        entry.frp().set_model(model);
    }
}



// ================
// === GridView ===
// ================


#[allow(missing_docs)]
#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct GridViewTemplate<E: 'static, M: frp::node::Data, P: frp::node::Data> {
    widget: Widget<Model<E, P>, Frp<M, P>>,
}

pub type GridView<E> = GridViewTemplate<E, <E as Entry>::Model, <E as Entry>::Params>;

impl<E: Entry> GridView<E> {
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let input = &frp.private.input;
        let out = &frp.private.output;
        frp::extend! { network
            set_entry_size <- input.set_entries_size.sampler();
            set_entry_params <- input.set_entries_params.sampler();
        }
        let entry_creation_ctx = EntryCreationCtx {
            app:              app.clone_ref(),
            network:          network.downgrade(),
            set_entry_size:   set_entry_size.into(),
            set_entry_params: set_entry_params.into(),
        };
        let model = Rc::new(Model::new(entry_creation_ctx));
        frp::extend! { network
            out.row_count <+ input.reset_entries._0();
            out.column_count <+ input.reset_entries._1();
            out.visible_area <+ input.set_visible_area;
            out.entries_size <+ input.set_entries_size;
            let some_other = input.set_entries_size.clone_ref();
            out.entries_params <+ input.set_entries_params;
            prop <- all_with4(
                &out.row_count, &out.column_count, &out.visible_area, &out.entries_size,
                |&row_count, &col_count, &visible_area, &entries_size| {
                    Properties { row_count, col_count, visible_area, entries_size }
                }
            );
            out.viewport_size <+ prop.map(|prop| prop.viewport_size());

            request_models_after_vis_area_change <=
                input.set_visible_area.map2(&prop, f!((_, p) model.update_entries_visibility(*p)));
            request_models_after_entry_size_change <=
                input.set_entries_size.map2(&prop, f!((_, p) model.update_after_entries_size_change(*p)));
            request_models_after_reset <=
                input.reset_entries.map2(&prop, f!((_, p) model.reset_entries(*p)));
            out.model_for_entry_needed <+ request_models_after_vis_area_change;
            out.model_for_entry_needed <+ request_models_after_entry_size_change;
            out.model_for_entry_needed <+ request_models_after_reset;

            model_for_entry_and_prop <- input.model_for_entry.map2(&prop, |model, prop| (model.clone(), *prop));
            eval model_for_entry_and_prop
                ((((row, col, entry_model), prop): &((Row, Col, E::Model), Properties))
                    model.update_entry(*row, *col, entry_model.clone(), prop.entries_size)
                );
        }
        let display_object = model.display_object.clone_ref();
        let widget = Widget::new(app, frp, model, display_object);
        Self { widget }
    }
}

impl<E, M: frp::node::Data, P: frp::node::Data> display::Object for GridViewTemplate<E, M, P> {
    fn display_object(&self) -> &display::object::Instance {
        &self.widget.display_object()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use ensogl_core::display::object::Instance;
    use ensogl_core::display::object::WeakInstance;
    use ensogl_core::display::Scene;

    #[derive(Copy, Clone, CloneRef, Debug, Default)]
    struct TestEntryParams {
        param: Immutable<usize>,
    }

    #[derive(Clone, CloneRef, Debug)]
    struct TestEntry {
        frp:            entry::EntryFrp<Self>,
        param_set:      Rc<Cell<usize>>,
        model_set:      Rc<Cell<usize>>,
        display_object: display::object::Instance,
    }

    impl Entry for TestEntry {
        type Model = Immutable<usize>;
        type Params = TestEntryParams;

        fn new(app: &Application) -> Self {
            let frp = entry::EntryFrp::<Self>::new();
            let network = frp.network();
            let param_set = Rc::new(Cell::new(0));
            let model_set = Rc::new(Cell::new(0));
            let display_object = display::object::Instance::new(Logger::new("TestEntry"));
            frp::extend! { network
                eval frp.input.set_model ((model) model_set.set(**model));
                eval frp.input.set_params ((param) param_set.set(*param.param));
            }
            Self { frp, param_set, model_set, display_object }
        }

        fn frp(&self) -> &EntryFrp<Self> {
            &self.frp
        }
    }

    impl display::Object for TestEntry {
        fn display_object(&self) -> &display::object::Instance {
            &self.display_object
        }
    }

    #[test]
    fn initializing_grid_view() {
        let app = Application::new("root");
        let grid_view = GridView::<TestEntry>::new(&app);
        let network = grid_view.network();
        frp::extend! { network
            updates_requested <- grid_view.model_for_entry_needed.count().sampler();
        }

        let vis_area = VisibleArea { left_top: Vector2(0.0, 0.0), size: Vector2(100.0, 100.0) };
        grid_view.set_entries_size(Vector2(20.0, 20.0));
        grid_view.reset_entries(100, 100);
        grid_view.set_visible_area(vis_area);
        grid_view.set_entries_params(TestEntryParams { param: Immutable(13) });

        assert_eq!(grid_view.model().visible_entries.borrow().len(), 0);

        for i in 0..5 {
            for j in 0..5 {
                grid_view.model_for_entry(i as Row, j as Col, Immutable(i * 200 + j));
            }
        }

        {
            let created_entries = grid_view.model().visible_entries.borrow();
            assert_eq!(created_entries.len(), 25);
            for ((row, col), entry) in created_entries.iter() {
                assert_eq!(entry.model_set.get(), row * 200 + col);
                assert_eq!(entry.param_set.get(), 13);
            }
        }
    }

    #[test]
    fn updating_entries_after_visible_area_change() {
        let app = Application::new("root");
        let grid_view = GridView::<TestEntry>::new(&app);
        let network = grid_view.network();
        let initial_vis_area =
            VisibleArea { left_top: Vector2(0.0, 0.0), size: Vector2(100.0, 100.0) };
        grid_view.set_entries_size(Vector2(20.0, 20.0));
        grid_view.reset_entries(100, 100);
        grid_view.set_visible_area(initial_vis_area);
        grid_view.set_entries_params(TestEntryParams { param: Immutable(13) });

        for i in 0..5 {
            for j in 0..5 {
                grid_view.model_for_entry(i, j, Immutable(i * 200 + j));
            }
        }

        frp::extend! { network
            updates_requested <- grid_view.model_for_entry_needed.count().sampler();
        }

        let uncovering_new_entries =
            VisibleArea { left_top: Vector2(5.0, -5.0), size: Vector2(100.0, 100.0) };
        grid_view.set_visible_area(uncovering_new_entries);
        assert_eq!(updates_requested.value(), 11);
        assert_eq!(grid_view.model().visible_entries.borrow().len(), 25);

        for i in 0..6 {
            grid_view.model_for_entry(5, i, Immutable(200 * 5 + i));
        }
        for i in 0..5 {
            grid_view.model_for_entry(i, 5, Immutable(200 * i + 5));
        }
        assert_eq!(grid_view.model().visible_entries.borrow().len(), 36);

        let hiding_old_entries =
            VisibleArea { left_top: Vector2(20.0, -20.0), size: Vector2(100.0, 100.0) };
        grid_view.set_visible_area(uncovering_new_entries);
        assert_eq!(updates_requested.value(), 11); // Count should not change.
        assert_eq!(grid_view.model().visible_entries.borrow().len(), 25);
        assert_eq!(grid_view.model().free_entries.borrow().len(), 11);
    }
}
