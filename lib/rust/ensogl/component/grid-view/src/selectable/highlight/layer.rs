//! A module containing the single _Masked layer_ [`Handler`] used in
//! [`crate::selectable::highlight::Handler`].

use crate::prelude::*;

use crate::selectable::highlight::shape;
use crate::Entry;

use ensogl_core::application::Application;
use ensogl_core::display::scene::Layer;



// ===============
// === Handler ===
// ===============

/// The highlight _Masked layer_ handler.
///
/// The handler can be created for some specific layer and _base_ grid view. It will add two
/// sub-layers:
/// * one with the new _inner_ [`crate::GridView`] component;
/// * second being set up as a text layer of aforementioned grid view.
/// The _inner_ Grid View will be fully synchronized with the _base_ one except the entries'
/// parameters. The layers will be masked with highlight [`shape`], so only the highlighted fragment
/// of the _inner_ grid view is actually displayed.
///
/// See [`selection::GridView`] docs for usage example.
#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Handler<Entry: 'static, EntryModel: frp::node::Data, EntryParams: frp::node::Data> {
    entries:   Layer,
    text:      Layer,
    mask:      Layer,
    /// The _inner_ grid view.
    pub grid:  crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
    /// The shape being a mask for the sub-layers.
    pub shape: shape::View,
}

impl<E: Entry> Handler<E, E::Model, E::Params> {
    /// Create new handler for given layer and _base_ [`GridView`](crate::GridView).
    pub fn new(app: &Application, parent_layer: &Layer, base_grid: &crate::GridView<E>) -> Self {
        let grid = crate::GridView::new(app);
        let shape = shape::View::new(Logger::new("HighlightMask"));
        let entries = parent_layer.create_sublayer();
        let text = parent_layer.create_sublayer();
        let mask = Layer::new_with_cam(
            Logger::new("grid_view::HighlightLayers::mask"),
            &parent_layer.camera(),
        );
        entries.set_mask(&mask);
        text.set_mask(&mask);
        entries.add_exclusive(&grid);
        grid.set_text_layer(Some(text.downgrade()));
        mask.add_exclusive(&shape);
        base_grid.add_child(&grid);
        grid.add_child(&shape);

        // The order of instructions below is very important! We need to initialize the viewport
        // and entries size first, because its required to receive `model_for_entry` events after
        // resizing grid properly.
        let network = grid.network();
        frp::extend! { network
            init <- source_();
            viewport <- all(init, base_grid.viewport)._1();
            eval viewport ([shape](&vp) shape::set_viewport(&shape, vp));
            grid.set_viewport <+ viewport;
            grid.set_entries_size <+ all(init, base_grid.entries_size)._1();
            grid.resize_grid <+ all(init, base_grid.grid_size)._1();
            grid.model_for_entry <+ base_grid.model_for_entry;

            different_entry_hovered <- grid.entry_hovered.map2(&base_grid.entry_hovered, |e1, e2| e1 != e2);
            base_grid.hover_entry <+ grid.entry_hovered.gate(&different_entry_hovered);
            base_grid.select_entry <+ grid.entry_selected;
            base_grid.accept_entry <+ grid.entry_accepted;
        }
        init.emit(());
        base_grid.request_model_for_visible_entries();

        Self { entries, text, mask, grid, shape }
    }
}
