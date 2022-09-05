//! A module containing the single _Masked layer_ [`Handler`] used in
//! [`crate::selectable::highlight::Handler`].

use crate::prelude::*;

use crate::header;
use crate::selectable::highlight::shape;
use crate::Entry;

use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;



// ===============
// === Handler ===
// ===============

/// The highlight _Masked layer_ handler.
///
/// The handler can be created for some specific layer and _base_ grid view. It will add two
/// sub-layers:
/// * one with the new _inner_ [Grid View component variant](crate);
/// * second being set up as a text layer of aforementioned grid view.
/// The _inner_ Grid View will be fully synchronized with the _base_ one except the entries'
/// parameters. The layers will be masked with highlight [`shape`], so only the highlighted fragment
/// of the _inner_ grid view is actually displayed.
///
/// See [`selection::GridView`] docs for usage example.
#[derive(Clone, CloneRef, Debug)]
#[clone_ref(bound = "InnerGridView: CloneRef")]
pub struct Handler<InnerGridView> {
    entries:     Layer,
    text:        Layer,
    mask:        Layer,
    header:      Immutable<Option<Layer>>,
    header_text: Immutable<Option<Layer>>,
    /// The _inner_ grid view.
    pub grid:    InnerGridView,
    /// The shape being a mask for the sub-layers.
    pub shape:   shape::View,
}

impl<InnerGridView> Handler<InnerGridView> {
    fn new_wrapping<E: Entry>(
        parent_layer: &Layer,
        grid: InnerGridView,
        base_grid: &InnerGridView,
    ) -> Self
    where
        InnerGridView: AsRef<crate::GridView<E>> + display::Object,
    {
        let shape = shape::View::new(Logger::new("HighlightMask"));
        let entries = parent_layer.create_sublayer();
        let text = parent_layer.create_sublayer();
        let header = default();
        let header_text = default();
        let mask = Layer::new_with_cam(
            Logger::new("grid_view::HighlightLayers::mask"),
            &parent_layer.camera(),
        );
        entries.set_mask(&mask);
        text.set_mask(&mask);
        entries.add_exclusive(&grid);
        let grid_frp = grid.as_ref().frp();
        let base_grid_frp = base_grid.as_ref().frp();
        grid_frp.set_text_layer(Some(text.downgrade()));
        mask.add_exclusive(&shape);
        base_grid.add_child(&grid);
        grid.add_child(&shape);

        // The order of instructions below is very important! We need to initialize the viewport
        // and entries size first, because its required to receive `model_for_entry` events after
        // resizing grid properly.
        let network = grid_frp.network();
        frp::extend! { network
            init <- source_();
            viewport <- all(init, base_grid_frp.viewport)._1();
            eval viewport ([shape](&vp) shape::set_viewport(&shape, vp));
            grid_frp.set_viewport <+ viewport;
            grid_frp.set_entries_size <+ all(init, base_grid_frp.entries_size)._1();
            grid_frp.resize_grid <+ all(init, base_grid_frp.grid_size)._1();
            grid_frp.model_for_entry <+ base_grid_frp.model_for_entry;

            different_entry_hovered <- grid_frp.entry_hovered.map2(&base_grid_frp.entry_hovered, |e1, e2| e1 != e2);
            base_grid_frp.hover_entry <+ grid_frp.entry_hovered.gate(&different_entry_hovered);
            base_grid_frp.select_entry <+ grid_frp.entry_selected;
            base_grid_frp.accept_entry <+ grid_frp.entry_accepted;
        }
        init.emit(());
        base_grid_frp.request_model_for_visible_entries();

        Self { entries, text, header, header_text, mask, grid, shape }
    }
}

/// A trait implemented by every [`Handler`] able to be constructed.
pub trait HasConstructor {
    /// The exact type of the _inner_ Grid View.
    type InnerGrid;

    /// Create new handler for given layer and _base_ Grid View.
    fn new(app: &Application, parent_layer: &Layer, base_grid: &Self::InnerGrid) -> Self;
}

impl<E: Entry> HasConstructor for Handler<crate::GridView<E>> {
    type InnerGrid = crate::GridView<E>;

    fn new(app: &Application, parent_layer: &Layer, base_grid: &Self::InnerGrid) -> Self {
        let grid = crate::GridView::new(app);
        Self::new_wrapping(parent_layer, grid, base_grid)
    }
}

impl<E: Entry, HeaderEntry: Entry<Params = E::Params>> HasConstructor
    for Handler<header::GridView<E, HeaderEntry>>
{
    type InnerGrid = header::GridView<E, HeaderEntry>;

    fn new(app: &Application, parent_layer: &Layer, base_grid: &Self::InnerGrid) -> Self {
        let grid = header::GridView::new(app);
        let mut this = Self::new_wrapping(parent_layer, grid, base_grid);

        let header_frp = this.grid.header_frp();
        let base_header_frp = base_grid.header_frp();
        frp::extend! { network
            header_frp.section_info <+ base_header_frp.section_info;
        }
        let header = parent_layer.create_sublayer();
        let header_text = parent_layer.create_sublayer();
        header_frp.set_layers(header::WeakLayers::new(&header, Some(&header_text)));
        header.set_mask(&this.mask);
        header_text.set_mask(&this.mask);
        this.header = Immutable(Some(header));
        this.header_text = Immutable(Some(header_text));
        this
    }
}
