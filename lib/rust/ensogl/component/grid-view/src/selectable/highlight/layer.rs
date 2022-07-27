use crate::prelude::*;
use crate::selectable::highlight::shape;
use crate::Col;
use crate::Entry;
use crate::Row;
use ensogl_core::application::Application;
use ensogl_core::display::scene::Layer;

#[derive(CloneRef, Debug, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Layers<Entry: 'static, EntryModel: frp::node::Data, EntryParams: frp::node::Data> {
    entries:   Layer,
    text:      Layer,
    mask:      Layer,
    pub grid:  crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
    pub shape: shape::View,
}

impl<E: Entry> Layers<E, E::Model, E::Params> {
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
        // and entries size first, only then we could resize grid and request for models. Otherwise,
        // the model updates could be ignored by `grid` (which could think they are not visible
        // anyway).
        //TODO[ao] rephrase maybe?
        let network = grid.network();
        frp::extend! { network
            init <- source_();
            viewport <- all(init, base_grid.viewport)._1();
            eval viewport ([shape](&vp) shape::set_viewport(&shape, vp));
            grid.set_viewport <+ viewport;
            grid.set_entries_size <+ all(init, base_grid.entries_size)._1();
            grid.resize_grid <+ all(init, base_grid.grid_size)._1();
            trace grid.resize_grid;
            grid.model_for_entry <+ base_grid.model_for_entry;
            trace grid.model_for_entry;
        }
        init.emit(());
        base_grid.request_model_for_visible_entries();

        Self { entries, text, mask, grid, shape }
    }
}
