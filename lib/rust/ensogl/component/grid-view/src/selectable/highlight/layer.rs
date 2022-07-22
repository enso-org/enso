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

macro_rules! propagate_grid_input {
    ($network: ident, $init:ident, $target:ident, $src:ident, [$($endpoint:ident),*]) => {
        frp::extend! { $network
            $($target.$endpoint <+ all(&$init, &$src.$endpoint)._1();)*
        }
    }
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

        let network = grid.network();
        frp::extend! { network
            init <- source_();
            viewport <- all(init, base_grid.viewport)._1();
            eval viewport ([shape](&vp) shape::set_viewport(&shape, vp));
        }
        propagate_grid_input!(network, init, grid, base_grid, [
            set_viewport,
            reset_entries,
            model_for_entry,
            set_entries_size
        ]);
        init.emit(());

        Self { entries, text, mask, grid, shape }
    }
}
