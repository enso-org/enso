//
// #[derive(CloneRef, Debug, Deref, Derivative)]
// #[derivative(Clone(bound = ""))]
// pub struct HighlightLayers<
//     Entry: 'static,
//     EntryModel: frp::node::Data,
//     EntryParams: frp::node::Data,
// > { entries:             Layer, text:                Layer, mask:                Layer, grid:
// > crate::GridViewTemplate<Entry, EntryModel, EntryParams>, shape:
// > highlight_mask::View, pub highlight_entry: frp::Any<Option<(Row, Col)>>,
// }
//
// macro_rules! propagate_grid_input {
//     ($network: ident, $init:ident, $target:ident, $src:ident, [$($endpoint:ident),*]) => {
//         frp::extend! {
//             $($target.$endpoint <+ all(&$init, &$target.$endpoint)._1();)*
//         }
//     }
// }
//
// impl<E: Entry> HighlightLayers<E, E::Model, E::Params> {
//     pub fn new(app: &Application, parent_layer: &Layer, base_grid: crate::GridView<E>) -> Self {
//         let grid = crate::GridView::new(app);
//         let shape = entry::shape::View::new(Logger::new("HighlightMask"));
//         let entries = parent_layer.create_sublayer();
//         let text = parent_layer.create_sublayer();
//         let mask =
//             Layer::new_with_cam(Logger::new("grid_view::HighlightLayers::mask"),
// &parent.camera());         entries.set_mask(&mask);
//         text.set_mask(&mask);
//         entries.add_exclusive(&grid);
//         grid.set_text_layer(Some(text.downgrade()));
//         mask.add_exclusive(shape);
//
//         let network = grid.network();
//         frp::extend! { network
//             init <- source_();
//
//             eval base_grid.viewport ([shape](vp) {
//                 shape.set_position(vp.center_point());
//                 shape.size.set(vp.size());
//             });
//         }
//         propagate_grid_input!(network, init, grid, base_grid, [
//             set_viewport,
//             reset_entries,
//             model_for_entry,
//             set_entries_size
//         ]);
//
//
//         Self { entries, text, mask, grid, shape, highlight_entry }
//     }
// }
