// use ensogl_core::application::Application;
// use ensogl_core::display::scene::Layer;
// use ensogl_core::display::shape::HOVER_COLOR;
// use ensogl_scroll_area::ScrollArea;
// use crate::{Entry, EntryFrp};
// use crate::prelude::*;
//
//
//
// pub struct EntryWithOverlay<Entry> {
//     entry: Entry,
//     overlay: highlight::View,
// }
//
// impl<E: Entry> Entry for EntryWithOverlay<E> {
//     type Model = E::Model;
//     type Params = E::Params;
//
//     fn new(app: &Application, text_layer: &Option<Layer>) -> Self {
//         let overlay =
// highlight::View::new(Logger::new("grid_view::selectable::EntryWithOverlay"));         overlay.
// color.set(HOVER_COLOR.into());         let entry = E::new(app, text_layer);
//         let frp = entry.frp();
//         let network = frp.network();
//         frp::extend! { network
//             eval entry.highlight_shape ([overlay](shape) {
//                 let padding = Vector2(highlight::PADDING_PX, highlight::PADDING_PX) * 2.0;
//                 overlay.size.set(shape.size + padding);
//                 overlay.corner_radius.set(shape.corner_radius);
//             });
//         }
//     }
//
//     fn frp(&self) -> &EntryFrp<Self> {
//         self.entry.frp()
//     }
// }
//
// #[derive(CloneRef, Debug, Deref, Derivative)]
// #[derivative(Clone(bound = ""))]
// pub struct Model<
//     Entry: 'static,
//     EntryModel: frp::node::Data,
//     EntryParams: frp::node::Data,
// > { grid:       crate::GridViewTemplate<Entry, EntryModel, EntryParams>, active_highlight:
// > highlight::View, hovered_highlight: highlight::View,
// }
//
// impl<E: Entry> Model<E, E::Model, E::Params> {
//     pub fn new()
// }
