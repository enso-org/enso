use ensogl_core::application::Application;
use ensogl_core::display::scene::Layer;
use ensogl_core::display::shape::HOVER_COLOR;
use ensogl_scroll_area::ScrollArea;
use crate::{Entry, EntryFrp};
use crate::prelude::*;

pub struct HighlightStyle {
    pub size:          Vector2,
    pub corner_radius: f32,
}

pub mod highlight {
    use super::*;

    /// A padding added to the background rectangle to avoid antialiasing glitches.
    pub const PADDING_PX: f32 = 5.0;

    ensogl_core::define_shape_system! {
        (style:Style, corner_radius: f32, color: Vector4) {
            let shape_width  : Var<Pixels> = "input_size.x".into();
            let shape_height : Var<Pixels> = "input_size.y".into();
            let width = shape_width - 2.0.px() * PADDING_PX;
            let height = shape_height - 2.0.px() * PADDING_PX;
            Rect((width, height)).radius(corner_radius.px()).fill(color).into()
        }
    }
}

pub struct EntryWithOverlay<Entry> {
    entry: Entry,
    overlay: highlight::View,
}

impl<E: Entry> Entry for EntryWithOverlay<E> {
    type Model = E::Model;
    type Params = E::Params;

    fn new(app: &Application, text_layer: &Option<Layer>) -> Self {
        let overlay = highlight::View::new(Logger::new("grid_view::selectable::EntryWithOverlay"));
        overlay.color.set(HOVER_COLOR.into());
        let entry = E::new(app, text_layer);
        let frp = entry.frp();
        let network = frp.network();
        frp::extend! { network
            eval entry.highlight_shape ([overlay](shape) {
                let padding = Vector2(highlight::PADDING_PX, highlight::PADDING_PX) * 2.0;
                overlay.size.set(shape.size + padding);
                overlay.corner_radius.set(shape.corner_radius);
            });
        }
    }

    fn frp(&self) -> &EntryFrp<Self> {
        self.entry.frp()
    }
}

#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Model<
    Entry: 'static,
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
> {
    grid:       crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
    active_highlight: highlight::View,
    hovered_highlight: highlight::View,
}

impl<E: Entry> Model<E, E::Model, E::Params> {
    pub fn new()
}
