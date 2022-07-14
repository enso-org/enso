use crate::prelude::*;

use crate::Entry;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_scroll_area::ScrollArea;

#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct GridViewTemplate<E: 'static, M: frp::node::Data, P: frp::node::Data> {
    area: ScrollArea,
    #[deref]
    grid: crate::GridViewTemplate<E, M, P>,
}

pub type GridView<E> = GridViewTemplate<E, <E as Entry>::Model, <E as Entry>::Params>;

impl<E: Entry> GridView<E> {
    pub fn new(app: &Application) -> Self {
        let area = ScrollArea::new(app);
        let grid = crate::GridView::<E>::new(app);
        area.content().add_child(&grid);
        let network = grid.network();

        frp::extend! { network
            grid.set_viewport <+ area.viewport;
            area.set_content_width <+ grid.content_size.map(|s| s.x);
            area.set_content_height <+ grid.content_size.map(|s| s.y);
        }

        Self { area, grid }
    }

    pub fn scroll_frp(&self) -> &ensogl_scroll_area::Frp {
        self.area.deref()
    }
}

impl<E, M: frp::node::Data, P: frp::node::Data> display::Object for GridViewTemplate<E, M, P> {
    fn display_object(&self) -> &display::object::Instance {
        self.area.display_object()
    }
}
