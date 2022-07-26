//! Module containing scrollable version of [`GridView`].

use crate::prelude::*;

use crate::selectable;
use crate::Entry;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_scroll_area::ScrollArea;



// ================
// === GridView ===
// ================

/// A template for [`GridView`] structure, where entry parameters and model are separate generic
/// arguments, similar to [`crate::GridViewTemplate`] - see its docs for details.
#[derive(Clone, CloneRef, Debug, Deref)]
#[clone_ref(bound = "InnerGridView: CloneRef")]
pub struct GridViewTemplate<InnerGridView> {
    area:       ScrollArea,
    #[deref]
    inner_grid: InnerGridView,
    text_layer: Layer,
}

// /// Scrollable Grid View Component.
// ///
// /// This Component displays any kind of entry `E` in a grid. It's a wrapper putting the
// /// [`crate::GridView`] into [`ScrollArea`] and updating the wrapped grid view with
// [`ScrollArea`]'s /// viewport.
// ///
// /// The FRP API of [`crate::GridView`] is exposed as [`Deref`] target. The [`scroll_frp`]
// /// method gives access to [`ScrollArea`] API.
// ///
// /// To have it working you must do same steps as in [`crate::GridView`], but instead of setting
// /// viewport you must set size of ScrollArea by calling [`resize`] method, or using `resize`
// /// endpoint of [`ScrollArea`] API.
// ///
// /// See [`crate::GridView`] docs for more info about entries instantiation and process of
// requesting /// for Models.
// pub type GridView<E> = GridViewTemplate<E, <E as Entry>::Model, <E as Entry>::Params>;

pub type GridView<E> = GridViewTemplate<crate::GridView<E>>;
pub type SelectableGridView<E> = GridViewTemplate<selectable::GridView<E>>;

impl<InnerGridView> GridViewTemplate<InnerGridView> {
    /// Create new Scrollable Grid View component.
    pub fn new_wrapping<E>(app: &Application, inner_grid: InnerGridView) -> Self
    where
        E: Entry,
        InnerGridView: AsRef<crate::GridView<E>> + display::Object, {
        let area = ScrollArea::new(app);
        let base_grid = inner_grid.as_ref();
        area.content().add_child(&inner_grid);
        let network = base_grid.network();
        let text_layer = area.content_layer().create_sublayer();
        base_grid.set_text_layer(Some(text_layer.downgrade()));

        frp::extend! { network
            base_grid.set_viewport <+ area.viewport;
            area.set_content_width <+ base_grid.content_size.map(|s| s.x);
            area.set_content_height <+ base_grid.content_size.map(|s| s.y);
        }

        Self { area, inner_grid, text_layer }
    }

    /// Resize the component. It's a wrapper for [`scroll_frp`]`().resize`.
    pub fn resize(&self, new_size: Vector2) {
        self.area.resize(new_size);
    }

    /// Access the [`ScrollArea`] FRP API. This way you can read scroll position, resize component
    /// or jump at position.
    pub fn scroll_frp(&self) -> &ensogl_scroll_area::Frp {
        self.area.deref()
    }
}

impl<E: Entry> GridView<E> {
    pub fn new(app: &Application) -> Self {
        Self::new_wrapping(app, crate::GridView::new(app))
    }
}

impl<E: Entry> SelectableGridView<E> {
    pub fn new(app: &Application) -> Self {
        Self::new_wrapping(app, selectable::GridView::new(app))
    }
}

impl<InnerGridView> display::Object for GridViewTemplate<InnerGridView> {
    fn display_object(&self) -> &display::object::Instance {
        self.area.display_object()
    }
}
