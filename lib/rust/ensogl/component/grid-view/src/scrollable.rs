//! Module containing scrollable version of [`GridView`].

use crate::prelude::*;

use crate::header;
use crate::selectable;
use crate::Entry;

use crate::header::WeakLayers;
use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::Layer;
use ensogl_scroll_area::ScrollArea;


// ================
// === GridView ===
// ================

/// A template for [`GridView`] and [`SelectableGridView`] structures, parametrized by the
/// exact GridView implementation inside scroll area.
#[derive(Clone, CloneRef, Debug, Deref)]
#[clone_ref(bound = "InnerGridView: CloneRef")]
pub struct GridViewTemplate<InnerGridView> {
    area:              ScrollArea,
    #[deref]
    inner_grid:        InnerGridView,
    text_layer:        Layer,
    header_layer:      Immutable<Option<Layer>>,
    header_text_layer: Immutable<Option<Layer>>,
}

/// Scrollable Grid View Component.
///
/// This Component displays any kind of entry `E` in a grid. It's a wrapper putting the
/// [`crate::GridView`] into [`ScrollArea`] and updating the wrapped grid view with [`ScrollArea`]'s
/// viewport.
///
/// The FRP API of [`crate::GridView`] is exposed as [`Deref`] target. The [`scroll_frp`]
/// method gives access to [`ScrollArea`] API.
///
/// To have it working you must do same steps as in [`crate::GridView`], but instead of setting
/// viewport you must set size of ScrollArea by calling [`resize`] method, or using `resize`
/// endpoint of [`ScrollArea`] API.
///
/// See [`crate::GridView`] docs for more info about entries instantiation and process of requesting
/// for Models.
pub type GridView<E> = GridViewTemplate<crate::GridView<E>>;

/// Scrollable Grid View with Headers
///
/// This Component displays any kind of entry `E` in a grid, where each column is organized in
/// sections. The headers of each section will remain visible during scrolling down.
///
/// Essentially, it's a [scrollable `GridView`](GridView) wrapping the
/// [`GridView` with headers](selectable::GridView). See their respective documentations for usage
/// information.
///
/// **Important** After construction, the Scroll Area content will have sub-layers for headers
/// already set up. There is no need of calling [`header::Frp::set_layers`] endpoint.
pub type GridViewWithHeaders<E, H> = GridViewTemplate<header::GridView<E, H>>;

/// Scrollable and Selectable Grid View Component.
///
/// This Component displays any kind of entry `E` in a grid, inside the Scroll area and allowing
/// displaying highlights for hovered and selected entries.
///
/// Essentially, it's a [scrollable `GridView`](GridView) wrapping the
/// [selectable `GridView`](selectable::GridView). See their respective documentations for usage
/// information.
pub type SelectableGridView<E> = GridViewTemplate<selectable::GridView<E>>;

/// Scrollable and Selectable Grid View Component With Headers.
///
/// This Component displays any kind of entry `E` in a grid, inside the Scroll area and allowing
/// displaying highlights for hovered and selected entries. Each column is organized in
/// sections. The headers of each section will remain visible during scrolling down
///
/// This is a most feature-rich Grid View versin in this crate, thus the most complex.
/// Inside it is a [scrollable `GridView`](GridView) wrapping the
/// [selectable `GridView`](selectable::GridViewWithHeaders) version
/// [with headers](header::GridView) See their respective documentations for usage information.
///
/// **Important** After construction, the Scroll Area content will have sub-layers for headers
/// already set up. There is no need of calling [`header::Frp::set_layers`] endpoint.
pub type SelectableGridViewWithHeaders<E, H> =
    GridViewTemplate<selectable::GridViewWithHeaders<E, H>>;

impl<InnerGridView> GridViewTemplate<InnerGridView> {
    /// Create new Scrollable Grid View component wrapping a created instance of `inner_grid`.
    pub fn new_wrapping<E>(app: &Application, inner_grid: InnerGridView) -> Self
    where
        E: Entry,
        InnerGridView: AsRef<crate::GridView<E>> + display::Object, {
        let area = ScrollArea::new(app);
        let base_grid = inner_grid.as_ref();
        area.content().add_child(&inner_grid);
        let network = base_grid.network();
        let text_layer = area.content_layer().create_sublayer();
        let header_layer = default();
        let header_text_layer = default();
        base_grid.set_text_layer(Some(text_layer.downgrade()));

        frp::extend! { network
            base_grid.set_viewport <+ area.viewport;
            area.set_content_width <+ base_grid.content_size.map(|s| s.x);
            area.set_content_height <+ base_grid.content_size.map(|s| s.y);
        }

        Self { area, inner_grid, text_layer, header_layer, header_text_layer }
    }

    /// Create new Scrollable Grid View component wrapping a created instance of `inner_grid` which
    /// is a variant of Grid View with Headers.
    ///
    /// The Scroll Area content will have created sub-layers for headers. There is no need of
    /// calling [`header::Frp::set_layers`] endpoint.
    pub fn new_wrapping_with_headers<E, NestedGridView, Header>(
        app: &Application,
        inner_grid: InnerGridView,
    ) -> Self
    where
        E: Entry,
        Header: Entry,
        InnerGridView: AsRef<crate::GridView<E>>
            + AsRef<
                header::GridViewTemplate<E, NestedGridView, Header, Header::Model, Header::Params>,
            > + display::Object,
    {
        let mut this = Self::new_wrapping(app, inner_grid);
        let header_grid: &header::GridViewTemplate<_, _, _, _, _> = this.inner_grid.as_ref();
        let header_frp = header_grid.header_frp();
        let header_layer = this.area.content_layer().create_sublayer();
        let header_text_layer = this.area.content_layer().create_sublayer();
        header_frp.set_layers(WeakLayers::new(&header_layer, Some(&header_text_layer)));
        this.header_layer = Immutable(Some(header_layer));
        this.header_text_layer = Immutable(Some(header_text_layer));
        this
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
    /// Create new scrollable [`GridView`] component.
    pub fn new(app: &Application) -> Self {
        Self::new_wrapping(app, crate::GridView::new(app))
    }
}

impl<E: Entry, H: Entry<Params = E::Params>> GridViewWithHeaders<E, H> {
    /// Create new scrollable [`SelectableGridView`] component.
    pub fn new(app: &Application) -> Self {
        Self::new_wrapping_with_headers(app, header::GridView::new(app))
    }
}

impl<E: Entry> SelectableGridView<E> {
    /// Create new scrollable [`SelectableGridView`] component.
    pub fn new(app: &Application) -> Self {
        Self::new_wrapping(app, selectable::GridView::new(app))
    }
}

impl<E: Entry, H: Entry<Params = E::Params>> SelectableGridViewWithHeaders<E, H> {
    /// Create new scrollable [`SelectableGridView`] component.
    pub fn new(app: &Application) -> Self {
        Self::new_wrapping_with_headers(app, selectable::GridViewWithHeaders::new(app))
    }
}

impl<InnerGridView> display::Object for GridViewTemplate<InnerGridView> {
    fn display_object(&self) -> &display::object::Instance {
        self.area.display_object()
    }
}
