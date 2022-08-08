//! A module containing the single highlight (selection or hover) [`Handler`] used in
//! [`crate::selectable::GridView`].

use crate::prelude::*;

use crate::entry;
use crate::header;
use crate::selectable::highlight::connected_entry::ConnectedEntry;
use crate::selectable::highlight::connected_entry::EntryEndpoints;
use crate::selectable::highlight::shape::HoverAttrSetter;
use crate::selectable::highlight::shape::SelectionAttrSetter;
use crate::Col;
use crate::Entry;
use crate::Row;

use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::scene::layer::WeakLayer;
use ensogl_core::Animation;


// ==============
// === Export ===
// ==============

pub mod connected_entry;
pub mod layer;
pub mod shape;



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! { <EntryParams: (frp::node::Data)>
    Input {
        entry_highlighted(Option<(Row, Col)>),
        setup_masked_layer(Option<WeakLayer>),
        set_entries_params(EntryParams),
    }
    Output {
        entries_params(EntryParams),
        position(Vector2),
        contour(entry::Contour),
        color(color::Rgba),
        is_masked_layer_set(bool),
    }
}


/// All highlight animations.
#[derive(Clone, CloneRef, Debug)]
pub struct Animations {
    position_jump:  Animation<Vector2>,
    position:       frp::Stream<Vector2>,
    size:           Animation<Vector2>,
    corners_radius: Animation<f32>,
    color:          Animation<color::Rgba>,
}

impl Animations {
    /// Set up the animations and connect their targets with handler's FRP.
    fn new<EntryParams: frp::node::Data>(frp: &Frp<EntryParams>) -> Self {
        let network = frp.network();
        let position_jump = Animation::<Vector2>::new(network);
        let size = Animation::<Vector2>::new(network);
        let corners_radius = Animation::<f32>::new(network);
        let color = Animation::<color::Rgba>::new(network);

        frp::extend! { network
            init <- source_();
            position <- all_with(&position_jump.value, &frp.position, |jump, pos| pos + jump);
            size.target <+ frp.contour.map(|&c| c.size);
            corners_radius.target <+ frp.contour.map(|&c| c.corners_radius);
            color.target <+ frp.color;

            // Skip animations when the highlight is not visible.
            size_val <- all(init, size.value)._1();
            not_visible <- size_val.map(|sz| sz.x < f32::EPSILON || sz.y < f32::EPSILON);
            corners_radius.skip <+ frp.color.gate(&not_visible).constant(());
            position_jump.skip <+ frp.position.gate(&not_visible).constant(());
            color.skip <+ frp.color.gate(&not_visible).constant(());
        }
        init.emit(());
        Self { position_jump, position, size, corners_radius, color }
    }
}



// =============
// === Model ===
// =============

/// Function returning [`EntryEndpoints`] of given entry, used as parameter of highlight
/// [`Handler`].
pub type EntryEndpointsGetter<Entry> = fn(&Entry) -> EntryEndpoints;
pub type EntryGetter<Entry: 'static, EntryModel: 'static, EntryParams: 'static> =
    impl connected_entry::Getter;
pub type HeaderGetter<Entry: 'static, EntryModel: 'static, EntryParams: 'static> =
    impl connected_entry::Getter;

/// The inner data structure for [`Handler`].
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Data<Entry: 'static, EntryModel: frp::node::Data, EntryParams: frp::node::Data> {
    app:              Application,
    grid:             crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
    connected_entry:  Rc<ConnectedEntry<EntryGetter<Entry, EntryModel, EntryParams>>>,
    connected_header: Rc<ConnectedEntry<HeaderGetter<Entry, EntryModel, EntryParams>>>,
    animations:       Animations,
    #[derivative(Debug = "ignore")]
    layers:           RefCell<Option<layer::Handler<Entry, EntryModel, EntryParams>>>,
}

impl<Entry, EntryModel, EntryParams> Data<Entry, EntryModel, EntryParams>
where
    Entry: CloneRef + 'static,
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    fn new(
        app: &Application,
        grid: &crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
        connected_entry_out: &connected_entry::Output,
        connected_header_out: &connected_entry::Output,
        animations: Animations,
        entry_endpoints_getter: EntryEndpointsGetter<Entry>,
    ) -> Self {
        let entry_getter = f!((r, c) grid.get_entry(r, c).map(|e| entry_endpoints_getter(&e)));
        let header_getter = f!((r, c) grid.get_header(r, c).map(|e| entry_endpoints_getter(&e)));
        Self {
            app: app.clone_ref(),
            grid: grid.clone_ref(),
            connected_entry: ConnectedEntry::new(entry_getter, connected_entry_out.clone_ref()),
            connected_header: ConnectedEntry::new(header_getter, connected_header_out.clone_ref()),
            animations,
            layers: default(),
        }
    }
}

impl<E: Entry> Data<E, E::Model, E::Params> {
    fn setup_masked_layer(
        &self,
        layer: Option<&WeakLayer>,
        entries_params: frp::Stream<E::Params>,
    ) -> bool {
        self.layers.take();
        let new_layers = layer.and_then(|l| l.upgrade()).map(|layer| {
            let layers = layer::Handler::new(&self.app, &layer, &self.grid);
            let shape = &layers.shape;
            let network = layers.grid.network();
            let highlight_grid_frp = layers.grid.frp();
            self.connect_with_shape::<HoverAttrSetter>(network, shape, false, None);
            frp::extend! { network
                highlight_grid_frp.set_entries_params <+ entries_params;
            }
            HoverAttrSetter::set_color(shape, color::Rgba::black());
            SelectionAttrSetter::set_size(shape, default());
            layers
        });
        let is_layer_set = new_layers.is_some();
        *self.layers.borrow_mut() = new_layers;
        is_layer_set
    }

    /// Update the highlight shape using given [`shape::AttrSetter`].
    ///
    /// This function is used both for updating highlight shape in case of _Basic_ highlight mode,
    /// or updating the mask of the highlight layer in case of _Masked Layer_ mode (See
    /// [`crate::selectable::GridView`] docs for more information about highlight modes).
    fn connect_with_shape<Setter: shape::AttrSetter>(
        &self,
        network: &frp::Network,
        shape: &shape::View,
        connect_color: bool,
        hide: Option<&frp::Stream<bool>>,
    ) {
        let grid_frp = self.grid.frp();
        frp::extend! { network
            init <- source_();
            position <- all(init, self.animations.position)._1();
            size <- all(init, self.animations.size.value)._1();
            corners_radius <- all(init, self.animations.corners_radius.value)._1();
        }
        let size = if let Some(hide) = hide {
            frp::extend! { network
                size <- all_with(&size, hide, |sz, hidden| if *hidden { default() } else { *sz });
            }
            size
        } else {
            size
        };
        frp::extend! { network
            pos_and_viewport <- all(position, grid_frp.viewport);
            eval pos_and_viewport ([shape](&(pos, vp)) Setter::set_position(&shape, pos, vp));
            eval size ([shape](&size) Setter::set_size(&shape, size));
            eval corners_radius ([shape](&r) Setter::set_corners_radius(&shape, r));
        }
        if connect_color {
            frp::extend! { network
                color <- all(init, self.animations.color.value)._1();
                eval color ([shape](&color) Setter::set_color(&shape, color));
            }
        }
        init.emit(())
    }
}



// ===============
// === Handler ===
// ===============

/// The Highlight Handler.
///
/// This is a helper structure for [`selectable::GridView`] which handles a single highlight:
/// selection or hover:
/// * It provides an FRP API for given highlight, where the exact position, contour and color may be
///   read, and the _Masked layer_ highlight mode may be configured (see the [grid
///   view](`selectable::GridView`) documentation for more info about modes.
/// * It can be connected to highlight shape, so it's position, contour and color will be updated.
/// * It sets/unsets the proper flag on [`Entry`] instance (`set_selected` or `set_hovered`).
#[allow(missing_docs)]
#[derive(CloneRef, Debug, Derivative, Deref)]
#[derivative(Clone(bound = ""))]
pub struct Handler<Entry: 'static, EntryModel: frp::node::Data, EntryParams: frp::node::Data> {
    #[deref]
    pub frp: Frp<EntryParams>,
    model:   Rc<Data<Entry, EntryModel, EntryParams>>,
}

impl<E: Entry> Handler<E, E::Model, E::Params> {
    /// Create new Handler.
    ///
    /// This is a generic constructor: the exact entry endpoints handled by it should be specified
    /// by `entry_endpoints_getter`, and the `entry_highlighted` endpoint in returned handler is
    /// not connected.
    ///
    /// Use [`new_for_selection_connected`] or [`new_for_hover_connected`] to create a fully working
    /// handler for specific highlight.
    pub fn new(
        app: &Application,
        grid: &crate::GridView<E>,
        entry_endpoints_getter: EntryEndpointsGetter<E>,
    ) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let animations = Animations::new(&frp);
        let connected_entry_out = connected_entry::Output::new(&network);
        let connected_header_out = connected_entry::Output::new(&network);
        let out = &frp.private.output;
        let model = Rc::new(Data::new(
            app,
            grid,
            &connected_entry_out,
            &connected_header_out,
            animations.clone_ref(),
            entry_endpoints_getter,
        ));
        let grid_frp = grid.frp();
        let headers_frp = grid.headers_frp();
        frp::extend! {network
            shown_with_highlighted <-
                grid_frp.entry_shown.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            highlighted_is_shown <-
                shown_with_highlighted.filter_map(|(sh, hlt)| hlt.contains(sh).and_option(Some(*hlt)));
            should_reconnect_entry <- any(frp.entry_highlighted, highlighted_is_shown);
            eval should_reconnect_entry ((loc) model.connected_entry.connect_new_highlighted_entry(*loc));

            header_shown_with_highlighted <-
                headers_frp.header_shown.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            highlighted_header_is_shown <-
                header_shown_with_highlighted.filter_map(|(sh, hlt)| hlt.contains(sh).and_option(Some(*hlt)));
            should_reconnect_header <- any(frp.entry_highlighted, highlighted_header_is_shown);
            trace should_reconnect_header;
            eval should_reconnect_header ((loc) model.connected_header.connect_new_highlighted_entry(*loc));
            header_hidden_with_highlighted <-
                headers_frp.header_hidden.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            should_disconnect_header <- header_hidden_with_highlighted.filter_map(|(hd, hlt)| hlt.contains(hd).and_option(*hlt));
            eval_ should_disconnect_header (model.connected_header.drop_guard());

            became_highlighted <- frp.entry_highlighted.filter_map(|l| *l);
            position_after_highlight <- became_highlighted.map(
                f!((&(row, col)) model.grid.header_or_entry_position(row, col))
            );
            position_after_header_disconnect <- should_disconnect_header.map(
                f!((&(row, col)) model.grid.header_or_entry_position(row, col))
            );
            highligthed_header_pos_change <- headers_frp.header_position_changed.map2(
                &frp.entry_highlighted,
                |&(row, col, pos), h| h.contains(&(row, col)).as_some(pos)
            );
            highligthed_header_pos_change <- highligthed_header_pos_change.filter_map(|p| *p);
            out.position <+ position_after_highlight;
            out.position <+ position_after_header_disconnect;
            out.position <+ highligthed_header_pos_change;
            prev_position <- out.position.previous();
            new_jump <- position_after_highlight.map2(&prev_position, |pos, prev| prev - pos);
            animations.position_jump.target <+ new_jump;
            animations.position_jump.skip <+ new_jump.constant(());
            animations.position_jump.target <+ new_jump.constant(Vector2(0.0, 0.0));

            let header_connected = &connected_header_out.is_entry_connected;
            out.contour <+ all_with3(
                header_connected,
                &connected_entry_out.contour,
                &connected_header_out.contour,
                |&from_header, &entry, &header| if from_header {header} else {entry}
            );
            out.color <+ all_with3(
                header_connected,
                &connected_entry_out.color,
                &connected_header_out.color,
                |&from_header, &entry, &header| if from_header {header} else {entry}
            );

            none_highlightd <- frp.entry_highlighted.filter(|opt| opt.is_none()).constant(());
            out.contour <+ none_highlightd.constant(default());

            out.entries_params <+ frp.set_entries_params;
            let entries_params = out.entries_params.clone_ref();
            out.is_masked_layer_set <+
                frp.setup_masked_layer.map(f!([model, entries_params](layer) model.setup_masked_layer(layer.as_ref(), (&entries_params).into())));

        }

        Self { frp, model }
    }

    /// Create selection highlight handler.
    ///
    /// The returned handler will have `entry_highlighted` properly connected to the passed `grid`.
    pub fn new_for_selection_connected(app: &Application, grid: &crate::GridView<E>) -> Self {
        let this = Self::new(app, grid, Self::selection_endpoints_getter);
        this.entry_highlighted.attach(&grid.entry_selected);
        this
    }

    /// Create hover highlight handler.
    ///
    /// The returned handler will have `entry_highlighted` properly connected to the passed `grid`.
    pub fn new_for_hover_connected(app: &Application, grid: &crate::GridView<E>) -> Self {
        let this = Self::new(app, grid, Self::hover_endpoints_getter);
        this.entry_highlighted.attach(&grid.entry_hovered);
        this
    }

    fn selection_endpoints_getter(entry: &E) -> EntryEndpoints {
        let frp = entry.frp();
        EntryEndpoints {
            flag:     frp.set_selected.clone_ref(),
            location: frp.set_location.clone_ref().into(),
            contour:  frp.highlight_contour.clone_ref().into(),
            color:    frp.selection_highlight_color.clone_ref().into(),
        }
    }

    fn hover_endpoints_getter(entry: &E) -> EntryEndpoints {
        let frp = entry.frp();
        EntryEndpoints {
            flag:     frp.set_hovered.clone_ref(),
            location: frp.set_location.clone_ref().into(),
            contour:  frp.highlight_contour.clone_ref().into(),
            color:    frp.hover_highlight_color.clone_ref().into(),
        }
    }

    /// Connects shape with the handler.
    ///
    /// The shape will be updated (using the specified `shape::AttrSetter`) according to the
    /// `position`, `contour` and `color` outputs.
    pub fn connect_with_shape<Setter: shape::AttrSetter>(&self, shape: &shape::View) {
        let network = self.frp.network();
        let shape_hidden = (&self.frp.is_masked_layer_set).into();
        self.model.connect_with_shape::<Setter>(network, shape, true, Some(&shape_hidden));
    }
}
