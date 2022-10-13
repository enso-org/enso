//! A module containing the single highlight (selection or hover) [`Handler`] used in
//! [`crate::selectable::GridView`].

use crate::prelude::*;

use crate::entry;
use crate::header;
use crate::selectable::highlight::connected_entry::ConnectedEntry;
use crate::selectable::highlight::connected_entry::EndpointsGetter;
use crate::selectable::highlight::layer::HasConstructor as TRAIT_HasConstructor;
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



// ============
// === Kind ===
// ============

/// A module with marker types for each highlight kind. Those structs may implement various traits
/// with operations specific for given highlight, for example [shape::AttrSetter] or
/// [connected_entry::EndpointsGetter].
#[allow(missing_docs, missing_copy_implementations)]
pub mod kind {
    #[derive(Debug)]
    pub struct Selection;
    #[derive(Debug)]
    pub struct Hover;
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! { <EntryParams: (frp::node::Data)>
    Input {
        entry_highlighted(Option<(Row, Col)>),
        setup_masked_layer(Option<WeakLayer>),
        set_entries_params(EntryParams),
        skip_animations(),
    }
    Output {
        entries_params(EntryParams),
        position(Vector2),
        contour(entry::Contour),
        color(color::Lcha),
        /// Used in [Grid Views with headers](header::GridView). The y position of line separating
        /// the header of scrolled down section from the rest of entries. If other entry than
        /// this header is selected, the highlight should be top-clipped at this position.
        header_separator(f32),
        /// The y position of line clipping the highlight from the top. Usually it's just the top
        /// of the viewport, but may be a lower line in case where a header in
        /// [Grid Views with headers](header::GridView) should cover part of the highlight.
        top_clip(f32),
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
    color:          color::Animation,
    top_clip_jump:  Animation<f32>,
    top_clip:       frp::Stream<f32>,
}

impl Animations {
    /// Set up the animations and connect their targets with handler's FRP.
    fn new<EntryParams: frp::node::Data>(frp: &Frp<EntryParams>) -> Self {
        let network = frp.network();
        let position_jump = Animation::<Vector2>::new(network);
        let size = Animation::<Vector2>::new(network);
        let corners_radius = Animation::<f32>::new(network);
        let color = color::Animation::new(network);
        let top_clip_jump = Animation::<f32>::new(network);

        frp::extend! { network
            init <- source_();
            position <- all_with(&position_jump.value, &frp.position, |jump, pos| pos + jump);
            size.target <+ frp.contour.map(|&c| c.size);
            corners_radius.target <+ frp.contour.map(|&c| c.corners_radius);
            color.target <+ frp.color;
            top_clip <- all_with(&top_clip_jump.value, &frp.top_clip, |jump, clip| clip + jump);

            // Skip animations when the highlight is not visible.
            size_val <- all(init, size.value)._1();
            not_visible <- size_val.map(|sz| sz.x < f32::EPSILON || sz.y < f32::EPSILON);
            corners_radius.skip <+ frp.color.gate(&not_visible).constant(());
            position_jump.skip <+ frp.position.gate(&not_visible).constant(());
            color.skip <+ frp.color.gate(&not_visible).constant(());
            size.skip <+ frp.private.input.skip_animations;
            corners_radius.skip <+ frp.private.input.skip_animations;
            color.skip <+ frp.private.input.skip_animations;
            top_clip_jump.skip <+ frp.private.input.skip_animations;
        }
        init.emit(());
        Self { position_jump, position, size, corners_radius, color, top_clip_jump, top_clip }
    }
}



// =============
// === Model ===
// =============

/// The inner data structure for [`Handler`].
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Data<InnerGridView, Entry> {
    app:        Application,
    grid:       InnerGridView,
    animations: Animations,
    #[derivative(Debug = "ignore")]
    layers:     RefCell<Option<layer::Handler<InnerGridView>>>,
    entry_type: PhantomData<Entry>,
}

impl<InnerGridView, Entry> Data<InnerGridView, Entry>
where InnerGridView: CloneRef
{
    fn new(app: &Application, grid: &InnerGridView, animations: Animations) -> Self {
        Self {
            app: app.clone_ref(),
            grid: grid.clone_ref(),
            animations,
            layers: default(),
            entry_type: default(),
        }
    }
}

impl<InnerGrid, E: Entry> Data<InnerGrid, E>
where InnerGrid: AsRef<crate::GridView<E>>
{
    fn setup_masked_layer(
        &self,
        layer: Option<&WeakLayer>,
        entries_params: frp::Stream<E::Params>,
    ) -> bool
    where
        layer::Handler<InnerGrid>: layer::HasConstructor<InnerGrid = InnerGrid>,
    {
        self.layers.take();
        let new_layers = layer.and_then(|l| l.upgrade()).map(|layer| {
            let layers = layer::Handler::<InnerGrid>::new(&self.app, &layer, &self.grid);
            let shape = &layers.shape;
            let network = layers.grid.as_ref().network();
            let highlight_grid_frp = layers.grid.as_ref().frp();
            self.connect_with_shape::<kind::Hover>(network, shape, false, None, None);
            frp::extend! { network
                highlight_grid_frp.set_entries_params <+ entries_params;
            }
            kind::Hover::set_color(shape, color::Lcha::black());
            kind::Selection::set_size(shape, default());
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
        bottom_clip: Option<&frp::Stream<f32>>,
    ) {
        let grid_frp = self.grid.as_ref().frp();
        frp::extend! { network
            init <- source_();
            position <- all(init, self.animations.position)._1();
            size <- all(init, self.animations.size.value)._1();
            corners_radius <- all(init, self.animations.corners_radius.value)._1();
            top_clip <- all(init, self.animations.top_clip)._1();
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
            top_clip_and_viewport <- all(top_clip, grid_frp.viewport);
            eval top_clip_and_viewport ([shape](&(c, v)) Setter::set_top_clip(&shape, c, v));
        }
        if connect_color {
            frp::extend! { network
                color <- all(init, self.animations.color.value)._1();
                eval color ([shape](&color) Setter::set_color(&shape, color));
            }
        }
        if let Some(bottom_clip) = bottom_clip {
            frp::extend! {network
                bottom_clip <- all(&init, bottom_clip)._1();
                bottom_clip_and_viewport <- all(bottom_clip, grid_frp.viewport);
                eval bottom_clip_and_viewport ([shape](&(c, v)) Setter::set_bottom_clip(&shape, c, v));
            }
        }
        init.emit(())
    }

    fn entry_highlight_position(&self, row: Row, col: Col) -> Vector2 {
        let grid = self.grid.as_ref();
        let entry = grid.get_entry(row, col);
        let offset = entry.map(|e| e.frp().highlight_contour_offset.value());
        grid.entry_position(row, col) + offset.unwrap_or_default()
    }
}

impl<E: Entry, Header: Entry<Params = E::Params>> Data<header::GridView<E, Header>, E> {
    fn header_or_entry_highlight_position(&self, row: Row, col: Col) -> Vector2 {
        let header_pos = self.grid.header_position(row, col).map(|pos| {
            let header = self.grid.get_header(row, col);
            let offset = header.map(|e| e.frp().highlight_contour_offset.value());
            pos + offset.unwrap_or_default()
        });
        header_pos.unwrap_or_else(|| self.entry_highlight_position(row, col))
    }
}



// ===============
// === Handler ===
// ===============

/// The Highlight Handler.
///
/// This is a helper structure for [`selectable::GridView`] which handles a single highlight:
/// selection or hover:
/// * It provides an FRP API for given highlight, where the exact position, contour, color and
///   clipping may be read, and the _Masked layer_ highlight mode may be configured (see the [grid
///   view](`selectable::GridView`) documentation for more info about modes).
/// * It can be connected to highlight shape, so it's position, contour, color and clipping will be
///   updated.
/// * It sets/unsets the proper flag on [`Entry`] instance (`set_selected` or `set_hovered`).
///
/// The `Kind` parameter should be any from [`kind`] module.
#[allow(missing_docs)]
#[derive(CloneRef, Debug, Derivative, Deref)]
#[derivative(Clone(bound = ""))]
pub struct Handler<Kind, InnerGridView, Entry, EntryParams: frp::node::Data> {
    #[deref]
    pub frp:   Frp<EntryParams>,
    model:     Rc<Data<InnerGridView, Entry>>,
    kind_type: PhantomData<Kind>,
}

/// The handler of selection highlight.
pub type SelectionHandler<InnerGridView, Entry, EntryParams> =
    Handler<kind::Selection, InnerGridView, Entry, EntryParams>;

/// The handler of mouse hover highlight.
pub type HoverHandler<InnerGridView, Entry, EntryParams> =
    Handler<kind::Hover, InnerGridView, Entry, EntryParams>;

impl<InnerGridView, E: Entry> SelectionHandler<InnerGridView, E, E::Params>
where InnerGridView: AsRef<crate::GridView<E>>
{
    /// Create selection highlight handler.
    ///
    /// The returned handler will have `entry_highlighted` properly connected to the passed `grid`.
    pub fn new_connected(app: &Application, grid: &InnerGridView) -> Self
    where Self: HasConstructor<InnerGridView = InnerGridView> {
        let this = Self::new(app, grid);
        this.entry_highlighted.attach(&grid.as_ref().entry_selected);
        this
    }
}

impl<InnerGridView, E: Entry> HoverHandler<InnerGridView, E, E::Params>
where InnerGridView: AsRef<crate::GridView<E>>
{
    /// Create hover highlight handler.
    ///
    /// The returned handler will have `entry_highlighted` properly connected to the passed `grid`.
    pub fn new_connected(app: &Application, grid: &InnerGridView) -> Self
    where Self: HasConstructor<InnerGridView = InnerGridView> {
        let this = Self::new(app, grid);
        this.entry_highlighted.attach(&grid.as_ref().entry_hovered);
        this
    }
}

impl<Kind: shape::AttrSetter, InnerGridView, E: Entry> Handler<Kind, InnerGridView, E, E::Params>
where InnerGridView: AsRef<crate::GridView<E>>
{
    /// Connects shape with the handler.
    ///
    /// The shape will be updated (using the specified `shape::AttrSetter`) according to the
    /// `position`, `contour`, `color` and `top_clip` outputs.
    pub fn connect_with_shape(&self, shape: &shape::View) {
        let network = self.frp.network();
        let shape_hidden = (&self.frp.is_masked_layer_set).into();
        self.model.connect_with_shape::<Kind>(network, shape, true, Some(&shape_hidden), None);
    }
}

impl<Kind: shape::AttrSetter, E: Entry, HeaderEntry: Entry>
    Handler<Kind, header::GridView<E, HeaderEntry>, E, E::Params>
{
    /// Connects shape designed for highlighting headers with the handler.
    ///
    /// The shape will be updated (using the specified `shape::AttrSetter`) according to the
    /// `position`, `contour`, `color` and `top_clip` outputs, and it's bottom clip will be always
    /// set to the current column's pushed-down header bottom (the `header_separator` value of
    /// [`header::FRP`]).
    pub fn connect_with_header_shape(&self, shape: &shape::View) {
        let network = self.frp.network();
        let shape_hidden = (&self.frp.is_masked_layer_set).into();
        let bottom_clip = (&self.frp.header_separator).into();
        self.model.connect_with_shape::<Kind>(
            network,
            shape,
            true,
            Some(&shape_hidden),
            Some(&bottom_clip),
        );
    }
}

/// A trait implemented by every [`Handler`] able to be constructed.
pub trait HasConstructor {
    /// The exact type of the _inner_ Grid View.
    type InnerGridView;

    /// Create new Handler.
    ///
    /// This is a generic constructor: the exact entry endpoints handled by it should be specified
    /// by `entry_endpoints_getter`, and the `entry_highlighted` endpoint in returned handler is
    /// not connected.
    ///
    /// Use [`new_for_selection_connected`] or [`new_for_hover_connected`] to create a fully working
    /// handler for specific highlight.
    fn new(app: &Application, grid: &Self::InnerGridView) -> Self;
}

impl<Kind: EndpointsGetter, E: Entry> HasConstructor
    for Handler<Kind, crate::GridView<E>, E, E::Params>
{
    type InnerGridView = crate::GridView<E>;

    fn new(app: &Application, grid: &Self::InnerGridView) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let animations = Animations::new(&frp);
        let entry_getter =
            f!((r, c) grid.get_entry(r, c).map(|e| Kind::get_endpoints_from_entry(&e)));
        let connected_entry_out = connected_entry::Output::new(network);
        let connected_entry = ConnectedEntry::new(entry_getter, connected_entry_out.clone_ref());
        let model = Rc::new(Data::new(app, grid, animations.clone_ref()));
        let out = &frp.private.output;
        let grid_frp = grid.frp();
        frp::extend! {network

            // === Updating `connected_entry` Field ===
            shown_with_highlighted <-
                grid_frp.entry_shown.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            highlighted_is_shown <-
                shown_with_highlighted.filter_map(|(sh, hlt)| hlt.contains(sh).and_option(Some(*hlt)));
            should_reconnect_entry <- any(frp.entry_highlighted, highlighted_is_shown);
            eval should_reconnect_entry ((loc) connected_entry.connect_new_highlighted_entry(*loc));


            // === Highlight Position ===

            became_highlighted <- frp.entry_highlighted.filter_map(|l| *l);
            position_after_highlight <- became_highlighted.map(f!((&(row, col))
                model.entry_highlight_position(row, col)
            ));
            out.position <+ position_after_highlight;
            prev_position <- out.position.previous();
            new_jump <- position_after_highlight.map2(&prev_position, |pos, prev| prev - pos);
            animations.position_jump.target <+ new_jump;
            animations.position_jump.skip <+ new_jump.constant(());
            animations.position_jump.target <+ new_jump.constant(Vector2::zero());


            // === Color and Contour ===

            out.contour <+ connected_entry_out.contour;
            out.color <+ connected_entry_out.color;

            none_highlighted <- frp.entry_highlighted.filter(|opt| opt.is_none()).constant(());
            out.contour <+ none_highlighted.constant(default());


            // === Setting up Masked Layer ===

            out.entries_params <+ frp.set_entries_params;
            let entries_params = out.entries_params.clone_ref();
            out.is_masked_layer_set <+
                frp.setup_masked_layer.map(f!([model, entries_params](layer) model.setup_masked_layer(layer.as_ref(), (&entries_params).into())));
        }

        Self { frp, model, kind_type: default() }
    }
}

impl<Kind: EndpointsGetter, E: Entry, HeaderEntry: Entry<Params = E::Params>> HasConstructor
    for Handler<Kind, header::GridView<E, HeaderEntry>, E, E::Params>
{
    type InnerGridView = header::GridView<E, HeaderEntry>;

    fn new(app: &Application, grid: &Self::InnerGridView) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let animations = Animations::new(&frp);
        let entry_getter =
            f!((r, c) grid.get_entry(r, c).map(|e| Kind::get_endpoints_from_entry(&e)));
        let header_getter =
            f!((r, c) grid.get_header(r, c).map(|e| Kind::get_endpoints_from_entry(&e)));
        let connected_entry_out = connected_entry::Output::new(network);
        let connected_header_out = connected_entry::Output::new(network);
        let connected_entry = ConnectedEntry::new(entry_getter, connected_entry_out.clone_ref());
        let connected_header = ConnectedEntry::new(header_getter, connected_header_out.clone_ref());
        let model = Rc::new(Data::new(app, grid, animations.clone_ref()));
        let out = &frp.private.output;
        let grid_frp = grid.frp();
        let headers_frp = grid.header_frp();
        frp::extend! {network

            // === Updating `connected_entry` Field ===

            shown_with_highlighted <-
                grid_frp.entry_shown.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            highlighted_is_shown <-
                shown_with_highlighted.filter_map(|(sh, hlt)| hlt.contains(sh).and_option(Some(*hlt)));
            should_reconnect_entry <- any(frp.entry_highlighted, highlighted_is_shown);
            eval should_reconnect_entry ((loc) connected_entry.connect_new_highlighted_entry(*loc));


            // === Updating `connected_header` Field ===

            header_shown_with_highlighted <-
                headers_frp.header_shown.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            highlighted_header_is_shown <-
                header_shown_with_highlighted.filter_map(|(sh, hlt)| hlt.contains(sh).and_option(Some(*hlt)));
            should_reconnect_header <- any(frp.entry_highlighted, highlighted_header_is_shown);
            eval should_reconnect_header ((loc) connected_header.connect_new_highlighted_entry(*loc));
            header_hidden_with_highlighted <-
                headers_frp.header_hidden.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            should_disconnect_header <- header_hidden_with_highlighted.filter_map(|(hd, hlt)| hlt.contains(hd).and_option(*hlt));
            eval_ should_disconnect_header (connected_header.drop_guard());


            // === Highlight Position ===

            became_highlighted <- frp.entry_highlighted.filter_map(|l| *l);
            position_after_highlight <- became_highlighted.map(
                f!((&(row, col)) model.header_or_entry_highlight_position(row, col))
            );
            position_after_header_disconnect <- should_disconnect_header.map(
                f!((&(row, col)) model.header_or_entry_highlight_position(row, col))
            );
            highligthed_header_pos_change <- headers_frp.header_position_changed.map2(
                &frp.entry_highlighted,
                f!([model](&(row, col, _), h) h.contains(&(row, col)).as_some(model.header_or_entry_highlight_position(row, col)))
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


            // === Contour and Color ===

            let header_connected = connected_header_out.is_entry_connected.clone_ref();
            out.contour <+ all_with3(
                &header_connected,
                &connected_entry_out.contour,
                &connected_header_out.contour,
                |&from_header, &entry, &header| if from_header {header} else {entry}
            );
            out.color <+ all_with3(
                &header_connected,
                &connected_entry_out.color,
                &connected_header_out.color,
                |&from_header, &entry, &header| if from_header {header} else {entry}
            );

            none_highlighted <- frp.entry_highlighted.filter(|opt| opt.is_none()).constant(());
            out.contour <+ none_highlighted.constant(default());


            // === Setting up Masked Layer ===

            out.entries_params <+ frp.set_entries_params;
            let entries_params = out.entries_params.clone_ref();
            out.is_masked_layer_set <+
                frp.setup_masked_layer.map(f!([model, entries_params](layer) model.setup_masked_layer(layer.as_ref(), (&entries_params).into())));


            // === Highlight Shape Clipping ===

            highlight_column <- became_highlighted._1();
            header_hidden_in_highlight_column <- headers_frp.header_hidden.map2(
                &frp.entry_highlighted,
                |&(_, col), h| h.map_or(false, |(_, h_col)| col == h_col)
            ).filter(|v| *v).constant(());
            viewport_changed <- grid_frp.viewport.constant(());
            header_sep_changed <- any(viewport_changed, header_hidden_in_highlight_column);
            out.header_separator <+ all_with(&highlight_column, &header_sep_changed, f!((col, ()) model.grid.header_separator(*col)));
            out.top_clip <+ all_with3(&out.header_separator, &header_connected, &grid_frp.viewport, |&sep, &hc, v| if hc {v.top} else {sep});
            new_clip_jump <- header_connected.on_change().map4(
                &out.header_separator,
                &grid_frp.viewport,
                &animations.top_clip_jump.value,
                |hc, hs, v, a| if *hc {hs - v.top} else {v.top - hs} + a
            );
            animations.top_clip_jump.target <+ new_clip_jump;
            animations.top_clip_jump.skip <+ new_clip_jump.constant(());
            animations.top_clip_jump.target <+ new_clip_jump.constant(0.0);
        }

        Self { frp, model, kind_type: default() }
    }
}
