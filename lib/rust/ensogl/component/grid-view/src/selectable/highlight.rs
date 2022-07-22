pub mod layer;
pub mod shape;

use crate::prelude::*;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::scene::layer::WeakLayer;
use ensogl_core::Animation;

use crate::entry;
use crate::entry_position;
use crate::selectable::highlight::shape::HoverAttrSetter;
use crate::selectable::highlight::shape::SelectionAttrSetter;
use crate::Col;
use crate::Entry;
use crate::Row;
use ensogl_scroll_area::Viewport;


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
    }
}

pub struct EntryEndpoints {
    flag:     frp::Any<bool>,
    location: frp::Stream<(Row, Col)>,
    contour:  frp::Stream<entry::Contour>,
    color:    frp::Stream<color::Rgba>,
}

#[derive(Clone, CloneRef, Debug)]
pub struct Animations {
    position:       Animation<Vector2>,
    size:           Animation<Vector2>,
    corners_radius: Animation<f32>,
    color:          Animation<color::Rgba>,
}

impl Animations {
    fn new<EntryParams: frp::node::Data>(frp: &Frp<EntryParams>) -> Self {
        let network = frp.network();
        let position = Animation::<Vector2>::new(network);
        let size = Animation::<Vector2>::new(network);
        let corners_radius = Animation::<f32>::new(network);
        let color = Animation::<color::Rgba>::new(network);

        frp::extend! { network
            init <- source_();
            position.target <+ frp.position;
            size.target <+ frp.contour.map(|&c| c.size);
            corners_radius.target <+ frp.contour.map(|&c| c.corners_radius);
            color.target <+ frp.color;

            size_val <- all(init, size.value)._1();
            not_visible <- size_val.map(|sz| sz.x < f32::EPSILON || sz.y < f32::EPSILON);
            corners_radius.skip <+ frp.color.gate(&not_visible).constant(());
            position.skip <+ frp.position.gate(&not_visible).constant(());
            color.skip <+ frp.color.gate(&not_visible).constant(());
        }
        init.emit(());
        Self { position, size, corners_radius, color }
    }
}


// ======================
// === HighlightGuard ===
// ======================

#[derive(Debug)]
struct ConnectedEntryGuard {
    network:           frp::Network,
    should_be_dropped: frp::Stream,
    dropped:           frp::Source,
}

impl ConnectedEntryGuard {
    fn new_for_entry<EntryParams: frp::node::Data>(
        entry_frp: EntryEndpoints,
        row: Row,
        col: Col,
        frp: &api::private::Output<EntryParams>,
    ) -> Self {
        let network = frp::Network::new("HighlightedEntryGuard");
        frp::extend! { network
            init <- source_();
            dropped <- source_();
            contour <- all(init, entry_frp.contour)._1();
            color <- all(init, entry_frp.color)._1();
            entry_frp.flag <+ init.constant(true);
            entry_frp.flag <+ dropped.constant(false);
            frp.contour <+ contour;
            frp.color <+ color;
            location_change <- entry_frp.location.filter(move |loc| *loc != (row, col));
            should_be_dropped <- location_change.constant(());
        }
        init.emit(());
        Self { network, dropped: dropped.into(), should_be_dropped: should_be_dropped.into() }
    }
}

impl Drop for ConnectedEntryGuard {
    fn drop(&mut self) {
        self.dropped.emit(());
    }
}



// =============
// === Model ===
// =============

pub type EntryEndpointsGetter<Entry> = fn(&Entry) -> EntryEndpoints;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Model<Entry: 'static, EntryModel: frp::node::Data, EntryParams: frp::node::Data> {
    app:                    Application,
    grid:                   crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
    connected_entry:        Cell<Option<(Row, Col)>>,
    guard:                  RefCell<Option<ConnectedEntryGuard>>,
    output:                 api::private::Output<EntryParams>,
    animations:             Animations,
    #[derivative(Debug = "ignore")]
    entry_endpoints_getter: EntryEndpointsGetter<Entry>,
    layers:                 RefCell<Option<layer::Layers<Entry, EntryModel, EntryParams>>>,
}

impl<Entry, EntryModel, EntryParams> Model<Entry, EntryModel, EntryParams>
where
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    fn new(
        app: &Application,
        grid: &crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
        output: &api::private::Output<EntryParams>,
        animations: Animations,
        entry_endpoints_getter: EntryEndpointsGetter<Entry>,
    ) -> Self {
        Self {
            app: app.clone_ref(),
            grid: grid.clone_ref(),
            connected_entry: default(),
            guard: default(),
            output: output.clone_ref(),
            animations,
            entry_endpoints_getter,
            layers: default(),
        }
    }

    fn drop_guard(&self) {
        self.connected_entry.set(None);
        self.guard.take();
    }
}

impl<E: Entry> Model<E, E::Model, E::Params> {
    fn connect_new_highlighted_entry(self: &Rc<Self>, location: Option<(Row, Col)>) {
        if let Some((row, col)) = location {
            let current_entry = self.connected_entry.get();
            let entry_changed = current_entry.map_or(true, |loc| loc != (row, col));
            if entry_changed {
                self.guard.take();
                let entry = self.grid.get_entry(row, col);
                *self.guard.borrow_mut() = entry.map(|e| {
                    let endpoints = (self.entry_endpoints_getter)(&e);
                    ConnectedEntryGuard::new_for_entry(endpoints, row, col, &self.output)
                });
                self.connected_entry.set(Some((row, col)));
                self.set_up_guard_dropping();
            }
        } else {
            self.drop_guard()
        }
    }

    fn set_up_guard_dropping(self: &Rc<Self>) {
        if let Some(guard) = &*self.guard.borrow() {
            let network = &guard.network;
            let this = self;
            frp::extend! { network
                eval_ guard.should_be_dropped (this.drop_guard());
            }
        }
    }

    fn setup_masked_layer(&self, layer: Option<&WeakLayer>) {
        self.layers.take();
        let new_layers = layer.and_then(|l| l.upgrade()).map(|layer| {
            let layers = layer::Layers::new(&self.app, &layer, &self.grid);
            let shape = &layers.shape;
            let network = layers.grid.network();
            let highlight_grid_frp = layers.grid.frp();
            let frp = &self.output;
            self.connect_with_shape::<HoverAttrSetter>(network, shape);
            frp::extend! { network
                highlight_grid_frp.set_entries_params <+ frp.entries_params;
            }
            HoverAttrSetter::set_color(&shape, color::Rgba::black());
            SelectionAttrSetter::set_size(&shape, default());
            layers
        });
        *self.layers.borrow_mut() = new_layers;
    }

    fn connect_with_shape<Setter: shape::AttrSetter>(
        &self,
        network: &frp::Network,
        shape: &shape::View,
    ) {
        let grid_frp = self.grid.frp();
        frp::extend! { network
            init <- source_();
            position <- all(init, self.animations.position.value)._1();
            size <- all(init, self.animations.size.value)._1();
            corners_radius <- all(init, self.animations.corners_radius.value)._1();

            pos_and_viewport <- all(position, grid_frp.viewport);
            eval pos_and_viewport ([shape](&(pos, vp)) Setter::set_position(&shape, pos, vp));
            eval size ([shape](&size) Setter::set_size(&shape, size));
            eval corners_radius ([shape](&r) Setter::set_corners_radius(&shape, r));
        }
        init.emit(())
    }
}

#[derive(CloneRef, Debug, Derivative, Deref)]
#[derivative(Clone(bound = ""))]
pub struct Handler<Entry: 'static, EntryModel: frp::node::Data, EntryParams: frp::node::Data> {
    #[deref]
    pub frp: Frp<EntryParams>,
    model:   Rc<Model<Entry, EntryModel, EntryParams>>,
}

impl<E: Entry> Handler<E, E::Model, E::Params> {
    pub fn new(
        app: &Application,
        grid: &crate::GridView<E>,
        entry_endpoints_getter: EntryEndpointsGetter<E>,
    ) -> Self {
        let frp = Frp::new();
        let animations = Animations::new(&frp);
        let out = &frp.private.output;
        let model = Rc::new(Model::new(app, grid, out, animations, entry_endpoints_getter));
        let network = frp.network();
        let grid_frp = grid.frp();
        frp::extend! {network
            shown_with_highlighted <- grid_frp.entry_shown.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            highlighted_is_shown <- shown_with_highlighted.map(|(sh, hlt)| hlt.contains(sh).and_option(*hlt));
            should_reconnect <- any(frp.entry_highlighted, highlighted_is_shown);
            eval should_reconnect ((loc) model.connect_new_highlighted_entry(*loc));

            became_highlighted <- frp.entry_highlighted.filter_map(|l| *l);
            out.position <+ became_highlighted.all_with(&grid_frp.entries_size, |&(row, col), &es| entry_position(row, col, es));
            none_highlightd <- frp.entry_highlighted.filter(|opt| opt.is_none()).constant(());
            out.contour <+ none_highlightd.constant(default());

            out.entries_params <+ frp.set_entries_params;
            eval frp.setup_masked_layer ((layer) model.setup_masked_layer(layer.as_ref()));
        }

        Self { frp, model }
    }

    pub fn new_for_selection_connected(app: &Application, grid: &crate::GridView<E>) -> Self {
        let this = Self::new(app, grid, Self::selection_endpoints_getter);
        let network = this.network();
        frp::extend! { network
            this.entry_highlighted <+ grid.entry_selected;
        }
        this
    }

    pub fn new_for_hover_connected(app: &Application, grid: &crate::GridView<E>) -> Self {
        let this = Self::new(app, grid, Self::hover_endpoints_getter);
        let network = this.network();
        frp::extend! { network
            this.entry_highlighted <+ grid.entry_hovered;
        }
        this
    }

    fn selection_endpoints_getter(entry: &E) -> EntryEndpoints {
        let frp = entry.frp();
        EntryEndpoints {
            flag:     frp.set_selected.clone_ref(),
            location: frp.set_location.clone_ref().into(),
            contour:  frp.contour.clone_ref().into(),
            color:    frp.selection_highlight_color.clone_ref().into(),
        }
    }

    fn hover_endpoints_getter(entry: &E) -> EntryEndpoints {
        let frp = entry.frp();
        EntryEndpoints {
            flag:     frp.set_hovered.clone_ref(),
            location: frp.set_location.clone_ref().into(),
            contour:  frp.contour.clone_ref().into(),
            color:    frp.hover_highlight_color.clone_ref().into(),
        }
    }

    pub fn connect_with_shape<Setter: shape::AttrSetter>(&self, shape: &shape::View) {
        let network = self.frp.network();
        self.model.connect_with_shape::<Setter>(network, shape);
        frp::extend! { network
            init <- source_();
            color <- all(init, self.model.animations.color.value)._1();
            eval color ([shape](&color) Setter::set_color(&shape, color));
        }
        init.emit(());
    }
}
