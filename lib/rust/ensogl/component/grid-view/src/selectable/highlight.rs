use crate::prelude::*;
use ensogl_core::data::color;

use crate::entry;
use crate::entry_position;
use crate::Col;
use crate::Entry;
use crate::Row;
use ensogl_scroll_area::Viewport;


// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        entry_highlighted(Option<(Row, Col)>),
    }
    Output {
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
    fn new_for_entry(
        entry_frp: EntryEndpoints,
        row: Row,
        col: Col,
        frp: &api::private::Output,
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
    grid:                   crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
    connected_entry:        Cell<Option<(Row, Col)>>,
    guard:                  RefCell<Option<ConnectedEntryGuard>>,
    output:                 api::private::Output,
    #[derivative(Debug = "ignore")]
    entry_endpoints_getter: EntryEndpointsGetter<Entry>,
}

impl<Entry, EntryModel, EntryParams> Model<Entry, EntryModel, EntryParams>
where
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    fn new(
        grid: &crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
        output: &api::private::Output,
        entry_endpoints_getter: EntryEndpointsGetter<Entry>,
    ) -> Self {
        Self {
            grid: grid.clone_ref(),
            connected_entry: default(),
            guard: default(),
            output: output.clone_ref(),
            entry_endpoints_getter,
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
}

#[derive(CloneRef, Debug, Derivative, Deref)]
#[derivative(Clone(bound = ""))]
pub struct Handler<Entry: 'static, EntryModel: frp::node::Data, EntryParams: frp::node::Data> {
    #[deref]
    pub frp: Frp,
    model:   Rc<Model<Entry, EntryModel, EntryParams>>,
}

impl<E: Entry> Handler<E, E::Model, E::Params> {
    pub fn new(grid: &crate::GridView<E>, entry_endpoints_getter: EntryEndpointsGetter<E>) -> Self {
        let frp = Frp::new();
        let model = Rc::new(Model::new(grid, &frp.private.output, entry_endpoints_getter));

        let network = frp.network();
        let grid_frp = grid.frp();
        let out = &frp.private.output;
        frp::extend! {network
            shown_with_highlighted <- grid_frp.entry_shown.map2(&frp.entry_highlighted, |a, b| (*a, *b));
            highlighted_is_shown <- shown_with_highlighted.map(|(sh, hlt)| hlt.contains(sh).and_option(*hlt));
            should_reconnect <- any(frp.entry_highlighted, highlighted_is_shown);
            eval should_reconnect ((loc) model.connect_new_highlighted_entry(*loc));

            became_highlighted <- frp.entry_highlighted.filter_map(|l| *l);
            out.position <+ became_highlighted.all_with(&grid_frp.entries_size, |&(row, col), &es| entry_position(row, col, es));
            none_highlightd <- frp.entry_highlighted.filter(|opt| opt.is_none()).constant(());
            out.contour <+ none_highlightd.constant(default());
        }

        Self { frp, model }
    }

    pub fn new_for_selection_connected(grid: &crate::GridView<E>) -> Self {
        let this = Self::new(grid, Self::selection_endpoints_getter);
        let network = this.network();
        frp::extend! { network
            this.entry_highlighted <+ grid.entry_selected;
        }
        this
    }

    pub fn new_for_hover_connected(grid: &crate::GridView<E>) -> Self {
        let this = Self::new(grid, Self::hover_endpoints_getter);
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
        let frp = &self.frp.public.output;
        let grid_frp = self.model.grid.widget.frp();
        frp::extend! { network
            pos_and_viewport <- all(frp.position, grid_frp.viewport);
            eval pos_and_viewport ([shape](&(pos, vp)) Setter::set_position(&shape, pos, vp));
            eval frp.contour ([shape](&contour) {
                Setter::set_size(&shape, contour.size);
                Setter::set_corners_radius(&shape, contour.corners_radius);
            });
            eval frp.color ([shape](&color) Setter::set_color(&shape, color));
        }
    }
}


pub mod shape {
    use super::*;
    use crate::entry::Contour;
    use ensogl_core::data::color;
    use ensogl_core::data::color::Rgba;

    ensogl_core::define_shape_system! {
        pointer_events = false;
        (
            style: Style,
            // Corners radii of viewport (x), hover highlight (y) and selection highlight (z).
            corners_radii: Vector3,
            // Positions of highlights: hover (xy) and selection (zw).
            highlights_pos: Vector4,
            // Sizes of highlights: hover (xy) and selection (zw).
            highlights_sizes: Vector4,
            hover_color: Vector4,
            selection_color: Vector4,
        ) {
            let viewport_width = Var::<Pixels>::from("input_size.x");
            let viewport_height = Var::<Pixels>::from("input_size.y");
            let viewport = Rect((viewport_width, viewport_height));
            let viewport = viewport.corners_radius(corners_radii.x().px());
            let hover = Rect(highlights_sizes.xy().px()).corners_radius(corners_radii.y().px());
            let hover_trans = highlights_pos.xy().px();
            let hover = hover.translate(&hover_trans);
            let hover = (&hover * &viewport).fill(hover_color);
            let selection = Rect(highlights_sizes.zw().px()).corners_radius(corners_radii.z().px());
            let selection = hover.translate(highlights_pos.zw().px() - hover_trans);
            let selection = (&selection * &viewport).fill(selection_color);
            let highlights = &hover + &selection;
            highlights.into()
        }
    }

    pub fn set_viewport(shape: &View, viewport: Viewport) {
        shape.size.set(viewport.size());
        shape.set_position_xy(viewport.center_point());
    }

    pub trait AttrSetter {
        fn set_position(shape: &View, position: Vector2, viewport: Viewport);
        fn set_size(shape: &View, size: Vector2);
        fn set_corners_radius(shape: &View, radius: f32);
        fn set_color(shape: &View, color: color::Rgba);
    }

    pub struct HoverAttrSetter;

    impl AttrSetter for HoverAttrSetter {
        fn set_position(shape: &View, position: Vector2, viewport: Viewport) {
            let viewport_position = viewport.center_point();
            let relative_pos = position - viewport_position;
            let mut attr = shape.highlights_pos.get();
            attr.x = relative_pos.x;
            attr.y = relative_pos.y;
            shape.highlights_pos.set(attr);
        }

        fn set_size(shape: &View, size: Vector2) {
            let mut attr = shape.highlights_sizes.get();
            attr.x = size.x;
            attr.y = size.y;
            shape.highlights_sizes.set(attr)
        }

        fn set_corners_radius(shape: &View, radius: f32) {
            let mut old_radii = shape.corners_radii.get();
            old_radii.y = radius;
            shape.corners_radii.set(old_radii);
        }

        fn set_color(shape: &View, color: color::Rgba) {
            shape.hover_color.set(color.into())
        }
    }

    pub struct SelectionAttrSetter;

    impl AttrSetter for SelectionAttrSetter {
        fn set_position(shape: &View, position: Vector2, viewport: Viewport) {
            let viewport_position = viewport.center_point();
            let relative_pos = position - viewport_position;
            let mut attr = shape.highlights_pos.get();
            attr.z = relative_pos.x;
            attr.w = relative_pos.y;
            shape.highlights_pos.set(attr);
        }

        fn set_size(shape: &View, size: Vector2) {
            let mut attr = shape.highlights_sizes.get();
            attr.z = size.x;
            attr.w = size.y;
            shape.highlights_sizes.set(attr)
        }

        fn set_corners_radius(shape: &View, radius: f32) {
            let mut old_radii = shape.corners_radii.get();
            old_radii.z = radius;
            shape.corners_radii.set(old_radii);
        }

        fn set_color(shape: &View, color: color::Rgba) {
            shape.selection_color.set(color.into())
        }
    }
}
