use crate::entry;
use crate::entry_position;
use crate::prelude::*;
use crate::Col;
use crate::Entry;
use crate::Row;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::gui::Widget;


// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {}
    Output {
        hover_highlight_pos(Vector2),
        hover_highlight_contour(entry::Contour),
        hover_highlight_color(color::Rgba),
        selection_highlight_pos(Vector2),
        selection_highlight_contour(entry::Contour),
        selection_highlight_color(color::Rgba),
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, Debug)]
struct HighlightedEntryGuard {
    network:           frp::Network,
    should_be_dropped: frp::Stream,
    dropped:           frp::Source,
}

impl Drop for HighlightedEntryGuard {
    fn drop(&mut self) {
        self.dropped.emit(());
    }
}

#[derive(Clone, Debug, Default)]
struct HighlightedEntryGuardCell {
    entry_location: Cell<Option<(Row, Col)>>,
    guard:          RefCell<Option<HighlightedEntryGuard>>,
}

impl HighlightedEntryGuardCell {
    fn update_with_new_highlighted_entry<Entry, EntryModel, EntryParams>(
        self: &Rc<Self>,
        grid: &crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
        location: Option<(Row, Col)>,
        guard_creator: impl FnOnce(Entry, Row, Col) -> HighlightedEntryGuard,
    ) where
        Entry: CloneRef,
        EntryModel: frp::node::Data,
        EntryParams: frp::node::Data,
    {
        if let Some((row, col)) = location {
            let current_entry = self.entry_location.get();
            let entry_changed = current_entry.map_or(true, |loc| loc != (row, col));
            if entry_changed {
                self.guard.take();
                self.entry_location.set(Some((row, col)));
                let entry = grid.get_entry(row, col);
                let guard = entry.map(|e| guard_creator(e, row, col));
                *self.guard.borrow_mut() = guard;
                self.set_up_dropping();
            }
        } else {
            self.drop_guard()
        }
    }

    fn set_up_dropping(self: &Rc<Self>) {
        if let Some(guard) = &*self.guard.borrow() {
            let network = &guard.network;
            let this = self;
            frp::extend! { network
                eval_ guard.should_be_dropped (this.drop_guard());
            }
        }
    }

    fn drop_guard(&self) {
        self.entry_location.set(None);
        self.guard.take();
    }
}


macro_rules! define_highlight_endpoints {
    (
        $Name:ident {
            frp: ($position:ident, $contour:ident, $color:ident),
            entry: ($entry_flag:ident, $entry_color:ident) $(,)?
        }
    ) => {
        #[derive(Clone, CloneRef, Debug)]
        struct $Name {
            position: frp::Any<Vector2>,
            contour:  frp::Any<entry::Contour>,
            color:    frp::Any<color::Rgba>,
        }

        impl $Name {
            fn new(frp: &Frp) -> Self {
                Self {
                    position: frp.private.output.$position.clone_ref(),
                    contour:  frp.private.output.$contour.clone_ref(),
                    color:    frp.private.output.$color.clone_ref(),
                }
            }

            fn create_guard_for_entry<E: Entry>(
                &self,
                entry: &E,
                row: Row,
                col: Col,
            ) -> HighlightedEntryGuard {
                let network = frp::Network::new("HighlightedEntryGuard");
                let entry_frp = entry.frp();
                frp::extend! { network
                    init <- source_();
                    dropped <- source_();
                    contour <- all(init, entry_frp.contour)._1();
                    color <- all(init, entry_frp.$entry_color)._1();
                    entry_frp.$entry_flag <+ init.constant(true);
                    entry_frp.$entry_flag <+ dropped.constant(false);
                    self.contour <+ contour;
                    self.color <+ color;
                    location_change <- entry_frp.set_location.filter(move |loc| *loc != (row, col));
                    should_be_dropped <- location_change.constant(());
                }
                init.emit(());
                HighlightedEntryGuard {
                    network,
                    dropped: dropped.into(),
                    should_be_dropped: should_be_dropped.into(),
                }
            }
        }
    };
}

define_highlight_endpoints! {
    SelectionEndpoints {
        frp: (selection_highlight_pos, selection_highlight_contour, selection_highlight_color),
        entry: (set_selected, selection_highlight_color)
    }
}

define_highlight_endpoints! {
    HoverEndpoints {
        frp: (hover_highlight_pos, hover_highlight_contour, hover_highlight_color),
        entry: (set_hovered, hover_highlight_color)
    }
}

#[derive(Clone, Debug)]
pub struct Model<Entry: 'static, EntryModel: frp::node::Data, EntryParams: frp::node::Data> {
    grid:                crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
    selection_endpoints: SelectionEndpoints,
    selection_guard:     Rc<HighlightedEntryGuardCell>,
    hover_endpoints:     HoverEndpoints,
    hover_guard:         Rc<HighlightedEntryGuardCell>,
}

impl<E: Entry> Model<E, E::Model, E::Params> {
    fn new(app: &Application, frp: &Frp) -> Self {
        let grid = crate::GridView::<E>::new(app);
        let selection_endpoints = SelectionEndpoints::new(frp);
        let selection_guard = default();
        let hover_endpoints = HoverEndpoints::new(frp);
        let hover_guard = default();
        Self { grid, selection_endpoints, selection_guard, hover_endpoints, hover_guard }
    }

    fn update_hovered_entry(&self, hovered_location: Option<(Row, Col)>) {
        let guard_creator = |e, r, c| self.hover_endpoints.create_guard_for_entry(&e, r, c);
        self.hover_guard.update_with_new_highlighted_entry(
            &self.grid,
            hovered_location,
            guard_creator,
        )
    }

    fn update_selected_entry(&self, selected_location: Option<(Row, Col)>) {
        let guard_creator = |e, r, c| self.selection_endpoints.create_guard_for_entry(&e, r, c);
        self.selection_guard.update_with_new_highlighted_entry(
            &self.grid,
            selected_location,
            guard_creator,
        )
    }
}



// ================
// === GridView ===
// ================

#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct GridViewTemplate<
    Entry: 'static,
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
> {
    widget: Widget<Model<Entry, EntryModel, EntryParams>, Frp>,
}

pub type GridView<E> = GridViewTemplate<E, <E as Entry>::Model, <E as Entry>::Params>;

impl<E: Entry> GridView<E> {
    fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let display_object = display::object::Instance::new(Logger::new("selectable::GridView"));
        let model = Rc::new(Model::new(app, &frp));
        let grid_frp = model.grid.widget.frp();
        let network = frp.network();
        let out = &frp.private.output;
        frp::extend! { network
            shown_entry_with_hovered <- grid_frp.entry_shown.map2(&grid_frp.entry_hovered, |a, b| (*a, *b));
            hovered_entry_is_shown <- shown_entry_with_hovered.map(|(sh, hov)| hov.contains(sh).and_option(*hov));
            should_update_hovered <- any(grid_frp.entry_hovered, hovered_entry_is_shown);
            eval should_update_hovered ((loc) model.update_hovered_entry(*loc));

            shown_entry_with_selected <- grid_frp.entry_shown.map2(&grid_frp.entry_selected, |a, b| (*a, *b));
            selected_entry_is_shown <- shown_entry_with_selected.map(|(sh, sel)| sel.contains(sh).and_option(*sel));
            should_update_selected <- any(grid_frp.entry_selected, selected_entry_is_shown);
            eval should_update_selected ((loc) model.update_selected_entry(*loc));

            hovered <- grid_frp.entry_hovered.filter_map(|l| *l);
            selected <- grid_frp.entry_selected.filter_map(|l| *l);
            out.hover_highlight_pos <+ hovered.all_with(&grid_frp.entries_size, |&(row, col), &es| entry_position(row, col, es));
            out.selection_highlight_pos <+ selected.all_with(&grid_frp.entries_size, |&(row, col), &es| entry_position(row, col, es));
            none_hovered <- grid_frp.entry_hovered.filter(|opt| opt.is_none()).constant(());
            none_selected <- grid_frp.entry_selected.filter(|opt| opt.is_none()).constant(());
            out.hover_highlight_contour <+ none_hovered.constant(default());
            out.selection_highlight_contour <+ none_selected.constant(default());
        }
        let widget = Widget::new(app, frp, model, display_object);
        Self { widget }
    }
}

impl<Entry, EntryModel, EntryParams> GridViewTemplate<Entry, EntryModel, EntryParams>
where
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    fn grid_frp(&self) -> &crate::Frp<EntryModel, EntryParams> {
        self.model().grid.frp()
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::EntryFrp;
    use ensogl_core::application::frp::API;
    use ensogl_core::display::scene::Layer;
    use ensogl_core::display::Scene;
    use ensogl_scroll_area::Viewport;
    use ensogl_text::View;
    use itertools::iproduct;

    const CONTOUR_VARIANTS: [entry::Contour; 3] = [
        entry::Contour { size: Vector2(10.0, 10.0), corner_radius: 0.0 },
        entry::Contour { size: Vector2(20.0, 20.0), corner_radius: 2.0 },
        entry::Contour { size: Vector2(15.0, 15.0), corner_radius: 3.0 },
    ];

    const COLOR_VARIANTS: [color::Rgba; 2] =
        [color::Rgba(1.0, 0.0, 0.0, 1.0), color::Rgba(0.0, 1.0, 0.0, 1.0)];

    #[derive(Clone, Debug, Default)]
    struct TestEntryModel {
        selected: Cell<bool>,
        hovered:  Cell<bool>,
        contour:  entry::Contour,
        color:    color::Rgba,
    }

    impl TestEntryModel {
        fn new(contour_variant: usize, color_variant: usize) -> Self {
            TestEntryModel {
                selected: default(),
                hovered:  default(),
                contour:  CONTOUR_VARIANTS[contour_variant],
                color:    COLOR_VARIANTS[color_variant],
            }
        }
    }

    #[derive(Clone, CloneRef, Debug)]
    struct TestEntry {
        display_object: display::object::Instance,
        frp:            EntryFrp<Self>,
    }

    impl Entry for TestEntry {
        type Model = Rc<TestEntryModel>;
        type Params = ();

        fn new(_: &Application, _: Option<&Layer>) -> Self {
            let frp = EntryFrp::<Self>::new();
            let display_object = display::object::Instance::new(Logger::new("TestEntry"));
            let network = frp.network();
            let input = &frp.private().input;
            let out = &frp.private().output;
            frp::extend! {network
                model_with_flags <- all(input.set_model, input.set_selected, input.set_hovered);
                eval model_with_flags ([]((model, selected, hovered)) {
                    model.selected.set(*selected);
                    model.hovered.set(*hovered);
                });
                out.contour <+ input.set_model.map(|m| m.contour);
                out.selection_highlight_color <+ input.set_model.map(|m| m.color);
                out.hover_highlight_color <+ input.set_model.map(|m| m.color);
            }
            Self { frp, display_object }
        }

        fn frp(&self) -> &EntryFrp<Self> {
            &self.frp
        }
    }

    impl display::Object for TestEntry {
        fn display_object(&self) -> &display::object::Instance<Scene> {
            &self.display_object
        }
    }

    #[test]
    fn selecting_entries() {
        let app = Application::new("root");
        let network = frp::Network::new("selecting_entries");
        let grid_view = GridView::<TestEntry>::new(&app);
        let grid_frp = grid_view.grid_frp();
        let entries: HashMap<(Row, Col), Rc<TestEntryModel>> = iproduct!(0..2, 0..2)
            .map(|(i, j)| ((i, j), Rc::new(TestEntryModel::new(i, j))))
            .collect();
        let models = entries.clone();
        frp::extend! { network
            grid_frp.model_for_entry <+ grid_frp.model_for_entry_needed.map(move |&(row, col)| (row, col, models[&(row, col)].clone_ref()));
        }
        grid_frp.set_entries_size(Vector2(20.0, 20.0));
        grid_frp.set_viewport(Viewport { left: 0.0, top: 0.0, right: 40.0, bottom: -40.0 });
        grid_frp.reset_entries(2, 2);

        assert_eq!(grid_view.selection_highlight_contour.value(), entry::Contour::default());

        for (row, col) in iproduct!(0..2, 0..2) {
            grid_frp.select_entry(Some((row, col)));
            let expected_pos = entry_position(row, col, Vector2(20.0, 20.0));
            assert_eq!(grid_view.selection_highlight_pos.value(), expected_pos);
            assert_eq!(grid_view.selection_highlight_contour.value(), CONTOUR_VARIANTS[row]);
            assert_eq!(grid_view.selection_highlight_color.value(), COLOR_VARIANTS[col]);
            for (&loc, model) in &entries {
                assert_eq!(model.selected.get(), loc == (row, col))
            }
        }
        grid_frp.select_entry(None);
        assert_eq!(grid_view.selection_highlight_contour.value(), entry::Contour::default());
    }

    #[test]
    fn covering_and_uncovering_selected_entry() {
        let app = Application::new("root");
        let network = frp::Network::new("selecting_entries");
        let grid_view = GridView::<TestEntry>::new(&app);
        let grid_frp = grid_view.grid_frp();
        let entries = (0..3).map(|i| Rc::new(TestEntryModel::new(i, 0))).collect_vec();
        let models = entries.clone();
        frp::extend! { network
            grid_frp.model_for_entry <+
                grid_frp.model_for_entry_needed.map(move |&(r, c)| (r, c, models[c].clone_ref()));
        }
        grid_frp.set_entries_size(Vector2(20.0, 20.0));
        let viewport_showing_first_two =
            Viewport { left: 1.0, top: 0.0, right: 21.0, bottom: -20.0 };
        let viewport_showing_last_two =
            Viewport { left: 39.0, top: 0.0, right: 59.0, bottom: -20.0 };
        grid_frp.set_viewport(viewport_showing_last_two);
        grid_frp.reset_entries(1, 3);
        grid_frp.select_entry(Some((0, 2)));
        assert!(!entries[1].selected.get());
        assert!(entries[2].selected.get());

        grid_frp.set_viewport(viewport_showing_first_two);
        // Make sure the set_selected flag was not kept in re-used entry.
        assert!(!entries[0].selected.get());
        assert!(!entries[1].selected.get());
        // We clear the selected flag; this way we'll check if it will be set to true once the third
        // (selected) entry will be uncovered.
        entries[2].selected.set(false);
        grid_frp.set_viewport(viewport_showing_last_two);
        assert!(!entries[1].selected.get());
        assert!(entries[2].selected.get());
    }
}
