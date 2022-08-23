//! A module containing the selectable [`GridView`] component.

use crate::prelude::*;

use crate::Entry;

use ensogl_core::application::Application;
use ensogl_core::display;


// ==============
// === Export ===
// ==============

pub mod highlight;



// ================
// === GridView ===
// ================

/// A template for [`GridView`] structure, where entry parameters and model are separate generic
/// arguments, similar to [`crate::GridViewTemplate`] - see its docs for details.
#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = ""))]
pub struct GridViewTemplate<
    Entry: 'static,
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
> {
    #[deref]
    grid:              crate::GridViewTemplate<Entry, EntryModel, EntryParams>,
    highlights:        highlight::shape::View,
    selection_handler: highlight::Handler<Entry, EntryModel, EntryParams>,
    hover_handler:     highlight::Handler<Entry, EntryModel, EntryParams>,
}

/// The Selectable Grid View.
///
/// An extension of [the base `GridView`](crate::GridView), where hovered and selected entries
/// will be highlighted. It shares the base [FRP API][`crate::Frp`], also providing additional
/// FRP APIs for handling [selection](selection_highlight_frp) and [hover](hover_highlight_frp)
/// highlights.
///
/// # Highlight modes
///
/// **Basic**: By default, the highlight is displayed as `highlight::shape::View` instance. The
/// shapes used by entries should specify if they need to be above or below highlight (for example
/// by adding `above = [ensogl_grid_view::selectable::highlight::shape]` clause to
/// [`define_shape_system!`] macro.
///
/// **Masked Layer**: The basic highlight, however does not work correctly with case where we want
/// selected entries having different style (e.g. different text color) when highlighted. Because
/// of the highlight's smooth transition between entries, there are frames where only part of
/// the entry is highlighted.
///
/// Therefore, you may specify a special layer, which is displayed over base grid view. The Grid
/// View will then create needed sub-layers (main and for text) and their mask: the sub-layers will
/// have another [`GridView`] instance with identical content but different entry parameters. They
/// are masked with the highlight shape, so only the highlighted fragment of the another grid view
/// is actually displayed.
///
/// The "Masked layer" mode may be set for highlight and selection independently, by calling
/// [`highlight::FRP::setup_masked_layer`] on the proper highlight API.
pub type GridView<E> = GridViewTemplate<E, <E as Entry>::Model, <E as Entry>::Params>;

impl<E: Entry> GridView<E> {
    /// Create new Selectable Grid View instance.
    pub fn new(app: &Application) -> Self {
        let grid = crate::GridView::<E>::new(app);
        let highlights = highlight::shape::View::new(Logger::new("highlights"));
        let selection_handler = highlight::Handler::new_for_selection_connected(app, &grid);
        let hover_handler = highlight::Handler::new_for_hover_connected(app, &grid);
        grid.add_child(&highlights);
        selection_handler.connect_with_shape::<highlight::shape::SelectionAttrSetter>(&highlights);
        hover_handler.connect_with_shape::<highlight::shape::HoverAttrSetter>(&highlights);

        let network = grid.frp().network();
        frp::extend! { network
            eval grid.viewport ([highlights](&vp) highlight::shape::set_viewport(&highlights, vp));
        }

        Self { grid, highlights, selection_handler, hover_handler }
    }
}

impl<Entry, EntryModel, EntryParams> GridViewTemplate<Entry, EntryModel, EntryParams>
where
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    /// Access to the Selection Highlight FRP.
    pub fn selection_highlight_frp(&self) -> &highlight::Frp<EntryParams> {
        &self.selection_handler.frp
    }

    /// Access to the Hover Highlight FRP.
    pub fn hover_highlight_frp(&self) -> &highlight::Frp<EntryParams> {
        &self.hover_handler.frp
    }
}

impl<Entry, EntryModel, EntryParams> AsRef<crate::GridViewTemplate<Entry, EntryModel, EntryParams>>
    for GridViewTemplate<Entry, EntryModel, EntryParams>
where
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    fn as_ref(&self) -> &crate::GridViewTemplate<Entry, EntryModel, EntryParams> {
        &self.grid
    }
}

impl<Entry, EntryModel, EntryParams> display::Object
    for GridViewTemplate<Entry, EntryModel, EntryParams>
where
    EntryModel: frp::node::Data,
    EntryParams: frp::node::Data,
{
    fn display_object(&self) -> &display::object::Instance {
        self.grid.display_object()
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entry;
    use crate::entry_position;
    use crate::Col;
    use crate::EntryFrp;
    use crate::Row;
    use ensogl_core::application::frp::API;
    use ensogl_core::data::color;
    use ensogl_core::display::scene::Layer;
    use ensogl_core::display::Scene;
    use ensogl_scroll_area::Viewport;
    use itertools::iproduct;

    const CONTOUR_VARIANTS: [entry::Contour; 3] = [
        entry::Contour { size: Vector2(10.0, 10.0), corners_radius: 0.0 },
        entry::Contour { size: Vector2(20.0, 20.0), corners_radius: 2.0 },
        entry::Contour { size: Vector2(15.0, 15.0), corners_radius: 3.0 },
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
        let highlight_frp = grid_view.selection_highlight_frp();
        let entries: HashMap<(Row, Col), Rc<TestEntryModel>> = iproduct!(0..2, 0..2)
            .map(|(i, j)| ((i, j), Rc::new(TestEntryModel::new(i, j))))
            .collect();
        let models = entries.clone();
        frp::extend! { network
            grid_view.model_for_entry <+ grid_view.model_for_entry_needed.map(move |&(row, col)| (row, col, models[&(row, col)].clone_ref()));
        }
        grid_view.set_entries_size(Vector2(20.0, 20.0));
        grid_view.set_viewport(Viewport { left: 0.0, top: 0.0, right: 40.0, bottom: -40.0 });
        grid_view.reset_entries(2, 2);

        assert_eq!(highlight_frp.contour.value(), entry::Contour::default());

        for (row, col) in iproduct!(0..2, 0..2) {
            grid_view.select_entry(Some((row, col)));
            let column_widths = &grid_view.model().column_widths;
            let expected_pos = entry_position(row, col, Vector2(20.0, 20.0), column_widths);
            assert_eq!(highlight_frp.position.value(), expected_pos);
            assert_eq!(highlight_frp.contour.value(), CONTOUR_VARIANTS[row]);
            assert_eq!(highlight_frp.color.value(), COLOR_VARIANTS[col]);
            for (&loc, model) in &entries {
                assert_eq!(model.selected.get(), loc == (row, col))
            }
        }
        grid_view.select_entry(None);
        assert_eq!(highlight_frp.contour.value(), entry::Contour::default());
    }

    #[test]
    fn covering_and_uncovering_selected_entry() {
        let app = Application::new("root");
        let network = frp::Network::new("selecting_entries");
        let grid_view = GridView::<TestEntry>::new(&app);
        let entries = (0..3).map(|i| Rc::new(TestEntryModel::new(i, 0))).collect_vec();
        let models = entries.clone();
        frp::extend! { network
            grid_view.model_for_entry <+
                grid_view.model_for_entry_needed.map(move |&(r, c)| (r, c, models[c].clone_ref()));
        }
        grid_view.set_entries_size(Vector2(20.0, 20.0));
        let viewport_showing_first_two =
            Viewport { left: 1.0, top: 0.0, right: 21.0, bottom: -20.0 };
        let viewport_showing_last_two =
            Viewport { left: 39.0, top: 0.0, right: 59.0, bottom: -20.0 };
        grid_view.set_viewport(viewport_showing_last_two);
        grid_view.reset_entries(1, 3);
        grid_view.select_entry(Some((0, 2)));
        assert!(!entries[1].selected.get());
        assert!(entries[2].selected.get());

        grid_view.set_viewport(viewport_showing_first_two);
        // Make sure the set_selected flag was not kept in re-used entry.
        assert!(!entries[0].selected.get());
        assert!(!entries[1].selected.get());
        // We clear the selected flag; this way we'll check if it will be set to true once the third
        // (selected) entry will be uncovered.
        entries[2].selected.set(false);
        grid_view.set_viewport(viewport_showing_last_two);
        assert!(!entries[1].selected.get());
        assert!(entries[2].selected.get());
    }
}
