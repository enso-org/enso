//! A module containing the selectable [`GridView`] component.

use crate::prelude::*;

use crate::header;
use crate::Col;
use crate::Entry;
use crate::Row;

use ensogl_core::application::Application;
use ensogl_core::display;


// ==============
// === Export ===
// ==============

pub mod highlight;



// ======================
// === MovementTarget ===
// ======================

/// An internal structure describing where selection would go after being moved (i.e.
/// after navigating with arrows). If moving the selection would put it outside the grid, the
/// [`OutOfBounds`] variant contains the direction in which the grid boundary would be crossed.
/// Otherwise, the [`Location`] variant contains row and column in the grid.
#[derive(Copy, Clone, Debug)]
enum MovementTarget {
    Location { row: Row, col: Col },
    OutOfBounds(frp::io::keyboard::ArrowDirection),
}

impl MovementTarget {
    /// Calculate row and column of the nearest entry in given direction from given row and col.
    /// Returns a [`MovementTarget::Location`] if the entry is in bounds of a grid with given
    /// amount of rows and columns. Returns [`MovementTarget::OutOfBounds`] otherwise.
    fn next_in_direction(
        row: Row,
        col: Col,
        direction: frp::io::keyboard::ArrowDirection,
        rows: Row,
        columns: Col,
    ) -> MovementTarget {
        use frp::io::keyboard::ArrowDirection::*;
        use MovementTarget::*;
        let row_below = row + 1;
        let col_to_the_right = col + 1;
        match direction {
            Up if row > 0 => Location { row: row - 1, col },
            Down if row_below < rows => Location { row: row_below, col },
            Left if col > 0 => Location { row, col: col - 1 },
            Right if col_to_the_right < columns => Location { row, col: col_to_the_right },
            _ => OutOfBounds(direction),
        }
    }

    /// In case of a [`Location`] variant, return the row and col it contains.
    fn location(self) -> Option<(Row, Col)> {
        match self {
            Self::Location { row, col } => Some((row, col)),
            Self::OutOfBounds(_) => None,
        }
    }

    /// In case of an [`OutOfBounds`] variant, return the arrow direction it contains.
    fn out_of_bounds(self) -> Option<frp::io::keyboard::ArrowDirection> {
        match self {
            Self::Location { .. } => None,
            Self::OutOfBounds(dir) => Some(dir),
        }
    }
}

impl Default for MovementTarget {
    fn default() -> Self {
        Self::Location { row: 0, col: 0 }
    }
}



// ================
// === GridView ===
// ================

/// A template for [`GridView`] structure, where entry parameters and model are separate generic
/// arguments, similar to [`crate::GridViewTemplate`] - see its docs for details.
#[derive(CloneRef, Debug, Deref, Derivative)]
#[derivative(Clone(bound = "InnerGridView: Clone"))]
#[clone_ref(bound = "InnerGridView: CloneRef")]
pub struct GridViewTemplate<InnerGridView, Entry, EntryParams: frp::node::Data> {
    #[deref]
    grid:              InnerGridView,
    highlights:        highlight::shape::View,
    header_highlights: Immutable<Option<highlight::shape::View>>,
    selection_handler: highlight::SelectionHandler<InnerGridView, Entry, EntryParams>,
    hover_handler:     highlight::HoverHandler<InnerGridView, Entry, EntryParams>,
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
/// [`shape!`] macro.
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
pub type GridView<E> = GridViewTemplate<crate::GridView<E>, E, <E as Entry>::Params>;

/// The Selectable Grid View with Headers.
///
/// An extension of [the `GridView` with headers](header::GridView), where hovered and selected
/// entries or pushed-down headers will be highlighted.
///
/// # Highlight Shape
///
/// The selection shape in this case is implemented in a pretty tricky way. We want to render the
/// highlight between specific `Entry` elements (e.g. between the text and the background). However,
/// the headers are rendered in different layer, and we want the selection to be above the header
/// background and below the text entries at the same time, which is not possible.
///
/// Therefore, this version has two highlight shapes instantiated, one for normal entries and one
/// for headers, the latter being clipped at the header bottom (using the [highlight shape clipping
/// ability](highlight::shape::AttrSetter::top_clip)).
///
/// # Highlight Mask in *Masked Layer* Mode.
///
/// The grid view displayed in the masked layer also is a grid view with headers. As In a scrolled
/// grid view, the user can select a partially visible entry behind the group's header, we manually
/// clip the highlight shape in the mask in such case, so the highlight will not be over the header.
pub type GridViewWithHeaders<E, HeaderEntry> =
    GridViewTemplate<header::GridView<E, HeaderEntry>, E, <E as Entry>::Params>;

impl<InnerGridView, E: Entry> GridViewTemplate<InnerGridView, E, E::Params>
where
    InnerGridView: AsRef<crate::GridView<E>> + display::Object,
    highlight::SelectionHandler<InnerGridView, E, E::Params>:
        highlight::HasConstructor<InnerGridView = InnerGridView>,
    highlight::HoverHandler<InnerGridView, E, E::Params>:
        highlight::HasConstructor<InnerGridView = InnerGridView>,
{
    fn new_wrapping(app: &Application, grid: InnerGridView) -> Self {
        use frp::io::keyboard::ArrowDirection as Direction;
        let highlights = highlight::shape::View::new();
        let header_highlights = Immutable(None);
        let selection_handler = highlight::SelectionHandler::new_connected(app, &grid);
        let hover_handler = highlight::HoverHandler::new_connected(app, &grid);
        grid.add_child(&highlights);
        selection_handler.connect_with_shape(&highlights);
        hover_handler.connect_with_shape(&highlights);

        let grid_ref = grid.as_ref();
        let grid_frp = grid_ref.frp();
        let network = grid_frp.network();
        let input = &grid_frp.private.input;
        frp::extend! { network
            eval grid_frp.viewport ([highlights](&vp) {
                highlight::shape::set_viewport(&highlights, vp);
            });


            // === Move selection by one position ===

            input_move_selection_dir <- any(...);
            input_move_selection_dir <+ input.move_selection_up.constant(Some(Direction::Up));
            input_move_selection_dir <+ input.move_selection_down.constant(Some(Direction::Down));
            input_move_selection_dir <+ input.move_selection_left.constant(Some(Direction::Left));
            input_move_selection_dir <+ input.move_selection_right.constant(Some(Direction::Right));
            let grid_size = &grid_frp.grid_size;
            let selection = &grid_frp.entry_selected;
            selection_after_movement <= input_move_selection_dir.map3(grid_size, selection,
                |dir, (rows, cols), selection| selection.zip(*dir).map(|((row, col), dir)|
                    MovementTarget::next_in_direction(row, col, dir, *rows, *cols)
                )
            );
            grid_frp.select_entry <+ selection_after_movement.filter_map(|s| s.location()).some();
            grid_frp.private.output.selection_movement_out_of_grid_prevented <+
                selection_after_movement.filter_map(|s| s.out_of_bounds());
        }

        Self { grid, highlights, header_highlights, selection_handler, hover_handler }
    }
}

impl<E: Entry> GridView<E> {
    /// Create new Selectable Grid View instance.
    pub fn new(app: &Application) -> Self {
        let grid = app.new_view::<crate::GridView<E>>();
        Self::new_wrapping(app, grid)
    }
}

impl<E: Entry, HeaderEntry: Entry<Params = E::Params>> GridViewWithHeaders<E, HeaderEntry> {
    /// Create new Selectable Grid View With Headers instance.
    pub fn new(app: &Application) -> Self {
        let mut this = Self::new_wrapping(app, header::GridView::<E, HeaderEntry>::new(app));
        let header_highlights = highlight::shape::View::new();
        this.grid.add_child(&header_highlights);
        this.selection_handler.connect_with_header_shape(&header_highlights);
        this.hover_handler.connect_with_header_shape(&header_highlights);

        let network = this.grid.frp().network();
        let header_frp = this.grid.header_frp();
        frp::extend! { network
            eval this.grid.viewport ([header_highlights](&vp) {
                highlight::shape::set_viewport(&header_highlights, vp);
            });
            eval header_frp.set_layers([header_highlights](layers) if let Some(layer) = layers.upgrade_header() {
                layer.add(&header_highlights);
            });
        }
        this.header_highlights = Immutable(Some(header_highlights));
        this
    }
}


impl<InnerGridView, Entry, EntryParams> GridViewTemplate<InnerGridView, Entry, EntryParams>
where EntryParams: frp::node::Data
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

impl<InnerGridView, E: Entry, T> AsRef<T> for GridViewTemplate<InnerGridView, E, E::Params>
where InnerGridView: AsRef<T>
{
    fn as_ref(&self) -> &T {
        self.grid.as_ref()
    }
}

impl<InnerGridView, Entry, EntryParams> display::Object
    for GridViewTemplate<InnerGridView, Entry, EntryParams>
where
    InnerGridView: display::Object,
    EntryParams: frp::node::Data,
{
    fn display_object(&self) -> &display::object::Instance {
        self.grid.display_object()
    }

    fn focus_receiver(&self) -> &display::object::Instance {
        self.grid.focus_receiver()
    }
}



// ============
// === Test ===
// ============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entry;
    use crate::header::WeakLayers;
    use crate::Col;
    use crate::EntryFrp;
    use crate::Row;
    use ensogl_core::application::frp::API;
    use ensogl_core::data::color;
    use ensogl_core::display::scene::Layer;
    use ensogl_scroll_area::Viewport;
    use itertools::iproduct;

    const CONTOUR_VARIANTS: [entry::Contour; 3] = [
        entry::Contour { size: Vector2(10.0, 10.0), corners_radius: 0.0 },
        entry::Contour { size: Vector2(20.0, 20.0), corners_radius: 2.0 },
        entry::Contour { size: Vector2(15.0, 15.0), corners_radius: 3.0 },
    ];

    const COLOR_VARIANTS: [color::Lcha; 2] =
        [color::Lcha(1.0, 0.0, 0.0, 1.0), color::Lcha(0.0, 1.0, 0.0, 1.0)];

    #[derive(Clone, Debug, Default)]
    struct TestEntryModel {
        selected: Cell<bool>,
        hovered:  Cell<bool>,
        contour:  entry::Contour,
        color:    color::Lcha,
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

    #[derive(Clone, CloneRef, Debug, display::Object)]
    struct TestEntry {
        display_object: display::object::Instance,
        frp:            EntryFrp<Self>,
    }

    impl Entry for TestEntry {
        type Model = Rc<TestEntryModel>;
        type Params = ();

        fn new(_: &Application, _: Option<&Layer>) -> Self {
            let frp = EntryFrp::<Self>::new();
            let display_object = display::object::Instance::new();
            let network = frp.network();
            let input = &frp.private().input;
            let out = &frp.private().output;
            frp::extend! {network
                model_with_flags <- all(input.set_model, input.set_selected, input.set_hovered);
                eval model_with_flags ([]((model, selected, hovered)) {
                    model.selected.set(*selected);
                    model.hovered.set(*hovered);
                });
                out.highlight_contour <+ input.set_model.map(|m| m.contour);
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
            let expected_pos =
                entry::visible::position(row, col, Vector2(20.0, 20.0), column_widths);
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

    #[test]
    fn selecting_header() {
        let app = Application::new("root");
        let network = frp::Network::new("selecting_header");
        let grid_view = GridViewWithHeaders::<TestEntry, TestEntry>::new(&app);
        let headers_layer = app.display.default_scene.layers.main.create_sublayer("headers");
        grid_view.header_frp().set_layers(WeakLayers::new(&headers_layer, None));
        let entries = (0..3).map(|i| Rc::new(TestEntryModel::new(i, 0))).collect_vec();
        let models = entries.clone();
        let header_model = Rc::new(TestEntryModel::new(1, 0));
        let selection_state = || entries.iter().map(|e| e.selected.get()).collect_vec();
        let headers = grid_view.header_frp();
        frp::extend! { network
            grid_view.model_for_entry <+
                grid_view.model_for_entry_needed.map(move |&(r, c)| (r, c, models[r].clone_ref()));
            headers.section_info <+
                headers.section_info_needed.filter_map(move |&(r, _)| (r > 0).as_some(((1..3), 0, header_model.clone_ref())));
        }
        grid_view.set_entries_size(Vector2(20.0, 20.0));
        let viewport_having_header_pushed_down =
            Viewport { left: 1.0, top: -21.0, right: 19.0, bottom: -51.0 };
        let viewport_having_header_pushed_down_further =
            Viewport { left: 1.0, top: -30.0, right: 19.0, bottom: -60.0 };
        let viewport_with_no_pushed_header =
            Viewport { left: 1.0, top: -15.0, right: 19.0, bottom: -45.0 };

        grid_view.set_viewport(viewport_having_header_pushed_down);
        grid_view.reset_entries(3, 1);
        grid_view.select_entry(Some((1, 0)));
        assert_eq!(selection_state(), vec![false, true, false]);
        assert_eq!(grid_view.selection_highlight_frp().position.value(), Vector2(10.0, -31.0));

        grid_view.set_viewport(viewport_having_header_pushed_down_further);
        assert_eq!(selection_state(), vec![false, true, false]);
        assert_eq!(grid_view.selection_highlight_frp().position.value(), Vector2(10.0, -40.0));

        debug!("About to go up");
        grid_view.set_viewport(viewport_with_no_pushed_header);
        assert_eq!(selection_state(), vec![false, true, false]);
        assert_eq!(grid_view.selection_highlight_frp().position.value(), Vector2(10.0, -30.0));

        grid_view.set_viewport(viewport_having_header_pushed_down);
        assert_eq!(selection_state(), vec![false, true, false]);
        assert_eq!(grid_view.selection_highlight_frp().position.value(), Vector2(10.0, -31.0));
    }
}
