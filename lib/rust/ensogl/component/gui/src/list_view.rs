//! ListView EnsoGL Component.
//!
//! ListView a displayed list of entries with possibility of selecting one and "choosing" by
//! clicking or pressing enter - similar to the HTML `<select>`.

pub mod entry;

use crate::prelude::*;
use crate::shadow;

use enso_frp as frp;
use ensogl_core::application;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::scene::layer::LayerId;
use ensogl_core::display::shape::*;
use ensogl_core::DEPRECATED_Animation;
use ensogl_hardcoded_theme as theme;

pub use entry::Entry;



// ==========================
// === Shapes Definitions ===
// ==========================

// === Constants ===

/// The size of shadow under element. It is not counted in the component width and height.
pub const SHADOW_PX: f32 = 10.0;
const SHAPE_PADDING: f32 = 5.0;


// === Selection ===

/// The selection rectangle shape.
pub mod selection {
    use super::*;

    /// The corner radius in pixels.
    pub const CORNER_RADIUS_PX: f32 = 12.0;

    ensogl_core::define_shape_system! {
        (style:Style) {
            let sprite_width  : Var<Pixels> = "input_size.x".into();
            let sprite_height : Var<Pixels> = "input_size.y".into();
            let padding_inner_x = style.get_number(ensogl_hardcoded_theme::application::searcher::selection::padding::horizontal);
            let padding_inner_y = style.get_number(ensogl_hardcoded_theme::application::searcher::selection::padding::vertical);
            let width         = sprite_width  - 2.0.px() * SHAPE_PADDING + 2.0.px() * padding_inner_x;
            let height        = sprite_height - 2.0.px() * SHAPE_PADDING + 2.0.px() * padding_inner_y;
            let color         = style.get_color(ensogl_hardcoded_theme::widget::list_view::highlight);
            let rect          = Rect((&width,&height)).corners_radius(CORNER_RADIUS_PX.px());
            let shape         = rect.fill(color);
            shape.into()
        }
    }
}


// === Background ===

/// The default list view background.
pub mod background {
    use super::*;

    /// The corner radius in pixels.
    pub const CORNER_RADIUS_PX: f32 = selection::CORNER_RADIUS_PX;

    ensogl_core::define_shape_system! {
        below = [selection];
        (style:Style) {
            let sprite_width  : Var<Pixels> = "input_size.x".into();
            let sprite_height : Var<Pixels> = "input_size.y".into();
            let width         = sprite_width - SHADOW_PX.px() * 2.0 - SHAPE_PADDING.px() * 2.0;
            let height        = sprite_height - SHADOW_PX.px() * 2.0 - SHAPE_PADDING.px() * 2.0;
            let color         = style.get_color(theme::widget::list_view::background);
            let rect          = Rect((&width,&height)).corners_radius(CORNER_RADIUS_PX.px());
            let shape         = rect.fill(color);

            let shadow  = shadow::from_shape(rect.into(),style);

            (shadow + shape).into()
        }
    }
}



// =============
// === Model ===
// =============

/// Information about displayed fragment of entries list.
#[derive(Copy, Clone, Debug, Default)]
struct View {
    position_y: f32,
    size:       Vector2<f32>,
}

/// The Model of Select Component.
#[derive(Clone, CloneRef, Debug)]
struct Model<E: Entry> {
    app:            Application,
    entries:        entry::List<E>,
    selection:      selection::View,
    background:     background::View,
    scrolled_area:  display::object::Instance,
    display_object: display::object::Instance,
}

impl<E: Entry> Model<E> {
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let logger = Logger::new("SelectionContainer");
        let display_object = display::object::Instance::new(&logger);
        let scrolled_area = display::object::Instance::new(&logger);
        let entries = entry::List::new(&logger, &app);
        let background = background::View::new(&logger);
        let selection = selection::View::new(&logger);
        display_object.add_child(&background);
        display_object.add_child(&scrolled_area);
        scrolled_area.add_child(&entries);
        scrolled_area.add_child(&selection);
        Model { app, entries, selection, background, scrolled_area, display_object }
    }

    fn padding(&self) -> f32 {
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&self.app.display.scene().style_sheet);
        styles.get_number(ensogl_hardcoded_theme::application::searcher::padding)
    }

    /// Update the displayed entries list when _view_ has changed - the list was scrolled or
    /// resized.
    fn update_after_view_change(&self, view: &View) {
        let visible_entries = Self::visible_entries(view, self.entries.entry_count());
        let padding_px = self.padding();
        let padding = 2.0 * padding_px + SHAPE_PADDING;
        let padding = Vector2(padding, padding);
        let shadow = Vector2(2.0 * SHADOW_PX, 2.0 * SHADOW_PX);
        self.entries.set_position_x(-view.size.x / 2.0);
        self.background.size.set(view.size + padding + shadow);
        self.scrolled_area.set_position_y(view.size.y / 2.0 - view.position_y);
        self.entries.update_entries(visible_entries);
    }

    fn set_entries(&self, provider: entry::AnyModelProvider<E>, view: &View) {
        let visible_entries = Self::visible_entries(view, provider.entry_count());
        self.entries.update_entries_new_provider(provider, visible_entries);
    }

    fn visible_entries(View { position_y, size }: &View, entry_count: usize) -> Range<entry::Id> {
        if entry_count == 0 {
            0..0
        } else {
            let entry_at_y_saturating =
                |y: f32| match entry::List::<E>::entry_at_y_position(y, entry_count) {
                    entry::list::IdAtYPosition::AboveFirst => 0,
                    entry::list::IdAtYPosition::UnderLast => entry_count - 1,
                    entry::list::IdAtYPosition::Entry(id) => id,
                };
            let first = entry_at_y_saturating(*position_y);
            let last = entry_at_y_saturating(position_y - size.y) + 1;
            first..last
        }
    }

    /// Check if the `point` is inside component assuming that it have given `size`.
    fn is_inside(&self, point: Vector2<f32>, size: Vector2<f32>) -> bool {
        let pos_obj_space =
            self.app.display.scene().screen_to_object_space(&self.background, point);
        let x_range = (-size.x / 2.0)..=(size.x / 2.0);
        let y_range = (-size.y / 2.0)..=(size.y / 2.0);
        x_range.contains(&pos_obj_space.x) && y_range.contains(&pos_obj_space.y)
    }

    fn selected_entry_after_jump(
        &self,
        current_entry: Option<entry::Id>,
        jump: isize,
    ) -> Option<entry::Id> {
        if jump < 0 {
            let current_entry = current_entry?;
            if current_entry == 0 {
                None
            } else {
                Some(current_entry.saturating_sub(-jump as usize))
            }
        } else {
            let max_entry = self.entries.entry_count().checked_sub(1)?;
            Some(current_entry.map_or(0, |id| id + (jump as usize)).min(max_entry))
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    <E:(Debug+'static)>
    Input {
        /// Move selection one position up.
        move_selection_up(),
        /// Move selection page up (jump over all visible entries).
        move_selection_page_up(),
        /// Move selection to the first argument.
        move_selection_to_first(),
        /// Move selection one position down.
        move_selection_down(),
        /// Move selection page down (jump over all visible entries).
        move_selection_page_down(),
        /// Move selection to the last argument.
        move_selection_to_last(),
        /// Chose the currently selected entry.
        chose_selected_entry(),
        /// Deselect all entries.
        deselect_entries(),

        resize       (Vector2<f32>),
        scroll_jump  (f32),
        set_entries  (entry::AnyModelProvider<E>),
        select_entry (entry::Id),
        chose_entry  (entry::Id),
    }

    Output {
        selected_entry  (Option<entry::Id>),
        chosen_entry    (Option<entry::Id>),
        size            (Vector2<f32>),
        scroll_position (f32),
    }
}



// ==========================
// === ListView Component ===
// ==========================

/// ListView Component.
///
/// This is a displayed list of entries (of any type `E`) with possibility of selecting one and
/// "choosing" by clicking or pressing enter. The basic entry types are defined in [`entry`] module.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct ListView<E: Entry> {
    model:   Model<E>,
    pub frp: Frp<E>,
}

impl<E: Entry> Deref for ListView<E> {
    type Target = Frp<E>;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl<E: Entry> ListView<E>
where E::Model: Default
{
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(app);
        ListView { model, frp }.init(app)
    }

    fn init(self, app: &Application) -> Self {
        const MAX_SCROLL: f32 = entry::HEIGHT / 2.0;
        const MOUSE_MOVE_THRESHOLD: f32 = std::f32::EPSILON;

        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let scene = app.display.scene();
        let mouse = &scene.mouse.frp;
        let view_y = DEPRECATED_Animation::<f32>::new(network);
        let selection_y = DEPRECATED_Animation::<f32>::new(network);
        let selection_height = DEPRECATED_Animation::<f32>::new(network);

        frp::extend! { network

            // === Mouse Position ===

            mouse_in <- all_with(&mouse.position,&frp.size,f!((pos,size)
                model.is_inside(*pos,*size)
            ));
            mouse_moved       <- mouse.distance.map(|dist| *dist > MOUSE_MOVE_THRESHOLD );
            mouse_y_in_scroll <- mouse.position.map(f!([model,scene](pos) {
                scene.screen_to_object_space(&model.scrolled_area,*pos).y
            }));
            mouse_pointed_entry <- mouse_y_in_scroll.map(f!([model](y)
                entry::List::<E>::entry_at_y_position(*y,model.entries.entry_count()).entry()
            ));


            // === Selected Entry ===
            frp.source.selected_entry <+ frp.select_entry.map(|id| Some(*id));

            selection_jump_on_one_up  <- frp.move_selection_up.constant(-1);
            selection_jump_on_page_up <- frp.move_selection_page_up.map(f_!([model]
                -(model.entries.visible_entry_count() as isize)
            ));
            selection_jump_on_one_down  <- frp.move_selection_down.constant(1);
            selection_jump_on_page_down <- frp.move_selection_page_down.map(f_!(
                model.entries.visible_entry_count() as isize
            ));
            selection_jump_up   <- any(selection_jump_on_one_up,selection_jump_on_page_up);
            selection_jump_down <- any(selection_jump_on_one_down,selection_jump_on_page_down);
            selected_entry_after_jump_up <- selection_jump_up.map2(&frp.selected_entry,
                f!((jump,id) model.selected_entry_after_jump(*id,*jump))
            );
            selected_entry_after_moving_first <- frp.move_selection_to_first.map(f!([model](())
                (model.entries.entry_count() > 0).and_option(Some(0))
            ));
            selected_entry_after_moving_last  <- frp.move_selection_to_last.map(f!([model] (())
                model.entries.entry_count().checked_sub(1)
            ));
            selected_entry_after_jump_down <- selection_jump_down.map2(&frp.selected_entry,
                f!((jump,id) model.selected_entry_after_jump(*id,*jump))
            );
            selected_entry_after_move_up <-
                any(selected_entry_after_jump_up,selected_entry_after_moving_first);
            selected_entry_after_move_down <-
                any(selected_entry_after_jump_down,selected_entry_after_moving_last);
            selected_entry_after_move <-
                any(&selected_entry_after_move_up,&selected_entry_after_move_down);
            mouse_selected_entry <- mouse_pointed_entry.gate(&mouse_in).gate(&mouse_moved);

            frp.source.selected_entry <+ selected_entry_after_move;
            frp.source.selected_entry <+ mouse_selected_entry;
            frp.source.selected_entry <+ frp.deselect_entries.constant(None);
            frp.source.selected_entry <+ frp.set_entries.constant(None);


            // === Chosen Entry ===

            any_entry_selected        <- frp.selected_entry.map(|e| e.is_some());
            any_entry_pointed         <- mouse_pointed_entry.map(|e| e.is_some());
            opt_selected_entry_chosen <- frp.selected_entry.sample(&frp.chose_selected_entry);
            opt_pointed_entry_chosen  <- mouse_pointed_entry.sample(&mouse.down_0).gate(&mouse_in);
            frp.source.chosen_entry   <+ opt_pointed_entry_chosen.gate(&any_entry_pointed);
            frp.source.chosen_entry   <+ frp.chose_entry.map(|id| Some(*id));
            frp.source.chosen_entry   <+ opt_selected_entry_chosen.gate(&any_entry_selected);


            // === Selection Size and Position ===

            target_selection_y <- frp.selected_entry.map(|id|
                id.map_or(0.0,entry::List::<E>::position_y_of_entry)
            );
            target_selection_height <- frp.selected_entry.map(f!([](id)
                if id.is_some() {entry::HEIGHT} else {0.0}
            ));
            eval target_selection_y      ((y) selection_y.set_target_value(*y));
            eval target_selection_height ((h) selection_height.set_target_value(*h));
            eval frp.set_entries         ([selection_y,selection_height](_) {
                selection_y.skip();
                selection_height.skip();
            });
            selectin_sprite_y <- all_with(&selection_y.value,&selection_height.value,
                |y,h| y + (entry::HEIGHT - h) / 2.0
            );
            eval selectin_sprite_y ((y) model.selection.set_position_y(*y));
            selection_size <- all_with(&frp.size,&selection_height.value,f!([](size,height) {
                let width = size.x;
                Vector2(width,*height)
            }));
            eval selection_size ((size) model.selection.size.set(*size));


            // === Scrolling ===

            selection_top_after_move_up <- selected_entry_after_move_up.map(|id|
                id.map(|id| entry::List::<E>::y_range_of_entry(id).end)
            );
            min_scroll_after_move_up <- selection_top_after_move_up.map(|top|
                top.unwrap_or(MAX_SCROLL)
            );
            scroll_after_move_up <- min_scroll_after_move_up.map2(&frp.scroll_position,|min,current|
                current.max(*min)
            );
            selection_bottom_after_move_down <- selected_entry_after_move_down.map(|id|
                id.map(|id| entry::List::<E>::y_range_of_entry(id).start)
            );
            max_scroll_after_move_down <- selection_bottom_after_move_down.map2(&frp.size,
                |y,size| y.map_or(MAX_SCROLL, |y| y + size.y)
            );
            scroll_after_move_down <- max_scroll_after_move_down.map2(&frp.scroll_position,
                |max_scroll,current| current.min(*max_scroll)
            );
            frp.source.scroll_position <+ scroll_after_move_up;
            frp.source.scroll_position <+ scroll_after_move_down;
            frp.source.scroll_position <+ frp.scroll_jump;
            frp.source.scroll_position <+ frp.set_entries.constant(MAX_SCROLL);
            eval frp.scroll_position ((scroll_y) view_y.set_target_value(*scroll_y));
            eval frp.set_entries     ((_) {
                view_y.set_target_value(MAX_SCROLL);
                view_y.skip();
            });


            // === Resize ===
            frp.source.size <+ frp.resize.map(f!([model](size)
                size - Vector2(model.padding(),model.padding()))
            );


            // === Update Entries ===

            view_info <- all_with(&view_y.value,&frp.size, |y,size|
                View{position_y:*y,size:*size}
            );
            // This should go before handling mouse events to have proper checking of
            eval view_info ((view) model.update_after_view_change(view));
            _new_entries <- frp.set_entries.map2(&view_info, f!((entries,view)
                model.set_entries(entries.clone_ref(),view)
            ));
        }

        view_y.set_target_value(MAX_SCROLL);
        view_y.skip();
        frp.scroll_jump(MAX_SCROLL);

        self
    }

    /// Sets the scene layer where the labels will be placed.
    pub fn set_label_layer(&self, layer: LayerId) {
        self.model.entries.set_label_layer(layer);
    }
}

impl<E: Entry> display::Object for ListView<E> {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl<E: Entry> application::command::FrpNetworkProvider for ListView<E> {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl<E: Entry> application::View for ListView<E> {
    fn label() -> &'static str {
        "ListView"
    }
    fn new(app: &Application) -> Self {
        ListView::new(app)
    }
    fn app(&self) -> &Application {
        &self.model.app
    }
    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::*;
        (&[
            (PressAndRepeat, "up", "move_selection_up"),
            (PressAndRepeat, "down", "move_selection_down"),
            (Press, "page-up", "move_selection_page_up"),
            (Press, "page-down", "move_selection_page_down"),
            (Press, "home", "move_selection_to_first"),
            (Press, "end", "move_selection_to_last"),
            (Press, "enter", "chose_selected_entry"),
        ])
            .iter()
            .map(|(a, b, c)| Self::self_shortcut(*a, *b, *c))
            .collect()
    }
}
