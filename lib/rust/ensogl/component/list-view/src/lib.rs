//! ListView EnsoGL Component.
//!
//! ListView a displayed list of entries with possibility of selecting one and "choosing" by
//! clicking or pressing enter - similar to the HTML `<select>`.

#![recursion_limit = "1024"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub mod entry;



/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

use crate::prelude::*;

use enso_frp as frp;
use ensogl_core::application;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::scene::layer::Layer;
use ensogl_core::display::shape::*;
use ensogl_core::display::style;
use ensogl_core::Animation;
use ensogl_hardcoded_theme as theme;

pub use entry::Entry;



// =================
// === Constants ===
// =================

const DEFAULT_STYLE_PATH: &str = theme::widget::list_view::HERE.str;

/// The additional padding inside list view background and selection, added for better antialiasing
pub const SHAPE_MARGIN: f32 = 5.0;

/// The corner radius in pixels of the background and the selection.
pub const CORNER_RADIUS_PX: f32 = 12.0;



// ==============
// === Shapes ===
// ==============

#[derive(Clone, Debug, Default, CloneRef)]
struct Selection {
    shape: Rectangle,
}

impl Selection {
    /// Set the size, and the y-position of the center of the object. These are set together because
    /// the object's implementation uses corner-origin coordinates, but the parent object uses
    /// center-origin coordinates, so the size is an input to the calculation of the y-position.
    fn set_size_and_center_y(&self, size: Vector2<f32>, center_y: f32) {
        self.shape.set_size(size);
        self.shape.set_xy(Vector2(0.0, center_y) - size / 2.0);
    }
}

impl display::Object for Selection {
    fn display_object(&self) -> &display::object::Instance {
        self.shape.display_object()
    }
}



// =============
// === Model ===
// =============

/// Information about displayed fragment of entries list.
#[derive(Copy, Clone, Debug, Default)]
pub struct View {
    position_y: f32,
    size:       Vector2<f32>,
}

/// An internal structure describing where selection would go after jump (i.e. after navigating with
/// arrows or PgUp/PgDown), assuming it can leave the list (hence `AboveAll` and `BelowAll`
/// variants).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum JumpTarget {
    Entry(entry::Id),
    AboveAll,
    BelowAll,
}

impl Default for JumpTarget {
    fn default() -> Self {
        Self::Entry(default())
    }
}

/// The Model of Select Component.
#[derive(Clone, CloneRef, Debug)]
struct Model<E: Entry> {
    app:            Application,
    entries:        entry::List<E>,
    selection:      Selection,
    background:     Rectangle,
    scrolled_area:  display::object::Instance,
    display_object: display::object::Instance,
}

impl<E: Entry> Model<E> {
    fn new(app: &Application) -> Self {
        let app = app.clone_ref();
        let display_object = display::object::Instance::new();
        let scrolled_area = display::object::Instance::new();
        let entries = entry::List::new(&app);
        let background = Rectangle();
        background.set_border_color(color::Rgba::transparent());
        let selection = Selection::default();
        selection.shape.set_pointer_events(false);
        display_object.add_child(&background);
        display_object.add_child(&scrolled_area);
        scrolled_area.add_child(&entries);
        scrolled_area.add_child(&selection);
        Model { app, entries, selection, background, scrolled_area, display_object }
    }

    /// Update the displayed entries list when _view_ has changed - the list was scrolled or
    /// resized.
    fn update_after_view_change(
        &self,
        view: &View,
        padding: f32,
        entry_padding: f32,
        style_prefix: &display::style::Path,
    ) {
        let visible_entries = Self::visible_entries(view, self.entries.entry_count());
        let padding = Vector2(2.0 * padding, 2.0 * padding);
        let entry_width = view.size.x - 2.0 * entry_padding;
        self.entries.set_x(-view.size.x / 2.0 + entry_padding);
        let background_size = view.size + padding;
        self.background.set_size(background_size);
        self.background.set_xy(-background_size / 2.0);
        self.scrolled_area.set_y(view.size.y / 2.0 - view.position_y + SHAPE_MARGIN / 2.0);
        self.entries.update_entries(visible_entries, entry_width, style_prefix);
    }

    fn set_entries(
        &self,
        provider: entry::AnyModelProvider<E>,
        view: &View,
        style_prefix: display::style::Path,
    ) {
        let visible_entries = Self::visible_entries(view, provider.entry_count());
        let entry_width = view.size.x;
        let entries = &self.entries;
        entries.update_entries_new_provider(provider, visible_entries, entry_width, style_prefix);
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

    fn jump_target(&self, current_entry: Option<entry::Id>, jump: isize) -> JumpTarget {
        if jump < 0 {
            match current_entry.and_then(|entry| entry.checked_sub(-jump as usize)) {
                Some(new_entry) => JumpTarget::Entry(new_entry),
                None => JumpTarget::AboveAll,
            }
        } else {
            let new_entry = current_entry.map_or(0, |entry| entry + jump as usize);
            if new_entry >= self.entries.entry_count() {
                JumpTarget::BelowAll
            } else {
                JumpTarget::Entry(new_entry)
            }
        }
    }

    fn selected_entry_after_jump(
        &self,
        current_entry: Option<entry::Id>,
        jump_target: JumpTarget,
    ) -> Option<entry::Id> {
        match jump_target {
            JumpTarget::Entry(entry) => Some(entry),
            JumpTarget::AboveAll if current_entry == Some(0) => None,
            JumpTarget::AboveAll if current_entry.is_some() => Some(0),
            JumpTarget::AboveAll => None,
            JumpTarget::BelowAll => self.entries.entry_count().checked_sub(1),
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
        /// Hide selection widget. The entries still could be selected, and the navigation with
        /// mouse and keyboard will work. Used in cases where the ListView user want to manage the
        /// selection widget (e.g. when the selection is shared between many lists).
        hide_selection(),
        /// Disable selecting entries when hovering the list view with the mouse. Choosing
        /// entries when clicking on them is still possible.
        disable_selecting_entries_with_mouse(),

        resize(Vector2<f32>),
        scroll_jump(f32),
        set_entries(entry::AnyModelProvider<E>),
        select_entry(Option<entry::Id>),
        chose_entry(entry::Id),
        set_style_prefix(String),
        set_background_corners_radius(f32),
        set_background_color(color::Rgba),
    }

    Output {
        is_mouse_over(bool),
        selected_entry(Option<entry::Id>),
        chosen_entry(Option<entry::Id>),
        size(Vector2<f32>),
        scroll_position(f32),
        /// The position where the selection widget  is animated to. May be used in cases where the
        /// ListView user want to manage the selection widget (e.g. when the selection is shared
        /// between many lists).
        selection_position_target(Vector2<f32>),
        /// The size of the selection widget to. May be used in cases where the ListView
        /// user want to manage the selection widget (e.g. when the selection is shared between many
        /// lists).
        selection_size(Vector2<f32>),
        tried_to_move_out_above(),
        tried_to_move_out_below(),
        style_prefix(String),
    }
}

/// A structure containing FRP nodes connected to appropriate style values.
///
/// [`ListView`] is a general-use component, and it different places it could be styled differently.
/// Therefore the [`ListView`] users (developers) may set the style prefix from where the
/// style values will be read. This structure keeps a network connecting a style values from a
/// particular prefix with its fields. It allows also reconnecting to another prefix without losing
/// the fields (so the connections from them will remain intact).
#[derive(Clone, CloneRef, Debug)]
struct StyleFrp {
    style_connection_network: Rc<CloneRefCell<Option<frp::Network>>>,
    background_color:         frp::Any<color::Rgba>,
    selection_color:          frp::Any<color::Rgba>,
    selection_corner_radius:  frp::Any<f32>,
    selection_height:         frp::Any<f32>,
    padding:                  frp::Any<f32>,
    entry_padding:            frp::Any<f32>,
}

impl StyleFrp {
    fn new(network: &frp::Network) -> Self {
        let style_connection_network = default();
        frp::extend! { network
            background_color <- any(...);
            selection_color <- any(...);
            selection_corner_radius <- any(...);
            selection_height <- any(...);
            padding <- any(...);
            entry_padding <- any(...);
        }
        Self {
            style_connection_network,
            background_color,
            selection_color,
            selection_corner_radius,
            selection_height,
            padding,
            entry_padding,
        }
    }

    /// Connect the structure's fields with new style prefix. The bindings with the previous
    /// prefix will be removed.
    fn connect_with_prefix(&self, style: &StyleWatchFrp, prefix: &style::Path) {
        let style_connection_network = frp::Network::new("list_view::StyleFrp");
        let background_color = style.get_color(prefix.sub("background"));
        let selection_color = style.get_color(prefix.sub("highlight"));
        let selection_corner_radius =
            style.get_number(prefix.sub("highlight").sub("corner_radius"));
        let selection_height = style.get_number(prefix.sub("highlight").sub("height"));
        let padding = style.get_number(prefix.sub("padding"));
        let entry_padding = style.get_number(prefix.sub("entry").sub("padding"));
        frp::extend! { style_connection_network
            init <- source_();
            self.background_color <+ all(&background_color, &init)._0();
            self.selection_color <+ all(&selection_color, &init)._0();
            self.selection_corner_radius <+ all(&selection_corner_radius, &init)._0();
            self.selection_height <+ all(&selection_height, &init)._0();
            self.padding <+ all(&padding, &init)._0();
            self.entry_padding <+ all(&entry_padding, &init)._0();
        }
        // At this point the old network is dropped, and old connections are removed.
        self.style_connection_network.set(Some(style_connection_network));
        init.emit(());
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
    model:     Model<E>,
    pub frp:   Frp<E>,
    style_frp: StyleFrp,
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
        let style_frp = StyleFrp::new(&frp.network);
        ListView { model, frp, style_frp }.init(app)
    }

    fn init(self, app: &Application) -> Self {
        const MOUSE_MOVE_THRESHOLD: f32 = std::f32::EPSILON;

        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let scene = &app.display.default_scene;
        let mouse = &scene.mouse.frp_deprecated;
        let view_y = Animation::<f32>::new(network);
        let selection_y = Animation::<f32>::new(network);
        let selection_height = Animation::<f32>::new(network);
        let style_watch = StyleWatchFrp::new(&scene.style_sheet);
        let style = &self.style_frp;

        frp::extend! { network

            // === Background ===

            init <- source_();
            default_background_corners_radius <- init.constant(CORNER_RADIUS_PX);
            background_corners_radius <- any(
                &default_background_corners_radius,&frp.set_background_corners_radius);
            eval background_corners_radius ((px) model.background.set_corner_radius(*px););
            background_color <- any(&style.background_color, &frp.set_background_color);
            eval background_color ((color) model.background.color.set(color.into()));


            // === Mouse Position ===

            let mouse_events = &model.background.events_deprecated;
            mouse_in <- bool(&mouse_events.mouse_out, &mouse_events.mouse_over);
            frp.source.is_mouse_over <+ mouse_in;
            mouse_moved <- mouse.distance.map(|dist| *dist > MOUSE_MOVE_THRESHOLD ).on_true();
            mouse_moved_in <- mouse_in.on_true();
            can_select <- any(&mouse_moved, &mouse_moved_in).gate(&mouse_in);
            mouse_y_in_scroll <- mouse.position.map(f!([model,scene](pos) {
                scene.screen_to_object_space(&model.scrolled_area,*pos).y
            }));
            mouse_pointed_entry <- mouse_y_in_scroll.map(f!([model](y)
                entry::List::<E>::entry_at_y_position(*y,model.entries.entry_count()).entry()
            ));
            mouse_selected_entry <- mouse_pointed_entry.sample(&can_select).filter(|e| e.is_some());


            // === Selected Entry ===

            eval frp.source.focused([frp](f) if !f { frp.deselect_entries.emit(()) } );

            frp.source.selected_entry <+ frp.select_entry;
            frp.source.selected_entry <+ frp.output.chosen_entry;

            selection_jump_on_one_up <- frp.move_selection_up.constant(-1);
            selection_jump_on_page_up <- frp.move_selection_page_up.map(f_!([model]
                -(model.entries.visible_entry_count() as isize)
            ));
            selection_jump_on_one_down <- frp.move_selection_down.constant(1);
            selection_jump_on_page_down <- frp.move_selection_page_down.map(f_!(
                model.entries.visible_entry_count() as isize
            ));
            selection_jump_up <- any(selection_jump_on_one_up,selection_jump_on_page_up);
            selection_jump_down <- any(selection_jump_on_one_down,selection_jump_on_page_down);
            jump_up_target <- selection_jump_up.map2(&frp.selected_entry,
                f!((jump,id) model.jump_target(*id,*jump))
            );
            selected_entry_after_jump_up <- jump_up_target.map2(&frp.selected_entry,
                f!((jump,id) model.selected_entry_after_jump(*id,*jump))
            );
            selected_entry_after_moving_first <- frp.move_selection_to_first.map(f!([model](())
                (model.entries.entry_count() > 0).and_option(Some(0))
            ));
            selected_entry_after_moving_last  <- frp.move_selection_to_last.map(f!([model] (())
                model.entries.entry_count().checked_sub(1)
            ));
            jump_down_target <- selection_jump_down.map2(&frp.selected_entry,
                f!((jump,id) model.jump_target(*id,*jump))
            );
            selected_entry_after_jump_down <- jump_down_target.map2(&frp.selected_entry,
                f!((jump,id) model.selected_entry_after_jump(*id,*jump))
            );
            selected_entry_after_move_up <-
                any(selected_entry_after_jump_up,selected_entry_after_moving_first);
            selected_entry_after_move_down <-
                any(selected_entry_after_jump_down,selected_entry_after_moving_last);
            selected_entry_after_move <-
                any(&selected_entry_after_move_up,&selected_entry_after_move_down);

            mouse_hover_selects_entries <- any(...);
            mouse_hover_selects_entries <+ init.constant(true);
            mouse_hover_selects_entries <+ frp.disable_selecting_entries_with_mouse.constant(false);
            frp.source.selected_entry <+ selected_entry_after_move;
            frp.source.selected_entry <+ mouse_selected_entry.gate(&mouse_hover_selects_entries);
            frp.source.selected_entry <+ frp.deselect_entries.constant(None);
            frp.source.selected_entry <+ frp.set_entries.constant(None);
            jump_target <- any(jump_up_target, jump_down_target);
            jumped_above <- jump_target.on_change().filter(|t| matches!(t, JumpTarget::AboveAll));
            jumped_below <- jump_target.on_change().filter(|t| matches!(t, JumpTarget::BelowAll));
            frp.source.tried_to_move_out_above <+ jumped_above.constant(());
            frp.source.tried_to_move_out_below <+ jumped_below.constant(());


            // === Chosen Entry ===

            any_entry_selected        <- frp.selected_entry.map(|e| e.is_some());
            any_entry_pointed         <- mouse_pointed_entry.map(|e| e.is_some());
            opt_selected_entry_chosen <- frp.selected_entry.sample(&frp.chose_selected_entry);
            opt_pointed_entry_chosen  <- mouse_pointed_entry.sample(&mouse.down_0).gate(&mouse_in);
            frp.source.chosen_entry   <+ opt_pointed_entry_chosen.gate(&any_entry_pointed);
            frp.source.chosen_entry   <+ frp.chose_entry.map(|id| Some(*id));
            frp.source.chosen_entry   <+ opt_selected_entry_chosen.gate(&any_entry_selected);


            // === Selection Size and Position ===

            selection_y.target <+ frp.selected_entry.filter_map(|id|
                id.map(entry::List::<E>::position_y_of_entry)
            );
            selection_height.target <+ all_with(&frp.selected_entry, &style.selection_height, |id, h|
                if id.is_some() {*h} else {-SHAPE_MARGIN}
            );
            selection_y.skip <+ frp.set_entries.constant(());
            selection_height.skip <+ frp.set_entries.constant(());
            selection_sprite_y <- all_with3(&selection_y.value, &selection_height.value, &style.selection_height,
                |y, h, max_h| y + (max_h - h) / 2.0
            );
            frp.source.selection_size <+ all_with3(&frp.size, &style.padding, &selection_height.value, f!([](size, padding, height) {
                let width = size.x - 2.0 * padding;
                Vector2(width,*height)
            }));
            selection_size_and_y <- all_with(&frp.selection_size, &selection_sprite_y, |size, y| (*size, *y));
            eval selection_size_and_y ([model]((size, y)) {
                model.selection.set_size_and_center_y(*size, *y);
            });
            eval_ frp.hide_selection (model.selection.unset_parent());


            // === Scrolling ===

            max_scroll <- style.selection_height.map(|h| *h / 2.0).sampler();
            selection_top_after_move_up <- selected_entry_after_move_up.map2(&style.selection_height, |id, h|
                id.map(|id| entry::List::<E>::position_y_of_entry(id) + *h / 2.0)
            );
            min_scroll_after_move_up <- selection_top_after_move_up.map2(&max_scroll, |top, max_scroll|
                top.unwrap_or(*max_scroll)
            );
            scroll_after_move_up <- min_scroll_after_move_up.map2(&frp.scroll_position,|min,current|
                current.max(*min)
            );
            selection_bottom_after_move_down <- selected_entry_after_move_down.map2(&style.selection_height, |id, h|
                id.map(|id| entry::List::<E>::position_y_of_entry(id) - *h / 2.0)
            );
            max_scroll_after_move_down <- selection_bottom_after_move_down.map4(
                &frp.size,
                &style.padding,
                &max_scroll,
                |y, size, padding, max_scroll| y.map_or(*max_scroll, |y| y + size.y - 2.0 * padding)
            );
            scroll_after_move_down <- max_scroll_after_move_down.map2(&frp.scroll_position,
                |max_scroll,current| current.min(*max_scroll)
            );
            frp.source.scroll_position <+ scroll_after_move_up;
            frp.source.scroll_position <+ scroll_after_move_down;
            frp.source.scroll_position <+ frp.scroll_jump;
            frp.source.scroll_position <+ max_scroll.sample(&frp.set_entries);
            view_y.target <+ frp.scroll_position;
            view_y.target <+ max_scroll.sample(&frp.set_entries);
            view_y.skip <+ frp.set_entries.constant(());
            view_y.target <+ max_scroll.sample(&init);
            view_y.skip <+ init;


            // === Resize ===
            frp.source.size <+ frp.resize;


            // === Update Entries ===

            view_info <- all_with3(&view_y.value, &frp.size, &style.padding, |&y, &size, &padding| {
                let padding = Vector2(2.0 * padding, 2.0 * padding);
                View { position_y: y, size: size - padding }
            });
            default_style_prefix <- init.constant(DEFAULT_STYLE_PATH.to_string());
            style_prefix <- any(&default_style_prefix,&frp.set_style_prefix);
            eval style_prefix ([model, style, style_watch](path) {
                style.connect_with_prefix(&style_watch, &path.into());
                model.entries.recreate_entries_with_style_prefix(path.into());
            });
            view_and_style <- all(view_info, style.padding, style.entry_padding, style_prefix);
            // This should go before handling mouse events to have proper checking of
            eval view_and_style (((view, padding, entry_padding, style))
                model.update_after_view_change(view, *padding, *entry_padding, &style.into()));
            _new_entries <- frp.set_entries.map2(&view_and_style, f!((entries, (view, _, _, style))
                model.set_entries(entries.clone_ref(), view, style.into())
            ));

            frp.source.selection_position_target <+ all_with4(
                &selection_y.target,
                &view_y.target,
                &frp.size,
                &style.padding,
                |sel_y, view_y, size, padding| Vector2(0.0, (size.y / 2.0 - padding) - view_y + sel_y)
            );
            eval style.selection_color ((color) model.selection.shape.color.set(color.into()));
            eval style.selection_corner_radius ((radius) model.selection.shape.corner_radius.set(*radius));
        }

        init.emit(());
        frp.scroll_jump(max_scroll.value());

        self
    }

    /// Sets the scene layer where the labels will be placed.
    pub fn set_label_layer(&self, layer: &Layer) {
        self.model.entries.set_label_layer(layer);
    }

    /// Set params used in the displayed entries and recreate all displayed entries.
    pub fn set_entry_params_and_recreate_entries(&self, params: E::Params) {
        let style_prefix = self.frp.style_prefix.value();
        self.model.entries.set_entry_params_and_recreate_entries(params, style_prefix.into());
    }

    /// Get previously set entry params.
    pub fn entry_params(&self) -> E::Params {
        self.model.entries.entry_params()
    }
}

impl<E: Entry> display::Object for ListView<E> {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl<E: Entry> FrpNetworkProvider for ListView<E> {
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
        [
            (PressAndRepeat, "up", "move_selection_up"),
            (PressAndRepeat, "down", "move_selection_down"),
            (Press, "page-up", "move_selection_page_up"),
            (Press, "page-down", "move_selection_page_down"),
            (Press, "home", "move_selection_to_first"),
            (Press, "end", "move_selection_to_last"),
            (Press, "enter", "chose_selected_entry"),
        ]
        .iter()
        .map(|(a, b, c)| Self::self_shortcut_when(*a, *b, *c, "focused"))
        .collect()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    use crate::entry::AnyModelProvider;

    use approx::assert_relative_eq;
    use enso_frp::future::EventOutputExt;

    #[test]
    fn navigating_list_view_with_keyboard() {
        let app = Application::new("root");
        let list_view = ListView::<entry::Label>::new(&app);
        let provider = AnyModelProvider::<entry::Label>::new(vec!["Entry 1", "Entry 2", "Entry 3"]);
        list_view.set_entries(provider);

        // Going down:
        assert_eq!(list_view.selected_entry.value(), None);
        list_view.move_selection_down();
        assert_eq!(list_view.selected_entry.value(), Some(0));
        list_view.move_selection_down();
        assert_eq!(list_view.selected_entry.value(), Some(1));
        list_view.move_selection_down();
        assert_eq!(list_view.selected_entry.value(), Some(2));
        let tried_to_move_out_below = list_view.tried_to_move_out_below.next_event();
        list_view.move_selection_down();
        assert_eq!(list_view.selected_entry.value(), Some(2));
        tried_to_move_out_below.expect();
        let tried_to_move_out_below = list_view.tried_to_move_out_below.next_event();
        list_view.move_selection_down();
        assert_eq!(list_view.selected_entry.value(), Some(2));
        tried_to_move_out_below.expect_not();

        // Going up:
        list_view.move_selection_up();
        assert_eq!(list_view.selected_entry.value(), Some(1));
        list_view.move_selection_up();
        assert_eq!(list_view.selected_entry.value(), Some(0));
        let tried_to_move_out_above = list_view.tried_to_move_out_above.next_event();
        list_view.move_selection_up();
        assert_eq!(list_view.selected_entry.value(), None);
        tried_to_move_out_above.expect();
        let tried_to_move_out_above = list_view.tried_to_move_out_above.next_event();
        list_view.move_selection_up();
        assert_eq!(list_view.selected_entry.value(), None);
        tried_to_move_out_above.expect_not();

        // Special case
        list_view.move_selection_down();
        assert_eq!(list_view.selected_entry.value(), Some(0));
        let tried_to_move_out_above = list_view.tried_to_move_out_above.next_event();
        list_view.move_selection_up();
        assert_eq!(list_view.selected_entry.value(), None);
        tried_to_move_out_above.expect();
    }

    #[test]
    fn selection_position() {
        use ensogl_hardcoded_theme::widget::list_view as theme;
        let app = Application::new("root");
        let style_sheet = &app.display.default_scene.style_sheet;
        style_sheet.set(theme::highlight::height, entry::HEIGHT);
        let list_view = ListView::<entry::Label>::new(&app);
        let provider =
            AnyModelProvider::<entry::Label>::new(vec!["Entry 1", "Entry 2", "Entry 3", "Entry 4"]);
        list_view.resize(Vector2(100.0, entry::HEIGHT * 3.0));
        list_view.set_entries(provider);
        list_view.select_entry(Some(0));
        assert_relative_eq!(list_view.selection_position_target.value().x, 0.0);
        assert_relative_eq!(list_view.selection_position_target.value().y, entry::HEIGHT);
        list_view.move_selection_down(); // Selected entry 1.
        assert_relative_eq!(list_view.selection_position_target.value().x, 0.0);
        assert_relative_eq!(list_view.selection_position_target.value().y, 0.0);
        list_view.move_selection_down(); // Selected entry 2.
        assert_relative_eq!(list_view.selection_position_target.value().x, 0.0);
        assert_relative_eq!(list_view.selection_position_target.value().y, -entry::HEIGHT);
        list_view.move_selection_down(); // Selected entry 3 (should scroll).
        assert_relative_eq!(list_view.selection_position_target.value().x, 0.0);
        assert_relative_eq!(list_view.selection_position_target.value().y, -entry::HEIGHT);
    }
}
