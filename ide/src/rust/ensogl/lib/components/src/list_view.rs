//! ListView EnsoGL Component.
//!
//! ListView a displayed list of entries with possibility of selecting one and "choosing" by
//! clicking or pressing enter - similar to the HTML `<select>`.

pub mod entry;

use crate::prelude::*;
use crate::scroll_area::ScrollArea;
use crate::selector::bounds::Bounds;
use crate::selector;

use enso_frp as frp;
use ensogl_core::DEPRECATED_Animation;
use ensogl_core::application::Application;
use ensogl_core::application::shortcut;
use ensogl_core::application;
use ensogl_core::data::color;
use ensogl_core::display::shape::*;
use ensogl_core::display;

/// Describes whether list entries should be selected by hovering or clicking. This also affects
/// how entries can be chosen, as described in each variant's documentation.
#[derive(Debug,Copy,Clone)]
pub enum SelectionMethod {
    /// Entries are selected by hover and chosen by click.
    Hover,
    /// Entries are selected by click and chosen by double click.
    Click,
}

impl Default for SelectionMethod {
    fn default() -> Self {
        SelectionMethod::Hover
    }
}



// ==========================
// === Shapes Definitions ===
// ==========================

// === Constants ===

// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
/// The selection's corner radius.
pub const CORNER_RADIUS_PX: f32 = 6.0;
// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
/// The padding on the left and the right of the list.
pub const PADDING_HORIZONTAL: f32 = 12.0;
// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
/// The padding above and below the list.
pub const PADDING_VERTICAL: f32 = 10.0;


// === Selection ===

/// A rounded rectangle, used to highlight the selected list entry.
pub mod selection {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style, color_rgba:Vector4) {
            let color = Var::<color::Rgba>::from(color_rgba);

            let sprite_width  : Var<Pixels> = "input_size.x".into();
            let sprite_height : Var<Pixels> = "input_size.y".into();

            let width  = sprite_width - PADDING_HORIZONTAL.px() * 2.0;
            let height = sprite_height;
            let rect   = Rect((&width,&height)).corners_radius(CORNER_RADIUS_PX.px());
            let shape  = rect.fill(color);
            shape.into()
        }
    }
}


// === IO Rect ===

/// Utility shape that is invisible but provides mouse input. Fills the whole sprite.
pub mod io_rect {
    use super::*;

    ensogl_core::define_shape_system! {
        () {
            Plane().fill(HOVER_COLOR).into()
        }
    }
}



// =============
// === Model ===
// =============

/// Information about displayed fragment of entries list.
#[derive(Copy,Clone,Debug,Default)]
struct View {
    position_y : f32,
    size       : Vector2<f32>,
}

impl View {
    fn y_range(&self) -> Range<f32> {
        let start = self.position_y - self.size.y;
        let end   = self.position_y;
        start..end
    }
}

/// The Model of Select Component.
#[derive(Debug)]
struct Model {
    app                        : Application,
    entries                    : entry::List,
    selection                  : selection::View,
    scroll_area                : ScrollArea,
    io_rect                    : io_rect::View,
    display_object             : display::object::Instance,
    selection_can_leave_at_top : Cell<bool>,
}

impl Model {
    fn new(app:&Application) -> Self {
        let app                        = app.clone_ref();
        let scene                      = app.display.scene();
        let layers                     = &scene.layers;
        let logger                     = Logger::new("SelectionContainer");
        let display_object             = display::object::Instance::new(&logger);
        let scroll_area                = ScrollArea::new(&app);
        let entries                    = entry::List::new(&logger,&app);
        let selection                  = selection::View::new(&logger);
        let io_rect                    = io_rect::View::new(&logger);
        let selection_can_leave_at_top = Cell::new(false);
        layers.add_shapes_order_dependency::<selection::View,io_rect::View>();
        layers.add_shapes_order_dependency::<io_rect::View,selector::shape::background::View>();
        display_object.add_child(&scroll_area);
        display_object.add_child(&io_rect);
        scroll_area.content().add_child(&entries);
        scroll_area.content().add_child(&selection);
        Model{app,entries,selection,scroll_area,io_rect,display_object,selection_can_leave_at_top}
    }

    /// Update the displayed entries list when _view_ has changed - the list was scrolled or
    /// resized.
    fn update_after_view_change(&self, view:&View) {
        self.selection.set_position_x(view.size.x/2.0);
        self.entries.set_position_xy(Vector2(PADDING_HORIZONTAL,-PADDING_VERTICAL));
        self.entries.set_entry_width(view.size.x - PADDING_HORIZONTAL * 2.0);
        self.entries.set_visible_range(view.y_range());
        self.scroll_area.resize(view.size);
        self.scroll_area.set_position_x(-view.size.x/2.0);
        self.scroll_area.set_position_y(view.size.y/2.0);
        self.io_rect.size.set(view.size);
    }

    fn set_entries(&self, provider:entry::AnyEntryProvider) {
        let list_height = entry::List::total_height(provider.entry_count());
        self.scroll_area.set_content_height(list_height+PADDING_VERTICAL*2.0);
        self.entries.set_provider(provider);
    }

    fn selected_entry_after_jump
    (&self, current_entry:Option<entry::Id>, jump:isize) -> Option<entry::Id> {
        if jump < 0 {
            let current_entry = current_entry?;
            if current_entry == 0 && self.selection_can_leave_at_top.get() {
                None
            } else {
                Some(current_entry.saturating_sub(-jump as usize))
            }
        } else {
            let max_entry = self.entries.entry_count().checked_sub(1)?;
            Some(current_entry.map_or(0, |id| id+(jump as usize)).min(max_entry))
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
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

        resize                         (Vector2<f32>),
        scroll_jump                    (f32),
        skip_animations                (),
        set_entries                    (entry::AnyEntryProvider),
        set_selection_method           (SelectionMethod),
        set_selection_can_leave_at_top (bool),
        select_entry                   (entry::Id),
        chose_entry                    (entry::Id),

        click        (),
        double_click (),
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

/// `ListView` Component.
///
/// Select is a displayed list of entries with possibility of selecting one and "chosing".
#[allow(missing_docs)]
#[derive(Clone,CloneRef,Debug)]
pub struct ListView {
    model   : Rc<Model>,
    pub frp : Frp,
}

impl Deref for ListView {
    type Target = Frp;
    fn deref(&self) -> &Self::Target { &self.frp }
}

impl ListView {
    /// Constructor.
    pub fn new(app:&Application) -> Self {
        let frp   = Frp::new();
        let model = Rc::new(Model::new(app));
        ListView {model,frp}.init(app)
    }

    fn init(self, app:&Application) -> Self {
        let frp              = &self.frp;
        let network          = &frp.network;
        let model            = &self.model;
        let scene            = app.display.scene();
        let mouse            = &scene.mouse.frp;
        let selection_y      = DEPRECATED_Animation::<f32>::new(&network);
        let selection_height = DEPRECATED_Animation::<f32>::new(&network);

        let style = StyleWatchFrp::new(&scene.style_sheet);
        use ensogl_theme::widget::list_view as theme;
        let selection_color           = color::Animation::new(&network);
        let focused_selection_color   = style.get_color(theme::selection::focused);
        let unfocused_selection_color = style.get_color(theme::selection::unfocused);

        frp::extend! { network
            init_selection_color      <- source::<()>();
            unfocused_selection_color <- all(&unfocused_selection_color,&init_selection_color)._0();
            focused_selection_color   <- all(&focused_selection_color,&init_selection_color)._0();

            selection_color_target_rgba <- frp.focused.switch(&unfocused_selection_color,
                &focused_selection_color);
            selection_color.target <+ selection_color_target_rgba.map(|c| color::Lcha::from(c));
            eval selection_color.value((c)
                model.selection.color_rgba.set(color::Rgba::from(c).into()));
            init_selection_color.emit(());


            // === Mouse Position ===

            mouse_in <- bool(&model.io_rect.events.mouse_out,&model.io_rect.events.mouse_over);
            mouse_y_in_scroll <- all_with(&mouse.position,&model.scroll_area.scroll_position_y,
                f!([model,scene](pos,_) {
                    scene.screen_to_object_space(&model.scroll_area.content(),*pos).y
                }));
            mouse_pointed_entry <- mouse_y_in_scroll.map(f!([model](y)
                let y_in_list = *y + PADDING_VERTICAL;
                entry::List::entry_at_y_position(y_in_list,model.entries.entry_count()).entry()
            ));
            any_entry_pointed    <- mouse_pointed_entry.map(|e| e.is_some());
            entry_clicked        <- mouse_pointed_entry.sample(&frp.click).gate(&mouse_in);
            entry_clicked        <- entry_clicked.gate(&any_entry_pointed);
            entry_double_clicked <- mouse_pointed_entry.sample(&frp.double_click).gate(&mouse_in);
            entry_double_clicked <- entry_double_clicked.gate(&any_entry_pointed);


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
            hover_selected_entry <- mouse_pointed_entry.gate(&mouse_in);
            hover_selected_entry <- hover_selected_entry.gate_not(&mouse.is_down_primary);

            select_on_hover <- frp.set_selection_method.map(|&method|
                matches!(method, SelectionMethod::Hover));
            select_on_click <- frp.set_selection_method.map(|&method|
                matches!(method, SelectionMethod::Click));

            frp.source.selected_entry <+ selected_entry_after_move;
            frp.source.selected_entry <+ hover_selected_entry.gate(&select_on_hover);
            frp.source.selected_entry <+ entry_clicked.gate(&select_on_click);
            frp.source.selected_entry <+ frp.deselect_entries.constant(None);
            frp.source.selected_entry <+ frp.set_entries.constant(None);

            eval frp.selected_entry((&selection) model.entries.set_selection(selection));
            eval frp.focused((&focused) model.entries.set_focused(focused));

            eval frp.set_selection_can_leave_at_top((&can_leave)
                model.selection_can_leave_at_top.set(can_leave));


            // === Chosen Entry ===

            any_entry_selected        <- frp.selected_entry.map(|e| e.is_some());
            opt_selected_entry_chosen <- frp.selected_entry.sample(&frp.chose_selected_entry);
            frp.source.chosen_entry   <+ entry_clicked.gate(&select_on_hover);
            frp.source.chosen_entry   <+ entry_double_clicked.gate(&select_on_click);
            frp.source.chosen_entry   <+ frp.chose_entry.map(|id| Some(*id));
            frp.source.chosen_entry   <+ opt_selected_entry_chosen.gate(&any_entry_selected);


            // === Selection Size and Position ===

            target_selection_y <- frp.selected_entry.map(|id|
                entry::List::position_y_of_entry(id.unwrap_or(0)) - PADDING_VERTICAL
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
            selection_sprite_y <- all_with(&selection_y.value,&selection_height.value,
                |y,h| y + (entry::HEIGHT - h) / 2.0
            );
            eval selection_sprite_y ((y) model.selection.set_position_y(*y));
            selection_size <- all_with(&frp.size,&selection_height.value,f!([](size,height) {
                let width = size.x;
                Vector2(width,*height)
            }));
            eval selection_size ((size) model.selection.size.set(*size));
            eval frp.skip_animations([selection_y,selection_height](_) {
                selection_y.skip();
                selection_height.skip();
            });
        }

        frp::extend!{ network

            // === Scrolling ===

            scroll_to_entry <- any_mut::<Option<entry::Id>>();
            scroll_to_entry <+ selected_entry_after_move;
            scroll_to_entry <+ entry_clicked.gate(&select_on_click);
            model.scroll_area.scroll_to_y_range <+ scroll_to_entry.filter_map(|&id| {
                let range = entry::List::y_range_of_entry(id?);
                Some(Bounds::new(-range.end,-range.start+2.0*PADDING_VERTICAL))
            });
            model.scroll_area.scroll_to_y <+ frp.scroll_jump;
            model.scroll_area.jump_to_y   <+ frp.set_entries.constant(0.0);
            frp.source.scroll_position    <+ model.scroll_area.scroll_position_y;


            // === Resize ===
            frp.source.size <+ frp.resize;


            // === Update Entries ===

            view_info <- all_with(&model.scroll_area.scroll_position_y,&frp.size,|y,size|
                View{position_y:-*y+PADDING_VERTICAL,size:*size}
            );
            // This should go before handling mouse events to have proper checking of
            eval view_info ((view) model.update_after_view_change(view));
            _new_entries <- frp.set_entries.map(f!((entries)
                model.set_entries(entries.clone_ref())
            ));
        }

        frp.set_selection_method(SelectionMethod::Hover);

        self
    }
}

impl display::Object for ListView {
    fn display_object(&self) -> &display::object::Instance { &self.model.display_object }
}

impl application::command::FrpNetworkProvider for ListView {
    fn network(&self) -> &frp::Network { &self.frp.network }
}

impl application::View for ListView {
    fn label() -> &'static str { "ListView" }
    fn new(app:&Application) -> Self { ListView::new(app) }
    fn app(&self) -> &Application { &self.model.app }
    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::*;
        (&[ (PressAndRepeat , "up"                , "move_selection_up"        , "focused")
          , (PressAndRepeat , "down"              , "move_selection_down"      , "focused")
          , (Press          , "page-up"           , "move_selection_page_up"   , "focused")
          , (Press          , "page-down"         , "move_selection_page_down" , "focused")
          , (Press          , "home"              , "move_selection_to_first"  , "focused")
          , (Press          , "end"               , "move_selection_to_last"   , "focused")
          , (Press          , "enter"             , "chose_selected_entry"     , "focused")
          , (Press          , "left-mouse-button" , "click"                    , "")
          , (DoublePress    , "left-mouse-button" , "double_click"             , "")
          ]).iter().map(|(a,b,c,d)|Self::self_shortcut_when(*a,*b,*c,*d)).collect()
    }
}
