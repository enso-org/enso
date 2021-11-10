//! The text area implementation. It serves the purpose of single and multi-line text labels and
//! text editors.

use crate::prelude::*;

use crate::buffer;
use crate::buffer::data::unit::*;
use crate::buffer::style;
use crate::buffer::Text;
use crate::buffer::Transform;
use crate::component::selection;
use crate::component::Selection;
use crate::typeface;
use crate::typeface::glyph;
use crate::typeface::glyph::Glyph;
use crate::typeface::pen;

use enso_frp as frp;
use enso_frp::io::keyboard::Key;
use ensogl_core::application;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::*;
use ensogl_core::gui::cursor;
use ensogl_core::system::web::clipboard;
use ensogl_core::DEPRECATED_Animation;
use std::ops::Not;



// =================
// === Constants ===
// =================

/// Record separator ASCII code. Used for separating of copied strings. It is defined as the `\RS`
/// escape code (`x1E`) (https://en.wikipedia.org/wiki/ASCII).
pub const RECORD_SEPARATOR: &str = "\x1E";
const LINE_VERTICAL_OFFSET: f32 = 4.0; // Set manually. May depend on font. To be improved.



// ====================
// === SelectionMap ===
// ====================

/// Mapping between selection id, `Selection`, and text location.
#[derive(Clone, Debug, Default)]
pub struct SelectionMap {
    id_map:       HashMap<usize, Selection>,
    location_map: HashMap<usize, HashMap<Column, usize>>,
}



// ============
// === Line ===
// ============

/// Visual line representation.
///
/// **Design Notes**
/// The `divs` and `centers` are kept as vectors for performance reasons. Especially, when clicking
/// inside of the text area, it allows us to binary search the place of the mouse pointer.
#[derive(Debug)]
pub struct Line {
    display_object: display::object::Instance,
    glyphs:         Vec<Glyph>,
    divs:           Vec<f32>,
    centers:        Vec<f32>,
}

impl Line {
    fn new(logger: impl AnyLogger) -> Self {
        let logger = Logger::new_sub(logger, "line");
        let display_object = display::object::Instance::new(logger);
        let glyphs = default();
        let divs = default();
        let centers = default();
        Self { display_object, glyphs, divs, centers }
    }

    /// Set the division points (offsets between letters). Also updates center points.
    fn set_divs(&mut self, divs: Vec<f32>) {
        let div_iter = divs.iter();
        let div_iter_skipped = divs.iter().skip(1);
        self.centers = div_iter.zip(div_iter_skipped).map(|(t, s)| (t + s) / 2.0).collect();
        self.divs = divs;
    }

    fn div_index_close_to(&self, offset: f32) -> usize {
        self.centers.binary_search_by(|t| t.partial_cmp(&offset).unwrap()).unwrap_both()
    }

    fn div_by_column(&self, column: Column) -> f32 {
        let ix = column.as_usize().min(self.divs.len() - 1);
        self.divs[ix]
    }

    fn resize_with(&mut self, size: usize, cons: impl Fn() -> Glyph) {
        let display_object = self.display_object().clone_ref();
        self.glyphs.resize_with(size, move || {
            let glyph = cons();
            display_object.add_child(&glyph);
            glyph
        });
    }
}

impl display::Object for Line {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =============
// === Lines ===
// =============

/// Set of all visible lines.
#[derive(Clone, CloneRef, Debug, Default)]
struct Lines {
    rc: Rc<RefCell<Vec<Line>>>,
}

impl Lines {
    /// The number of visible lines.
    pub fn len(&self) -> usize {
        self.rc.borrow().len()
    }

    /// Resize the line container and use the provided function to construct missing elements.
    pub fn resize_with(&self, size: usize, cons: impl Fn(usize) -> Line) {
        let vec = &mut self.rc.borrow_mut();
        let mut ix = vec.len();
        vec.resize_with(size, || {
            let line = cons(ix);
            ix += 1;
            line
        })
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        /// Insert character of the last pressed key at every cursor.
        insert_char_of_last_pressed_key(),
        /// Increase the indentation of all lines containing cursors.
        increase_indentation(),
        /// Decrease the indentation of all lines containing cursors.
        decrease_indentation(),
        /// Removes the character on the left of every cursor.
        delete_left(),
        /// Removes the character on the right of every cursor.
        delete_right(),
        /// Removes the word on the left of every cursor.
        delete_word_left(),
        /// Removes the word on the right of every cursor.
        delete_word_right(),
        /// Set the text cursor at the mouse cursor position.
        set_cursor_at_mouse_position(),
        /// Set the text cursor at the end of text.
        set_cursor_at_end(),
        /// Add a new cursor at the mouse cursor position.
        add_cursor_at_mouse_position(),
        /// Remove all cursors.
        remove_all_cursors(),
        /// Start changing the shape of the newest selection with the mouse position.
        start_newest_selection_end_follow_mouse(),
        /// Stop changing the shape of the newest selection with the mouse position.
        stop_newest_selection_end_follow_mouse(),
        /// Move the cursor to the left by one character.
        cursor_move_left(),
        /// Move the cursor to the right by one character.
        cursor_move_right(),
        /// Move the cursor to the left by one word.
        cursor_move_left_word(),
        /// Move the cursor to the right by one word.
        cursor_move_right_word(),
        /// Move the cursor to the beginning of the line.
        cursor_move_left_of_line(),
        /// Move the cursor to the end of the line.
        cursor_move_right_of_line(),
        /// Move the cursor down one line.
        cursor_move_down(),
        /// Move the cursor up one line.
        cursor_move_up(),
        /// Extend the cursor selection to the left by one character.
        cursor_select_left(),
        /// Extend the cursor selection to the right by one character.
        cursor_select_right(),
        /// Extend the cursor selection down one line.
        cursor_select_down(),
        /// Extend the cursor selection up one line.
        cursor_select_up(),
        /// Extend the cursor selection to the left by one word.
        cursor_select_left_word(),
        /// Extend the cursor selection to the right by one word.
        cursor_select_right_word(),
        /// Select all characters.
        select_all(),
        /// Select the word at cursor position.
        select_word_at_cursor(),
        /// Discard all but the first selection.
        keep_first_selection_only(),
        /// Discard all but the last selection.
        keep_last_selection_only(),
        /// Discard all but the first selection and convert it to cursor.
        keep_first_cursor_only(),
        /// Discard all but the last selection and convert it to cursor.
        keep_last_cursor_only(),
        /// Discard all but the newest selection.
        keep_newest_selection_only(),
        /// Discard all but the oldest selection.
        keep_oldest_selection_only(),
        /// Discard all but the newest selection and convert it to cursor.
        keep_newest_cursor_only(),
        /// Discard all but the oldest selection and convert it to cursor.
        keep_oldest_cursor_only(),
        /// Set the oldest selection end to mouse position.
        set_newest_selection_end_to_mouse_position(),
        /// Set the newest selection end to mouse position.
        set_oldest_selection_end_to_mouse_position(),
        /// Undo the last operation.
        undo(),
        /// Redo the last operation.
        redo(),
        /// Copy the selected text to the clipboard.
        copy(),
        /// Copy the selected text to the clipboard and remove it from the text area.
        cut(),
        /// Paste the selected text from the clipboard.
        paste(),

        hover(),
        unhover(),
        single_line(bool),
        set_hover(bool),

        set_cursor            (Location),
        add_cursor            (Location),
        paste_string          (String),
        insert                (String),
        set_color_bytes       (buffer::Range<Bytes>,color::Rgba),
        set_color_all         (color::Rgba),
        set_default_color     (color::Rgba),
        set_selection_color   (color::Rgb),
        set_default_text_size (style::Size),
        set_content           (String),
    }
    Output {
        pointer_style   (cursor::Style),
        width           (f32),
        height          (f32),
        changed         (Vec<buffer::view::Change>),
        content         (Text),
        hovered         (bool),
        selection_color (color::Rgb),
    }
}



// ============
// === Area ===
// ============

/// Hardcoded line height. To be generalized in the future.
pub const LINE_HEIGHT: f32 = 14.0;

/// The visual text area implementation.
#[derive(Clone, CloneRef, Debug)]
#[allow(missing_docs)]
pub struct Area {
    data:    Rc<AreaModel>,
    pub frp: Rc<Frp>,
}

impl Deref for Area {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Area {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Rc::new(Frp::new());
        let data = Rc::new(AreaModel::new(app, &frp.output));
        Self { data, frp }.init()
    }

    fn init(self) -> Self {
        let network = &self.frp.network;
        let model = &self.data;
        let scene = model.app.display.scene();
        let mouse = &scene.mouse.frp;
        let input = &self.frp.input;
        let out = &self.frp.output;
        let pos = DEPRECATED_Animation::<Vector2>::new(network);
        let keyboard = &scene.keyboard;
        let m = &model;
        pos.update_spring(|spring| spring * 2.0);

        frp::extend! { network

            // === Multi / Single Line ===

            eval input.single_line((t) m.single_line.set(*t));


            // === Hover ===

            hover_events  <- bool(&input.unhover,&input.hover);
            hovered       <- any(&input.set_hover,&hover_events);
            out.source.hovered <+ hovered;


            // === Pointer Style ===

            pointer_style <- hovered.map(|hovered| {
                if *hovered { cursor::Style::new_text_cursor() } else { cursor::Style::default() }
            });
            out.source.pointer_style <+ pointer_style;


            // === Set / Add cursor ===

            mouse_on_set_cursor      <- mouse.position.sample(&input.set_cursor_at_mouse_position);
            mouse_on_add_cursor      <- mouse.position.sample(&input.add_cursor_at_mouse_position);
            loc_on_set_cursor_mouse  <- mouse_on_set_cursor.map(f!((p) m.get_in_text_location(*p)));
            loc_on_add_cursor_mouse  <- mouse_on_add_cursor.map(f!((p) m.get_in_text_location(*p)));
            loc_on_set_cursor_at_end <- input.set_cursor_at_end.map(f_!(model.buffer.text().location_of_text_end()));
            loc_on_set_cursor        <- any(input.set_cursor,loc_on_set_cursor_mouse,loc_on_set_cursor_at_end);
            loc_on_add_cursor        <- any(&input.add_cursor,&loc_on_add_cursor_mouse);

            eval loc_on_set_cursor ((loc) m.buffer.frp.set_cursor(loc));
            eval loc_on_add_cursor ((loc) m.buffer.frp.add_cursor(loc));

            _eval <- m.buffer.frp.selection_edit_mode.map2
                (&scene.frp.frame_time,f!([m](selections,time) {
                        // FIXME: added for undo redo. Should not be needed.
                        //        See https://github.com/enso-org/ide/issues/1031
                        m.redraw(true);
                        m.on_modified_selection(selections,*time,true)
                    }
            ));

            _eval <- m.buffer.frp.selection_non_edit_mode.map2
                (&scene.frp.frame_time,f!([m](selections,time) {
                    // FIXME: added for undo redo. Should not be needed.
                    //        See https://github.com/enso-org/ide/issues/1031
                    m.redraw(true);
                    m.on_modified_selection(selections,*time,false)
                }
            ));

            selecting <- bool
                ( &input.stop_newest_selection_end_follow_mouse
                , &input.start_newest_selection_end_follow_mouse
                );

            set_sel_end_1 <- mouse.position.gate(&selecting);
            set_sel_end_2 <- mouse.position.sample(&input.set_newest_selection_end_to_mouse_position);
            set_newest_selection_end <- any(&set_sel_end_1,&set_sel_end_2);

            eval set_newest_selection_end([m](screen_pos) {
                let location = m.get_in_text_location(*screen_pos);
                m.buffer.frp.set_newest_selection_end(location);
            });


            // === Copy / Cut / Paste ===

            copy_sels      <- input.copy.map(f_!(m.buffer.selections_contents()));
            all_empty_sels <- copy_sels.map(|s|s.iter().all(|t|t.is_empty()));
            line_sel_mode  <- copy_sels.gate(&all_empty_sels);

            eval_ line_sel_mode (m.buffer.frp.cursors_select(Some(Transform::Line)));
            non_line_sel_mode_sels <- copy_sels.gate_not(&all_empty_sels);
            line_sel_mode_sels     <- line_sel_mode.map(f_!(m.buffer.selections_contents()));
            sels                   <- any(&line_sel_mode_sels,&non_line_sel_mode_sels);
            eval sels ((s) m.copy(s));

            cut_sels           <- input.cut.map(f_!(m.buffer.selections_contents()));
            all_empty_sels_cut <- cut_sels.map(|s|s.iter().all(|t|t.is_empty()));
            line_sel_mode_cut  <- cut_sels.gate(&all_empty_sels_cut);

            eval_ line_sel_mode_cut (m.buffer.frp.cursors_select(Some(Transform::Line)));
            non_line_sel_mode_cut_sels <- cut_sels.gate_not(&all_empty_sels_cut);
            line_sel_mode_cut_sels     <- line_sel_mode_cut.map(f_!(m.buffer.selections_contents()));
            sels_cut                   <- any(&line_sel_mode_cut_sels,&non_line_sel_mode_cut_sels);
            eval sels_cut ((s) m.cut(s));
            eval_ sels_cut (m.buffer.frp.delete_left());

            eval_ input.paste (m.paste());
            eval input.paste_string((s) m.paste_string(s));


            eval_ m.buffer.frp.text_change (m.redraw(true));

            eval_ input.remove_all_cursors (m.buffer.frp.remove_all_cursors());

            eval_ input.keep_first_selection_only (m.buffer.frp.keep_first_selection_only());
            eval_ input.keep_last_selection_only  (m.buffer.frp.keep_last_selection_only());
            eval_ input.keep_first_cursor_only    (m.buffer.frp.keep_first_cursor_only());
            eval_ input.keep_last_cursor_only     (m.buffer.frp.keep_last_cursor_only());

            eval_ input.keep_newest_selection_only (m.buffer.frp.keep_newest_selection_only());
            eval_ input.keep_oldest_selection_only (m.buffer.frp.keep_oldest_selection_only());
            eval_ input.keep_newest_cursor_only    (m.buffer.frp.keep_newest_cursor_only());
            eval_ input.keep_oldest_cursor_only    (m.buffer.frp.keep_oldest_cursor_only());

            eval_ input.cursor_move_left  (m.buffer.frp.cursors_move(Transform::Left));
            eval_ input.cursor_move_right (m.buffer.frp.cursors_move(Transform::Right));
            eval_ input.cursor_move_up    (m.buffer.frp.cursors_move(Transform::Up));
            eval_ input.cursor_move_down  (m.buffer.frp.cursors_move(Transform::Down));

            eval_ input.cursor_move_left_word  (m.buffer.frp.cursors_move(Transform::LeftWord));
            eval_ input.cursor_move_right_word (m.buffer.frp.cursors_move(Transform::RightWord));

            eval_ input.cursor_move_left_of_line  (m.buffer.frp.cursors_move(Transform::LeftOfLine));
            eval_ input.cursor_move_right_of_line (m.buffer.frp.cursors_move(Transform::RightOfLine));

            eval_ input.cursor_select_left  (m.buffer.frp.cursors_select(Transform::Left));
            eval_ input.cursor_select_right (m.buffer.frp.cursors_select(Transform::Right));
            eval_ input.cursor_select_up    (m.buffer.frp.cursors_select(Transform::Up));
            eval_ input.cursor_select_down  (m.buffer.frp.cursors_select(Transform::Down));

            eval_ input.cursor_select_left_word  (m.buffer.frp.cursors_select(Transform::LeftWord));
            eval_ input.cursor_select_right_word (m.buffer.frp.cursors_select(Transform::RightWord));

            eval_ input.select_all            (m.buffer.frp.cursors_select(Transform::All));
            eval_ input.select_word_at_cursor (m.buffer.frp.cursors_select(Transform::Word));

            eval_ input.delete_left       (m.buffer.frp.delete_left());
            eval_ input.delete_right      (m.buffer.frp.delete_right());
            eval_ input.delete_word_left  (m.buffer.frp.delete_word_left());
            eval_ input.delete_word_right (m.buffer.frp.delete_word_right());

            eval_ input.undo (m.buffer.frp.undo());
            eval_ input.redo (m.buffer.frp.redo());


            // === Insert ===

            key_inserted  <- keyboard.frp.down.gate_not(&keyboard.frp.is_modifier_down);
            key_to_insert <= key_inserted.map(f!((key) m.key_to_string(key)));
            str_to_insert <- any(&input.insert,&key_to_insert);
            eval str_to_insert ((s) m.buffer.frp.insert(s));
            eval input.set_content ([input](s) {
                input.set_cursor(&default());
                input.select_all();
                input.insert(s);
                input.remove_all_cursors();
            });

            // === Colors ===

            eval input.set_default_color     ((t) m.buffer.frp.set_default_color(*t));
            eval input.set_default_text_size ((t) {
                m.buffer.frp.set_default_text_size(*t);
                m.redraw(true);
            });
            eval input.set_color_all         ([input](color) {
                let all_bytes = buffer::Range::from(Bytes::from(0)..Bytes(i32::max_value()));
                input.set_color_bytes.emit((all_bytes,*color));
            });
            // FIXME: The color-setting operation is very slow now. For every new color, the whole
            //        text is re-drawn. See https://github.com/enso-org/ide/issues/1031
            eval input.set_color_bytes ((t) {
                m.buffer.frp.set_color_bytes.emit(*t);
                m.redraw(false);
            });
            self.frp.source.selection_color <+ self.frp.set_selection_color;

            // === Changes ===

            self.frp.source.changed <+ m.buffer.frp.text_change;
            self.frp.source.content <+ m.buffer.frp.text_change.map(f_!(m.buffer.text()));
        }
        self
    }

    /// Add the text area to a specific scene layer. The mouse event positions will be mapped to
    /// this view regardless the previous views this component could be added to.
    // TODO https://github.com/enso-org/ide/issues/1576
    //     This function needs to be updated. However, it requires a few steps:
    //     1. The new `ShapeView` and `DynamicShape` are implemented and they use display objects to
    //        pass information about scene layers they are assigned to. However, the [`GlyphSystem`]
    //        is a very non-standard implementation, and thus has to handle the new display object
    //        callbacks in a special way as well.
    //     2. The `self.data.camera` has to still be used, otherwise there would be no way to
    // convert        the screen to object space (see the [`to_object_space`] function). This is
    // a very        temporary solution, as any object can be assigned to more than one scene
    // layer, and thus        can be rendered from more than one camera. Screen / object space
    // location of events        should thus become much more primitive information /
    // mechanisms.     Please note, that this function handles the selection management
    // correctly, as it uses     the new shape system definition, and thus, inherits the scene
    // layer settings from this     display object.
    pub fn add_to_scene_layer(&self, layer: &display::scene::Layer) {
        for symbol in self.symbols() {
            layer.add_exclusive(&symbol);
        }
        self.data.camera.set(layer.camera());
        layer.add_exclusive(self);
    }

    /// Remove this component from view.
    // TODO see TODO in add_to_scene_layer method.
    #[allow(non_snake_case)]
    pub fn remove_from_scene_layer(&self, layer: &display::scene::Layer) {
        for symbol in self.symbols() {
            layer.remove_symbol(&symbol);
        }
    }

    fn symbols(&self) -> SmallVec<[display::Symbol; 1]> {
        let text_symbol = self.data.glyph_system.sprite_system().symbol.clone_ref();
        let shapes = &self.data.app.display.scene().shapes;
        let selection_system = shapes.shape_system(PhantomData::<selection::shape::Shape>);
        let _selection_symbol = selection_system.shape_system.symbol.clone_ref();
        //TODO[ao] we cannot move selection symbol, as it is global for all the text areas.
        SmallVec::from_buf([text_symbol /* selection_symbol */])
    }
}



// =================
// === AreaModel ===
// =================

/// Internal representation of `Area`.
#[derive(Clone, CloneRef, Debug)]
pub struct AreaModel {
    app:    Application,
    // FIXME[ao]: this is a temporary solution to handle properly areas in different views. Should
    //            be replaced with proper object management.
    camera: Rc<CloneRefCell<display::camera::Camera2d>>,

    logger:         Logger,
    frp_endpoints:  FrpEndpoints,
    buffer:         buffer::View,
    display_object: display::object::Instance,
    glyph_system:   glyph::System,
    lines:          Lines,
    single_line:    Rc<Cell<bool>>,
    selection_map:  Rc<RefCell<SelectionMap>>,
}

impl AreaModel {
    /// Constructor.
    pub fn new(app: &Application, frp_endpoints: &FrpEndpoints) -> Self {
        let app = app.clone_ref();
        let scene = app.display.scene();
        let logger = Logger::new("text_area");
        let selection_map = default();
        let fonts = scene.extension::<typeface::font::Registry>();
        let font = fonts.load("DejaVuSansMono");
        let glyph_system = typeface::glyph::System::new(&scene, font);
        let display_object = display::object::Instance::new(&logger);
        let buffer = default();
        let lines = default();
        let single_line = default();
        let camera = Rc::new(CloneRefCell::new(scene.camera().clone_ref()));

        display_object.add_child(&glyph_system);

        // FIXME[WD]: These settings should be managed wiser. They should be set up during
        // initialization of the shape system, not for every area creation. To be improved during
        // refactoring of the architecture some day.
        let shape_system = scene.shapes.shape_system(PhantomData::<selection::shape::Shape>);
        let symbol = &shape_system.shape_system.sprite_system.symbol;
        shape_system.shape_system.set_pointer_events(false);

        // FIXME[WD]: This is temporary sorting utility, which places the cursor in front of mouse
        // pointer and nodes. Should be refactored when proper sorting mechanisms are in place.
        scene.layers.main.remove_symbol(symbol);
        scene.layers.label.add_exclusive(symbol);

        let frp_endpoints = frp_endpoints.clone_ref();

        Self {
            app,
            camera,
            logger,
            frp_endpoints,
            buffer,
            display_object,
            glyph_system,
            lines,
            single_line,
            selection_map,
        }
        .init()
    }

    fn on_modified_selection(
        &self,
        selections: &buffer::selection::Group,
        time: f32,
        do_edit: bool,
    ) {
        {
            let mut selection_map = self.selection_map.borrow_mut();
            let mut new_selection_map = SelectionMap::default();
            for sel in selections {
                let sel = self.buffer.snap_selection(*sel);
                let id = sel.id;
                let start_line = sel.start.line.as_usize();
                let end_line = sel.end.line.as_usize();
                let pos_x = |line: usize, column: Column| {
                    if line >= self.lines.len() {
                        self.lines
                            .rc
                            .borrow()
                            .last()
                            .and_then(|l| l.divs.last().cloned())
                            .unwrap_or(0.0)
                    } else {
                        self.lines.rc.borrow()[line].div_by_column(column)
                    }
                };
                let min_pos_x = pos_x(start_line, sel.start.column);
                let max_pos_x = pos_x(end_line, sel.end.column);
                let logger = Logger::new_sub(&self.logger, "cursor");
                let min_pos_y = -LINE_HEIGHT / 2.0 - LINE_HEIGHT * start_line as f32;
                let pos = Vector2(min_pos_x, min_pos_y);
                let width = max_pos_x - min_pos_x;
                let selection = match selection_map.id_map.remove(&id) {
                    Some(selection) => {
                        let select_left = selection.width.simulator.target_value() < 0.0;
                        let select_right = selection.width.simulator.target_value() > 0.0;
                        let tgt_pos_x = selection.position.simulator.target_value().x;
                        let tgt_width = selection.width.simulator.target_value();
                        let mid_point = tgt_pos_x + tgt_width / 2.0;
                        let go_left = pos.x < mid_point;
                        let go_right = pos.x > mid_point;
                        let need_flip = (select_left && go_left) || (select_right && go_right);
                        if width == 0.0 && need_flip {
                            selection.flip_sides()
                        }
                        selection.position.set_target_value(pos);
                        selection
                    }
                    None => {
                        let selection = Selection::new(&logger, do_edit);
                        selection.letter_width.set(7.0); // FIXME hardcoded values
                        self.add_child(&selection);
                        selection.position.set_target_value(pos);
                        selection.position.skip();
                        let selection_network = &selection.network;
                        // FIXME[wd]: memory leak. To be fixed with the below note as a part of
                        //            https://github.com/enso-org/ide/issues/670 . Once fixed,
                        //            delete code removing all cursors on Area drop.
                        let model = self.clone_ref();
                        frp::extend! { selection_network
                            // FIXME[WD]: This is ultra-slow. Redrawing all glyphs on each
                            //            animation frame. Multiple times, once per cursor.
                            //            https://github.com/enso-org/ide/issues/1031
                            eval_ selection.position.value (model.redraw(true));
                            selection.frp.set_color <+ self.frp_endpoints.selection_color;
                        }
                        selection.frp.set_color.emit(self.frp_endpoints.selection_color.value());
                        selection
                    }
                };
                selection.width.set_target_value(width);
                selection.edit_mode.set(do_edit);
                selection.start_time.set(time);
                new_selection_map.id_map.insert(id, selection);
                new_selection_map
                    .location_map
                    .entry(start_line)
                    .or_default()
                    .insert(sel.start.column, id);
            }
            *selection_map = new_selection_map;
        }
        self.redraw(true)
    }

    /// Transforms screen position to the object (display object) coordinate system.
    fn to_object_space(&self, screen_pos: Vector2) -> Vector2 {
        let camera = self.camera.get();
        let origin_world_space = Vector4(0.0, 0.0, 0.0, 1.0);
        let origin_clip_space = camera.view_projection_matrix() * origin_world_space;
        let inv_object_matrix = self.transform_matrix().try_inverse().unwrap();

        let shape = self.app.display.scene().frp.shape.value();
        let clip_space_z = origin_clip_space.z;
        let clip_space_x = origin_clip_space.w * 2.0 * screen_pos.x / shape.width;
        let clip_space_y = origin_clip_space.w * 2.0 * screen_pos.y / shape.height;
        let clip_space = Vector4(clip_space_x, clip_space_y, clip_space_z, origin_clip_space.w);
        let world_space = camera.inversed_view_projection_matrix() * clip_space;
        (inv_object_matrix * world_space).xy()
    }

    fn get_in_text_location(&self, screen_pos: Vector2) -> Location {
        let object_space = self.to_object_space(screen_pos);
        let line_index = (-object_space.y / LINE_HEIGHT) as usize;
        let line_index = std::cmp::min(line_index, self.lines.len() - 1);
        let div_index = self.lines.rc.borrow()[line_index].div_index_close_to(object_space.x);
        let line = line_index.into();
        let column = div_index.into();
        Location(line, column)
    }

    fn init(self) -> Self {
        self.redraw(true);
        self
    }

    /// Redraw the text.
    fn redraw(&self, size_may_change: bool) {
        let lines = self.buffer.view_lines();
        let line_count = lines.len();
        self.lines.resize_with(line_count, |ix| self.new_line(ix));
        let widths = lines
            .into_iter()
            .enumerate()
            .map(|(view_line_index, content)| self.redraw_line(view_line_index, content))
            .collect_vec();
        let width = widths.into_iter().max_by(|x, y| x.partial_cmp(y).unwrap()).unwrap_or_default();
        if size_may_change {
            let height = self.calculate_height();
            self.frp_endpoints.source.width.emit(width);
            self.frp_endpoints.source.height.emit(height);
        }
    }

    fn calculate_height(&self) -> f32 {
        self.lines.len() as f32 * LINE_HEIGHT
    }

    fn redraw_line(&self, view_line_index: usize, content: String) -> f32 {
        let cursor_map = self
            .selection_map
            .borrow()
            .location_map
            .get(&view_line_index)
            .cloned()
            .unwrap_or_default();
        let line = &mut self.lines.rc.borrow_mut()[view_line_index];
        let line_object = line.display_object().clone_ref();
        let line_range = self.buffer.byte_range_of_view_line_index_snapped(view_line_index.into());
        let mut line_style = self.buffer.sub_style(line_range.start..line_range.end).iter();
        let mut pen = pen::Pen::new(&self.glyph_system.font);
        let mut divs = vec![];
        let mut column = 0.column();
        let mut last_cursor = None;
        let mut last_cursor_target = default();
        line.resize_with(content.chars().count(), || self.glyph_system.new_glyph());
        let mut iter = line.glyphs.iter_mut().zip(content.chars());
        loop {
            let next = iter.next();
            let style = line_style.next().unwrap_or_default();
            let chr_size = style.size.raw;
            let char_info = next.as_ref().map(|t| pen::CharInfo::new(t.1, chr_size));
            let info = pen.advance(char_info);

            cursor_map.get(&column).for_each(|id| {
                self.selection_map.borrow().id_map.get(id).for_each(|cursor| {
                    if cursor.edit_mode.get() {
                        let pos_y = LINE_HEIGHT / 2.0 - LINE_VERTICAL_OFFSET;
                        last_cursor = Some(cursor.clone_ref());
                        last_cursor_target = Vector2(info.offset, pos_y);
                    }
                });
            });

            divs.push(info.offset);
            let opt_glyph = next.map(|t| t.0);
            match opt_glyph.zip(info.char) {
                Some((glyph, chr)) => {
                    let chr_bytes: Bytes = chr.len_utf8().into();
                    line_style.drop(chr_bytes - 1.bytes());
                    let glyph_info = self.glyph_system.font.glyph_info(chr);
                    let size = glyph_info.scale.scale(chr_size);
                    let glyph_offset = glyph_info.offset.scale(chr_size);
                    let glyph_x = info.offset + glyph_offset.x;
                    let glyph_y = glyph_offset.y;
                    glyph.set_position_xy(Vector2(glyph_x, glyph_y));
                    glyph.set_char(chr);
                    glyph.set_color(style.color);
                    glyph.size.set(size);
                    match &last_cursor {
                        None => line_object.add_child(glyph),
                        Some(cursor) => {
                            cursor.right_side.add_child(glyph);
                            glyph.mod_position_xy(|p| p - last_cursor_target);
                        }
                    }
                }
                None => break,
            }
            column += 1.column();
        }

        let last_offset = divs.last().cloned().unwrap_or_default();
        let cursor_offset = last_cursor.map(|cursor| last_cursor_target.x - cursor.position().x);
        let cursor_offset = cursor_offset.unwrap_or_default();
        line.set_divs(divs);
        last_offset - cursor_offset
    }

    fn new_line(&self, index: usize) -> Line {
        let line = Line::new(&self.logger);
        let y_offset = -((index + 1) as f32) * LINE_HEIGHT + LINE_VERTICAL_OFFSET;
        line.set_position_y(y_offset);
        self.add_child(&line);
        line
    }

    fn copy(&self, selections: &[String]) {
        let encoded = match selections {
            [] => "".to_string(),
            [s] => s.clone(),
            lst => lst.join(RECORD_SEPARATOR),
        };
        clipboard::write_text(encoded);
    }

    fn cut(&self, selections: &[String]) {
        self.copy(selections);
    }

    fn paste(&self) {
        let paste_string = self.frp_endpoints.input.paste_string.clone_ref();
        clipboard::read_text(move |t| paste_string.emit(t));
    }

    /// Paste new text in the place of current selections / cursors. In case of pasting multiple
    /// chunks (e.g. after copying multiple selections), the chunks will be pasted into subsequent
    /// selections. In case there are more chunks than selections, end chunks will be dropped. In
    /// case there is more selections than chunks, end selections will be replaced with empty
    /// strings. I `self.single_line` is set to true then each chunk will be truncated to its first
    /// line.
    fn paste_string(&self, s: &str) {
        let mut chunks = self.decode_paste(s);
        if self.single_line.get() {
            for f in &mut chunks {
                Self::drop_all_but_first_line(f);
            }
        }
        self.buffer.frp.paste(chunks);
    }

    fn decode_paste(&self, encoded: &str) -> Vec<String> {
        encoded.split(RECORD_SEPARATOR).map(|s| s.into()).collect()
    }

    fn drop_all_but_first_line(s: &mut String) {
        *s = s.lines().next().unwrap_or("").to_string();
    }

    fn key_to_string(&self, key: &Key) -> Option<String> {
        match key {
            Key::Character(s) => Some(s.clone()),
            Key::Enter => self.single_line.get().not().as_some("\n".into()),
            Key::Space => Some(" ".into()),
            _ => None,
        }
    }
}

impl display::Object for AreaModel {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl display::Object for Area {
    fn display_object(&self) -> &display::object::Instance {
        self.data.display_object()
    }
}

impl application::command::FrpNetworkProvider for Area {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for Area {
    fn label() -> &'static str {
        "TextArea"
    }

    fn new(app: &Application) -> Self {
        Area::new(app)
    }

    fn app(&self) -> &Application {
        &self.data.app
    }

    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use shortcut::ActionType::*;
        (&[
            (PressAndRepeat, "left", "cursor_move_left"),
            (PressAndRepeat, "right", "cursor_move_right"),
            (PressAndRepeat, "up", "cursor_move_up"),
            (PressAndRepeat, "down", "cursor_move_down"),
            (PressAndRepeat, "cmd left", "cursor_move_left_word"),
            (PressAndRepeat, "cmd right", "cursor_move_right_word"),
            (Press, "alt left", "cursor_move_left_of_line"),
            (Press, "alt right", "cursor_move_right_of_line"),
            (Press, "home", "cursor_move_left_of_line"),
            (Press, "end", "cursor_move_right_of_line"),
            (PressAndRepeat, "shift left", "cursor_select_left"),
            (PressAndRepeat, "shift right", "cursor_select_right"),
            (PressAndRepeat, "cmd shift left", "cursor_select_left_word"),
            (PressAndRepeat, "cmd shift right", "cursor_select_right_word"),
            (PressAndRepeat, "shift up", "cursor_select_up"),
            (PressAndRepeat, "shift down", "cursor_select_down"),
            (PressAndRepeat, "backspace", "delete_left"),
            (PressAndRepeat, "delete", "delete_right"),
            (PressAndRepeat, "cmd backspace", "delete_word_left"),
            (PressAndRepeat, "cmd delete", "delete_word_right"),
            (Press, "shift left-mouse-button", "set_newest_selection_end_to_mouse_position"),
            (DoublePress, "left-mouse-button", "select_word_at_cursor"),
            (Press, "left-mouse-button", "set_cursor_at_mouse_position"),
            (Press, "left-mouse-button", "start_newest_selection_end_follow_mouse"),
            (Release, "left-mouse-button", "stop_newest_selection_end_follow_mouse"),
            (Press, "cmd left-mouse-button", "add_cursor_at_mouse_position"),
            (Press, "cmd left-mouse-button", "start_newest_selection_end_follow_mouse"),
            (Release, "cmd left-mouse-button", "stop_newest_selection_end_follow_mouse"),
            (Press, "cmd a", "select_all"),
            (Press, "cmd c", "copy"),
            (Press, "cmd x", "cut"),
            (Press, "cmd v", "paste"),
            (Press, "escape", "keep_oldest_cursor_only"),
        ])
            .iter()
            .map(|(action, rule, command)| {
                let only_hovered = *action != Release && rule.contains("left-mouse-button");
                let condition = if only_hovered { "focused & hovered" } else { "focused" };
                Self::self_shortcut_when(*action, *rule, *command, condition)
            })
            .collect()
    }
}

impl Drop for Area {
    fn drop(&mut self) {
        // TODO[ao]: This is workaround for memory leak causing text to stay even when component
        //           is deleted. See "FIXME: memory leak." comment above.
        self.remove_all_cursors();
    }
}
