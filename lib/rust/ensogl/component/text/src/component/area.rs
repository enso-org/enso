//! The text area implementation. It serves the purpose of single and multi-line text labels and
//! text editors.

use crate::prelude::*;
use enso_text::unit;
use enso_text::unit::*;
use ensogl_core::display::shape::*;

use crate::buffer;
use crate::buffer::style;
use crate::buffer::Text;
use crate::buffer::Transform;
use crate::component::selection;
use crate::component::Selection;
use crate::font;
use crate::font::glyph;
use crate::font::glyph::Glyph;

use enso_frp as frp;
use enso_frp::io::keyboard::Key;
use ensogl_core::application;
use ensogl_core::application::shortcut;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::gui::cursor;
use ensogl_core::system::web::clipboard;
use ensogl_core::DEPRECATED_Animation;
use owned_ttf_parser::AsFaceRef;
use rustybuzz;
use std::ops::Not;

pub use crate::buffer::TextRange;



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
#[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
pub struct SelectionMap {
    id_map:       HashMap<usize, Selection>,
    location_map: HashMap<unit::ViewLine, HashMap<CodePointIndex, usize>>,
}



// ============
// === Line ===
// ============

/// Visual line representation.
///
/// **Design Notes**
/// The `divs` and `centers` are kept as vectors for performance reasons. Especially, when clicking
/// inside of the text area, it allows us to binary search the place of the mouse pointer.
#[derive(Debug, Default)]
#[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
pub struct Line {
    display_object: display::object::Instance,
    glyphs:         Vec<Glyph>,
    divs:           Vec<f32>,
    centers:        Vec<f32>,
    index:          usize,
}

impl Line {
    fn new() -> Self {
        default()
    }

    /// Set the division points (offsets between letters). Also updates center points.
    #[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
    fn set_divs(&mut self, divs: Vec<f32>) {
        let div_iter = divs.iter();
        let div_iter_skipped = divs.iter().skip(1);
        self.centers = div_iter.zip(div_iter_skipped).map(|(t, s)| (t + s) / 2.0).collect();
        self.divs = divs;
    }

    fn div_index_close_to(&self, offset: f32) -> usize {
        self.centers.binary_search_by(|t| t.partial_cmp(&offset).unwrap()).unwrap_both()
    }

    #[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
    fn div_by_column(&self, column: CodePointIndex) -> f32 {
        let ix = column.as_usize().min(self.divs.len() - 1);
        if ix < self.divs.len() {
            self.divs[ix]
        } else {
            if column != CodePointIndex(0) {
                warn!("Internal error. Incorrect text divisions.")
            }
            0.0
        }
    }

    fn last_div(&self) -> f32 {
        self.divs.last().copied().unwrap_or(0.0)
    }

    #[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
    fn resize_with(&mut self, size: usize, cons: impl Fn() -> Glyph) {
        let display_object = self.display_object().clone_ref();
        self.glyphs.resize_with(size, move || {
            let glyph = cons();
            display_object.add_child(&glyph);
            glyph
        });
    }

    fn push_glyph(&mut self, cons: impl Fn() -> Glyph) {
        let display_object = self.display_object().clone_ref();
        let glyph = cons();
        display_object.add_child(&glyph);
        self.glyphs.push(glyph);
    }

    fn set_index(&mut self, index: usize) {
        let y_offset = -((index + 1) as f32) * LINE_HEIGHT + LINE_VERTICAL_OFFSET;
        self.set_position_y(y_offset);
        self.index = index;
    }
}

impl display::Object for Line {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

fn set_line_index(line: &display::object::Instance, index: usize) {
    let y_offset = -((index + 1) as f32) * LINE_HEIGHT + LINE_VERTICAL_OFFSET;
    line.set_position_y(y_offset);
}



// =============
// === Lines ===
// =============

/// Set of all visible lines.
#[derive(Clone, CloneRef, Debug, Default, Deref)]
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
    Input {[TRACE_ALL]
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

        set_cursor            (Location), // fixme: should use column here
        add_cursor            (Location),
        paste_string          (String),
        insert                (String),
        format                (TextRange, style::Property),
        /// Sets the color for all text that has no explicit color set.
        set_default_color     (color::Rgba),
        set_selection_color   (color::Rgb),
        set_default_text_size (style::Size),
        /// Set font in the text area. The name will be looked up in [`font::Registry`].
        ///
        /// Note, that this is a relatively heavy operation - it requires not only redrawing all
        /// lines, but also re-load internal structures for rendering (like WebGL buffers,
        /// MSDF texture, etc.).
        set_font              (String),
        set_content           (String),
        /// Set content, truncating the trailing characters on every line to fit a width in pixels
        /// when rendered with current font and font size. The truncated substrings are replaced
        /// with an ellipsis character ("…").
        ///
        /// Unix (`\n`) and MS-DOS (`\r\n`) style line endings are recognized.
        set_content_truncated (String, f32),
    }
    Output {
        pointer_style   (cursor::Style),
        width           (f32),
        height          (f32),
        changed         (Vec<crate::ChangeWithSelection>),
        content         (Text),
        hovered         (bool),
        selection_color (color::Rgb),
        /// Color that is used for all text that does not explicitly have a color set.
        default_color   (color::Rgba),
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
    pub data: Rc<AreaModel>,
    pub frp:  Rc<Frp>,
}

impl Deref for Area {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl Area {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(app: &Application) -> Self {
        let frp = Rc::new(Frp::new());
        let data = Rc::new(AreaModel::new(app, &frp.output));
        Self { data, frp }.init()
    }

    fn init(self) -> Self {
        let network = &self.frp.network;
        let model = &self.data;
        let scene = &model.app.display.default_scene;
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
            trace mouse_on_set_cursor;
            mouse_on_add_cursor      <- mouse.position.sample(&input.add_cursor_at_mouse_position);
            trace mouse_on_add_cursor;
            loc_on_set_cursor_mouse  <- mouse_on_set_cursor.map(f!((p) m.get_in_text_location(*p)));
            trace loc_on_set_cursor_mouse;
            loc_on_add_cursor_mouse  <- mouse_on_add_cursor.map(f!((p) m.get_in_text_location(*p)));
            trace loc_on_add_cursor_mouse;
            loc_on_set_cursor_at_end <- input.set_cursor_at_end.map(f_!(model.buffer.text().location_of_text_end()));
            loc_on_set_cursor        <- any(input.set_cursor,loc_on_set_cursor_mouse,loc_on_set_cursor_at_end);
            loc_on_add_cursor        <- any(&input.add_cursor,&loc_on_add_cursor_mouse);
            trace loc_on_add_cursor;

            eval loc_on_set_cursor ((loc) m.buffer.frp.set_cursor(loc));
            eval loc_on_add_cursor ((loc) m.buffer.frp.add_cursor(loc));

            _eval <- m.buffer.frp.selection_edit_mode.map2
                (&scene.frp.frame_time,f!([m](selections,time) {
                        debug!(">> 1");
                        m.on_modified_selection(&selections.selection_group,Some(&selections.changes),*time)
                    }
            ));

            _eval <- m.buffer.frp.selection_non_edit_mode.map2
                (&scene.frp.frame_time,f!([m](selections,time) {
                    debug!(">> selection_non_edit_mode");
                    m.on_modified_selection(selections,None,*time)
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


            // eval_ m.buffer.frp.text_change (m.redraw(true));

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
            trace key_to_insert;
            str_to_insert <- any(&input.insert, &key_to_insert);
            trace str_to_insert;
            eval str_to_insert ((s) m.buffer.frp.insert(s));
            eval input.set_content ([input](s) {
                input.set_cursor(&default());
                input.select_all();
                input.insert(s);
                input.remove_all_cursors();
            });
            input.set_content <+ input.set_content_truncated.map(f!(((text, max_width_px)) {
                m.text_truncated_with_ellipsis(text.clone(), m.default_font_size(), *max_width_px)
            }));


            // === Font ===

            eval input.set_font ((t) m.set_font(t));


            // === Colors ===

            eval input.set_default_color ((t)
                m.buffer.frp.set_default_color(*t);
                m.redraw(false) ;
            );
            self.frp.source.default_color <+ self.frp.set_default_color;

            eval input.set_default_text_size ([m](t) {
                warn!(">> set_default_text_size");
                m.buffer.frp.set_default_text_size(*t);
                m.redraw(true);
            });

            // // FIXME: The color-setting operation is very slow now. For every new color, the whole
            // //        text is re-drawn. See https://github.com/enso-org/ide/issues/1031
            // m.buffer.frp.set_color <+ input.set_color;
            // eval input.set_color ((t) m.modify_glyphs_in_ranges(&t.0, |g| g.set_color(t.1)));
            self.frp.source.selection_color <+ self.frp.set_selection_color;


            // === Style ===

            format <- input.format.map(f!([m]((range, prop)) (m.text_to_byte_ranges(range),prop.clone())));
            eval format ((t) m.set_glyphs_property(&t.0, t.1));
            m.buffer.frp.format <+ format;


            // === Changes ===

            // The `content` event should be fired first, as any listener for `changed` may want to
            // read the new content, so it should be up-to-date.
            self.frp.source.content <+ m.buffer.frp.text_change.map(f_!(m.buffer.text()));
            self.frp.source.changed <+ m.buffer.frp.text_change;
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
    //     2. The `self.data.layer` currently needs to be stored for two main purposes:
    //        - so that the [`set_font`] function can add newly created Glyphs to a layer to make
    //          them visible;
    //        - to provide a way to convert the screen to object space (see the [`to_object_space`]
    //          function).
    //        This is a very temporary solution, as any object can be assigned to more than one
    //        scene layer. Screen / object space location of events should thus become much more
    //        primitive information / mechanisms. Please note, that this function handles the
    //        selection management correctly, as it uses the new shape system definition, and thus,
    //        inherits the scene layer settings from this display object.
    pub fn add_to_scene_layer(&self, layer: &display::scene::Layer) {
        self.data.layer.set(layer.clone_ref());
        self.data.add_symbols_to_scene_layer();
        layer.add_exclusive(self);
    }

    /// Remove this component from view.
    // TODO see TODO in add_to_scene_layer method.
    #[allow(non_snake_case)]
    pub fn remove_from_scene_layer(&self, layer: &display::scene::Layer) {
        self.data.remove_symbols_from_scene_layer(layer);
    }
}



// =================
// === AreaModel ===
// =================

/// Internal representation of `Area`.
#[derive(Clone, CloneRef, Debug)]
pub struct AreaModel {
    app:   Application,
    // FIXME[ao]: this is a temporary solution to handle properly areas in different views. Should
    //            be replaced with proper object management.
    layer: Rc<CloneRefCell<display::scene::Layer>>,

    frp_endpoints:  FrpEndpoints,
    buffer:         buffer::View,
    display_object: display::object::Instance,
    glyph_system:   Rc<RefCell<glyph::System>>,
    lines:          Lines,
    single_line:    Rc<Cell<bool>>,
    selection_map:  Rc<RefCell<SelectionMap>>,
}

impl AreaModel {
    /// Constructor.
    pub fn new(app: &Application, frp_endpoints: &FrpEndpoints) -> Self {
        let app = app.clone_ref();
        let scene = &app.display.default_scene;
        let selection_map = default();
        let display_object = display::object::Instance::new();
        let glyph_system = {
            let fonts = scene.extension::<font::Registry>();
            let font = fonts.load(font::DEFAULT_FONT_MONO);
            let glyph_system = font::glyph::System::new(&scene, font);
            display_object.add_child(&glyph_system);
            Rc::new(RefCell::new(glyph_system))
        };
        let buffer = default();
        let lines = default();
        let single_line = default();
        let layer = Rc::new(CloneRefCell::new(scene.layers.main.clone_ref()));

        let shape_system = scene.shapes.shape_system(PhantomData::<selection::shape::Shape>);
        let symbol = &shape_system.shape_system.sprite_system.symbol;

        // FIXME[WD]: This is temporary sorting utility, which places the cursor in front of mouse
        // pointer and nodes. Should be refactored when proper sorting mechanisms are in place.
        scene.layers.main.remove_symbol(symbol);
        scene.layers.label.add_exclusive(symbol);

        let frp_endpoints = frp_endpoints.clone_ref();

        Self {
            app,
            layer,
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

    pub fn text_to_byte_ranges(&self, range: &TextRange) -> Vec<buffer::Range<UBytes>> {
        match range {
            TextRange::Selections => self
                .buffer
                .byte_selections()
                .into_iter()
                .map(|t| {
                    let start = std::cmp::min(t.start, t.end);
                    let end = std::cmp::max(t.start, t.end);
                    buffer::Range::new(start, end)
                })
                .collect(),
            TextRange::BufferRange(range) => vec![range.clone()],
            TextRange::RangeBytes(range) => vec![range.into()],
            TextRange::RangeFull(_) => vec![self.buffer.full_range()],
        }
    }

    /// Implementation of lazy line redrawing. After a change, only the needed lines are redrawn.
    /// If a change produced more lines than the current number of lines, the new lines are inserted
    /// in appropriate positions. If a change produces fewer lines than the current number of lines,
    /// appropriate lines are removed. Then, a minimal line redraw range is computed and performed.
    ///
    ///
    /// # Smooth animations
    /// This function also attaches unchanged lines below a cursor to the cursor for smooth vertical
    /// animation. Please note that when cursors are moved in a non-edit mode (e.g. after pressing
    /// an arrow key), the animations are skipped. This is a performance optimization. If we would
    /// like to continue the animations, we would need to either attach animation system to every
    /// glyph, or create multiple, possibly hierarchical "virtual cursors". The animations are so
    /// fast that this is barely noticeable.
    ///
    /// # Possible future optimizations.
    /// This section describes possible future optimizations that are not implemented now because
    /// of their complexity and/or lack of use cases.
    ///
    /// ## Limiting the number of glyphs per line.
    /// Currently, nothing limits the number of glyphs in line. They are computed and displayed even
    /// if they are not visible on the screen.
    ///
    /// ## Redrawing parts of lines only.
    /// Currently, the whole line is redrawn after any change. This is not optimal, especially for
    /// lines containing a lot of visible characters. However, redrawing only parts of a changed
    /// line is way more complex than it seems. Let's consider the input `அட0`. If we insert `்`
    /// after `ட`, then we should get `ட்` instead of `ட`, but we do not need to redraw neither
    /// `அ` nor `0`. Inserting a new code point can affect any number of code points to the left
    /// and to the right of the insertion point. Unfortunately, the current Rustybuzz
    /// implementation does not support such use cases:
    /// https://github.com/RazrFalcon/rustybuzz/issues/54
    fn update_lines_after_change(&self, changes: Option<&[crate::ChangeWithSelection]>) {
        debug_span!("update_lines_after_change").in_scope(|| {
            let view_line_range = self.buffer.view_line_range();
            if let Some(changes) = changes {
                let lines_to_redraw = changes
                    .iter()
                    .filter_map(|change_with_selection| {
                        debug!("Change: {:#?}", change_with_selection);

                        let selection = change_with_selection.selection;
                        let view_selection = self.buffer.selection_to_view_selection(selection);

                        // Compute shape. If it was a backspace at line start, we need to redraw
                        // the previous line as well.
                        let mut shape = view_selection.shape.normalized();
                        if change_with_selection.is_backspace_at_line_start() {
                            shape.start.line = shape.start.line - ViewLine(1);
                        }

                        // Count newlines in the inserted text and the line difference after the
                        // change.
                        let change = &change_with_selection.change;
                        let change_str = change.text.to_string();
                        let newlines = change_str.chars().filter(|c| *c == '\n').count();
                        let line_diff = newlines as i32 - (shape.end.line - shape.start.line).value;

                        // Measurements used for computing the line range to add or remove.
                        let second_line_index = shape.start.line.value as usize + 1;
                        let line_after_end_index = shape.end.line.value as usize + 1;
                        let mut lines = self.lines.borrow_mut();

                        if line_diff != 0 {
                            // Attach unchanged lines under cursor to the cursor for a smooth
                            // vertical animation.
                            let selection_map = self.selection_map.borrow();
                            let cursor = selection_map.id_map.get(&view_selection.id).unwrap();
                            for index in line_after_end_index..lines.len() {
                                lines[index].set_index(index - line_after_end_index);
                                cursor.bottom_snapped_left.add_child(&lines[index]);
                            }
                        }

                        if line_diff > 0 {
                            // Add missing lines. They will be redrawn later. This is needed for
                            // proper partial redraw (redrawing only the lines that changed).
                            for i in 0..line_diff as usize {
                                let index_to_insert = second_line_index + i;
                                if index_to_insert < lines.len() {
                                    lines.insert(index_to_insert, self.new_line(index_to_insert));
                                }
                            }
                        } else if line_diff < 0 {
                            // Remove lines that are no longer needed. This is needed for proper
                            // partial redraw (redrawing only the lines that changed).
                            let line_diff = (-line_diff) as usize;
                            lines.drain(second_line_index..second_line_index + line_diff);
                        }

                        let range = shape.start.line..=shape.end.line + ViewLine(line_diff);
                        debug!("Range to redraw: {:?}", range);
                        range.intersect(&view_line_range)
                    })
                    .collect_vec();

                let lines_to_redraw = std_ext::range::merge_overlapping_ranges(&lines_to_redraw);
                let lines_to_redraw = lines_to_redraw.collect_vec();
                debug!("Lines to redraw: {:#?}", lines_to_redraw);

                self.resize_lines();
                self.redraw_sorted_line_ranges(&lines_to_redraw);
            } else {
                self.remove_glyphs_from_cursors();
                self.remove_lines_from_cursors();
            }
        })
    }

    #[profile(Debug)]
    fn on_modified_selection(
        &self,
        buffer_selections: &buffer::selection::Group,
        changes: Option<&[crate::ChangeWithSelection]>,
        time: f32,
    ) {
        let do_edit = changes.is_some();
        debug!("on_modified_selection {:?} {:?} {:?}", buffer_selections, time, do_edit);
        {
            self.update_lines_after_change(changes);

            let mut selection_map = self.selection_map.borrow_mut();

            debug!("{:?}", selection_map);
            let mut new_selection_map = SelectionMap::default();

            for buffer_selection in buffer_selections {
                debug!(">>1 {:?}", buffer_selection);
                let buffer_selection = self.limit_selection_to_known_values(*buffer_selection);
                debug!(">>2 {:?}", buffer_selection);
                let id = buffer_selection.id;
                let selection_start_line =
                    self.buffer.line_to_view_line(buffer_selection.start.line);
                let selection_end_line = self.buffer.line_to_view_line(buffer_selection.end.line);
                let get_pos_x = |line: unit::ViewLine, code_pt_ix: CodePointIndex| {
                    if line >= unit::ViewLine(self.lines.len() as i32) {
                        self.lines.borrow().last().map(|line| line.last_div()).unwrap_or(0.0)
                    } else {
                        self.lines.borrow()[line.as_usize()].div_by_column(code_pt_ix)
                    }
                };
                let start_x =
                    get_pos_x(selection_start_line, buffer_selection.start.code_point_index);
                let end_x = get_pos_x(selection_end_line, buffer_selection.end.code_point_index);
                debug!("start_x {start_x}, end_x {end_x}");
                let selection_y =
                    -LINE_HEIGHT / 2.0 - LINE_HEIGHT * selection_start_line.value as f32;
                debug!("selection_y {selection_y}");
                let pos = Vector2(start_x, selection_y);
                debug!("pos {pos}");
                let width = end_x - start_x;
                debug!("width {width}");
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
                        debug!("{select_left}, {select_right}, {tgt_pos_x}, {tgt_width}, {mid_point}, {go_left}, {go_right}, {need_flip}");
                        selection.position.set_target_value(pos);
                        selection
                    }
                    None => {
                        let selection = Selection::new(do_edit);
                        self.add_child(&selection);
                        selection.letter_width.set(7.0); // FIXME hardcoded values
                        selection.position.set_target_value(pos);
                        selection.position.skip();
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
                    .entry(selection_start_line)
                    .or_default()
                    .insert(buffer_selection.start.code_point_index, id);
            }
            debug!("new_selection_map = {new_selection_map:#?}");
            *selection_map = new_selection_map;
        }
        if do_edit {
            self.add_glyphs_to_cursors();
        }
        // self.redraw(true)
    }

    /// Transforms screen position to the object (display object) coordinate system.
    fn to_object_space(&self, screen_pos: Vector2) -> Vector2 {
        let camera = self.layer.get().camera();
        let origin_world_space = Vector4(0.0, 0.0, 0.0, 1.0);
        let origin_clip_space = camera.view_projection_matrix() * origin_world_space;
        let inv_object_matrix = self.transform_matrix().try_inverse().unwrap();

        let shape = self.app.display.default_scene.frp.shape.value();
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
        let div_index = self.lines.borrow()[line_index].div_index_close_to(object_space.x);
        let line = line_index.into();
        let column = div_index.into();
        Location(line, column)
    }

    #[profile(Debug)]
    fn init(self) -> Self {
        // self.redraw(true);
        self
    }

    pub fn redraw_sorted_line_ranges(&self, sorted_line_ranges: &[RangeInclusive<ViewLine>]) {
        warn!("redraw_sorted_line_ranges: {sorted_line_ranges:?}");
        for range in sorted_line_ranges {
            let lines_content = self.buffer.lines_content(range.clone());
            for (index, line) in range.clone().into_iter().enumerate() {
                /// FIXME: this strange getting to the content is because when removing whole line
                /// the content is []. To be checked why.
                let content: &str =
                    if index < lines_content.len() { &lines_content[index] } else { "" };
                self.redraw_line(line, content);
            }
        }
    }

    pub fn resize_lines(&self) {
        let lines = self.buffer.view_lines();
        let line_count = lines.len();
        self.lines.resize_with(line_count, |ix| self.new_line(ix));
    }

    /// Redraw the text.
    #[profile(Debug)]
    pub fn redraw(&self, size_may_change: bool) {
        debug!("redraw {:?}", size_may_change);
        let lines = self.buffer.view_lines();
        let line_count = lines.len();
        self.lines.resize_with(line_count, |ix| self.new_line(ix));
        let widths = lines
            .into_iter()
            .enumerate()
            .map(|(view_line_index, content)| {
                self.redraw_line(unit::ViewLine(view_line_index as i32), &content)
            })
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

    pub fn chunks_per_font_face<'a>(
        font: &'a font::Font,
        line_style: &'a style::Formatting,
        content: &'a str,
    ) -> impl Iterator<Item = (Range<UBytes>, font::NonVariableFaceHeader)> + 'a {
        gen_iter!(move {
            match font {
                font::Font::NonVariable(_) =>
                    for a in line_style.chunks_per_font_face(content) {
                        yield a;
                    }
                font::Font::Variable(_) => {
                    let range = UBytes(0)..UBytes(content.len());
                    // For variable fonts, we do not care about non-variable variations.
                    let non_variable_variations = font::NonVariableFaceHeader::default();
                    yield (range, non_variable_variations);
                }
            }
        })
    }

    fn redraw_line(&self, view_line_index: unit::ViewLine, content: &str) -> f32 {
        debug!("redraw_line {:?} {:?}", view_line_index, content);

        let cursor_map = self
            .selection_map
            .borrow()
            .location_map
            .get(&view_line_index)
            .cloned()
            .unwrap_or_default();
        debug!("cursor_map {:?}", cursor_map);
        let line = &mut self.lines.borrow_mut()[view_line_index.as_usize()];
        let line_object = line.display_object().clone_ref();
        let line_range = self.buffer.byte_range_of_view_line_index_snapped(view_line_index.into());
        debug!("line style {:#?}", self.buffer.sub_style(line_range.start..line_range.end));

        let line_style = self.buffer.sub_style(line_range.start..line_range.end);

        let glyph_system = self.glyph_system.borrow();

        let mut divs = vec![0.0];
        // let mut last_cursor = None;
        // let mut last_cursor_target = default();

        let font = &glyph_system.font;
        let mut line_style_iter = line_style.iter();
        let mut glyph_offset_x = 0.0;
        let mut code_point_index = 0.code_point_index();
        // FIXME: after removing glyph in the middle we have two the same headers here - to be
        // optimized
        for (range, requested_non_variable_variations) in
            Self::chunks_per_font_face(font, &line_style, content)
        {
            let non_variable_variations_match =
                font.closest_non_variable_variations_or_panic(requested_non_variable_variations);
            let non_variable_variations = non_variable_variations_match.variations;
            if non_variable_variations_match.was_closest() {
                warn!(
                    "The font is not defined for the variation {:?}. Using {:?} instead.",
                    requested_non_variable_variations, non_variable_variations
                );
            }
            // Safe because the non_variable_variations was chosen above.
            font.with_borrowed_face(non_variable_variations, |face| {
                let ttf_face = face.ttf.as_face_ref();
                // This is safe. Unwrap should be removed after rustybuzz is fixed:
                // https://github.com/RazrFalcon/rustybuzz/issues/52
                let buzz_face = rustybuzz::Face::from_face(ttf_face.clone()).unwrap();
                let mut buffer = rustybuzz::UnicodeBuffer::new();
                buffer.push_str(&content[range.start.value..range.end.value]);
                let shaped = rustybuzz::shape(&buzz_face, &[], buffer);
                let shaped_iter = shaped.glyph_positions().iter().zip(shaped.glyph_infos());
                let mut prev_cluster_byte_off = UBytes(0);
                for (glyph_position, glyph_info) in shaped_iter {
                    let glyph_byte_start = UBytes(glyph_info.cluster as usize);
                    let cluster_diff = glyph_byte_start.saturating_sub(prev_cluster_byte_off);
                    // Drop styles assigned to skipped bytes. One byte will be skipped
                    // during the call to `line_style_iter.next()`.
                    line_style_iter.drop(cluster_diff.saturating_sub(UBytes(1)));
                    let style = line_style_iter.next().unwrap_or_default();
                    prev_cluster_byte_off = glyph_byte_start;
                    if code_point_index >= CodePointIndex::from(line.glyphs.len()) {
                        line.push_glyph(|| glyph_system.new_glyph());
                    }
                    // FIXME: indexing by codepoint is wrong here as glyph may have multiple code
                    // points
                    let glyph = &line.glyphs[code_point_index.value as usize];
                    glyph.start_code_point.set(code_point_index);

                    let size = style.size;
                    let units_per_em = ttf_face.units_per_em();
                    let rustybuzz_scale = units_per_em as f32 / size.value * 1.0;

                    let glyph_id = font::GlyphId(glyph_info.glyph_id as u16);
                    glyph.set_color(style.color);
                    glyph.set_sdf_weight(style.sdf_weight.value);
                    glyph.set_size(size);
                    glyph.set_properties(non_variable_variations);
                    glyph.set_glyph_id(glyph_id);

                    let variable_variations = glyph.variations.borrow();

                    let glyph_render_info = font.glyph_info_of_known_face(
                        non_variable_variations,
                        &variable_variations,
                        glyph_id,
                        face,
                    );
                    let glyph_render_offset = glyph_render_info.offset.scale(size.value);
                    glyph.sprite.set_position_xy(glyph_render_offset);
                    glyph.set_position_xy(Vector2(glyph_offset_x, 0.0));

                    glyph_offset_x += glyph_position.x_advance as f32 / rustybuzz_scale;
                    divs.push(glyph_offset_x);

                    line_object.add_child(glyph);

                    code_point_index += 1.code_point_index();
                }
            });
        }
        line.glyphs.truncate(code_point_index.value as usize);

        line.set_divs(divs);

        0.0 // FIXME
            // let last_offset = divs.last().cloned().unwrap_or_default();
            // let cursor_offset = last_cursor.map(|cursor| last_cursor_target.x -
            // cursor.position().x); let cursor_offset =
            // cursor_offset.unwrap_or_default(); last_offset - cursor_offset
    }

    pub fn set_glyphs_property(
        &self,
        ranges: &Vec<buffer::Range<UBytes>>,
        property: style::Property,
    ) {
        let resolved_property = self.buffer.resolve_property(property);
        self.modify_glyphs_in_ranges(ranges, |glyph| glyph.set_property(resolved_property));
    }

    pub fn modify_glyphs_in_ranges(&self, ranges: &Vec<buffer::Range<UBytes>>, f: impl Fn(&Glyph)) {
        warn!("modify_glyphs_in_ranges {:?}", ranges);
        for &range in ranges {
            warn!(">> {:?}", range);
            let range = self.buffer.offset_range_to_location(range);
            let range = self.buffer.location_range_to_view_location_range(range);
            let lines = self.lines.borrow();
            if range.start.line == range.end.line {
                let line = &lines[range.start.line.as_usize()];
                for glyph in &line.glyphs {
                    if glyph.start_code_point.get() >= range.end.code_point_index {
                        break;
                    }
                    if glyph.start_code_point.get() >= range.start.code_point_index {
                        f(&glyph)
                    }
                }
            } else {
                let first_line = range.start.line;
                let second_line = first_line + ViewLine(1);
                let last_line = range.end.line;
                for glyph in &lines[first_line.as_usize()].glyphs {
                    if glyph.start_code_point.get() >= range.start.code_point_index {
                        f(&glyph)
                    }
                }
                for line in &lines[second_line.as_usize()..last_line.as_usize()] {
                    for glyph in &line.glyphs {
                        f(&glyph)
                    }
                }
                for glyph in &lines[last_line.as_usize()].glyphs {
                    if glyph.start_code_point.get() < range.end.code_point_index {
                        f(&glyph)
                    }
                }
            }
        }
    }

    pub fn add_glyphs_to_cursors(&self) {
        for line in 0..self.buffer.view_lines().len() {
            let line = unit::ViewLine(line as i32);
            self.add_line_glyphs_to_cursors(line)
        }
    }

    fn add_line_glyphs_to_cursors(&self, view_line_index: unit::ViewLine) {
        debug!("add_line_glyphs_to_cursors {:?}", view_line_index);

        let cursor_map = self
            .selection_map
            .borrow()
            .location_map
            .get(&view_line_index)
            .cloned()
            .unwrap_or_default();
        debug!("cursor_map {:?}", cursor_map);
        let line = &mut self.lines.borrow_mut()[view_line_index.as_usize()];

        let mut last_cursor = None;
        let mut last_cursor_target = default();
        let mut code_point_index = 0.code_point_index();

        for glyph in &line.glyphs {
            cursor_map.get(&code_point_index).for_each(|id| {
                if let Some(cursor) = self.selection_map.borrow().id_map.get(id) {
                    if cursor.edit_mode.get() {
                        let pos_y = LINE_HEIGHT / 2.0 - LINE_VERTICAL_OFFSET;
                        last_cursor = Some(cursor.clone_ref());
                        last_cursor_target = Vector2(glyph.position().x, pos_y);
                    }
                }
            });

            if let Some(cursor) = &last_cursor {
                cursor.right_side.add_child(glyph);
                glyph.mod_position_xy(|p| p - last_cursor_target);
            }

            code_point_index += 1.code_point_index();
        }
    }

    pub fn remove_glyphs_from_cursors(&self) {
        let selection_map = self.selection_map.borrow();
        for (line, cursor_map) in &selection_map.location_map {
            for (_, cursor_id) in cursor_map {
                let selection = selection_map.id_map.get(cursor_id).unwrap();
                for glyph in selection.right_side.remove_all_children() {
                    self.lines.borrow_mut()[line.as_usize()].add_child(&glyph);
                    let pos_x = selection.position.target_value().x;
                    glyph.mod_position_xy(|pos| Vector2(pos.x + pos_x, 0.0));
                }
            }
        }
    }

    pub fn remove_lines_from_cursors(&self) {
        let selection_map = self.selection_map.borrow();
        for cursor_id in selection_map.id_map.keys() {
            let selection = selection_map.id_map.get(cursor_id).unwrap();
            for line in selection.bottom_snapped_left.remove_all_children() {
                self.add_child(&line);
                let pos_y = selection.position.target_value().y;
                line.mod_position_y(|pos| pos + pos_y - LINE_HEIGHT / 2.0);
            }
        }
    }


    // FIXME: to be rewritten with the new line layouter. https://www.pivotaltracker.com/story/show/182746060
    /// Truncate a `line` of text if its length on screen exceeds `max_width_px` when rendered
    /// using the current font at `font_size`. Return the truncated string with an ellipsis ("…")
    /// character appended, or `content` if not truncated.
    ///
    /// The truncation point is chosen such that the resulting string with ellipsis will fit in
    /// `max_width_px` if possible. The `line` must not contain newline characters.
    fn line_truncated_with_ellipsis(
        &self,
        line: &str,
        _font_size: style::Size,
        _max_width_px: f32,
    ) -> String {
        // const ELLIPSIS: char = '\u{2026}';
        // let mut pen = pen::Pen::new(&self.glyph_system.borrow().font);
        // let mut truncation_point = 0.ubytes();
        // let truncate = line.char_indices().any(|(i, ch)| {
        //     let char_info = pen::CharInfo::new(ch, font_size.value);
        //     let pen_info = pen.advance(Some(char_info));
        //     let next_width = pen_info.offset + char_info.size;
        //     if next_width > max_width_px {
        //         return true;
        //     }
        //     let width_of_ellipsis = pen::CharInfo::new(ELLIPSIS, font_size.value).size;
        //     let char_length: UBytes = ch.len_utf8().into();
        //     if next_width + width_of_ellipsis <= max_width_px {
        //         truncation_point = UBytes::from(i) + char_length;
        //     }
        //     false
        // });
        // if truncate {
        //     let truncated_content = line[..truncation_point.as_usize()].to_string();
        //     truncated_content + String::from(ELLIPSIS).as_str()
        // } else {
        //     line.to_string()
        // }
        line.to_string()
    }

    /// Truncate trailing characters on every line of `text` that exceeds `max_width_px` when
    /// rendered using the current font at `font_size`. Return `text` with every truncated
    /// substring replaced with an ellipsis character ("…").
    ///
    /// The truncation point of every line is chosen such that the truncated string with ellipsis
    /// will fit in `max_width_px` if possible. Unix (`\n`) and MS-DOS (`\r\n`) style line endings
    /// are recognized and preserved in the returned string.
    fn text_truncated_with_ellipsis(
        &self,
        text: String,
        font_size: style::Size,
        max_width_px: f32,
    ) -> String {
        let lines = text.split_inclusive('\n');
        /// Return the length of a trailing Unix (`\n`) or MS-DOS (`\r\n`) style line ending in
        /// `s`, or 0 if not found.
        fn length_of_trailing_line_ending(s: &str) -> usize {
            if s.ends_with("\r\n") {
                2
            } else if s.ends_with('\n') {
                1
            } else {
                0
            }
        }
        let tuples_of_lines_and_endings =
            lines.map(|line| line.split_at(line.len() - length_of_trailing_line_ending(line)));
        let lines_truncated_with_ellipsis = tuples_of_lines_and_endings.map(|(line, ending)| {
            self.line_truncated_with_ellipsis(line, font_size, max_width_px) + ending
        });
        lines_truncated_with_ellipsis.collect()
    }

    fn default_font_size(&self) -> style::Size {
        self.buffer.formatting.get().size.default
    }

    fn new_line(&self, index: usize) -> Line {
        let mut line = Line::new();
        line.set_index(index);
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

    /// Constrain the selection to values fitting inside of the current text buffer. This can be
    /// needed when using the API and providing invalid values.
    #[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
    fn limit_selection_to_known_values(
        &self,
        selection: buffer::selection::Selection,
    ) -> buffer::selection::Selection {
        let start = self.buffer.snap_location(selection.start);
        let end = self.buffer.snap_location(selection.end);
        selection.with_start(start).with_end(end)
    }

    #[profile(Debug)]
    fn set_font(&self, font_name: &str) {
        warn!(">>> set_font");
        let app = &self.app;
        let scene = &app.display.default_scene;
        let fonts = scene.extension::<font::Registry>();
        let font = fonts.load(font_name);
        let glyph_system = font::glyph::System::new(&scene, font);
        self.display_object.add_child(&glyph_system);
        let old_glyph_system = self.glyph_system.replace(glyph_system);
        self.display_object.remove_child(&old_glyph_system);
        // Remove old Glyph structures, as they still refer to the old Glyph System.
        self.lines.take();
        self.add_symbols_to_scene_layer();
        self.redraw(true);
    }

    fn add_symbols_to_scene_layer(&self) {
        let layer = &self.layer.get();
        for symbol in self.symbols() {
            layer.add_exclusive(&symbol);
        }
    }

    fn remove_symbols_from_scene_layer(&self, layer: &display::scene::Layer) {
        for symbol in self.symbols() {
            layer.remove_symbol(&symbol);
        }
    }

    fn symbols(&self) -> SmallVec<[display::Symbol; 1]> {
        let text_symbol = self.glyph_system.borrow().sprite_system().symbol.clone_ref();
        let shapes = &self.app.display.default_scene.shapes;
        let selection_system = shapes.shape_system(PhantomData::<selection::shape::Shape>);
        let _selection_symbol = selection_system.shape_system.symbol.clone_ref();
        //TODO[ao] we cannot move selection symbol, as it is global for all the text areas.
        SmallVec::from_buf([text_symbol /* selection_symbol */])
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



#[cfg(test)]
mod tests {
    use super::*;

    /// Assert that there is no inherent memory leak in the [text::Area].
    #[test]
    fn assert_no_leak() {
        let app = Application::new("root");
        let text = app.new_view::<Area>();
        let text_frp = Rc::downgrade(&text.frp);
        let text_data = Rc::downgrade(&text.data);
        drop(text);
        assert_eq!(text_frp.strong_count(), 0, "There are FRP references left.");
        assert_eq!(text_data.strong_count(), 0, "There are  data references left.");
    }
}
