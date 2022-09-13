//! View part of the text editor.

use crate::prelude::*;
use enso_text::unit::*;

use crate::buffer;
use crate::buffer::style;
use crate::buffer::style::Formatting;
use crate::buffer::Buffer;
// use crate::buffer::DefaultSetter;
use crate::buffer::TextRange;

use crate::font;
use crate::font::Font;
use crate::font::GlyphRenderInfo;
use enso_frp as frp;
use enso_text::text::BoundsError;
use enso_text::text::Change;
use enso_text::Text;
use ensogl_core::data::color;
use ensogl_text_font_family::NonVariableFaceHeader;
use owned_ttf_parser::AsFaceRef;


// ==============
// === Export ===
// ==============

pub mod movement;
pub mod selection;
pub mod word;

pub use movement::*;
pub use selection::Selection;



// =================
// === Constants ===
// =================

/// Default visible line count in a new buffer view.
const DEFAULT_LINE_COUNT: usize = 40;



// ===============
// === History ===
// ===============

/// Modifications history. Contains data used by undo / redo mechanism.
#[derive(Debug, Clone, CloneRef, Default)]
pub struct History {
    data: Rc<RefCell<HistoryData>>,
}

/// Internal representation of `History`.
#[derive(Debug, Clone, Default)]
pub struct HistoryData {
    undo_stack: Vec<(Text, Formatting, selection::Group)>,
    #[allow(dead_code)]
    /// Not yet implemented.
    redo_stack: Vec<(Text, Formatting, selection::Group)>,
}



// ===============
// === Changes ===
// ===============

/// The summary of single text modification, usually returned by `modify`-like functions in
/// `ViewBuffer`.
#[derive(Clone, Debug, Default)]
pub struct Modification<T = UBytes> {
    pub changes:         Vec<ChangeWithSelection<T>>,
    pub selection_group: selection::Group,
    /// Byte offset of this modification. For example, after pressing a backspace with a cursor
    /// placed after an ASCII char, this should result in `-1`.
    pub byte_offset:     Bytes,
}

impl<T> Modification<T> {
    fn merge(&mut self, other: Modification<T>) {
        self.changes.extend(other.changes);
        for selection in other.selection_group {
            self.selection_group.merge(selection)
        }
        self.byte_offset += other.byte_offset;
    }
}


#[derive(Clone, Debug, Default, Eq, PartialEq, Deref)]
pub struct ChangeWithSelection<Metric = UBytes, Str = Text, Loc = Location> {
    #[deref]
    pub change:    Change<Metric, Str>,
    pub selection: Selection<Loc>,
}

impl ChangeWithSelection<UBytes, Text, Location> {
    pub fn is_backspace_at_line_start(&self) -> bool {
        let single_place_edit = self.selection.shape.start == self.selection.shape.end;
        let edit_on_line_start = self.selection.shape.start.offset == UBytes(0);
        let no_insert = self.change.text.is_empty();
        let empty_range = self.change.range.is_empty();
        single_place_edit && edit_on_line_start && no_insert && !empty_range
    }
}


// ==================
// === ViewBuffer ===
// ==================

#[derive(Debug)]
pub struct ShapedGlyph {
    pub position:    rustybuzz::GlyphPosition,
    pub info:        rustybuzz::GlyphInfo,
    pub render_info: GlyphRenderInfo,
}

#[derive(Debug)]
pub struct ShapedGlyphSet {
    pub units_per_em:            u16,
    pub non_variable_variations: NonVariableFaceHeader,
    /// Please note that shaped glyphs in this set have cumulative offsets. This means that even if
    /// they were produced by separate calls to `rustybuzz::shape`, their `info.cluster` is summed
    /// between the calls.
    pub glyphs:                  Vec<ShapedGlyph>,
}


/// Specialized form of `Buffer` with view-related information, such as selection and undo redo
/// history (containing also cursor movement history). This form of buffer is mainly used by `View`,
/// but can also be combined with other `ViewBuffer`s to display cursors, selections, and edits of
/// several users at the same time.
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct ViewBuffer {
    #[deref]
    pub buffer:            Buffer,
    pub selection:         Rc<RefCell<selection::Group>>,
    pub next_selection_id: Rc<Cell<usize>>,
    pub font:              Font,
    pub shaped_lines:      Rc<RefCell<HashMap<Line, Vec<ShapedGlyphSet>>>>,
    pub history:           History,
}

impl ViewBuffer {
    pub fn new(font: Font) -> Self {
        let buffer = default();
        let selection = default();
        let next_selection_id = default();
        let shaped_lines = default();
        let history = default();
        Self { buffer, selection, next_selection_id, font, shaped_lines, history }
    }

    pub fn prev_column_location(&self, location: Location) -> Location {
        self.with_shaped_line(location.line, |shaped_line| {
            let mut byte_offset = None;
            let mut found = false;
            for glyph_set in shaped_line {
                for glyph in &glyph_set.glyphs {
                    let glyph_byte_offset = UBytes(glyph.info.cluster as usize);
                    if glyph_byte_offset >= location.offset {
                        found = true;
                        break;
                    }
                    byte_offset = Some(glyph_byte_offset);
                }
                if found {
                    break;
                }
            }
            byte_offset.map(|t| location.with_offset(t)).unwrap_or_else(|| {
                if location.line > Line(0) {
                    let line = location.line - Line(1);
                    let offset = self.end_byte_offset_of_line_index(line).unwrap();
                    Location { line, offset }
                } else {
                    default()
                }
            })
        })
    }

    pub fn next_column_location(&self, location: Location) -> Location {
        warn!("next_column_location: {:?}", location);
        self.with_shaped_line(location.line, |shaped_line| {
            let mut byte_offset = None;
            let mut prev_was_exact_match = false;
            for glyph_set in shaped_line {
                for glyph in &glyph_set.glyphs {
                    let glyph_byte_offset = UBytes(glyph.info.cluster as usize);
                    if glyph_byte_offset == location.offset {
                        prev_was_exact_match = true;
                    } else if glyph_byte_offset > location.offset {
                        byte_offset = Some(glyph_byte_offset);
                        break;
                    }
                }
                if byte_offset.is_some() {
                    break;
                }
            }
            byte_offset.map(|t| location.with_offset(t)).unwrap_or_else(|| {
                if prev_was_exact_match {
                    let line = location.line;
                    let offset = self.end_byte_offset_of_line_index(line).unwrap();
                    Location { line, offset }
                } else {
                    let last_line = self.last_line_index();
                    if location.line < last_line {
                        let line = location.line + Line(1);
                        let offset = UBytes(0);
                        Location { line, offset }
                    } else {
                        let line = last_line;
                        let offset = self.end_byte_offset_of_line_index(line).unwrap();
                        Location { line, offset }
                    }
                }
            })
        })
    }

    pub fn with_shaped_line<T>(&self, line: Line, mut f: impl FnMut(&[ShapedGlyphSet]) -> T) -> T {
        let mut shaped_lines = self.shaped_lines.borrow_mut();
        if let Some(shaped_line) = shaped_lines.get(&line) {
            f(shaped_line)
        } else {
            let shaped_line = self.shape_line(line);
            let out = f(&shaped_line);
            shaped_lines.insert(line, shaped_line);
            out
        }
    }

    pub fn shape_line(&self, line: Line) -> Vec<ShapedGlyphSet> {
        let line_range = self.byte_range_of_line_index_snapped(line);
        let line_style = self.sub_style(line_range.start..line_range.end);
        let content = self.single_line_content2(line);
        let font = &self.font;
        let mut shaped_line = vec![];
        let mut prev_cluster_byte_offset = 0;
        for (range, requested_non_variable_variations) in
            Self::chunks_per_font_face(font, &line_style, &content)
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
            font.with_borrowed_face(non_variable_variations, |face| {
                let ttf_face = face.ttf.as_face_ref();
                let units_per_em = ttf_face.units_per_em();
                // This is safe. Unwrap should be removed after rustybuzz is fixed:
                // https://github.com/RazrFalcon/rustybuzz/issues/52
                let buzz_face = rustybuzz::Face::from_face(ttf_face.clone()).unwrap();
                let mut buffer = rustybuzz::UnicodeBuffer::new();
                buffer.push_str(&content[range.start.value..range.end.value]);
                let shaped = rustybuzz::shape(&buzz_face, &[], buffer);
                let variable_variations = default();
                let glyphs = shaped
                    .glyph_positions()
                    .into_iter()
                    .zip(shaped.glyph_infos())
                    .map(|(&position, &info)| {
                        let mut info = info;
                        // FIXME:
                        // let variable_variations = glyph.variations.borrow();
                        let glyph_id = font::GlyphId(info.glyph_id as u16);
                        let render_info = font.glyph_info_of_known_face(
                            non_variable_variations,
                            &variable_variations,
                            glyph_id,
                            face,
                        );
                        info.cluster += prev_cluster_byte_offset;
                        ShapedGlyph { position, info, render_info }
                    })
                    .collect();
                let shaped_glyph_set =
                    ShapedGlyphSet { units_per_em, non_variable_variations, glyphs };
                shaped_line.push(shaped_glyph_set);
            });
            prev_cluster_byte_offset = range.end.value as u32;
        }
        shaped_line
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

    pub fn single_line_content2(&self, line: Line) -> String {
        let start_byte_offset = self.byte_offset_of_line_index(line).unwrap();
        let end_byte_offset = self.end_byte_offset_of_line_index_snapped(line);
        let range = start_byte_offset..end_byte_offset;
        self.lines_vec(range).first().cloned().unwrap_or_default()
    }
}

// impl From<Buffer> for ViewBuffer {
//     fn from(buffer: Buffer) -> Self {
//         let selection = default();
//         let next_selection_id = default();
//         let history = default();
//         let shaped_lines = default();
//         Self { buffer, selection, next_selection_id, shaped_lines, history }
//     }
// }

// impl From<&Buffer> for ViewBuffer {
//     fn from(buffer: &Buffer) -> Self {
//         buffer.clone_ref().into()
//     }
// }
//
// impl Default for ViewBuffer {
//     fn default() -> Self {
//         Buffer::default().into()
//     }
// }

impl ViewBuffer {
    fn commit_history(&self) {
        let text = self.buffer.text();
        let style = self.buffer.style();
        let selection = self.selection.borrow().clone();
        self.history.data.borrow_mut().undo_stack.push((text, style, selection));
    }

    fn undo(&self) -> Option<selection::Group> {
        let item = self.history.data.borrow_mut().undo_stack.pop();
        item.map(|(text, style, selection)| {
            self.buffer.set_text(text);
            self.buffer.set_style(style);
            selection
        })
    }

    fn first_selection(&self) -> selection::Group {
        self.selection.borrow().first().cloned().into()
    }

    fn last_selection(&self) -> selection::Group {
        self.selection.borrow().last().cloned().into()
    }

    fn first_cursor(&self) -> selection::Group {
        self.first_selection().snap_selections_to_start()
    }

    fn last_cursor(&self) -> selection::Group {
        self.last_selection().snap_selections_to_start()
    }

    fn newest_selection(&self) -> selection::Group {
        self.selection.borrow().newest().cloned().into()
    }

    fn oldest_selection(&self) -> selection::Group {
        self.selection.borrow().oldest().cloned().into()
    }

    fn newest_cursor(&self) -> selection::Group {
        self.newest_selection().snap_selections_to_start()
    }

    fn oldest_cursor(&self) -> selection::Group {
        self.oldest_selection().snap_selections_to_start()
    }

    fn new_cursor(&self, location: Location) -> Selection {
        let id = self.next_selection_id.get();
        self.next_selection_id.set(id + 1);
        Selection::new_cursor(location, id)
    }

    /// Returns the last used selection or a new one if no active selection exists. This allows for
    /// nice animations when moving cursor between lines after clicking with mouse.
    fn set_cursor(&self, location: Location) -> selection::Group {
        let opt_existing = self.selection.borrow().last().map(|t| t.with_location(location));
        opt_existing.unwrap_or_else(|| self.new_cursor(location)).into()
    }

    fn add_cursor(&self, location: Location) -> selection::Group {
        let mut selection = self.selection.borrow().clone();
        let selection_group = self.new_cursor(location);
        selection.merge(selection_group);
        selection
    }

    fn set_newest_selection_end(&self, location: Location) -> selection::Group {
        let mut group = self.selection.borrow().clone();
        group.newest_mut().for_each(|s| s.end = location);
        group
    }

    fn set_oldest_selection_end(&self, location: Location) -> selection::Group {
        let mut group = self.selection.borrow().clone();
        group.oldest_mut().for_each(|s| s.end = location);
        group
    }

    /// Insert new text in the place of current selections / cursors.
    fn insert(&self, text: impl Into<Text>) -> Modification {
        self.modify(text, None)
    }

    /// Paste new text in the place of current selections / cursors. In case of pasting multiple
    /// chunks (e.g. after copying multiple selections), the chunks will be pasted into subsequent
    /// selections. In case there are more chunks than selections, end chunks will be dropped. In
    /// case there is more selections than chunks, end selections will be replaced with empty
    /// strings. In case there is only one chunk, it will be pasted to all selections.
    fn paste(&self, text: &[String]) -> Modification {
        if text.len() == 1 {
            self.modify_iter(iter::repeat(&text[0]), None)
        } else {
            self.modify_iter(text.iter(), None)
        }
    }

    // TODO
    // Delete left should first delete the vowel (if any) and do not move cursor. After pressing
    // backspace second time, the consonant should be removed. Please read this topic to learn
    // more: https://phabricator.wikimedia.org/T53472
    fn delete_left(&self) -> Modification {
        self.modify("", Some(Transform::Left))
    }

    fn delete_right(&self) -> Modification {
        self.modify("", Some(Transform::Right))
    }

    fn delete_word_left(&self) -> Modification {
        self.modify("", Some(Transform::LeftWord))
    }

    fn delete_word_right(&self) -> Modification {
        self.modify("", Some(Transform::RightWord))
    }

    /// Generic buffer modify utility. It replaces each selection range with the provided `text`.
    ///
    /// If `transform` is provided, it will modify the selections being a simple cursor before
    /// applying modification, what is useful when handling delete operations.
    ///
    /// ## Implementation details.
    /// This function converts all selections to byte-based ones first, and then applies all
    /// modification rules. This way, it can work in an 1D byte-based space (as opposed to 2D
    /// location-based space), which makes handling multiple cursors much easier.
    fn modify(&self, text: impl Into<Text>, transform: Option<Transform>) -> Modification {
        self.commit_history();
        let text = text.into();
        debug!(
            "\n\n\n\n-----------------------------------------\nmodify {:?} {:?}",
            text, transform
        );
        let mut modification = Modification::default();
        for rel_byte_selection in self.byte_selections() {
            warn!("rel_byte_selection: {:?}", rel_byte_selection);
            let byte_selection = rel_byte_selection.map(|t| t + modification.byte_offset);
            let byte_selection = byte_selection.map_shape(|t| {
                t.map(|bytes| {
                    UBytes::try_from(bytes).unwrap_or_else(|_| {
                        error!("Negative byte selection");
                        UBytes(0)
                    })
                })
            });
            let selection = self.to_location_selection(byte_selection);
            debug!(
                ">> {:?}\n{:?}\n{:?}\n{:?}",
                modification, rel_byte_selection, byte_selection, selection
            );
            modification.merge(self.modify_selection(selection, text.clone(), transform));
        }
        modification
    }

    /// Generic buffer modify utility. It replaces each selection range with next iterator item.
    ///
    /// If `transform` is provided, it will modify the selections being a simple cursor before
    /// applying modification, what is useful when handling delete operations.
    fn modify_iter<I, S>(&self, mut iter: I, transform: Option<Transform>) -> Modification
    where
        I: Iterator<Item = S>,
        S: Into<Text>, {
        self.commit_history();
        let mut modification = Modification::default();
        for rel_byte_selection in self.byte_selections() {
            let text = iter.next().map(|t| t.into()).unwrap_or_default();
            let byte_selection = rel_byte_selection.map(|t| t + modification.byte_offset);
            let byte_selection = byte_selection.map_shape(|t| {
                t.map(|bytes| {
                    UBytes::try_from(bytes).unwrap_or_else(|_| {
                        error!("Negative byte selection");
                        UBytes(0)
                    })
                })
            }); // FIXME repeated code above
            let selection = self.to_location_selection(byte_selection);
            modification.merge(self.modify_selection(selection, text, transform));
        }
        modification
    }

    /// Generic selection modify utility. It replaces selection range with given text.
    ///
    /// If `transform` is provided and selection is a simple cursor, it will modify it before
    /// applying modification, what is useful when handling delete operations.
    ///
    /// It returns selection after modification and byte offset of the next selection ranges.
    fn modify_selection(
        &self,
        selection: Selection,
        text: Text,
        transform: Option<Transform>,
    ) -> Modification {
        debug!("modify_selection: {:?} {:?} {:?}", selection, text, transform);
        let text_byte_size = text.byte_size();
        let transformed = match transform {
            Some(t) if selection.is_cursor() => self.moved_selection_region(t, selection, true),
            _ => selection,
        };
        debug!("transformed: {:?}", transformed);

        self.shaped_lines.borrow_mut().remove(&transformed.start.line);

        let byte_selection = self.to_bytes_selection(transformed);
        debug!("byte_selection {:?}", byte_selection);
        let range = byte_selection.range();
        debug!("range {:?}", range);
        self.buffer.replace(range, &text);
        let new_byte_cursor_pos = range.start + text_byte_size;
        debug!("new_byte_cursor_pos {:?}", new_byte_cursor_pos);
        let new_byte_selection = Selection::new_cursor(new_byte_cursor_pos, selection.id);
        debug!("new_byte_selection {:?}", new_byte_selection);
        let change = Change { range, text };
        let change = ChangeWithSelection { change, selection };
        let changes = vec![change];
        debug!("change {:?}", changes);
        let selection_group =
            selection::Group::from(self.to_location_selection(new_byte_selection));
        debug!(">>> {}, {}", text_byte_size, range.size());
        let byte_offset = text_byte_size - range.size();
        Modification { changes, selection_group, byte_offset }
    }

    pub fn byte_selections(&self) -> Vec<Selection<UBytes>> {
        self.selection.borrow().iter().map(|s| self.to_bytes_selection(*s)).collect()
    }

    fn to_bytes_selection(&self, selection: Selection) -> Selection<UBytes> {
        let start = self.byte_offset_of_location_snapped(selection.start);
        let end = self.byte_offset_of_location_snapped(selection.end);
        let id = selection.id;
        Selection::new(start, end, id)
    }

    fn to_location_selection(&self, selection: Selection<UBytes>) -> Selection {
        let start = self.offset_to_location(selection.start);
        let end = self.offset_to_location(selection.end);
        let id = selection.id;
        Selection::new(start, end, id)
    }

    fn offset_to_location(&self, offset: UBytes) -> Location {
        // warn!("offset_to_location: {:?}", offset);
        let line = self.line_index_of_byte_offset_snapped(offset);
        // warn!("line: {:?}", line);
        let line_offset = self.byte_offset_of_line_index(line).unwrap();
        // warn!("line_offset: {:?}", line_offset);
        let line_offset = UBytes::try_from(line_offset).unwrap_or_else(|_| {
            error!("Internal error. Line offset overflow ({:?}).", line_offset);
            UBytes(0)
        });
        // warn!("line_offset: {:?}", line_offset);
        // fixme: tu byl snap_bytes_location_error - potrzebny?
        let byte_offset = UBytes::try_from(offset - line_offset).unwrap();
        // warn!("byte_offset: {:?}", byte_offset);
        Location(line, byte_offset)
    }

    pub fn offset_range_to_location(
        &self,
        range: buffer::Range<UBytes>,
    ) -> buffer::Range<Location> {
        let start = self.offset_to_location(range.start);
        let end = self.offset_to_location(range.end);
        buffer::Range::new(start, end)
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input { [TRACE_ALL]
        cursors_move               (Option<Transform>),
        cursors_select             (Option<Transform>),
        set_cursor                 (Location),
        add_cursor                 (Location),
        set_newest_selection_end   (Location),
        set_oldest_selection_end   (Location),
        insert                     (String),
        paste                      (Vec<String>),
        remove_all_cursors         (),
        delete_left                (),
        delete_right               (),
        delete_word_left           (),
        delete_word_right          (),
        clear_selection            (),
        keep_first_selection_only  (),
        keep_last_selection_only   (),
        keep_first_cursor_only     (),
        keep_last_cursor_only      (),
        keep_oldest_selection_only (),
        keep_newest_selection_only (),
        keep_oldest_cursor_only    (),
        keep_newest_cursor_only    (),
        undo                       (),
        redo                       (),
        // set_default_color          (color::Rgba),
        // set_default_text_size      (style::Size),
        set_property               (Vec<buffer::Range<UBytes>>, style::Property),
        set_property_default       (style::ResolvedProperty),
    }

    Output {
        selection_edit_mode     (Modification),
        selection_non_edit_mode (selection::Group),
        text_change             (Vec<ChangeWithSelection>),
    }
}



// ============
// === View ===
// ============

/// View for a region of a buffer. There are several cases where multiple views share the same
/// buffer, including displaying the buffer in separate tabs or displaying multiple users in the
/// same file (keeping a view per user and merging them visually).
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct View {
    #[deref]
    model:   ViewModel,
    pub frp: Frp,
}

impl View {
    /// Constructor.
    pub fn new(view_buffer: impl Into<ViewBuffer>) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let input = &frp.input;
        let output = &frp.output;
        let model = ViewModel::new(input, view_buffer);
        let m = &model;

        frp::extend! { network
            mod_on_insert            <- input.insert.map(f!((s) m.insert(s)));
            mod_on_paste             <- input.paste.map(f!((s) m.paste(s)));
            mod_on_delete_left       <- input.delete_left.map(f_!(m.delete_left()));
            mod_on_delete_right      <- input.delete_right.map(f_!(m.delete_right()));
            mod_on_delete_word_left  <- input.delete_word_left.map(f_!(m.delete_word_left()));
            mod_on_delete_word_right <- input.delete_word_right.map(f_!(m.delete_word_right()));
            mod_on_delete            <- any(mod_on_delete_left,mod_on_delete_right
                ,mod_on_delete_word_left,mod_on_delete_word_right);
            modification              <- any(mod_on_insert,mod_on_paste,mod_on_delete);
            trace modification;
            // sel_on_modification       <- modification.map(|m| m.selection_group.clone());
            changed                   <- modification.map(|m| !m.changes.is_empty());
            output.source.text_change <+ modification.gate(&changed).map(|m| m.changes.clone());

            sel_on_move            <- input.cursors_move.map(f!((t) m.moved_selection2(*t,false)));
            sel_on_mod             <- input.cursors_select.map(f!((t) m.moved_selection2(*t,true)));
            sel_on_clear           <- input.clear_selection.constant(default());
            sel_on_keep_last       <- input.keep_last_selection_only.map(f_!(m.last_selection()));
            sel_on_keep_first      <- input.keep_first_selection_only.map(f_!(m.first_selection()));
            sel_on_keep_lst_cursor <- input.keep_last_cursor_only.map(f_!(m.last_cursor()));
            sel_on_keep_fst_cursor <- input.keep_first_cursor_only.map(f_!(m.first_cursor()));

            sel_on_keep_newest       <- input.keep_newest_selection_only.map(f_!(m.newest_selection()));
            sel_on_keep_oldest       <- input.keep_oldest_selection_only.map(f_!(m.oldest_selection()));
            sel_on_keep_newest_cursor <- input.keep_newest_cursor_only.map(f_!(m.newest_cursor()));
            sel_on_keep_oldest_cursor <- input.keep_oldest_cursor_only.map(f_!(m.oldest_cursor()));

            sel_on_set_cursor        <- input.set_cursor.map(f!((t) m.set_cursor(*t)));
            sel_on_add_cursor        <- input.add_cursor.map(f!((t) m.add_cursor(*t)));
            trace sel_on_add_cursor;
            sel_on_set_newest_end    <- input.set_newest_selection_end.map(f!((t) m.set_newest_selection_end(*t)));
            sel_on_set_oldest_end    <- input.set_oldest_selection_end.map(f!((t) m.set_oldest_selection_end(*t)));

            sel_on_remove_all <- input.remove_all_cursors.map(|_| default());
            sel_on_undo       <= input.undo.map(f_!(m.undo()));

            // eval input.set_default_color     ((t) m.set_default(*t));
            // eval input.set_default_text_size ((t) m.set_default(*t));
            eval input.set_property          (((range,value)) m.replace(range,*value));
            eval input.set_property_default  ((prop) m.set_property_default(*prop));
            // eval input.set_default_color     ((color) m.set_default(*color));

            output.source.selection_edit_mode     <+ modification;
            // output.source.selection_edit_mode     <+ sel_on_undo;
            output.source.selection_non_edit_mode <+ sel_on_move;
            output.source.selection_non_edit_mode <+ sel_on_mod;
            output.source.selection_non_edit_mode <+ sel_on_clear;
            output.source.selection_non_edit_mode <+ sel_on_keep_last;
            output.source.selection_non_edit_mode <+ sel_on_keep_first;
            output.source.selection_non_edit_mode <+ sel_on_keep_newest;
            output.source.selection_non_edit_mode <+ sel_on_keep_oldest;
            output.source.selection_non_edit_mode <+ sel_on_keep_lst_cursor;
            output.source.selection_non_edit_mode <+ sel_on_keep_fst_cursor;
            output.source.selection_non_edit_mode <+ sel_on_keep_newest_cursor;
            output.source.selection_non_edit_mode <+ sel_on_keep_oldest_cursor;
            output.source.selection_non_edit_mode <+ sel_on_set_cursor;
            output.source.selection_non_edit_mode <+ sel_on_add_cursor;
            output.source.selection_non_edit_mode <+ sel_on_set_newest_end;
            output.source.selection_non_edit_mode <+ sel_on_set_oldest_end;
            output.source.selection_non_edit_mode <+ sel_on_remove_all;

            eval output.source.selection_edit_mode     ((t) m.set_selection(&t.selection_group));
            eval output.source.selection_non_edit_mode ((t) m.set_selection(t));
        }
        Self { model, frp }
    }
}


// =================
// === ViewModel ===
// =================

/// Internal model for the `View`.
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct ViewModel {
    #[deref]
    pub view_buffer: ViewBuffer,
    pub frp:         FrpInputs,
    /// The line that corresponds to `ViewLine(0)`.
    first_view_line: Rc<Cell<Line>>,
    view_line_count: Rc<Cell<usize>>,
}

impl ViewModel {
    /// Constructor.
    pub fn new(frp: &FrpInputs, view_buffer: impl Into<ViewBuffer>) -> Self {
        let frp = frp.clone_ref();
        let view_buffer = view_buffer.into();
        let first_view_line = default();
        let view_line_count = Rc::new(Cell::new(DEFAULT_LINE_COUNT));
        Self { frp, view_buffer, first_view_line, view_line_count }
    }
}

impl ViewModel {
    fn replace(&self, ranges: &Vec<buffer::Range<UBytes>>, property: style::Property) {
        for range in ranges {
            let range = self.crop_byte_range(range);
            self.data.formatting.set_property(range, property)
        }
    }

    fn set_property_default(&self, property: style::ResolvedProperty) {
        self.data.formatting.borrow_mut().set_property_default(property)
    }

    pub fn resolve_property(&self, property: style::Property) -> style::ResolvedProperty {
        self.formatting.borrow().resolve_property(property)
    }

    pub fn text_to_byte_ranges(&self, range: &TextRange) -> Vec<buffer::Range<UBytes>> {
        match range {
            TextRange::Selections => self
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
            TextRange::RangeFull(_) => vec![self.full_range()],
        }
    }

    /// Set the selection to a new value.
    pub fn set_selection(&self, selection: &selection::Group) {
        *self.selection.borrow_mut() = selection.clone();
    }

    /// Return all active selections.
    pub fn selections(&self) -> selection::Group {
        self.selection.borrow().clone()
    }

    /// Return all selections as vector of strings. For cursors, the string will be empty.
    pub fn selections_contents(&self) -> Vec<String> {
        let mut result = Vec::<String>::new();
        for selection in self.byte_selections() {
            result.push(self.buffer.text.sub(selection.range()).into())
        }
        result
    }

    // FIXME: rename - left fixme
    fn moved_selection2(&self, movement: Option<Transform>, modify: bool) -> selection::Group {
        movement.map(|t| self.moved_selection(t, modify)).unwrap_or_default()
    }

    /// Index of the first line of this buffer view.
    pub fn first_view_line(&self) -> Line {
        self.first_view_line.get()
    }

    /// Index of the last line of this buffer view.
    pub fn last_view_line(&self) -> Line {
        let max_line = self.last_line_index();
        let view_line_count = Line(self.view_line_count() as i32);
        max_line.min(self.first_view_line() + view_line_count)
    }

    /// Number of lines visible in this buffer view.
    pub fn view_line_count(&self) -> usize {
        self.view_line_count.get()
    }

    // /// Range of line indexes of this buffer view.
    // pub fn view_line_range(&self) -> Range<Line> {
    //     self.first_view_line()..self.last_view_line()
    // }

    pub fn view_line_range(&self) -> RangeInclusive<ViewLine> {
        ViewLine(0)..=self.line_to_view_line(self.last_view_line())
    }

    pub fn line_to_view_line(&self, line: Line) -> ViewLine {
        ViewLine((line - self.first_view_line()).value)
    }

    pub fn view_line_to_line(&self, view_line: ViewLine) -> Line {
        Line(self.first_view_line().value + view_line.value)
    }

    pub fn location_to_view_location(&self, location: Location) -> ViewLocation {
        let line = self.line_to_view_line(location.line);
        let offset = location.offset;
        Location { line, offset }
    }

    pub fn location_range_to_view_location_range(
        &self,
        range: buffer::Range<Location>,
    ) -> buffer::Range<ViewLocation> {
        let start = self.location_to_view_location(range.start);
        let end = self.location_to_view_location(range.end);
        buffer::Range { start, end }
    }

    pub fn selection_to_view_selection(
        &self,
        selection: Selection<Location>,
    ) -> Selection<ViewLocation> {
        let start = self.location_to_view_location(selection.shape.start);
        let end = self.location_to_view_location(selection.shape.end);
        let shape = selection::Shape { start, end };
        let id = selection.id;
        Selection { shape, id }
    }

    pub fn change_with_selection_to_view_change_with_selection<Metric, Str>(
        &self,
        change_with_selection: ChangeWithSelection<Metric, Str, Location>,
    ) -> ChangeWithSelection<Metric, Str, ViewLocation> {
        let selection = self.selection_to_view_selection(change_with_selection.selection);
        let change = change_with_selection.change;
        ChangeWithSelection { selection, change }
    }

    /// Byte offset of the first line of this buffer view.
    pub fn first_view_line_byte_offset(&self) -> UBytes {
        self.byte_offset_of_line_index(self.first_view_line().into()).unwrap() // FIXME
    }

    /// Byte offset of the last line of this buffer view.
    pub fn last_view_line_byte_offset(&self) -> UBytes {
        self.byte_offset_of_line_index(self.last_view_line().into()).unwrap()
    }

    /// Byte offset range of lines visible in this buffer view.
    pub fn view_line_byte_offset_range(&self) -> Range<UBytes> {
        self.first_view_line_byte_offset()..self.last_view_line_byte_offset()
    }

    /// Byte offset of the end of this buffer view. Snapped to the closest valid value.
    pub fn view_end_byte_offset_snapped(&self) -> UBytes {
        self.end_byte_offset_of_line_index_snapped(self.last_view_line().into())
    }

    /// Return the offset after the last character of a given view line if the line exists.
    pub fn end_offset_of_view_line(&self, line: Line) -> Option<UBytes> {
        self.end_byte_offset_of_line_index(line + self.first_view_line.get()).ok()
    }

    /// The byte range of this buffer view.
    pub fn view_byte_range(&self) -> Range<UBytes> {
        self.first_view_line_byte_offset()..self.view_end_byte_offset_snapped()
    }

    /// The byte offset of the given buffer view line index.
    pub fn byte_offset_of_view_line_index(&self, view_line: Line) -> Result<UBytes, BoundsError> {
        let line = self.first_view_line() + view_line;
        self.byte_offset_of_line_index(line)
    }

    /// Byte range of the given view line.
    pub fn byte_range_of_view_line_index_snapped(&self, view_line: ViewLine) -> Range<UBytes> {
        let line = Line(view_line.value + self.first_view_line.get().value);
        self.byte_range_of_line_index_snapped(line)
    }

    // FIXME: clone of str vec!
    /// Return all lines of this buffer view.
    pub fn view_lines(&self) -> Vec<String> {
        self.lines_vec(self.view_byte_range())
    }

    pub fn lines_content(&self, range: RangeInclusive<ViewLine>) -> Vec<String> {
        let start_line = self.view_line_to_line(*range.start());
        let end_line = self.view_line_to_line(*range.end());
        let start_byte_offset = self.byte_offset_of_line_index(start_line).unwrap();
        let end_byte_offset = self.end_byte_offset_of_line_index_snapped(end_line);
        let range = start_byte_offset..end_byte_offset;
        self.lines_vec(range)
    }

    pub fn last_line_end_byte_offset(&self) -> UBytes {
        self.buffer.text().last_line_end_byte_offset()
    }

    pub fn full_range(&self) -> buffer::Range<UBytes> {
        let start = UBytes::from(0);
        let end = self.buffer.last_line_end_byte_offset();
        buffer::Range { start, end }
    }
}

pub trait FromInContext<Ctx, T> {
    fn from_in_context(context: Ctx, arg: T) -> Self;
}

pub trait IntoInContext<Ctx, T> {
    fn into_in_context(self, context: Ctx) -> T;
}

impl<Ctx, T, U> IntoInContext<Ctx, U> for T
where U: FromInContext<Ctx, T>
{
    fn into_in_context(self, context: Ctx) -> U {
        U::from_in_context(context, self)
    }
}


impl<T, U> FromInContext<&View, U> for T
where T: for<'t> FromInContext<&'t ViewBuffer, U>
{
    fn from_in_context(context: &View, elem: U) -> Self {
        T::from_in_context(&context.model.view_buffer, elem)
    }
}


impl FromInContext<&ViewBuffer, Location> for Location<Column> {
    fn from_in_context(context: &ViewBuffer, location: Location) -> Self {
        context.with_shaped_line(location.line, |shaped_line| {
            let mut column = Column(0);
            let mut found_column = None;
            for glyph_set in shaped_line {
                for glyph in &glyph_set.glyphs {
                    let byte_offset = UBytes(glyph.info.cluster as usize);
                    if byte_offset >= location.offset {
                        if byte_offset > location.offset {
                            warn!("Glyph byte offset mismatch");
                        }
                        found_column = Some(column);
                        break;
                    }
                    column += Column(1);
                }
                if found_column.is_some() {
                    break;
                }
            }
            found_column.map(|t| location.with_offset(t)).unwrap_or_else(|| {
                let offset = context.end_byte_offset_of_line_index(location.line).unwrap();
                if offset != location.offset {
                    warn!("Glyph byte offset mismatch.");
                }
                location.with_offset(column)
            })
        })
    }
}

impl FromInContext<&ViewBuffer, Location<Column>> for Location {
    fn from_in_context(context: &ViewBuffer, location: Location<Column>) -> Self {
        context.with_shaped_line(location.line, |shaped_line| {
            let mut byte_offset = None;
            let mut found = false;
            let mut column = Column(0);
            for glyph_set in shaped_line {
                for glyph in &glyph_set.glyphs {
                    if column == location.offset {
                        byte_offset = Some(UBytes(glyph.info.cluster as usize));
                        found = true;
                        break;
                    }
                    column += Column(1);
                }
                if found {
                    break;
                }
            }
            byte_offset.map(|t| location.with_offset(t)).unwrap_or_else(|| {
                let offset = context.end_byte_offset_of_line_index(location.line).unwrap();
                if column != location.offset {
                    warn!("Glyph byte offset mismatch.");
                }
                location.with_offset(offset)
            })
        })
    }
}
