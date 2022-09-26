//! Buffer part of the text editor.

use crate::prelude::*;
use enso_text::unit::*;

use crate::buffer;
use crate::buffer::formatting;
use crate::buffer::formatting::Formatting;
use crate::buffer::movement::*;
use crate::buffer::rope::formatted::FormattedRope;
use crate::buffer::selection;
use crate::buffer::selection::Selection;
use crate::font::Font;
use crate::font::GlyphId;
use crate::font::GlyphRenderInfo;

use enso_frp as frp;
use enso_text::text::BoundsError;
use enso_text::text::Change;
use enso_text::Rope;
use ensogl_text_font_family::NonVariableFaceHeader;
use owned_ttf_parser::AsFaceRef;



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
    undo_stack: Vec<(Rope, Formatting, selection::Group)>,
    #[allow(dead_code)]
    /// Not yet implemented.
    redo_stack: Vec<(Rope, Formatting, selection::Group)>,
}



// ====================
// === Modification ===
// ====================

/// The summary of single text modification, usually returned by `modify`-like functions in
/// `BufferModel`.
#[allow(missing_docs)]
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



// ===========================
// === ChangeWithSelection ===
// ===========================

/// A change to the text with a selection showing where it was made.
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Deref)]
pub struct ChangeWithSelection<Metric = UBytes, Str = Rope, Loc = Location> {
    #[deref]
    pub change:       Change<Metric, Str>,
    selection:        Selection<Loc>,
    pub change_range: RangeInclusive<Line>,
    pub line_diff:    LineDiff,
}

impl<Metric: Default, Str: Default, Loc: Default> Default
    for ChangeWithSelection<Metric, Str, Loc>
{
    fn default() -> Self {
        let change = default();
        let selection = default();
        let line_diff = default();
        let change_range = Line(0)..=Line(0);
        Self { change, selection, change_range, line_diff }
    }
}



// ===============
// === Shaping ===
// ===============

/// A shaped line of glyphs.
#[allow(missing_docs)]
#[derive(Debug)]
pub enum ShapedLine {
    NonEmpty {
        glyph_sets: NonEmptyVec<ShapedGlyphSet>,
    },
    Empty {
        /// Shaped newline character ending the previous line if any. It is used to calculate the
        /// line style (e.g. its height) for empty lines.
        prev_glyph_info: Option<(UBytes, ShapedGlyphSet)>,
    },
}

impl ShapedLine {
    /// Number of glyphs in line.
    pub fn glyph_count(&self) -> usize {
        match self {
            Self::NonEmpty { glyph_sets } => glyph_sets.iter().map(|set| set.glyphs.len()).sum(),
            Self::Empty { .. } => 0,
        }
    }
}

/// A shaped set of glyphs.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct ShapedGlyphSet {
    pub units_per_em:            u16,
    pub ascender:                i16,
    pub descender:               i16,
    pub line_gap:                i16,
    pub non_variable_variations: NonVariableFaceHeader,
    /// Please note that shaped glyphs in this set have cumulative offsets. This means that even if
    /// they were produced by separate calls to `rustybuzz::shape`, their `info.cluster` is summed
    /// between the calls.
    pub glyphs:                  Vec<ShapedGlyph>,
}

/// A shaped glyph description. See the [`rustybuzz`] library to learn more about data stored in
/// this struct.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy)]
pub struct ShapedGlyph {
    pub position:    rustybuzz::GlyphPosition,
    pub info:        rustybuzz::GlyphInfo,
    pub render_info: GlyphRenderInfo,
}

impl ShapedGlyph {
    /// Returns the byte start of this glyph.
    pub fn start_byte(&self) -> UBytes {
        UBytes(self.info.cluster as usize)
    }

    /// The glyph id, index of glyph in the font.
    pub fn id(&self) -> GlyphId {
        GlyphId(self.info.glyph_id as u16)
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        cursors_move               (Option<Transform>),
        cursors_select             (Option<Transform>),
        set_cursor                 (Location),
        add_cursor                 (Location),
        set_newest_selection_end   (Location),
        set_oldest_selection_end   (Location),
        insert                     (ImString),
        paste                      (Rc<Vec<String>>),
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
        set_property               (Rc<Vec<buffer::Range<UBytes>>>, Option<formatting::Property>),
        mod_property               (Rc<Vec<buffer::Range<UBytes>>>, Option<formatting::PropertyDiff>),
        set_property_default       (Option<formatting::ResolvedProperty>),
        set_first_view_line        (Line),
        mod_first_view_line        (LineDiff),
    }

    Output {
        selection_edit_mode     (Modification),
        selection_non_edit_mode (selection::Group),
        text_change             (Rc<Vec<ChangeWithSelection>>),
        first_view_line         (Line),
    }
}



// ==============
// === Buffer ===
// ==============

/// Buffer for a region of a buffer. There are several cases where multiple views share the same
/// buffer, including displaying the buffer in separate tabs or displaying multiple users in the
/// same file (keeping a view per user and merging them visually).
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct Buffer {
    #[deref]
    model:   BufferModel,
    pub frp: Frp,
}

impl Buffer {
    /// Constructor.
    pub fn new(model: impl Into<BufferModel>) -> Self {
        let frp = Frp::new();
        let network = &frp.network;
        let input = &frp.input;
        let output = &frp.output;
        let model = model.into();
        let m = &model;

        frp::extend! { network
            mod_on_insert <- input.insert.map(f!((s) m.insert(s)));
            mod_on_paste <- input.paste.map(f!((s) m.paste(s)));
            mod_on_delete_left <- input.delete_left.map(f_!(m.delete_left()));
            mod_on_delete_right <- input.delete_right.map(f_!(m.delete_right()));
            mod_on_delete_word_left <- input.delete_word_left.map(f_!(m.delete_word_left()));
            mod_on_delete_word_right <- input.delete_word_right.map(f_!(m.delete_word_right()));
            mod_on_delete <- any(mod_on_delete_left,mod_on_delete_right, mod_on_delete_word_left,
                mod_on_delete_word_right);
            any_mod <- any(mod_on_insert, mod_on_paste, mod_on_delete);
            changed <- any_mod.map(|m| !m.changes.is_empty());
            output.source.text_change <+ any_mod.gate(&changed).map(|m| Rc::new(m.changes.clone()));

            sel_on_move <- input.cursors_move.map(f!((t) m.moved_selection2(*t,false)));
            sel_on_mod <- input.cursors_select.map(f!((t) m.moved_selection2(*t,true)));
            sel_on_clear <- input.clear_selection.constant(default());
            sel_on_keep_last <- input.keep_last_selection_only.map(f_!(m.last_selection()));
            sel_on_keep_first <- input.keep_first_selection_only.map(f_!(m.first_selection()));
            sel_on_keep_lst_cursor <- input.keep_last_cursor_only.map(f_!(m.last_cursor()));
            sel_on_keep_fst_cursor <- input.keep_first_cursor_only.map(f_!(m.first_cursor()));

            sel_on_keep_newest <- input.keep_newest_selection_only.map(f_!(m.newest_selection()));
            sel_on_keep_oldest <- input.keep_oldest_selection_only.map(f_!(m.oldest_selection()));
            sel_on_keep_newest_cursor <- input.keep_newest_cursor_only.map(f_!(m.newest_cursor()));
            sel_on_keep_oldest_cursor <- input.keep_oldest_cursor_only.map(f_!(m.oldest_cursor()));

            sel_on_set_cursor <- input.set_cursor.map(f!((t) m.set_cursor(*t)));
            sel_on_add_cursor <- input.add_cursor.map(f!((t) m.add_cursor(*t)));
            sel_on_set_newest_end <- input.set_newest_selection_end.map
                (f!((t) m.set_newest_selection_end(*t)));
            sel_on_set_oldest_end <- input.set_oldest_selection_end.map
                (f!((t) m.set_oldest_selection_end(*t)));

            sel_on_remove_all <- input.remove_all_cursors.map(|_| default());
            sel_on_undo <= input.undo.map(f_!(m.undo()));

            eval input.set_property (((range,value)) m.replace(range,*value));
            eval input.mod_property (((range,value)) m.mod_property(range,*value));
            eval input.set_property_default ((prop) m.set_property_default(*prop));

            output.source.selection_edit_mode <+ any_mod;
            output.source.selection_non_edit_mode <+ sel_on_undo;
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

            eval output.source.selection_edit_mode ((t) m.set_selection(&t.selection_group));
            eval output.source.selection_non_edit_mode ((t) m.set_selection(t));

            // === Buffer Area Management ===

            eval input.set_first_view_line ((line) m.set_first_view_line(*line));
            output.source.first_view_line <+ input.set_first_view_line;

            new_first_view_line <- input.mod_first_view_line.map
                (f!((diff) m.mod_first_view_line(*diff)));
            output.source.first_view_line <+ new_first_view_line;
        }
        Self { model, frp }
    }
}



// ===================
// === BufferModel ===
// ===================

/// Buffer of styled text associated with selections and text-view related information (like first
/// or last visible line).
#[derive(Debug, Clone, CloneRef, Deref)]
pub struct BufferModel {
    #[deref]
    data: Rc<BufferModelData>,
}

/// Internal representation of [`BufferModel`].
#[allow(missing_docs)]
#[derive(Debug, Deref)]
pub struct BufferModelData {
    #[deref]
    pub rope:              FormattedRope,
    pub selection:         RefCell<selection::Group>,
    pub next_selection_id: Cell<selection::Id>,
    pub font:              Font,
    /// Cache of shaped lines. Shaped lines are needed for many operations, like cursor movement.
    /// For example, moving the cursor right requires knowing the glyph on its right side, which
    /// depends on the used font. It also applies to the non-visible lines.
    pub shaped_lines:      RefCell<BTreeMap<Line, ShapedLine>>,
    pub history:           History,
    /// The line that corresponds to `ViewLine(0)`.
    first_view_line:       Cell<Line>,
    view_line_count:       Cell<Option<usize>>,
}

impl BufferModel {
    /// Constructor.
    pub fn new(font: Font) -> Self {
        let rope = default();
        let selection = default();
        let next_selection_id = default();
        let shaped_lines = default();
        let history = default();
        let first_view_line = default();
        let view_line_count = default();
        let data = BufferModelData {
            rope,
            selection,
            next_selection_id,
            font,
            shaped_lines,
            history,
            first_view_line,
            view_line_count,
        };
        Self { data: Rc::new(data) }
    }
}

// === Location ===

impl BufferModel {
    /// The full text range.
    pub fn full_range(&self) -> buffer::Range<UBytes> {
        let start = UBytes::from(0);
        let end = self.last_line_end_byte_offset();
        buffer::Range { start, end }
    }

    /// Get the previous column of the provided location.
    pub fn prev_column(&self, location: Location) -> Location {
        if location.offset > Column(0) {
            location.with_offset(location.offset - Column(1))
        } else if location.line > Line(0) {
            let location = location.dec_line();
            location.with_offset(self.line_last_column(location.line))
        } else {
            location
        }
    }

    /// Get the next column of the provided location.
    pub fn next_column(&self, location: Location) -> Location {
        let desired_column = location.offset + Column(1);
        if desired_column <= self.line_last_column(location.line) {
            location.with_offset(desired_column)
        } else if location.line < self.last_line_index() {
            location.inc_line().zero_offset()
        } else {
            location
        }
    }
}

// === Selection ===

impl BufferModel {
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
        self.next_selection_id.set(selection::Id { value: id.value + 1 });
        Selection::new_cursor(location, id)
    }

    /// Returns the last used selection or a new one if no active selection exists. This allows for
    /// nice animations when moving cursor between lines after clicking with mouse.
    fn set_cursor(&self, location: Location) -> selection::Group {
        let last_selection = self.selection.borrow().last().cloned();
        let opt_existing = last_selection.map(|t| t.with_location(location));
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

    /// Current selections expressed in bytes.
    pub fn byte_selections(&self) -> Vec<Selection<UBytes>> {
        let selections = self.selection.borrow().clone();
        selections.iter().map(|s| Selection::<UBytes>::from_in_context_snapped(self, *s)).collect()
    }
}


// === Line Shaping ===

impl BufferModel {
    /// Run the closure with the shaped line. If the line was not in the shaped lines cache, it will
    /// be first re-shaped.
    pub fn with_shaped_line<T>(&self, line: Line, mut f: impl FnMut(&ShapedLine) -> T) -> T {
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

    /// Recompute the shape of the provided byte range.
    fn shape_range(&self, range: Range<UBytes>) -> Vec<ShapedGlyphSet> {
        let line_style = self.sub_style(range.clone());
        let content = self.rope.sub(range).to_string();
        let font = &self.font;
        let mut glyph_sets = vec![];
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
                let ascender = ttf_face.ascender();
                let descender = ttf_face.descender();
                let line_gap = ttf_face.line_gap();
                // This is safe. Unwrap should be removed after rustybuzz is fixed:
                // https://github.com/RazrFalcon/rustybuzz/issues/52
                let buzz_face = rustybuzz::Face::from_face(ttf_face.clone()).unwrap();
                let mut buffer = rustybuzz::UnicodeBuffer::new();
                buffer.push_str(&content[range.start.value..range.end.value]);
                let shaped = rustybuzz::shape(&buzz_face, &[], buffer);
                let variable_variations = default();
                let glyphs = shaped
                    .glyph_positions()
                    .iter()
                    .zip(shaped.glyph_infos())
                    .map(|(&position, &info)| {
                        let mut info = info;
                        // FIXME:
                        // let variable_variations = glyph.variations.borrow();
                        let glyph_id = GlyphId(info.glyph_id as u16);
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
                let shaped_glyph_set = ShapedGlyphSet {
                    units_per_em,
                    ascender,
                    descender,
                    line_gap,
                    non_variable_variations,
                    glyphs,
                };
                glyph_sets.push(shaped_glyph_set);
            });
            prev_cluster_byte_offset = range.end.value as u32;
        }
        glyph_sets
    }

    /// Recompute the shape of the provided line index.
    pub fn shape_line(&self, line: Line) -> ShapedLine {
        let line_range = self.byte_range_of_line_index_snapped(line);
        let glyph_sets = self.shape_range(line_range.clone());
        match NonEmptyVec::try_from(glyph_sets) {
            Ok(glyph_sets) => ShapedLine::NonEmpty { glyph_sets },
            Err(_) => {
                if let Some(prev_grapheme_offset) = self.rope.prev_grapheme_offset(line_range.start)
                {
                    let prev_char_range = prev_grapheme_offset..line_range.start;
                    let prev_glyph_sets = self.shape_range(prev_char_range);
                    let prev_glyph_info =
                        prev_glyph_sets.into_iter().last().map(|t| (prev_grapheme_offset, t));
                    ShapedLine::Empty { prev_glyph_info }
                } else {
                    ShapedLine::Empty { prev_glyph_info: None }
                }
            }
        }
    }

    /// Return list of spans for different [`NonVariableFaceHeader`].
    pub fn chunks_per_font_face<'a>(
        font: &'a Font,
        line_style: &'a Formatting,
        content: &'a str,
    ) -> impl Iterator<Item = (Range<UBytes>, NonVariableFaceHeader)> + 'a {
        gen_iter!(move {
            match font {
                Font::NonVariable(_) =>
                    for a in line_style.chunks_per_font_face(content) {
                        yield a;
                    }
                Font::Variable(_) => {
                    let range = UBytes(0)..UBytes(content.len());
                    // For variable fonts, we do not care about non-variable variations.
                    let non_variable_variations = NonVariableFaceHeader::default();
                    yield (range, non_variable_variations);
                }
            }
        })
    }

    /// Content of the provided line.
    pub fn single_line_content2(&self, line: Line) -> String {
        let start_byte_offset = self.byte_offset_of_line_index(line).unwrap();
        let end_byte_offset = self.end_byte_offset_of_line_index_snapped(line);
        let range = start_byte_offset..end_byte_offset;
        self.lines_vec(range).first().cloned().unwrap_or_default()
    }
}

// === Modification ===

impl BufferModel {
    /// Insert new text in the place of current selections / cursors.
    fn insert(&self, text: impl Into<Rope>) -> Modification {
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
    fn modify(&self, text: impl Into<Rope>, transform: Option<Transform>) -> Modification {
        self.commit_history();
        let text = text.into();
        let mut modification = Modification::default();
        for rel_byte_selection in self.byte_selections() {
            let byte_selection = rel_byte_selection.map(|t| t + modification.byte_offset);
            let byte_selection = byte_selection.map_shape(|t| {
                t.map(|bytes| {
                    UBytes::try_from(bytes).unwrap_or_else(|_| {
                        error!("Negative byte selection");
                        UBytes(0)
                    })
                })
            });
            let selection = Selection::<Location>::from_in_context_snapped(self, byte_selection);
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
        S: Into<Rope>, {
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
            let selection = Selection::<Location>::from_in_context_snapped(self, byte_selection);
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
        text: Rope,
        transform: Option<Transform>,
    ) -> Modification {
        let text_byte_size = text.byte_size();
        let transformed = match transform {
            Some(t) if selection.is_cursor() => self.moved_selection_region(t, selection, true),
            _ => selection,
        };


        let byte_selection = Selection::<UBytes>::from_in_context_snapped(self, transformed);
        let range = byte_selection.range();
        self.rope.replace(range, &text);



        let new_byte_cursor_pos = range.start + text_byte_size;
        let new_byte_selection = Selection::new_cursor(new_byte_cursor_pos, selection.id);
        let local_byte_selection = self.to_location_selection2(new_byte_selection);

        let redraw_start_line = transformed.min().line;
        let redraw_end_line = transformed.max().line;

        warn!("redraw_start_line: {}, redraw_end_line: {}", redraw_start_line, redraw_end_line);

        // FIXME, rmoeve these + Line(1), as they zero in diff computation
        let selected_line_count = redraw_end_line - redraw_start_line + Line(1);
        let inserted_line_count = local_byte_selection.end.line - redraw_start_line + Line(1);

        let line_diff = inserted_line_count - selected_line_count;


        if line_diff != LineDiff(0) {
            let mut shaped_lines = self.shaped_lines.borrow_mut();
            let to_update = shaped_lines.drain_filter(|line, _| *line > redraw_end_line);
            let to_update = to_update.map(|(line, s)| (line + line_diff, s)).collect_vec();
            shaped_lines.extend(to_update);
        }

        let redraw_range = redraw_start_line.value..=(redraw_end_line + line_diff).value;

        // FIXME: make it more inteligent.
        for line in redraw_range {
            let line = Line(line);
            self.shaped_lines.borrow_mut().remove(&line);
        }

        // FIXME: construct the below with the local_byte_selection information above
        let loc_selection =
            Selection::<Location>::from_in_context_snapped(self, new_byte_selection);
        let selection_group = selection::Group::from(loc_selection);
        let change = Change { range, text };
        let change_range = redraw_start_line..=redraw_end_line;
        let change = ChangeWithSelection { change, selection, change_range, line_diff };
        warn!("change: {:?}", change);
        let changes = vec![change];
        let byte_offset = text_byte_size - range.size();
        Modification { changes, selection_group, byte_offset }
    }
}


// === Undo / Redo ===

impl BufferModel {
    fn commit_history(&self) {
        let text = self.rope.text();
        let style = self.rope.style();
        let selection = self.selection.borrow().clone();
        self.history.data.borrow_mut().undo_stack.push((text, style, selection));
    }

    fn undo(&self) -> Option<selection::Group> {
        let item = self.history.data.borrow_mut().undo_stack.pop();
        item.map(|(text, style, selection)| {
            self.rope.set_text(text);
            self.rope.set_style(style);
            selection
        })
    }

    // fn to_location_selection(&self, selection: Selection<UBytes>) -> Selection {
    //     let start = Location::from_in_context_snapped(self, selection.start);
    //     let end = Location::from_in_context_snapped(self, selection.end);
    //     let id = selection.id;
    //     Selection::new(start, end, id)
    // }

    fn to_location_selection2(&self, selection: Selection<UBytes>) -> Selection<Location<UBytes>> {
        let start = self.offset_to_location(selection.start);
        let end = self.offset_to_location(selection.end);
        let id = selection.id;
        Selection::new(start, end, id)
    }

    fn offset_to_location(&self, offset: UBytes) -> Location<UBytes> {
        let line = self.line_index_of_byte_offset_snapped(offset);
        let line_offset = self.byte_offset_of_line_index(line).unwrap();
        // fixme: tu byl snap_bytes_location_error - potrzebny?
        let byte_offset = UBytes::try_from(offset - line_offset).unwrap();
        Location(line, byte_offset)
    }

    /// Convert the byte range to location range.
    pub fn offset_range_to_location(
        &self,
        range: buffer::Range<UBytes>,
    ) -> buffer::Range<Location<UBytes>> {
        let start = self.offset_to_location(range.start);
        let end = self.offset_to_location(range.end);
        buffer::Range::new(start, end)
    }

    /// Last column of the provided line index.
    pub fn line_last_column(&self, line: Line) -> Column {
        self.with_shaped_line(line, |shaped_line| Column(shaped_line.glyph_count()))
    }

    /// Last column of the last line.
    pub fn last_line_last_column(&self) -> Column {
        self.line_last_column(self.last_line_index())
    }

    /// Last location of the last line.
    pub fn last_line_last_location(&self) -> Location {
        let line = self.last_line_index();
        let offset = self.line_last_column(line);
        Location { line, offset }
    }
}

impl BufferModel {
    fn set_first_view_line(&self, line: Line) {
        self.first_view_line.set(line);
    }

    fn mod_first_view_line(&self, diff: LineDiff) -> Line {
        let line = self.first_view_line.get() + diff;
        self.set_first_view_line(line);
        line
    }

    fn replace(&self, ranges: &Vec<buffer::Range<UBytes>>, property: Option<formatting::Property>) {
        if let Some(property) = property {
            for range in ranges {
                let range = self.crop_byte_range(range);
                self.formatting.set_property(range, property)
            }
        }
    }

    fn mod_property(
        &self,
        ranges: &Vec<buffer::Range<UBytes>>,
        property: Option<formatting::PropertyDiff>,
    ) {
        if let Some(property) = property {
            for range in ranges {
                let range = self.crop_byte_range(range);
                self.formatting.mod_property(range, property)
            }
        }
    }

    fn set_property_default(&self, property: Option<formatting::ResolvedProperty>) {
        if let Some(property) = property {
            self.formatting.set_property_default(property)
        }
    }

    /// Resolve the provided property by applying a default value if needed.
    pub fn resolve_property(&self, property: formatting::Property) -> formatting::ResolvedProperty {
        self.formatting.resolve_property(property)
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
            result.push(self.rope.text.sub(selection.range()).into())
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
        let view_line_diff = LineDiff(self.view_line_count() as i32 - 1);
        let last_view_line = self.first_view_line() + view_line_diff;
        last_view_line.min(max_line)
    }

    /// Number of lines visible in this buffer view.
    pub fn view_line_count(&self) -> usize {
        self.view_line_count
            .get()
            .unwrap_or_else(|| self.last_line_index().value + 1 - self.first_view_line.get().value)
    }

    /// Last index of visible lines.
    pub fn last_view_line_index(&self) -> ViewLine {
        ViewLine(self.view_line_count() - 1)
    }

    /// Range of visible lines.
    pub fn view_line_range(&self) -> RangeInclusive<ViewLine> {
        ViewLine(0)..=ViewLine::from_in_context_snapped(self, self.last_view_line())
    }

    // FIXME: remove
    /// remove
    pub fn location_to_view_location<T>(&self, location: Location<T>) -> ViewLocation<T> {
        let line = ViewLine::from_in_context_snapped(self, location.line);
        let offset = location.offset;
        Location { line, offset }
    }

    // FIXME: move to from_in_context_snapped
    /// Convert the location range to view location range.
    pub fn location_range_to_view_location_range(
        &self,
        range: buffer::Range<Location<UBytes>>,
    ) -> buffer::Range<ViewLocation<UBytes>> {
        let start = self.location_to_view_location(range.start);
        let end = self.location_to_view_location(range.end);
        buffer::Range { start, end }
    }

    // FIXME: move to from_in_context_snapped
    /// Convert the selection to view selection.
    pub fn selection_to_view_selection(&self, selection: Selection) -> Selection<ViewLocation> {
        let start = self.location_to_view_location(selection.shape.start);
        let end = self.location_to_view_location(selection.shape.end);
        let shape = selection::Shape { start, end };
        let id = selection.id;
        Selection { shape, id }
    }

    /// Byte offset of the first line of this buffer view.
    pub fn first_view_line_byte_offset(&self) -> UBytes {
        self.byte_offset_of_line_index(self.first_view_line()).unwrap() // FIXME
    }

    /// Byte offset of the last line of this buffer view.
    pub fn last_view_line_byte_offset(&self) -> UBytes {
        self.byte_offset_of_line_index(self.last_view_line()).unwrap()
    }

    /// Byte offset range of lines visible in this buffer view.
    pub fn view_line_byte_offset_range(&self) -> Range<UBytes> {
        self.first_view_line_byte_offset()..self.last_view_line_byte_offset()
    }

    /// Byte offset of the end of this buffer view. Snapped to the closest valid value.
    pub fn view_end_byte_offset_snapped(&self) -> UBytes {
        self.end_byte_offset_of_line_index_snapped(self.last_view_line())
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

    // FIXME: is this snapping needed?
    /// Byte range of the given view line.
    pub fn byte_range_of_view_line_index_snapped(&self, view_line: ViewLine) -> Range<UBytes> {
        let line = Line::from_in_context_snapped(self, view_line);
        self.byte_range_of_line_index_snapped(line)
    }

    // FIXME: clone of str vec!
    /// Return all lines of this buffer view.
    pub fn view_lines(&self) -> Vec<String> {
        self.lines_vec(self.view_byte_range())
    }

    /// Get content for lines in the given range.
    pub fn lines_content(&self, range: RangeInclusive<ViewLine>) -> Vec<String> {
        let start_line = Line::from_in_context_snapped(self, *range.start());
        let end_line = Line::from_in_context_snapped(self, *range.end());
        let start_byte_offset = self.byte_offset_of_line_index(start_line).unwrap();
        let end_byte_offset = self.end_byte_offset_of_line_index_snapped(end_line);
        let range = start_byte_offset..end_byte_offset;
        self.lines_vec(range)
    }

    /// End byte offset of the last line.
    pub fn last_line_end_byte_offset(&self) -> UBytes {
        self.rope.text().last_line_end_byte_offset()
    }
}



// =================
// === RangeLike ===
// =================

/// A range-like description. Any of the enum variants can be used to describe a range in text.
/// There are conversions defined, so you never need to use this type explicitly.
#[allow(missing_docs)]
#[derive(Debug, Clone, Default, From)]
pub enum RangeLike {
    #[default]
    Selections,
    BufferRangeUBytes(buffer::Range<UBytes>),
    BufferRangeLocationColumn(buffer::Range<Location>),
    RangeBytes(Range<UBytes>),
    RangeFull(RangeFull),
}

impl RangeLike {
    /// Expand the [`RangeLike`] to vector of text ranges.
    pub fn expand(&self, buffer: &BufferModel) -> Vec<buffer::Range<UBytes>> {
        match self {
            RangeLike::Selections => {
                let byte_selections = buffer.byte_selections().into_iter();
                byte_selections
                    .map(|t| {
                        let start = std::cmp::min(t.start, t.end);
                        let end = std::cmp::max(t.start, t.end);
                        buffer::Range::new(start, end)
                    })
                    .collect()
            }
            RangeLike::BufferRangeUBytes(range) => vec![*range],
            RangeLike::RangeBytes(range) => vec![range.into()],
            RangeLike::RangeFull(_) => vec![buffer.full_range()],
            RangeLike::BufferRangeLocationColumn(range) => {
                let start = UBytes::from_in_context_snapped(buffer, range.start);
                let end = UBytes::from_in_context_snapped(buffer, range.end);
                vec![buffer::Range::new(start, end)]
            }
        }
    }
}



// ====================
// === LocationLike ===
// ====================

/// A location-like description. Any of the enum variants can be used to describe a location in
/// text. There are conversions defined, so you never need to use this type explicitly.
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, From)]
pub enum LocationLike {
    LocationColumnLine(Location<Column, Line>),
    LocationUBytesLine(Location<UBytes, Line>),
    LocationColumnViewLine(Location<Column, ViewLine>),
    LocationUBytesViewLine(Location<UBytes, ViewLine>),
}

impl Default for LocationLike {
    fn default() -> Self {
        LocationLike::LocationColumnLine(default())
    }
}

impl LocationLike {
    pub fn expand(self, buffer: &BufferModel) -> Location<Column, Line> {
        match self {
            LocationLike::LocationColumnLine(loc) => loc,
            LocationLike::LocationUBytesLine(loc) => Location::from_in_context_snapped(buffer, loc),
            LocationLike::LocationColumnViewLine(loc) =>
                Location::from_in_context_snapped(buffer, loc),
            LocationLike::LocationUBytesViewLine(loc) =>
                Location::from_in_context_snapped(buffer, loc),
        }
    }
}


// ===================
// === Conversions ===
// ===================

pub trait FromInContext<Ctx, T> {
    fn from_in_context(context: Ctx, arg: T) -> Self;
}

/// Perform conversion between two values. It is just like the [`From`] trait, but it performs the
/// conversion in a "context". The "context" is an object containing additional information required
/// to perform the conversion. For example, the context can be a text buffer, which is needed to
/// convert byte offset to line-column location.
#[allow(missing_docs)]
pub trait FromInContextSnapped<Ctx, T> {
    fn from_in_context_snapped(context: Ctx, arg: T) -> Self;
}

/// Try performing conversion between two values. It is like the [`FromInContextSnapped`] trait, but
/// can fail.
#[allow(missing_docs)]
pub trait TryFromInContext<Ctx, T>
where Self: Sized {
    type Error;
    fn try_from_in_context(context: Ctx, arg: T) -> Result<Self, Self::Error>;
}


// === Generic Impls ===

impl<'t, T, U> FromInContext<&'t Buffer, U> for T
where T: FromInContext<&'t BufferModel, U>
{
    fn from_in_context(buffer: &'t Buffer, elem: U) -> Self {
        T::from_in_context(&buffer.model, elem)
    }
}

impl<'t, T, U> FromInContextSnapped<&'t Buffer, U> for T
where T: FromInContextSnapped<&'t BufferModel, U>
{
    fn from_in_context_snapped(buffer: &'t Buffer, elem: U) -> Self {
        T::from_in_context_snapped(&buffer.model, elem)
    }
}

impl<'t, T, U> TryFromInContext<&'t Buffer, U> for T
where T: TryFromInContext<&'t BufferModel, U>
{
    type Error = <T as TryFromInContext<&'t BufferModel, U>>::Error;
    fn try_from_in_context(buffer: &'t Buffer, elem: U) -> Result<Self, Self::Error> {
        T::try_from_in_context(&buffer.model, elem)
    }
}


// === Conversions to Line ===

#[derive(Debug, Clone, Copy)]
pub enum ViewLineToLineConversionError {
    TooSmall,
    TooBig,
}

impl TryFromInContext<&BufferModel, ViewLine> for Line {
    type Error = ViewLineToLineConversionError;
    fn try_from_in_context(buffer: &BufferModel, view_line: ViewLine) -> Result<Self, Self::Error> {
        let line = buffer.first_view_line() + Line(view_line.value);
        if line > buffer.last_line_index() {
            Err(ViewLineToLineConversionError::TooBig)
        } else {
            Ok(line)
        }
    }
}

impl FromInContextSnapped<&BufferModel, ViewLineToLineConversionError> for Line {
    fn from_in_context_snapped(buffer: &BufferModel, err: ViewLineToLineConversionError) -> Self {
        match err {
            ViewLineToLineConversionError::TooSmall => Line(0),
            ViewLineToLineConversionError::TooBig => buffer.last_line_index(),
        }
    }
}

impl FromInContextSnapped<&BufferModel, ViewLine> for Line {
    fn from_in_context_snapped(buffer: &BufferModel, view_line: ViewLine) -> Self {
        Line::try_from_in_context(buffer, view_line)
            .unwrap_or_else(|err| Line::from_in_context_snapped(buffer, err))
    }
}

impl<T> FromInContextSnapped<&BufferModel, Location<T, Line>> for Line {
    fn from_in_context_snapped(_: &BufferModel, location: Location<T, Line>) -> Self {
        location.line
    }
}

impl<T> FromInContextSnapped<&BufferModel, Location<T, ViewLine>> for Line {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<T, ViewLine>) -> Self {
        Line::from_in_context_snapped(buffer, location.line)
    }
}

impl FromInContextSnapped<&BufferModel, UBytes> for Line {
    fn from_in_context_snapped(buffer: &BufferModel, offset: UBytes) -> Self {
        Location::<UBytes, Line>::from_in_context_snapped(buffer, offset).line
    }
}


// === Conversions to ViewLine ===

#[derive(Debug, Clone, Copy)]
pub enum LineToViewLineConversionError {
    TooSmall,
    TooBig,
}

impl TryFromInContext<&BufferModel, Line> for ViewLine {
    type Error = LineToViewLineConversionError;
    fn try_from_in_context(buffer: &BufferModel, line: Line) -> Result<Self, Self::Error> {
        let line_diff = line - buffer.first_view_line();
        if line_diff.value < 0 {
            Err(LineToViewLineConversionError::TooSmall)
        } else {
            let view_line = ViewLine(line_diff.value as usize);
            if view_line > buffer.last_view_line_index() {
                Err(LineToViewLineConversionError::TooBig)
            } else {
                Ok(view_line)
            }
        }
    }
}

impl FromInContextSnapped<&BufferModel, LineToViewLineConversionError> for ViewLine {
    fn from_in_context_snapped(buffer: &BufferModel, err: LineToViewLineConversionError) -> Self {
        match err {
            LineToViewLineConversionError::TooSmall => ViewLine(0),
            LineToViewLineConversionError::TooBig => buffer.last_view_line_index(),
        }
    }
}

impl FromInContextSnapped<&BufferModel, Line> for ViewLine {
    fn from_in_context_snapped(buffer: &BufferModel, line: Line) -> Self {
        ViewLine::try_from_in_context(buffer, line)
            .unwrap_or_else(|err| ViewLine::from_in_context_snapped(buffer, err))
    }
}


// === Conversions to UBytes ===

impl FromInContextSnapped<&BufferModel, Location<UBytes, Line>> for UBytes {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<UBytes, Line>) -> Self {
        buffer.byte_offset_of_line_index(location.line).unwrap() + location.offset
    }
}

impl FromInContextSnapped<&BufferModel, Location<UBytes, ViewLine>> for UBytes {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<UBytes, ViewLine>) -> Self {
        let location = Location::<UBytes, Line>::from_in_context_snapped(buffer, location);
        UBytes::from_in_context_snapped(buffer, location)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, Line>> for UBytes {
    fn from_in_context_snapped(context: &BufferModel, location: Location) -> Self {
        let location = Location::<UBytes, Line>::from_in_context_snapped(context, location);
        UBytes::from_in_context_snapped(context, location)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, ViewLine>> for UBytes {
    fn from_in_context_snapped(
        context: &BufferModel,
        location: Location<Column, ViewLine>,
    ) -> Self {
        let location = Location::<UBytes, Line>::from_in_context_snapped(context, location);
        UBytes::from_in_context_snapped(context, location)
    }
}


// === Conversions to Location<Column, Line> ===

impl FromInContextSnapped<&BufferModel, Location<UBytes, Line>> for Location<Column, Line> {
    fn from_in_context_snapped(context: &BufferModel, location: Location<UBytes, Line>) -> Self {
        context.with_shaped_line(location.line, |shaped_line| {
            let mut column = Column(0);
            let mut found_column = None;
            if let ShapedLine::NonEmpty {glyph_sets} = &shaped_line {
                for glyph_set in glyph_sets {
                    for glyph in &glyph_set.glyphs {
                        let byte_offset = UBytes(glyph.info.cluster as usize);
                        if byte_offset >= location.offset {
                            if byte_offset > location.offset {
                                error!("Glyph byte offset mismatch");
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
            }
            found_column.map(|t| location.with_offset(t)).unwrap_or_else(|| {
                let offset = context.line_byte_length(location.line);
                if offset != location.offset {
                    error!("Glyph byte offset mismatch. Requested {} byte offset, but line {} has only {} bytes. Using max offset instead.", location.offset, location.line, offset);
                }
                location.with_offset(column)
            })
        })
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, ViewLine>> for Location<Column, Line> {
    fn from_in_context_snapped(
        context: &BufferModel,
        location: Location<Column, ViewLine>,
    ) -> Self {
        let line = Line::from_in_context_snapped(context, location.line);
        Location(line, location.offset)
    }
}

impl FromInContextSnapped<&BufferModel, Location<UBytes, ViewLine>> for Location<Column, Line> {
    fn from_in_context_snapped(
        context: &BufferModel,
        location: Location<UBytes, ViewLine>,
    ) -> Self {
        let line = Line::from_in_context_snapped(context, location.line);
        Location::from_in_context_snapped(context, Location(line, location.offset))
    }
}

impl FromInContextSnapped<&BufferModel, UBytes> for Location<Column, Line> {
    fn from_in_context_snapped(context: &BufferModel, offset: UBytes) -> Self {
        Location::from_in_context_snapped(
            context,
            Location::<UBytes>::from_in_context_snapped(context, offset),
        )
    }
}


// === Conversions to Location<UBytes, ViewLine> ===

impl FromInContextSnapped<&BufferModel, Location<UBytes, Line>> for Location<UBytes, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<UBytes, Line>) -> Self {
        let line = ViewLine::from_in_context_snapped(buffer, location.line);
        location.with_line(line)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, Line>> for Location<UBytes, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, Line>) -> Self {
        let location = Location::<UBytes, Line>::from_in_context_snapped(buffer, location);
        Location::<UBytes, ViewLine>::from_in_context_snapped(buffer, location)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, ViewLine>> for Location<UBytes, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(buffer, location.line);
        Location::<UBytes, ViewLine>::from_in_context_snapped(buffer, location.with_line(line))
    }
}

impl FromInContextSnapped<&BufferModel, UBytes> for Location<UBytes, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, offset: UBytes) -> Self {
        let location = Location::<UBytes, Line>::from_in_context_snapped(buffer, offset);
        Location::<UBytes, ViewLine>::from_in_context_snapped(buffer, location)
    }
}


// === Conversions to Location<Column, ViewLine> ===

impl FromInContextSnapped<&BufferModel, Location<Column, Line>> for Location<Column, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, Line>) -> Self {
        let line = ViewLine::from_in_context_snapped(buffer, location.line);
        location.with_line(line)
    }
}

impl FromInContextSnapped<&BufferModel, Location<UBytes, Line>> for Location<Column, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<UBytes, Line>) -> Self {
        let location = Location::<Column, Line>::from_in_context_snapped(buffer, location);
        Location::<Column, ViewLine>::from_in_context_snapped(buffer, location)
    }
}

impl FromInContextSnapped<&BufferModel, Location<UBytes, ViewLine>> for Location<Column, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<UBytes, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(buffer, location.line);
        Location::<Column, ViewLine>::from_in_context_snapped(buffer, location.with_line(line))
    }
}

impl FromInContextSnapped<&BufferModel, UBytes> for Location<Column, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, offset: UBytes) -> Self {
        let location = Location::<Column, Line>::from_in_context_snapped(buffer, offset);
        Location::<Column, ViewLine>::from_in_context_snapped(buffer, location)
    }
}


// === Conversions to Location<UBytes, Line> ===

impl FromInContextSnapped<&BufferModel, Location<Column, Line>> for Location<UBytes, Line> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, Line>) -> Self {
        buffer.with_shaped_line(location.line, |shaped_line| {
            let mut byte_offset = None;
            let mut found = false;
            let mut column = Column(0);
            if let ShapedLine::NonEmpty {glyph_sets} = &shaped_line {
                for glyph_set in glyph_sets {
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
            }
            let out = byte_offset.map(|t| location.with_offset(t)).unwrap_or_else(|| {
                if column != location.offset {
                    error!(
                        "Column {} requested while line {} has only {} columns. Using last column instead.",
                        location.offset, location.line, column
                    );
                }
                // FIXME: unwrap
                let end_byte_offset = buffer.end_byte_offset_of_line_index(location.line).unwrap();
                let location2 = Location::<UBytes, Line>::from_in_context_snapped(buffer, end_byte_offset);
                let offset = location2.offset;
                location.with_offset(offset)
            });
            out
        })
    }
}

impl FromInContextSnapped<&BufferModel, UBytes> for Location<UBytes, Line> {
    fn from_in_context_snapped(context: &BufferModel, offset: UBytes) -> Self {
        let line = context.line_index_of_byte_offset_snapped(offset);
        let line_offset = context.byte_offset_of_line_index(line).unwrap();
        let byte_offset = UBytes::try_from(offset - line_offset).unwrap();
        Location(line, byte_offset)
    }
}

impl FromInContextSnapped<&BufferModel, Location<UBytes, ViewLine>> for Location<UBytes, Line> {
    fn from_in_context_snapped(buffer: &BufferModel, offset: Location<UBytes, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(buffer, offset.line);
        Location(line, offset.offset)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, ViewLine>> for Location<UBytes, Line> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(buffer, location.line);
        Location::from_in_context_snapped(buffer, location.with_line(line))
    }
}


// === Conversions of Range ====

impl<'t, S, T> FromInContextSnapped<&'t BufferModel, buffer::Range<S>> for buffer::Range<T>
where T: FromInContextSnapped<&'t BufferModel, S>
{
    fn from_in_context_snapped(context: &'t BufferModel, range: buffer::Range<S>) -> Self {
        let start = T::from_in_context_snapped(context, range.start);
        let end = T::from_in_context_snapped(context, range.end);
        buffer::Range::new(start, end)
    }
}



// === Selections ===

impl<'t, T, S> FromInContextSnapped<&'t BufferModel, Selection<T>> for Selection<S>
where
    T: Copy,
    S: FromInContextSnapped<&'t BufferModel, T>,
{
    fn from_in_context_snapped(buffer: &'t BufferModel, selection: Selection<T>) -> Self {
        let start = S::from_in_context_snapped(buffer, selection.start);
        let end = S::from_in_context_snapped(buffer, selection.end);
        let id = selection.id;
        Selection::new(start, end, id)
    }
}


// // warn!("offset_to_location: {:?}", offset);
// let line = self.line_index_of_byte_offset_snapped(offset);
// // warn!("line: {:?}", line);
// let line_offset = self.byte_offset_of_line_index(line).unwrap();
// // warn!("line_offset: {:?}", line_offset);
// let line_offset = UBytes::try_from(line_offset).unwrap_or_else(|_| {
// error!("Internal error. Line offset overflow ({:?}).", line_offset);
// UBytes(0)
// });
// // warn!("line_offset: {:?}", line_offset);
// // fixme: tu byl snap_bytes_location_error - potrzebny?
// let byte_offset = UBytes::try_from(offset - line_offset).unwrap();
// // warn!("byte_offset: {:?}", byte_offset);
// Location(line, byte_offset)
