//! Root of text buffer implementation. The text buffer is a sophisticated model for text styling
//! and editing operations.

use crate::index::*;
use crate::prelude::*;
use enso_text::unit::*;

use crate::buffer::formatting::Formatting;
use crate::buffer::rope::formatted::FormattedRope;
use crate::buffer::selection::Selection;

use enso_frp as frp;
use enso_text::text;
use enso_text::text::BoundsError;
use ensogl_text_font_family::NonVariableFaceHeader;


// ==============
// === Export ===
// ==============

pub mod formatting;
pub mod index;
pub mod movement;
pub mod rope;
pub mod selection;



/// Common traits.
pub mod traits {
    pub use enso_text::traits::*;
}

pub use formatting::*;
pub use movement::*;

pub use enso_text::index::*;
pub use enso_text::unit::*;
pub use enso_text::Range;
pub use enso_text::Rope;
pub use enso_text::RopeCell;



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
pub struct Modification<T = Byte> {
    pub changes:         Vec<Change<T>>,
    pub selection_group: selection::Group,
    /// Byte offset of this modification. For example, after pressing a backspace with a cursor
    /// placed after an ASCII char, this should result in `-1`.
    pub byte_offset:     ByteDiff,
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



// ==============
// === Change ===
// ==============

/// A buffer change. It is a rope change with additional information required for lazy line
/// redrawing.
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, PartialEq, Deref)]
pub struct Change<Metric = Byte, Str = Rope> {
    #[deref]
    pub change:       text::Change<Metric, Str>,
    pub change_range: RangeInclusive<Line>,
    pub line_diff:    LineDiff,
    pub selection:    Selection<ViewLocation>,
}

impl<Metric: Default, Str: Default> Default for Change<Metric, Str> {
    fn default() -> Self {
        let change = default();
        let line_diff = default();
        let change_range = Line(0)..=Line(0);
        let selection = default();
        Self { change, change_range, line_diff, selection }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        cursors_move               (Transform),
        cursors_select             (Transform),
        set_cursor                 (Location),
        add_cursor                 (Location),
        set_single_selection       (selection::Shape),
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
        set_property               (Rc<Vec<Range<Byte>>>, Option<Property>),
        mod_property               (Rc<Vec<Range<Byte>>>, Option<PropertyDiff>),
        set_property_default       (Option<ResolvedProperty>),
        set_first_view_line        (Line),
        mod_first_view_line        (LineDiff),
    }

    Output {
        selection_edit_mode     (Modification),
        selection_non_edit_mode (selection::Group),
        text_change             (Rc<Vec<Change>>),
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

            sel_on_move <- input.cursors_move.map(f!((t) m.moved_selection(*t,false)));
            sel_on_mod <- input.cursors_select.map(f!((t) m.moved_selection(*t,true)));
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
            sel_on_set_single_selection <- input.set_single_selection.map(
                f!((t) m.set_single_selection(*t))
            );
            sel_on_set_newest_end <- input.set_newest_selection_end.map
                (f!((t) m.set_newest_selection_end(*t)));
            sel_on_set_oldest_end <- input.set_oldest_selection_end.map
                (f!((t) m.set_oldest_selection_end(*t)));

            sel_on_remove_all <- input.remove_all_cursors.map(|_| default());
            sel_on_undo <= input.undo.map(f_!(m.undo()));

            eval input.set_property (((range,value)) m.set_property(range,*value));
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
            output.source.selection_non_edit_mode <+ sel_on_set_single_selection;
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
#[derive(Debug, Clone, CloneRef, Deref, Default)]
pub struct BufferModel {
    #[deref]
    data: Rc<BufferModelData>,
}

/// Internal representation of [`BufferModel`].
#[allow(missing_docs)]
#[derive(Debug, Deref, Default)]
pub struct BufferModelData {
    #[deref]
    pub rope:          FormattedRope,
    pub selection:     RefCell<selection::Group>,
    next_selection_id: Cell<selection::Id>,
    pub history:       History,
    /// The line that corresponds to `ViewLine(0)`.
    first_view_line:   Cell<Line>,
    view_line_count:   Cell<Option<usize>>,
}

impl BufferModel {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }
}


// === Location ===

impl BufferModel {
    /// The full text range.
    pub fn full_range(&self) -> Range<Byte> {
        let start = Byte::from(0);
        let end = self.last_line_end_offset();
        Range { start, end }
    }

    /// Get the previous column of the provided location.
    pub fn prev_column(&self, location: Location) -> Location {
        // Column can be bigger than last line column if the cursor moved from longer line to a
        // shorter one. We keep the bigger column in the cursor, so if it moves back to the longer
        // line, the column selection will be preserved.
        let line_last_column = self.line_last_column(location.line);
        let current_column = std::cmp::min(location.offset, line_last_column);
        if current_column > Column(0) {
            location.with_offset(current_column - Column(1))
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

    /// Last column of the provided line index.
    pub fn line_last_column(&self, line: Line) -> Column {
        self.rope.line_last_column(line).unwrap()
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

    /// Byte offset of the first line of this buffer view.
    pub fn first_view_line_byte_offset(&self) -> Byte {
        self.line_offset(self.first_view_line()).unwrap()
    }

    /// Byte offset of the last line of this buffer view.
    pub fn last_view_line_byte_offset(&self) -> Byte {
        self.line_offset(self.last_view_line()).unwrap()
    }

    /// Byte offset range of lines visible in this buffer view.
    pub fn view_line_byte_offset_range(&self) -> std::ops::Range<Byte> {
        self.first_view_line_byte_offset()..self.last_view_line_byte_offset()
    }

    /// Byte offset of the end of this buffer view. Snapped to the closest valid value.
    pub fn view_end_byte_offset_snapped(&self) -> Byte {
        self.line_end_offset_snapped(self.last_view_line())
    }

    /// Return the offset after the last character of a given view line if the line exists.
    pub fn end_offset_of_view_line(&self, line: Line) -> Option<Byte> {
        self.line_end_offset(line + self.first_view_line.get()).ok()
    }

    /// The byte range of this buffer view.
    pub fn view_byte_range(&self) -> std::ops::Range<Byte> {
        self.first_view_line_byte_offset()..self.view_end_byte_offset_snapped()
    }

    /// The byte offset of the given buffer view line index.
    pub fn byte_offset_of_view_line_index(&self, view_line: Line) -> Result<Byte, BoundsError> {
        let line = self.first_view_line() + view_line;
        self.line_offset(line)
    }

    /// Byte range of the given view line.
    pub fn byte_range_of_view_line_index_snapped(
        &self,
        view_line: ViewLine,
    ) -> std::ops::Range<Byte> {
        let line = Line::from_in_context_snapped(self, view_line);
        self.line_range_snapped(line)
    }

    /// End byte offset of the last line.
    pub fn last_line_end_offset(&self) -> Byte {
        self.rope.text().last_line_end_offset()
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

    fn new_selection(&self, shape: selection::Shape) -> Selection {
        let id = self.next_selection_id.get();
        self.next_selection_id.set(selection::Id { value: id.value + 1 });
        Selection { shape, id }
    }

    fn new_cursor(&self, location: Location) -> Selection {
        self.new_selection(selection::Shape::new_cursor(location))
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

    fn set_single_selection(&self, shape: selection::Shape) -> selection::Group {
        let last_selection = self.selection.borrow().last().cloned();
        let opt_existing = last_selection.map(|t| t.with_shape(shape));
        opt_existing.unwrap_or_else(|| self.new_selection(shape)).into()
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
    pub fn byte_selections(&self) -> Vec<Selection<Byte>> {
        let selections = self.selection.borrow().clone();
        selections.iter().map(|s| Selection::<Byte>::from_in_context_snapped(self, *s)).collect()
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
}


// === Line Shaping ===

impl BufferModel {}


// === Modification ===

impl BufferModel {
    /// Get content for lines in the given range.
    pub fn lines_content(&self, range: RangeInclusive<ViewLine>) -> Vec<String> {
        let start_line = Line::from_in_context_snapped(self, *range.start());
        let end_line = Line::from_in_context_snapped(self, *range.end());
        let start_byte_offset = self.line_offset(start_line).unwrap();
        let end_byte_offset = self.line_end_offset_snapped(end_line);
        let range = start_byte_offset..end_byte_offset;
        self.lines_vec(range)
    }

    /// Insert new text in the place of current selections / cursors.
    fn insert(&self, text: impl Into<Rope>) -> Modification {
        self.modify_selections(iter::repeat(text.into()), None)
    }

    /// Paste new text in the place of current selections / cursors. In case of pasting multiple
    /// chunks (e.g. after copying multiple selections), the chunks will be pasted into subsequent
    /// selections. In case there are more chunks than selections, end chunks will be dropped. In
    /// case there is more selections than chunks, end selections will be replaced with empty
    /// strings. In case there is only one chunk, it will be pasted to all selections.
    fn paste(&self, text: &[String]) -> Modification {
        if text.len() == 1 {
            self.modify_selections(iter::repeat((&text[0]).into()), None)
        } else {
            self.modify_selections(text.iter().map(|t| t.into()), None)
        }
    }

    // TODO: Delete left should first delete the vowel (if any) and do not move cursor. After
    //   pressing backspace second time, the consonant should be removed. Please read this topic
    //   to learn more: https://phabricator.wikimedia.org/T53472
    fn delete_left(&self) -> Modification {
        self.modify_selections(iter::empty(), Some(Transform::Left))
    }

    fn delete_right(&self) -> Modification {
        self.modify_selections(iter::empty(), Some(Transform::Right))
    }

    fn delete_word_left(&self) -> Modification {
        self.modify_selections(iter::empty(), Some(Transform::LeftWord))
    }

    fn delete_word_right(&self) -> Modification {
        self.modify_selections(iter::empty(), Some(Transform::RightWord))
    }

    /// Generic buffer modify utility. It replaces each selection range with next iterator item.
    ///
    /// If `transform` is provided, it will modify the selections being a simple cursor before
    /// applying modification, what is useful when handling delete operations.
    fn modify_selections<I>(&self, mut iter: I, transform: Option<Transform>) -> Modification
    where I: Iterator<Item = Rope> {
        self.commit_history();
        let mut modification = Modification::default();
        for rel_byte_selection in self.byte_selections() {
            let text = iter.next().unwrap_or_default();
            let byte_selection = rel_byte_selection.map(|t| t + modification.byte_offset);
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
        let text_byte_size = text.last_byte_index();
        let transformed = match transform {
            Some(t) if selection.is_cursor() => self.moved_selection_region(t, selection, true),
            _ => selection,
        };

        let byte_selection = Selection::<Byte>::from_in_context_snapped(self, transformed);
        let line_selection =
            Selection::<ViewLocation>::from_in_context_snapped(self, byte_selection);
        let line_selection = line_selection.map_shape(|s| s.normalized());
        let range = byte_selection.range();
        self.rope.replace(range, &text);

        let new_byte_cursor_pos = range.start + text_byte_size;
        let new_byte_selection = Selection::new_cursor(new_byte_cursor_pos, selection.id);
        let local_byte_selection =
            Selection::<Location<Byte>>::from_in_context_snapped(self, new_byte_selection);

        let redraw_start_line = transformed.min().line;
        let redraw_end_line = transformed.max().line;
        let selected_line_count = redraw_end_line - redraw_start_line + Line(1);
        let inserted_line_count = local_byte_selection.end.line - redraw_start_line + Line(1);
        let line_diff = inserted_line_count - selected_line_count;

        let loc_selection =
            Selection::<Location>::from_in_context_snapped(self, new_byte_selection);
        let selection_group = selection::Group::from(loc_selection);
        let change = text::Change { range, text };
        let change_range = redraw_start_line..=redraw_end_line;
        let change = Change { change, change_range, line_diff, selection: line_selection };
        let changes = vec![change];
        let byte_offset = text_byte_size.to_diff() - range.size();
        Modification { changes, selection_group, byte_offset }
    }
}


// === Properties ===

impl BufferModel {
    fn set_property(&self, ranges: &Vec<Range<Byte>>, property: Option<Property>) {
        if let Some(property) = property {
            for range in ranges {
                let range = self.crop_byte_range(range);
                self.formatting.set_property(range, property)
            }
        }
    }

    fn mod_property(&self, ranges: &Vec<Range<Byte>>, property: Option<PropertyDiff>) {
        if let Some(property) = property {
            for range in ranges {
                let range = self.crop_byte_range(range);
                self.formatting.mod_property(range, property)
            }
        }
    }

    fn set_property_default(&self, property: Option<ResolvedProperty>) {
        if let Some(property) = property {
            self.formatting.set_property_default(property)
        }
    }

    /// Resolve the provided property by applying a default value if needed.
    pub fn resolve_property(&self, property: Property) -> ResolvedProperty {
        self.formatting.resolve_property(property)
    }
}


// === View ===

impl BufferModel {
    fn set_first_view_line(&self, line: Line) {
        self.first_view_line.set(line);
    }

    fn mod_first_view_line(&self, diff: LineDiff) -> Line {
        let line = self.first_view_line.get() + diff;
        self.set_first_view_line(line);
        line
    }

    /// Index of the first line of this buffer view.
    pub fn first_view_line(&self) -> Line {
        self.first_view_line.get()
    }

    /// Index of the last line of this buffer view.
    pub fn last_view_line(&self) -> Line {
        let last_view_line = ViewLine(self.first_view_line().value + self.view_line_count() - 1);
        Line::from_in_context_snapped(self, last_view_line)
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

    /// Return all lines of this buffer view.
    pub fn view_lines_content(&self) -> Vec<String> {
        self.lines_vec(self.view_byte_range())
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
    BufferRangeUBytes(Range<Byte>),
    BufferRangeLocationColumn(Range<Location>),
    RangeBytes(std::ops::Range<Byte>),
    RangeFull(RangeFull),
}

impl RangeLike {
    /// Expand the [`RangeLike`] to vector of text ranges.
    pub fn expand(&self, buffer: &BufferModel) -> Vec<Range<Byte>> {
        match self {
            RangeLike::Selections => {
                let byte_selections = buffer.byte_selections().into_iter();
                byte_selections
                    .map(|t| {
                        let start = std::cmp::min(t.start, t.end);
                        let end = std::cmp::max(t.start, t.end);
                        Range::new(start, end)
                    })
                    .collect()
            }
            RangeLike::BufferRangeUBytes(range) => vec![*range],
            RangeLike::RangeBytes(range) => vec![range.into()],
            RangeLike::RangeFull(_) => vec![buffer.full_range()],
            RangeLike::BufferRangeLocationColumn(range) => {
                let start = Byte::from_in_context_snapped(buffer, range.start);
                let end = Byte::from_in_context_snapped(buffer, range.end);
                vec![Range::new(start, end)]
            }
        }
    }
}

impl From<&Range<Byte>> for RangeLike {
    fn from(range: &Range<Byte>) -> Self {
        RangeLike::BufferRangeUBytes(*range)
    }
}

impl From<&Range<Location>> for RangeLike {
    fn from(range: &Range<Location>) -> Self {
        RangeLike::BufferRangeLocationColumn(*range)
    }
}

impl From<&std::ops::Range<Byte>> for RangeLike {
    fn from(range: &std::ops::Range<Byte>) -> Self {
        RangeLike::RangeBytes(range.clone())
    }
}

impl From<&RangeFull> for RangeLike {
    fn from(range: &RangeFull) -> Self {
        RangeLike::RangeFull(*range)
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
    LocationUBytesLine(Location<Byte, Line>),
    LocationColumnViewLine(Location<Column, ViewLine>),
    LocationUBytesViewLine(Location<Byte, ViewLine>),
    Byte(Byte),
}

impl Default for LocationLike {
    fn default() -> Self {
        LocationLike::LocationColumnLine(default())
    }
}

impl LocationLike {
    /// Expand the [`LocationLike`] structure to a known location.
    pub fn expand(self, buffer: &BufferModel) -> Location<Column, Line> {
        match self {
            LocationLike::LocationColumnLine(loc) => loc,
            LocationLike::LocationUBytesLine(loc) => Location::from_in_context_snapped(buffer, loc),
            LocationLike::LocationColumnViewLine(loc) =>
                Location::from_in_context_snapped(buffer, loc),
            LocationLike::LocationUBytesViewLine(loc) =>
                Location::from_in_context_snapped(buffer, loc),
            LocationLike::Byte(byte) => Location::from_in_context_snapped(buffer, byte),
        }
    }
}


// ===================
// === Conversions ===
// ===================

/// Perform conversion between two values. It is just like the [`From`] trait, but it performs the
/// conversion in a "context". The "context" is an object containing additional information required
/// to perform the conversion. For example, the context can be a text buffer, which is needed to
/// convert byte offset to line-column location.
#[allow(missing_docs)]
pub trait FromInContext<Ctx, T> {
    fn from_in_context(context: Ctx, arg: T) -> Self;
}

/// Perform conversion between two values. It is just like [`FromInContext`], but the result value
/// is snapped to the closest valid location. For example, when performing conversion between
/// [`Line`] and [`ViewLine`], if the line is not visible, the closest visible line will be
/// returned.
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


// === Conversion Redirection ===

/// Redirects the conversion to [`Rope`] counterpart of conversion function. This trait introduces
/// a new metric, the [`ViewLine`] and extends [`Rope`] conversions with it.
macro_rules! redirect_conversion_to_rope {
    ([$($ctx:tt)*] [$($from:tt)*] [$($to:tt)*]) => {
        impl<$($ctx)*> FromInContextSnapped<&BufferModel, $($from)*> for $($to)*
            where for<'t> $($to)*: enso_text::FromInContextSnapped<&'t Rope, $($from)*>
        {
            fn from_in_context_snapped(buffer: &BufferModel, value: $($from)*) -> Self {
                <$($to)* as enso_text::FromInContextSnapped<&Rope, $($from)*>>::
                    from_in_context_snapped(&*buffer.rope.text.borrow(),value)
            }
        }
    };
}



// === Conversions to Line ===

redirect_conversion_to_rope!([T][Location<T, Line>][Line]);
redirect_conversion_to_rope!([][Byte][Line]);

/// Conversion error between [`ViewLine`] and [`Line`].
#[allow(missing_docs)]
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

impl<T> FromInContextSnapped<&BufferModel, Location<T, ViewLine>> for Line {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<T, ViewLine>) -> Self {
        Line::from_in_context_snapped(buffer, location.line)
    }
}


// === Conversions to ViewLine ===

/// Conversion error between [`Line`] and [`ViewLine`].
#[allow(missing_docs)]
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


// === Conversions to Byte ===

impl FromInContextSnapped<&BufferModel, Location<Byte, Line>> for Byte {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Byte, Line>) -> Self {
        buffer.line_offset(location.line).unwrap() + location.offset
    }
}

impl FromInContextSnapped<&BufferModel, Location<Byte, ViewLine>> for Byte {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Byte, ViewLine>) -> Self {
        let location = Location::<Byte, Line>::from_in_context_snapped(buffer, location);
        Byte::from_in_context_snapped(buffer, location)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, Line>> for Byte {
    fn from_in_context_snapped(context: &BufferModel, location: Location) -> Self {
        let location = Location::<Byte, Line>::from_in_context_snapped(context, location);
        Byte::from_in_context_snapped(context, location)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, ViewLine>> for Byte {
    fn from_in_context_snapped(
        context: &BufferModel,
        location: Location<Column, ViewLine>,
    ) -> Self {
        let location = Location::<Byte, Line>::from_in_context_snapped(context, location);
        Byte::from_in_context_snapped(context, location)
    }
}


// === Conversions to Location<Column, Line> ===

redirect_conversion_to_rope!([][Location<Byte, Line>][Location<Column, Line>]);
redirect_conversion_to_rope!([][Byte][Location<Column, Line>]);

impl FromInContextSnapped<&BufferModel, Location<Column, ViewLine>> for Location<Column, Line> {
    fn from_in_context_snapped(
        context: &BufferModel,
        location: Location<Column, ViewLine>,
    ) -> Self {
        let line = Line::from_in_context_snapped(context, location.line);
        Location(line, location.offset)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Byte, ViewLine>> for Location<Column, Line> {
    fn from_in_context_snapped(context: &BufferModel, location: Location<Byte, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(context, location.line);
        Location::from_in_context_snapped(context, Location(line, location.offset))
    }
}


// === Conversions to Location<Byte, ViewLine> ===

impl FromInContextSnapped<&BufferModel, Location<Byte, Line>> for Location<Byte, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Byte, Line>) -> Self {
        let line = ViewLine::from_in_context_snapped(buffer, location.line);
        location.with_line(line)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, Line>> for Location<Byte, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, Line>) -> Self {
        let location = Location::<Byte, Line>::from_in_context_snapped(buffer, location);
        Location::<Byte, ViewLine>::from_in_context_snapped(buffer, location)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, ViewLine>> for Location<Byte, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(buffer, location.line);
        Location::<Byte, ViewLine>::from_in_context_snapped(buffer, location.with_line(line))
    }
}

impl FromInContextSnapped<&BufferModel, Byte> for Location<Byte, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, offset: Byte) -> Self {
        let location = Location::<Byte, Line>::from_in_context_snapped(buffer, offset);
        Location::<Byte, ViewLine>::from_in_context_snapped(buffer, location)
    }
}


// === Conversions to Location<Column, ViewLine> ===

impl FromInContextSnapped<&BufferModel, Location<Column, Line>> for Location<Column, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, Line>) -> Self {
        let line = ViewLine::from_in_context_snapped(buffer, location.line);
        location.with_line(line)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Byte, Line>> for Location<Column, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Byte, Line>) -> Self {
        let location = Location::<Column, Line>::from_in_context_snapped(buffer, location);
        Location::<Column, ViewLine>::from_in_context_snapped(buffer, location)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Byte, ViewLine>> for Location<Column, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Byte, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(buffer, location.line);
        Location::<Column, ViewLine>::from_in_context_snapped(buffer, location.with_line(line))
    }
}

impl FromInContextSnapped<&BufferModel, Byte> for Location<Column, ViewLine> {
    fn from_in_context_snapped(buffer: &BufferModel, offset: Byte) -> Self {
        let location = Location::<Column, Line>::from_in_context_snapped(buffer, offset);
        Location::<Column, ViewLine>::from_in_context_snapped(buffer, location)
    }
}


// === Conversions to Location<Byte, Line> ===

redirect_conversion_to_rope!([][Location<Column, Line>][Location<Byte, Line>]);
redirect_conversion_to_rope!([][Byte][Location<Byte, Line>]);

impl FromInContextSnapped<&BufferModel, Location<Byte, ViewLine>> for Location<Byte, Line> {
    fn from_in_context_snapped(buffer: &BufferModel, offset: Location<Byte, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(buffer, offset.line);
        Location(line, offset.offset)
    }
}

impl FromInContextSnapped<&BufferModel, Location<Column, ViewLine>> for Location<Byte, Line> {
    fn from_in_context_snapped(buffer: &BufferModel, location: Location<Column, ViewLine>) -> Self {
        let line = Line::from_in_context_snapped(buffer, location.line);
        Location::from_in_context_snapped(buffer, location.with_line(line))
    }
}


// === Conversions of Range ====

impl<'t, S, T> FromInContextSnapped<&'t BufferModel, Range<S>> for Range<T>
where T: FromInContextSnapped<&'t BufferModel, S>
{
    fn from_in_context_snapped(context: &'t BufferModel, range: Range<S>) -> Self {
        let start = T::from_in_context_snapped(context, range.start);
        let end = T::from_in_context_snapped(context, range.end);
        Range::new(start, end)
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
