#![allow(missing_docs)]

use crate::prelude::*;

pub mod movement;
pub mod selection;

pub use movement::*;
pub use selection::Selection;


use crate::buffer::data;
use crate::buffer::data::Data;
use crate::buffer::data::unit::*;
use crate::buffer::Buffer;

use enso_frp as frp;


// ==================
// === Frp Macros ===
// ==================

// FIXME: these are generic FRP utilities. To be refactored out after the API settles down.
// FIXME: They are already copy-pasted in the EnsoGL code. To be unified and refactored.
macro_rules! define_frp {
    (
        Input  { $($in_field  : ident : $in_field_type  : ty),* $(,)? }
        Output { $($out_field : ident : $out_field_type : ty),* $(,)? }
    ) => {
        #[derive(Debug,Clone,CloneRef)]
        pub struct Frp {
            pub network : frp::Network,
            pub input   : FrpInputs,
            pub output  : FrpOutputs,
        }

        impl Frp {
            pub fn new(network:frp::Network, input:FrpInputs, output:FrpOutputs) -> Self {
                Self {network,input,output}
            }
        }

        #[derive(Debug,Clone,CloneRef)]
        pub struct FrpInputs {
            $(pub $in_field : frp::Source<$in_field_type>),*
        }

        impl FrpInputs {
            pub fn new(network:&frp::Network) -> Self {
                frp::extend! { network
                    $($in_field <- source();)*
                }
                Self { $($in_field),* }
            }
        }

        #[derive(Debug,Clone,CloneRef)]
        pub struct FrpOutputsSource {
            $($out_field : frp::Any<$out_field_type>),*
        }

        #[derive(Debug,Clone,CloneRef)]
        pub struct FrpOutputs {
            source : FrpOutputsSource,
            $(pub $out_field : frp::Stream<$out_field_type>),*
        }

        impl FrpOutputsSource {
            pub fn new(network:&frp::Network) -> Self {
                frp::extend! { network
                    $($out_field <- any(...);)*
                }
                Self {$($out_field),*}
            }
        }

        impl FrpOutputs {
            pub fn new(network:&frp::Network) -> Self {
                let source = FrpOutputsSource::new(network);
                $(let $out_field = source.$out_field.clone_ref().into();)*
                Self {source,$($out_field),*}
            }
        }
    };
}



// =================
// === Constants ===
// =================

/// When paging through a file, the number of lines from the previous page that will also be visible
/// in the next.
const SCROLL_OVERLAP : isize = 2;

/// Default visible line count in a new buffer view.
const DEFAULT_LINE_COUNT : usize = 10;



// ==================
// === ViewBuffer ===
// ==================

/// Specialized form of `Buffer` with view-related information, such as selection. This form of
/// buffer is mainly used by `View`, but can also be combined with other `ViewBuffer`s to display
/// cursors, selections, and edits of several users at the same time.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct ViewBuffer {
    pub buffer    : Buffer,
    pub selection : Rc<RefCell<selection::Group>>,
}

impl Deref for ViewBuffer {
    type Target = Buffer;
    fn deref(&self) -> &Self::Target {
        &self.buffer
    }
}

impl From<Buffer> for ViewBuffer {
    fn from(buffer:Buffer) -> Self {
        let selection = default();
        Self {buffer,selection}
    }
}

impl From<&Buffer> for ViewBuffer {
    fn from(buffer:&Buffer) -> Self {
        buffer.clone_ref().into()
    }
}

impl Default for ViewBuffer {
    fn default() -> Self {
        Buffer::default().into()
    }
}

// FIXME: Make all these utils private, and use FRP to control the model instead.
impl ViewBuffer {
    /// Add a new selection to the current view.
    pub fn add_selection(&self, selection:impl Into<Selection>) {
        self.selection.borrow_mut().add(selection.into())
    }

    /// Add a new cursor for the given byte offset.
    pub fn add_cursor(&self, offset:Bytes) {
        self.add_selection(Selection::new_cursor(offset))
    }

    /// Insert new text in the place of current selections / cursors.
    pub fn insert(&self, text:impl Into<Data>) {
        let text = text.into();
        for selection in &*self.selection.borrow() {
            self.buffer.data.borrow_mut().insert(selection.range(),&text);
        }
    }

    /// Perform undo operation.
    pub fn undo(&self) {
        self.buffer.data.borrow_mut().undo();
    }

    /// Perform redo operation.
    pub fn redo(&self) {
        self.buffer.data.borrow_mut().redo();
    }
}



// ===========
// === FRP ===
// ===========

define_frp! {
    Input {
        move_carets      : Option<Movement>,
        modify_selection : Option<Movement>,
        set_cursor       : Location,
        clear_selection  : (),
    }

    Output {
        selection : selection::Group,
    }
}



// ============
// === View ===
// ============

/// View for a region of a buffer. There are several cases where multiple views share the same
/// buffer, including displaying the buffer in separate tabs or displaying multiple users in the
/// same file (keeping a view per user and merging them visually).
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct View {
    model   : ViewModel,
    pub frp : Frp,
}

impl Deref for View {
    type Target = ViewModel;
    fn deref(&self) -> &Self::Target {
        &self.model
    }
}

impl View {
    /// Constructor.
    pub fn new(view_buffer:impl Into<ViewBuffer>) -> Self {
        let network = frp::Network::new();
        let model   = ViewModel::new(&network,view_buffer);
        let input  = model.frp.clone_ref();
        let output = FrpOutputs::new(&network);

        frp::extend! { network

            selection_on_move  <- input.move_carets.map(f!((t) model.moved_selection2(*t,false)));
            selection_on_mod   <- input.modify_selection.map(f!((t) model.moved_selection2(*t,true)));
            selection_on_clear <- input.clear_selection.constant(default());

            selection_on_set_cursor <- input.set_cursor.map(f!([model](t) Selection::new_cursor(model.offset_of_view_location(t)).into()));

            output.source.selection <+ selection_on_move;
            output.source.selection <+ selection_on_mod;
            output.source.selection <+ selection_on_clear;
            output.source.selection <+ selection_on_set_cursor;

            eval output.source.selection ((t) model.set_selection(t));
        }
        let frp = Frp::new(network,input,output);
        Self {frp,model}
    }
}

impl Default for View {
    fn default() -> Self {
        Self::new(ViewBuffer::default())
    }
}



// =================
// === ViewModel ===
// =================

/// Internal model for the `View`.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct ViewModel {
    pub frp           : FrpInputs,
    pub view_buffer   : ViewBuffer,
    first_line_number : Rc<Cell<Line>>,
    line_count        : Rc<Cell<usize>>,
}

impl Deref for ViewModel {
    type Target = ViewBuffer;
    fn deref(&self) -> &Self::Target {
        &self.view_buffer
    }
}

impl ViewModel {
    /// Constructor.
    pub fn new(network:&frp::Network, view_buffer:impl Into<ViewBuffer>) -> Self {
        let frp               = FrpInputs::new(network);
        let view_buffer       = view_buffer.into();
        let first_line_number = default();
        let line_count        = Rc::new(Cell::new(DEFAULT_LINE_COUNT));
        Self {frp,view_buffer,first_line_number,line_count}
    }
}

impl ViewModel {
    /// Set the selection to a new value.
    pub fn set_selection(&self, selection:&selection::Group) {
        *self.selection.borrow_mut() = selection.clone();
    }

    /// Return all active selections.
    pub fn selections(&self) -> selection::Group {
        self.selection.borrow().clone()
    }

    // FIXME: rename
    fn moved_selection2(&self, movement:Option<Movement>, modify:bool) -> selection::Group {
        movement.map(|t| self.moved_selection(t,modify)).unwrap_or_default()
    }

    /// Computes the actual desired amount of scrolling (generally slightly less than the height of
    /// the viewport, to allow overlap).
    fn page_scroll_height(&self) -> isize {
        std::cmp::max(self.line_count.get() as isize - SCROLL_OVERLAP, 1)
    }

    pub fn first_line_number(&self) -> Line {
        self.first_line_number.get()
    }

    pub fn last_line_number(&self) -> Line {
        self.first_line_number() + self.line_count().line()
    }

    pub fn line_count(&self) -> usize {
        self.line_count.get()
    }

    pub fn line_range(&self) -> Range<Line> {
        self.first_line_number() .. self.last_line_number()
    }

    pub fn first_line_offset(&self) -> Bytes {
        self.offset_of_line(self.first_line_number())
    }

    pub fn last_line_offset(&self) -> Bytes {
        self.offset_of_line(self.last_line_number())
    }

    pub fn line_offset_range(&self) -> Range<Bytes> {
        self.first_line_offset() .. self.last_line_offset()
    }

    pub fn offset_of_view_line(&self, view_line:Line) -> Bytes {
        let line = self.first_line_number() + view_line;
        self.offset_of_line(line)
    }

    pub fn offset_of_view_location(&self, location:impl Into<Location>) -> Bytes {
        let location = location.into();
        self.offset_of_view_line(location.line) + location.column.value.bytes()
    }

    pub fn line_byte_size(&self, line:Line) -> Bytes {
        let start = self.offset_of_view_line(line);
        let end   = self.offset_of_view_line(line + 1.line());
        end - start
    }

    // FIXME: this sohuld not include line break.
    pub fn range_of_view_line_raw(&self, view_line:Line) -> Range<Bytes> {
        let start = self.offset_of_view_line(view_line);
        let end   = self.offset_of_view_line(view_line + 1.line());
        start .. end
    }

//    pub fn lines(&self) -> buffer::Lines {
//        let range = self.line_offset_range();
//        self.buffer.data.borrow().data.rope.lines(range.start.raw .. range.end.raw)
//    }

    // FIXME: this is inefficient now
    pub fn lines(&self) -> Vec<String> {
        let range = self.line_offset_range();
        self.buffer.data.borrow().data.rope.lines(range.start.value .. range.end.value).map(|t| t.into()).collect_vec()
    }

//    pub fn get(&self, line:Line) -> String {
//        let last_line_number = self.line_of_offset(self.data().len());
//        let start   = self.offset_of_line(line);
//        let end     = self.offset_of_line(line+1);
//        let end     = self.buffer.text.prev_grapheme_offset(end).unwrap_or(end);
//        let content = self.buffer.text.rope.subseq(start.raw .. end.raw);
//        println!("buffer line count: {}", last_line_number.raw);
//        content.into()
//    }

//    fn scroll_to_cursor(&mut self, text: &Text) {
//        let end = self.sel_regions().last().unwrap().end;
//        let line = self.line_of_offset(text, end);
//        if line < self.first_line_number {
//            self.first_line_number = line;
//        } else if self.first_line_number + self.height <= line {
//            self.first_line_number = line - (self.height - 1);
//        }
//        // We somewhat arbitrarily choose the last region for setting the old-style
//        // selection state, and for scrolling it into view if needed. This choice can
//        // likely be improved.
//        self.scroll_to = Some(end);
//    }
}

impl LineOffset for ViewModel {
    fn data(&self) -> Data {
        self.buffer.data.borrow().data.clone() // FIXME
    }

    fn offset_of_line(&self,line:Line) -> Bytes {
        let line = std::cmp::min(line.value,self.data().measure::<data::metric::Lines>() + 1);
        Bytes(self.data().offset_of_line(line))
    }

    fn line_of_offset(&self,offset:Bytes) -> Line {
        Line(self.data().line_of_offset(offset.value))
    }
}



// ==================
// === LineOffset ===
// ==================

/// A trait from which lines and columns in a document can be calculated
/// into offsets inside a text an vice versa.
pub trait LineOffset {
    // use own breaks if present, or text if not (no line wrapping)

    fn data(&self) -> Data;

    /// Returns the byte offset corresponding to the given line.
    fn offset_of_line(&self, line:Line) -> Bytes {
        Bytes(self.data().offset_of_line(line.value))
    }

    /// Returns the visible line number containing the given offset.
    fn line_of_offset(&self, offset:Bytes) -> Line {
        Line(self.data().line_of_offset(offset.value))
    }

    // How should we count "column"? Valid choices include:
    // * Unicode codepoints
    // * grapheme clusters
    // * Unicode width (so CJK counts as 2)
    // * Actual measurement in text layout
    // * Code units in some encoding
    //
    // Of course, all these are identical for ASCII. For now we use UTF-8 code units
    // for simplicity.

    fn offset_to_line_col(&self, offset:Bytes) -> Location {
        let line = self.line_of_offset(offset);
        let col  = (offset - self.offset_of_line(line)).column();
        Location(line,col)
    }

    fn line_col_to_offset(&self, line:Line, col:Column) -> Bytes {
        let mut offset = self.offset_of_line(line).saturating_add(col.value.bytes()); // fixme: raw.bytes seems wrong
        let len = self.data().len();
        if offset >= len {
            offset = len;
            if self.line_of_offset(offset) <= line {
                return offset;
            }
        } else {
            // Snap to grapheme cluster boundary
            offset = self.data().prev_grapheme_offset(offset + 1.bytes()).unwrap_or_default();
        }

        // clamp to end of line
        let next_line_offset = self.offset_of_line(line + 1.line());
        if offset >= next_line_offset {
            if let Some(prev) = self.data().prev_grapheme_offset(next_line_offset) {
                offset = prev;
            }
        }
        offset
    }

//    /// Get the line range of a selected region.
//    fn get_line_range(&self, text: &Text, region: &Selection) -> std::ops::Range<usize> {
//        let (first_line_number, _) = self.offset_to_line_col(text, region.min());
//        let (mut last_line, last_col) = self.offset_to_line_col(text, region.max());
//        if last_col == 0 && last_line > first_line_number {
//            last_line -= 1;
//        }
//
//        first_line_number..(last_line + 1)
//    }
}
