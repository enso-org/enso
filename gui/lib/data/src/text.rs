//! The common structures for text location and manipulation.

use enso_prelude::*;

use std::ops::Add;
use std::ops::AddAssign;
use std::ops::Range;
use std::ops::Sub;
use std::ops::SubAssign;
use serde::Serialize;
use serde::Deserialize;



/// ======================================
/// === Text Coordinates And Distances ===
/// ======================================

// === Index ===

/// Strongly typed index into container.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct Index { pub value:usize }

impl Index {
    /// Initializes Index with given value.
    pub fn new(value:usize) -> Self {
        Index {value}
    }
}


// === Size ===

/// Strongly typed size of container.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct Size { pub value:usize }

impl Size {
    /// Initializes Size with given value.
    pub fn new(value:usize) -> Self {
        Size {value}
    }
}

impl Add for Size {
    type Output = Size;
    fn add(self, rhs:Size) -> Size {
        Size {value:self.value + rhs.value}
    }
}

impl AddAssign for Size {
    fn add_assign(&mut self, rhs: Size) {
        *self = *self + rhs;
    }
}

impl Sub for Size {
    type Output = Size;
    fn sub(self, rhs:Size) -> Size {
        Size{value: self.value - rhs.value}
    }
}

impl SubAssign for Size {
    fn sub_assign(&mut self, rhs: Size) {
        *self = *self - rhs;
    }
}

// === Span ===

/// Strongly typed span into container with index and size.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct Span { pub index:Index, pub size:Size }

impl Span {
    /// Initializes Span with given values.
    pub fn new(index:Index, size:Size) -> Self {
        Span {index,size}
    }

    /// Get the character after last character of this span.
    ///
    /// If span has size 0, it returns the `index` field.
    pub fn end(&self) -> Index {
        self.index + self.size
    }

    /// Check if this span contains character under `index`.
    pub fn contains(&self, index:Index) -> bool {
        self.index <= index && self.end() > index
    }

    /// Check if this span contains the whole another span.
    pub fn contains_span(&self, span:&Span) -> bool {
        self.index <= span.index && self.end() >= span.end()
    }
}


// === Operators for Index and Size ===

impl Add<Size> for Index {
    type Output = Index;
    fn add(self, rhs:Size) -> Index {
        Index {value:self.value + rhs.value}
    }
}

impl AddAssign<Size> for Index {
    fn add_assign(&mut self, rhs: Size) {
        *self = *self + rhs;
    }
}

impl Sub<Size> for Index {
    type Output = Index;
    fn sub(self, rhs:Size) -> Index {
        Index {value:self.value - rhs.value}
    }
}

impl SubAssign<Size> for Index {
    fn sub_assign(&mut self, rhs: Size) {
        *self = *self - rhs;
    }
}

impl Sub for Index {
    type Output = Size;
    fn sub(self, rhs:Index) -> Size {
        Size {value:self.value - rhs.value}
    }
}

// === TextLocation ===

/// A position of character in a multiline text.
#[derive(Copy,Clone,Debug,PartialEq,Eq,PartialOrd,Ord)]
pub struct TextLocation {
    /// Line index.
    pub line: usize,
    /// Column is a index of char in given line.
    pub column: usize,
}

impl TextLocation {
    /// Create location at begin of given line.
    pub fn at_line_begin(line_index:usize) -> TextLocation {
        TextLocation {
            line   : line_index,
            column : 0,
        }
    }

    /// Create location at begin of the whole document.
    pub fn at_document_begin() -> TextLocation {
        TextLocation {
            line   : 0,
            column : 0,
        }
    }
}



// ==============
// === Change ===
// ==============

/// A change type
#[derive(Copy,Clone,Debug)]
pub enum ChangeType {
    /// A change where we replace fragment of one line with text without new lines.
    SingleLine,
    /// A multi-line change is a change which is not a single line change (see docs for SingleLine).
    MultiLine
}

/// A structure describing a text operation in one place.
#[derive(Clone,Debug)]
pub struct TextChange {
    /// Text fragment to be replaced. If we don't mean to remove any text, this should be an empty
    /// range with start set at position there `lines` will be inserted (see `TextChange::insert`
    /// definition).
    pub replaced : Range<TextLocation>,
    /// Lines to insert instead of replaced fragment.
    pub lines : Vec<Vec<char>>,
}

impl TextChange {
    /// Creates operation which inserts text at given position.
    pub fn insert(at:TextLocation, text:&str) -> Self {
        TextChange {
            replaced : at..at,
            lines    : Self::mk_lines_as_char_vector(text)
        }
    }

    /// Creates operation which deletes text at given range.
    pub fn delete(range:Range<TextLocation>) -> Self {
        TextChange {
            replaced : range,
            lines    : vec![vec![]],
        }
    }

    /// Creates operation which replaces text at given range with given string.
    pub fn replace(replaced:Range<TextLocation>, text:&str) -> Self {
        TextChange {replaced,
            lines : Self::mk_lines_as_char_vector(text)
        }
    }

    /// A type of this change. See `ChangeType` doc for details.
    pub fn change_type(&self) -> ChangeType {
        if self.lines.is_empty() {
            panic!("Invalid change");
        }
        let is_one_line_modified = self.replaced.start.line == self.replaced.end.line;
        let is_one_line_inserted = self.lines.len() == 1;
        if is_one_line_modified && is_one_line_inserted {
            ChangeType::SingleLine
        } else {
            ChangeType::MultiLine
        }
    }

    /// Converts change representation to String.
    pub fn inserted_string(&self) -> String {
        self.lines.iter().map(|line| line.iter().collect::<String>()).join("\n")
    }

    /// Returns text location range where the inserted text will appear after making this change.
    pub fn inserted_text_range(&self) -> Range<TextLocation> {
        let start         = self.replaced.start;
        let end_line      = start.line + self.lines.len().saturating_sub(1);
        let last_line_len = self.lines.last().map_or(0, |l| l.len());
        let end_column = if start.line == end_line {
            start.column + last_line_len
        } else {
            last_line_len
        };
        start..TextLocation{line:end_line, column:end_column}
    }

    fn mk_lines_as_char_vector(text:&str) -> Vec<Vec<char>> {
        split_to_lines(text).map(|s| s.chars().collect_vec()).collect()
    }
}



// ===========================
// === Change Notification ===
// ===========================

/// A notification about text change.
///
/// In essence, it's `TextChange` with some additional useful information.
#[derive(Clone,Debug,Shrinkwrap)]
pub struct TextChangedNotification {
    /// A change which has occurred.
    #[shrinkwrap(main_field)]
    pub change : TextChange,
    /// The replaced range as char positions from document begin, instead of row:column pairs.
    pub replaced_chars : Range<usize>,
}


// =================
// === Utilities ===
// =================

/// Split text to lines handling both CR and CRLF line endings.
pub fn split_to_lines(text:&str) -> impl Iterator<Item=String> + '_ {
    text.split('\n').map(cut_cr_at_end_of_line).map(|s| s.to_string())
}

/// Returns slice without carriage return (also known as CR or `'\r'`) at line's end
fn cut_cr_at_end_of_line(from:&str) -> &str {
    if from.ends_with('\r') {
        &from[..from.len()-1]
    } else {
        from
    }
}