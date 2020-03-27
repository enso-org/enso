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

    /// Creates a span describing a range between two indices.
    pub fn from_indices(begin:Index, end:Index) -> Self {
        if end < begin {
            Self::from_indices(end,begin)
        } else {
            let index = begin;
            let size  = end - begin;
            Span {index,size}
        }
    }

    /// Creates a span from zero index with given length.
    pub fn from_beginning(size:Size) -> Self {
        Span {index:Index::new(0), size}
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

/// A template for structure describing a text operation in one place.
///
/// This is a generalized template, because we use different representation for both index
/// (e.g. `Index` or `TextLocation`) and inserted content (it may be just String, but also e.g.
/// Vec<char>, or Vec<Vec<char>> split by newlines).
#[derive(Clone,Debug)]
pub struct TextChangeTemplate<Index,Content> {
    /// Text fragment to be replaced. If we don't mean to remove any text, this should be an empty
    /// range with start set at position there `lines` will be inserted
    /// (see `TextChangeTemplate::insert` definition).
    pub replaced : Range<Index>,
    /// Text which replaces fragment described in `replaced` field.
    pub inserted: Content,
}

/// The simplest change representation.
pub type TextChange = TextChangeTemplate<Index,String>;


// === Constructors ===

impl<Index:Copy,Content> TextChangeTemplate<Index,Content> {
    /// Creates operation which inserts text at given position.
    pub fn insert(at:Index, text:Content) -> Self {
        TextChangeTemplate {
            replaced : at..at,
            inserted: text,
        }
    }
}

impl<Index,Content> TextChangeTemplate<Index,Content> {
    /// Creates operation which replaces text at given range with given string.
    pub fn replace(replaced:Range<Index>, text:Content) -> Self {
        let inserted = text;
        TextChangeTemplate {replaced,inserted}
    }
}

impl<Index,Content:Default> TextChangeTemplate<Index,Content> {
    /// Creates operation which deletes text at given range.
    pub fn delete(range:Range<Index>) -> Self {
        TextChangeTemplate {
            replaced : range,
            inserted : default(),
        }
    }
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