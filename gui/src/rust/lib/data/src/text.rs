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
#[derive(Clone,Copy,Debug,Default,Hash,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct Index { pub value:usize }

impl Index {
    /// Initializes Index with given value.
    pub fn new(value:usize) -> Self {
        Index {value}
    }

    /// Create char index from the byte index. It must traverse the content to count chars.
    pub fn convert_byte_index(content:impl Str, index:ByteIndex) -> Self {
        let slice = &content.as_ref()[..index.value];
        Self::new(slice.chars().count())
    }
}


// === ByteIndex ===

/// Strongly typed index of byte in String (which may differ with analogous character index,
/// because some chars takes more than one byte).
//TODO[ao] We should use structures from ensogl::,math::topology to represent different quantities
// and units.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,Hash,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct ByteIndex { pub value:usize }

impl ByteIndex {
    /// Initializes Index with given value.
    pub fn new(value:usize) -> Self {
        ByteIndex {value}
    }
}


// === Size ===

/// Strongly typed size of container.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,Hash,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
pub struct Size { pub value:usize }

impl Size {
    /// Initializes Size with given value.
    pub fn new(value:usize) -> Self {
        Size {value}
    }

    /// Checks if this is a non-empty size (more than zero elements).
    pub fn non_empty(self) -> bool {
        self.value > 0
    }

    /// Checks if this is an empty size (zero elements).
    pub fn is_empty(self) -> bool {
        self.value == 0
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

impl Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{}",self.value)
    }
}

impl From<&str> for Size {
    fn from(text:&str) -> Self {
        Size::new(text.len())
    }
}


// === Span ===

/// Strongly typed span into container with index and size.
#[allow(missing_docs)]
#[derive(Clone,Copy,Debug,Default,Hash,PartialEq,Eq,PartialOrd,Ord,Serialize,Deserialize)]
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

    /// Converts span to `Range<usize>`.
    pub fn range(self) -> Range<usize> {
        let start = self.index.value;
        let end   = self.end().value;
        start .. end
    }

    /// Expand the span by moving its left (start) index.
    pub fn extend_left(&mut self, size:Size) {
        self.index -= size;
        self.size += size;
    }

    /// Expand the span by moving its right (end) index.
    pub fn extend_right(&mut self, size:Size) {
        self.size += size;
    }

    /// Shrink the span by moving its left (start) index.
    pub fn shrink_left(&mut self, size:Size) {
        self.index += size;
        self.size -= size;
    }

    /// Shrink the span by moving its right (end) index.
    pub fn shrink_right(&mut self, size:Size) {
        self.size -= size;
    }

    /// Move the whole span left, maintaining its size.
    pub fn move_left(&mut self, size:Size) {
        self.index -= size;
    }

    /// Move the whole span right, maintaining its size.
    pub fn move_right(&mut self, size:Size) {
        self.index += size;
    }

    /// Move the start index of the span, adjusting the size.
    pub fn set_left(&mut self, new_left:Index) {
        let end = self.end();
        self.index = new_left;
        self.size = end - new_left;
    }

    /// Move the end index of the span, adjusting the size.
    pub fn set_right(&mut self, new_right:Index) {
        self.size = new_right - self.index;
    }
}

impls! { From + &From <Range<usize>> for Span { |range|
    Span::from_indices(Index::new(range.start), Index::new(range.end))
}}

impls! { Into + &Into <Range<usize>> for Span { |this|
    this.range()
}}

impl PartialEq<Range<usize>> for Span {
    fn eq(&self, other:&Range<usize>) -> bool {
        &self.range() == other
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{}..{}",self.index.value,self.end().value)
    }
}

impl std::ops::Index<Span> for str {
    type Output = str;

    fn index(&self, index:Span) -> &Self::Output {
        &self[index.range()]
    }
}

impl std::ops::Index<Span> for String {
    type Output = str;

    fn index(&self, index:Span) -> &Self::Output {
        &self.as_str()[index]
    }
}

impl From<Range<Index>> for Span {
    fn from(range:Range<Index>) -> Self {
        Span::from_indices(range.start,range.end)
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

/// Short pretty print representation in the form of `line:column`.
impl Display for TextLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{}:{}",self.line,self.column)
    }
}

impl TextLocation {
    /// Create location at begin of given line.
    pub fn at_line_begin(line_index:usize) -> Self {
        TextLocation {
            line   : line_index,
            column : 0,
        }
    }

    /// Create location at begin of the whole document.
    pub fn at_document_begin() -> Self {
        TextLocation {
            line   : 0,
            column : 0,
        }
    }

    /// Create location at and of the whole document. It iterates over all the content.
    pub fn at_document_end(content:impl Str) -> Self {
        Self::after_chars(content.as_ref().chars())
    }

    /// Convert from index of document with `content`. It iterates over all characters before
    /// `index`.
    pub fn from_index(content:impl Str, index:Index) -> Self {
        let before = content.as_ref().chars().take(index.value);
        Self::after_chars(before)
    }

    /// Converts a range of indices into a range of TextLocation. It iterates over all characters
    /// before range's end.
    pub fn convert_range(content:impl Str, range:&Range<Index>) -> Range<Self> {
        let content = content.as_ref();
        Self::from_index(content,range.start)..Self::from_index(content,range.end)
    }

    /// Converts a range in bytes into a range of TextLocation. It iterates over all characters
    /// before range's end.
    pub fn convert_byte_range(content:impl Str, range:&Range<ByteIndex>) -> Range<Self> {
        let start = Index::convert_byte_index(content.as_ref(), range.start);
        let end   = Index::convert_byte_index(content.as_ref(), range.end);
        Self::convert_range(content,&(start..end))
    }

    fn after_chars<IntoCharsIter>(chars:IntoCharsIter) -> Self
    where IntoCharsIter : IntoIterator<Item=char, IntoIter:Clone> {
        let iter             = chars.into_iter();
        let len              = iter.clone().count();
        let newlines         = iter.enumerate().filter(|(_,c)| *c == '\n');
        let newlines_indices = newlines.map(|(i,_)| i);
        TextLocation {
            line   : newlines_indices.clone().count(),
            column : len - newlines_indices.last().map_or(0, |i| i + 1),
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
#[derive(Clone,Debug,Eq,Hash,PartialEq)]
pub struct TextChangeTemplate<Index,Content> {
    /// Text fragment to be replaced. If we don't mean to remove any text, this should be an empty
    /// range with start set at position there `lines` will be inserted
    /// (see `TextChangeTemplate::insert` definition).
    pub replaced: Range<Index>,
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

impl<Index:Sub+Clone,Content> TextChangeTemplate<Index,Content> {
    /// Calculate the size of the replaced text.
    pub fn replaced_size(&self) -> Index::Output {
        self.replaced.end.clone() - self.replaced.start.clone()
    }
}

impl<Content> TextChangeTemplate<Index,Content> {
    /// Calculate the size of the replaced text.
    pub fn replaced_span(&self) -> Span {
        let index = self.replaced.start;
        let size  = self.replaced_size();
        Span {index,size}
    }

    /// Applies the text edit on given `String` value.
    ///
    /// # Panics
    ///
    /// Panics if the replaced span is out of the string value bounds.
    pub fn apply(&self, target:&mut String) where Content:AsRef<str> {
        //debug!(logger, "change: {change:?}, my code: \n```\n{code}\n```");
        let replaced_indices  = self.replaced.start.value..self.replaced.end.value;
        //debug!(logger, "replacing range {replaced_indices:?} with {change.inserted}");
        target.replace_range(replaced_indices,self.inserted.as_ref());
    }

    /// Applies the text edit on string and returns the result.
    ///
    /// # Panics
    ///
    /// Panics if the replaced span is out of the string value bounds.
    pub fn applied(&self, target:&str) -> String where Content:AsRef<str> {
        let mut target = target.to_string();
        self.apply(&mut target);
        target
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



// ============
// === Text ===
// ============

#[cfg(test)]
mod test {
    use super::*;

    use super::Index;

    #[test]
    fn converting_index_to_location() {
        let str = "first\nsecond\nthird";
        assert_eq!(TextLocation::from_index(str,Index::new(0)),  TextLocation {line:0, column:0});
        assert_eq!(TextLocation::from_index(str,Index::new(5)),  TextLocation {line:0, column:5});
        assert_eq!(TextLocation::from_index(str,Index::new(6)),  TextLocation {line:1, column:0});
        assert_eq!(TextLocation::from_index(str,Index::new(9)),  TextLocation {line:1, column:3});
        assert_eq!(TextLocation::from_index(str,Index::new(12)), TextLocation {line:1, column:6});
        assert_eq!(TextLocation::from_index(str,Index::new(13)), TextLocation {line:2, column:0});
        assert_eq!(TextLocation::from_index(str,Index::new(18)), TextLocation {line:2, column:5});

        let str = "";
        assert_eq!(TextLocation {line:0, column:0}, TextLocation::from_index(str,Index::new(0)));

        let str= "\n";
        assert_eq!(TextLocation {line:0, column:0}, TextLocation::from_index(str,Index::new(0)));
        assert_eq!(TextLocation {line:1, column:0}, TextLocation::from_index(str,Index::new(1)));
    }

    #[test]
    fn text_location_at_end() {
        let str = "first\nsecond\nthird";
        assert_eq!(TextLocation::at_document_end(str) , TextLocation {line:2, column:5});
        assert_eq!(TextLocation::at_document_end("")  , TextLocation {line:0, column:0});
        assert_eq!(TextLocation::at_document_end("\n"), TextLocation {line:1, column:0});
    }
}
