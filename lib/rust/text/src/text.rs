//! The data hold by the text buffer. Under the hood it is implemented as an efficient string rope.

use crate::prelude::*;
use crate::unit::*;

use crate::prelude::fmt::Formatter;
use crate::range::Range;
use crate::range::RangeBounds;
use crate::rope;

use enso_types::min;



// ============
// === Rope ===
// ============

/// Efficient, immutable text container used by the text buffer. Implemented as a rope under the
/// hood. Use `RopeCell` if you are looking for an internally mutable version.
///
/// A [rope](https://en.wikipedia.org/wiki/Rope_(data_structure)) is a data structure for strings,
/// specialized for incremental editing operations. Most operations (such as insert, delete,
/// substring) are O(log n). This module provides an immutable (also known as
/// [persistent](https://en.wikipedia.org/wiki/Persistent_data_structure)) version of Ropes, and if
/// there are many copies of similar strings, the common parts are shared.
///
/// Internally, the implementation uses thread safe reference counting. Mutations are generally
/// copy-on-write, though in-place edits are supported as an optimization when only one reference
/// exists, making the implementation as efficient as a mutable version.
///
/// This type provides multiple `From` implementations for easy conversions from string-like types,
/// and vice-versa.
///
/// Please note that the underlying rope implementation comes from `xi-rope` crate which does not
/// use strong types for all units (like line number, column number, byte offset), so part of
/// responsibility of this struct is to wrap the underlying API with strong types introduced in this
/// library.
#[derive(Debug, Clone, Default, Deref)]
#[allow(missing_docs)]
pub struct Rope {
    pub rope: xi_rope::Rope,
}
impl_clone_ref_as_clone!(Rope);



// =========================
// === Generic Utilities ===
// =========================

// === Constructors and Info ===

impl Rope {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Check whether the text is empty.
    pub fn is_empty(&self) -> bool {
        self.rope.is_empty()
    }

    /// Return text narrowed to the given range.
    pub fn sub(&self, range: impl RangeBounds) -> Rope {
        let range = self.crop_byte_range(range);
        let rope = self.rope.subseq(range.into_rope_interval());
        Rope { rope }
    }

    /// The number of grapheme clusters in this text.
    pub fn grapheme_count(&self) -> usize {
        let mut offset = 0;
        let mut count = 0;
        while let Some(off) = self.rope.next_grapheme_offset(offset) {
            offset = off;
            count += 1;
        }
        count
    }

    /// Return the len of the text in bytes.
    pub fn byte_size(&self) -> Byte {
        Byte(self.rope.len())
    }

    /// Range of the text in bytes.
    pub fn byte_range(&self) -> Range<Byte> {
        (..self.byte_size()).into()
    }

    /// Constraint the provided byte range, so it will be contained of the range of this data. This
    /// ensures that the resulting byte range will be valid for operations on this data.
    pub fn crop_byte_range(&self, range: impl RangeBounds) -> Range<Byte> {
        range.with_upper_bound(self.byte_size())
    }

    /// Constraint the provided location, so it will be contained of the range of this data. This
    /// ensures that the resulting location will be valid for operations on this data.
    pub fn snap_location(&self, location: Location<Byte>) -> Location<Byte> {
        use BoundsError::*;
        match self.validate_line_index(location.line) {
            Err(TooSmall) => self.first_line_start_location(),
            Err(TooBig) => self.last_line_end_location(),
            Ok(line) => {
                let byte_offset =
                    min(location.offset, self.end_byte_offset_of_line_index(line).unwrap());
                Location(line, byte_offset)
            }
        }
    }

    /// Return the offset to the next codepoint if any. See the [`crate`] documentation to learn
    /// more about codepoints.
    pub fn next_codepoint_offset(&self, offset: Byte) -> Option<Byte> {
        self.rope.next_codepoint_offset(offset.value).map(Byte)
    }

    /// Return the offset to the previous codepoint if any. See the [`crate`] documentation to learn
    /// more about codepoints.
    pub fn prev_codepoint_offset(&self, offset: Byte) -> Option<Byte> {
        self.rope.prev_codepoint_offset(offset.value).map(Byte)
    }

    /// Return the offset to the next grapheme if any. See the documentation of the library to
    /// learn more about graphemes.
    pub fn next_grapheme_offset(&self, offset: Byte) -> Option<Byte> {
        self.rope.next_grapheme_offset(offset.value).map(Byte)
    }

    /// Return the offset to the previous grapheme if any. See the documentation of the library to
    /// learn more about graphemes.
    pub fn prev_grapheme_offset(&self, offset: Byte) -> Option<Byte> {
        self.rope.prev_grapheme_offset(offset.value).map(Byte)
    }

    /// An iterator over the lines of a rope.
    ///
    /// Lines are ended with either Unix (`\n`) or MS-DOS (`\r\n`) style line endings. The line
    /// ending is stripped from the resulting string. The final line ending is optional.
    pub fn lines<T: rope::IntervalBounds>(&self, range: T) -> rope::Lines {
        self.rope.lines(range)
    }

    /// Replaces the provided range with the provided text.
    pub fn replace(&mut self, range: impl RangeBounds, text: impl Into<Rope>) {
        let text = text.into();
        let range = self.crop_byte_range(range);
        self.rope.edit(range.into_rope_interval(), text.rope);
    }

    /// Apply the given change on the current text.
    ///
    /// See also [`Self::replace`].
    pub fn apply_change(&mut self, change: Change<Byte, impl Into<Rope>>) {
        self.replace(change.range, change.text)
    }
}


// === First Line ===

impl Rope {
    /// The first valid line index in this text.
    pub fn first_line_index(&self) -> Line {
        Line(0)
    }

    /// The first valid line byte offset in this text.
    pub fn first_line_byte_offset(&self) -> Byte {
        0.byte()
    }

    /// The start location of the first line.
    pub fn first_line_start_location(&self) -> Location<Byte> {
        let line = self.first_line_index();
        let byte_offset = self.first_line_byte_offset();
        Location(line, byte_offset)
    }
}


// === Last Line ===

impl Rope {
    /// The last valid line index in this text. If the text ends with the newline character,
    /// it means that there is an empty last line.
    pub fn last_line_index(&self) -> Line {
        (self.rope.measure::<rope::metric::Lines>()).into()
    }

    /// The last valid line byte offset in this text. If the text ends with the newline character,
    /// it means that there is an empty last line.
    pub fn last_line_byte_offset(&self) -> Byte {
        self.byte_offset_of_line_index_unchecked(self.last_line_index())
    }

    /// The start location of the last line.
    pub fn last_line_start_location(&self) -> Location<Byte> {
        let line = self.last_line_index();
        let byte_offset = Byte(0);
        Location(line, byte_offset)
    }

    /// The end location of the last line.
    pub fn last_line_end_column_byte_offset(&self) -> Byte {
        self.line_byte_offset_of_byte_offset(self.byte_size()).unwrap()
    }

    /// The byte offset of the end of the last line. Equal to the byte size of the whole text.
    pub fn last_line_end_byte_offset(&self) -> Byte {
        self.byte_size()
    }

    /// The location of the last character in the text.
    pub fn last_line_end_location(&self) -> Location<Byte> {
        let line = self.last_line_index();
        let byte_offset = self.last_line_end_column_byte_offset();
        Location(line, byte_offset)
    }
}


// === Validation ===

impl Rope {
    /// Check whether the provided line index is valid in this text.
    pub fn validate_line_index(&self, line: Line) -> Result<Line, BoundsError> {
        use BoundsError::*;
        if line < Line(0) {
            Err(TooSmall)
        } else if line > self.last_line_index() {
            Err(TooBig)
        } else {
            Ok(line)
        }
    }

    /// Check whether the provided byte offset is valid in this text.
    pub fn validate_byte_offset(&self, offset: Byte) -> Result<Byte, BoundsError> {
        use BoundsError::*;
        if offset < 0.byte() {
            Err(TooSmall)
        } else if offset > self.byte_size() {
            Err(TooBig)
        } else {
            Ok(offset)
        }
    }
}



// ===================
// === Conversions ===
// ===================

// === Into Byte Offset ===

impl Rope {
    /// Return the offset after the last character of a given line if the line exists.
    pub fn end_byte_offset_of_line_index(&self, line: Line) -> Result<Byte, BoundsError> {
        self.validate_line_index(line)?;
        let next_line = line + Line(1);
        let next_line_off = self.byte_offset_of_line_index(next_line).ok();
        let next_line_prev = next_line_off.and_then(|t| {
            self.prev_grapheme_offset(t).and_then(|prev1| {
                self.prev_grapheme_offset(prev1).map(|prev2| {
                    let was_rn_seq = self.slice(prev2.value..prev1.value).to_string() == "\r";
                    let off = if was_rn_seq { prev2 } else { prev1 };
                    off
                })
            })
        });
        Ok(next_line_prev.unwrap_or_else(|| self.byte_size()))
    }

    // FIXME: unwraps
    /// Byte length of the given line. Does not include the newline characters.
    pub fn line_byte_length(&self, line: Line) -> Byte {
        let line_start = self.byte_offset_of_line_index(line).unwrap();
        let line_end = self.end_byte_offset_of_line_index(line).unwrap();
        Byte::try_from(line_end - line_start).unwrap()
    }

    /// Return the offset after the last character of a given line if the line exists. Snapped to
    /// the closest valid value.
    pub fn end_byte_offset_of_line_index_snapped(&self, line: Line) -> Byte {
        self.snap_bytes_bounds_result(self.end_byte_offset_of_line_index(line))
    }

    /// The line byte offset. Panics in case the line index was invalid.
    pub fn byte_offset_of_line_index_unchecked(&self, line: Line) -> Byte {
        self.rope.offset_of_line(line.value).into()
    }

    /// The byte offset of the given line index.
    pub fn byte_offset_of_line_index(&self, line: Line) -> Result<Byte, BoundsError> {
        self.validate_line_index(line)?;
        Ok(self.byte_offset_of_line_index_unchecked(line))
    }

    /// The byte offset of the given line. Snapped to the closest valid byte offset in case the
    /// line index was invalid.
    pub fn byte_offset_of_line_index_snapped(&self, line: Line) -> Byte {
        use BoundsError::*;
        match self.byte_offset_of_line_index(line) {
            Ok(offset) => offset,
            Err(TooSmall) => self.first_line_byte_offset(),
            Err(TooBig) => self.last_line_byte_offset(),
        }
    }

    /// Byte offset of the given location.
    pub fn byte_offset_of_location(
        &self,
        location: Location<Byte>,
    ) -> Result<Byte, LocationError<Byte>> {
        let line_offset = self.byte_offset_of_line_index(location.line)?;
        Ok(line_offset + location.offset)
    }

    /// Byte offset of the given location. Snapped to the closest valid value.
    pub fn byte_offset_of_location_snapped(&self, location: Location<Byte>) -> Byte {
        let offset = self.byte_offset_of_location(location);
        self.snap_bytes_location_result(offset)
    }

    /// Byte range of the given line.
    pub fn byte_range_of_line_index(
        &self,
        line: Line,
    ) -> Result<std::ops::Range<Byte>, BoundsError> {
        let start = self.byte_offset_of_line_index(line)?;
        let end = self.end_byte_offset_of_line_index(line)?;
        Ok(start..end)
    }

    /// Byte range of the given line. Snapped to the closest valid value.
    pub fn byte_range_of_line_index_snapped(&self, line: Line) -> std::ops::Range<Byte> {
        let start = self.byte_offset_of_line_index_snapped(line);
        let end = self.end_byte_offset_of_line_index_snapped(line);
        start..end
    }
}


// === Into Line Index ===

impl Rope {
    /// The line of a given byte offset. Panics in case the offset was invalid.
    pub fn line_index_of_byte_offset_unchecked(&self, offset: Byte) -> Line {
        self.rope.line_of_offset(offset.value).into()
    }

    /// The line index of the given byte offset.
    pub fn line_index_of_byte_offset(&self, offset: Byte) -> Result<Line, BoundsError> {
        self.validate_byte_offset(offset)?;
        Ok(self.line_index_of_byte_offset_unchecked(offset))
    }

    /// The line index of the given byte offset. Snapped to the closest valid line index in case the
    /// byte offset was invalid.
    pub fn line_index_of_byte_offset_snapped(&self, offset: Byte) -> Line {
        use BoundsError::*;
        match self.line_index_of_byte_offset(offset) {
            Ok(index) => index,
            Err(TooSmall) => self.first_line_index(),
            Err(TooBig) => self.last_line_index(),
        }
    }
}


// === Into CodePointIndex ===

impl Rope {
    /// The byte offset of the beginning of the line containing the provided byte offset.
    pub fn line_byte_offset_of_byte_offset(
        &self,
        tgt_offset: Byte,
    ) -> Result<Byte, LocationError<Byte>> {
        let line_index = self.line_index_of_byte_offset(tgt_offset)?;
        let line_offset = self.byte_offset_of_line_index(line_index)?;
        let offset = Byte::try_from(tgt_offset - line_offset).unwrap();
        Ok(offset)
    }
}


// === Into Location ===


impl Rope {
    /// The location of text end.
    pub fn location_of_text_end(&self) -> Location<Byte> {
        let lines_count = self.lines(self.byte_range()).count();
        let last_char_off = self.rope.prev_codepoint_offset(self.len());
        let last_char = last_char_off.map(|off| self.rope.slice_to_cow(off..));
        let ends_with_eol = last_char.map_or(false, |ch| ch.starts_with('\n'));
        if ends_with_eol {
            let line: Line = lines_count.into();
            Location(line, Byte(0))
        } else if lines_count == 0 {
            default()
        } else {
            let line = Line(lines_count - 1);
            let byte_offset = self.end_byte_offset_of_line_index(line).unwrap();
            Location(line, byte_offset)
        }
    }

    /// The location of the provided byte offset.
    pub fn location_of_byte_offset(&self, offset: Byte) -> Result<Location<Byte>, BoundsError> {
        let line = self.line_index_of_byte_offset(offset)?;
        let line_offset = offset - self.byte_offset_of_line_index(line).unwrap(); // fixme unwrap
        let line_offset = Byte::try_from(line_offset).unwrap_or_else(|_| {
            error!("Internal error, wrong line byte offset.");
            Byte(0)
        });
        let byte_offset = Byte::try_from(offset - line_offset).unwrap();
        Ok(Location(line, byte_offset))
    }

    /// The location of the provided byte offset. Snapped to the closest valid
    /// value.
    pub fn location_of_byte_offset_snapped(&self, offset: Byte) -> Location<Byte> {
        use BoundsError::*;
        match self.location_of_byte_offset(offset) {
            Ok(location) => location,
            Err(TooSmall) => self.first_line_start_location(),
            Err(TooBig) => self.last_line_end_location(),
        }
    }
}



// ==============
// === Errors ===
// ==============

/// Error indicating the usage of incorrect line index or byte offset - negative, or bigger than the
/// size of the text.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub enum BoundsError {
    TooSmall,
    TooBig,
}

/// Error indicating the usage of incorrect location in the text.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub enum LocationError<T> {
    BoundsError(BoundsError),
    LineTooShort(T),
    NotClusterBoundary(T),
}

impl<T> From<BoundsError> for LocationError<T> {
    fn from(err: BoundsError) -> Self {
        Self::BoundsError(err)
    }
}

impl Rope {
    /// Snaps the `LocationError<Byte>` to the closest valid byte offset.
    pub fn snap_bytes_location_error(&self, err: LocationError<Byte>) -> Byte {
        use self::BoundsError::*;
        use LocationError::*;
        match err {
            BoundsError(TooSmall) => 0.byte(),
            BoundsError(TooBig) => self.last_line_end_byte_offset(),
            LineTooShort(offset) => offset,
            NotClusterBoundary(offset) => offset,
        }
    }

    /// Snaps the `BoundsError` to the closest valid byte offset.
    pub fn snap_bytes_bounds_error(&self, err: BoundsError) -> Byte {
        use self::BoundsError::*;
        match err {
            TooSmall => 0.byte(),
            TooBig => self.last_line_end_byte_offset(),
        }
    }

    /// Snaps the `LocationResult<Byte>` to the closest valid byte offset.
    pub fn snap_bytes_location_result(&self, result: Result<Byte, LocationError<Byte>>) -> Byte {
        match result {
            Ok(bytes) => bytes,
            Err(err) => self.snap_bytes_location_error(err),
        }
    }

    /// Snaps the `Result<Byte,BoundsError>` to the closest valid byte offset.
    pub fn snap_bytes_bounds_result(&self, result: Result<Byte, BoundsError>) -> Byte {
        match result {
            Ok(bytes) => bytes,
            Err(err) => self.snap_bytes_bounds_error(err),
        }
    }
}


// === Common Prefix and Suffix ===

/// The return value of [`Rope::common_prefix_and_suffix`] function.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default)]
pub struct CommonPrefixAndSuffix {
    pub prefix: ByteDiff,
    pub suffix: ByteDiff,
}

impl Rope {
    /// Returns the length in bytes of common prefix and suffix.
    ///
    /// The prefix and suffix lengths does not overlap, so the sum of their length will not exceed
    /// the length of both texts.
    pub fn common_prefix_and_suffix(&self, other: &Rope) -> CommonPrefixAndSuffix {
        let mut scanner = xi_rope::compare::RopeScanner::new(&self.rope, &other.rope);
        let (prefix, suffix) = scanner.find_min_diff_range();
        CommonPrefixAndSuffix { prefix: prefix.into(), suffix: suffix.into() }
    }
}

// === Display ===

impl Display for Rope {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.rope, f)
    }
}


// ===================
// === Conversions ===
// ===================

impl From<xi_rope::Rope> for Rope {
    fn from(t: xi_rope::Rope) -> Self {
        Self { rope: t }
    }
}

impl From<&xi_rope::Rope> for Rope {
    fn from(t: &xi_rope::Rope) -> Self {
        t.clone().into()
    }
}

impl From<&str> for Rope {
    fn from(t: &str) -> Self {
        Self { rope: t.into() }
    }
}

impl From<String> for Rope {
    fn from(t: String) -> Self {
        Self { rope: t.into() }
    }
}

impl From<&String> for Rope {
    fn from(t: &String) -> Self {
        Self { rope: t.into() }
    }
}

impl From<&&String> for Rope {
    fn from(t: &&String) -> Self {
        (*t).into()
    }
}

impl From<ImString> for Rope {
    fn from(t: ImString) -> Self {
        Self { rope: t.into() }
    }
}

impl From<&ImString> for Rope {
    fn from(t: &ImString) -> Self {
        Self { rope: t.into() }
    }
}

impl From<&&ImString> for Rope {
    fn from(t: &&ImString) -> Self {
        (*t).into()
    }
}

impl From<&&str> for Rope {
    fn from(t: &&str) -> Self {
        (*t).into()
    }
}

impl From<Rope> for String {
    fn from(t: Rope) -> Self {
        t.rope.into()
    }
}

impl From<&Rope> for String {
    fn from(t: &Rope) -> Self {
        t.clone().into()
    }
}

impl From<&&Rope> for String {
    fn from(t: &&Rope) -> Self {
        (*t).into()
    }
}



// =====================
// === FromInContext ===
// =====================

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


// === Conversions to Line ===

impl<T> FromInContextSnapped<&Rope, Location<T, Line>> for Line {
    fn from_in_context_snapped(_: &Rope, location: Location<T, Line>) -> Self {
        location.line
    }
}

// impl FromInContextSnapped<&Rope, Byte> for Line {
//     fn from_in_context_snapped(buffer: &Rope, offset: Byte) -> Self {
//         Location::<Byte, Line>::from_in_context_snapped(buffer, offset).line
//     }
// }

// === Conversions to Byte ===

impl FromInContextSnapped<&Rope, Location<Byte, Line>> for Byte {
    fn from_in_context_snapped(rope: &Rope, location: Location<Byte, Line>) -> Self {
        rope.byte_offset_of_line_index(location.line).unwrap() + location.offset
    }
}

// impl FromInContextSnapped<&Rope, Location<Column, Line>> for Byte {
//     fn from_in_context_snapped(context: &Rope, location: Location) -> Self {
//         let location = Location::<Byte, Line>::from_in_context_snapped(context, location);
//         Byte::from_in_context_snapped(context, location)
//     }
// }


// === Conversions to Location<Column, Line> ===

impl FromInContextSnapped<&Rope, Location<Byte, Line>> for Location<Column, Line> {
    fn from_in_context_snapped(rope: &Rope, location: Location<Byte, Line>) -> Self {
        warn!("ROPE from_in_context_snapped, location: {:?}", location);
        let offset_start = rope.byte_offset_of_line_index_snapped(location.line);
        let offset_end = offset_start + location.offset;
        let sub_rope = rope.sub(offset_start..offset_end);
        let mut offset = Byte(0);
        let mut column = Column(0);
        loop {
            match sub_rope.next_grapheme_offset(offset) {
                Some(next_offset) => {
                    offset = next_offset;
                    column += 1;
                }
                None => break,
            }
        }
        location.with_offset(column)
    }
}

impl FromInContextSnapped<&Rope, Byte> for Location<Column, Line> {
    fn from_in_context_snapped(rope: &Rope, offset: Byte) -> Self {
        Location::<Column, Line>::from_in_context_snapped(
            rope,
            Location::<Byte, Line>::from_in_context_snapped(rope, offset),
        )
    }
}


// === Conversions to Location<Byte, Line> ===

impl FromInContextSnapped<&Rope, Location<Column, Line>> for Location<Byte, Line> {
    fn from_in_context_snapped(rope: &Rope, location: Location<Column, Line>) -> Self {
        let offset_start = rope.byte_offset_of_line_index_snapped(location.line);
        let offset_end = rope.end_byte_offset_of_line_index_snapped(location.line);
        let sub_rope = rope.sub(offset_start..offset_end);
        let mut offset = Byte(0);
        let mut column = Column(0);
        while column != location.offset {
            match sub_rope.next_grapheme_offset(offset) {
                Some(next_offset) => {
                    offset = next_offset;
                    column += 1;
                }
                None => break,
            }
        }
        location.with_offset(offset)
    }
}

impl FromInContextSnapped<&Rope, Byte> for Location<Byte, Line> {
    fn from_in_context_snapped(rope: &Rope, offset: Byte) -> Self {
        let line = rope.line_index_of_byte_offset_snapped(offset);
        let line_offset = rope.byte_offset_of_line_index_snapped(line);
        let byte_offset = Byte::try_from(offset - line_offset).unwrap();
        Location(line, byte_offset)
    }
}


// === Conversions of Range ====


impl<'t, S, T> FromInContextSnapped<&'t Rope, Range<S>> for Range<T>
where T: FromInContextSnapped<&'t Rope, S>
{
    fn from_in_context_snapped(rope: &'t Rope, range: Range<S>) -> Self {
        let start = T::from_in_context_snapped(rope, range.start);
        let end = T::from_in_context_snapped(rope, range.end);
        Range::new(start, end)
    }
}



// ================
// === RopeCell ===
// ================

/// Internally mutable version of `Rope`.
#[derive(Debug, Clone, Default, Deref)]
#[allow(missing_docs)]
pub struct RopeCell {
    cell: RefCell<Rope>,
}

impl RopeCell {
    /// Getter of the current value of the cell.
    pub fn get(&self) -> Rope {
        self.cell.borrow().clone()
    }

    /// Setter of the value of the cell.
    pub fn set(&self, new_text: impl Into<Rope>) {
        let new_text = new_text.into();
        *self.cell.borrow_mut() = new_text;
    }

    /// Get all lines in the provided range as strings.
    pub fn lines_vec(&self, range: std::ops::Range<Byte>) -> Vec<String> {
        let rope_range = range.start.value..range.end.value;
        let mut lines = self.cell.borrow().lines(rope_range).map(|t| t.into()).collect_vec();
        if lines.is_empty() {
            // xi_rope::Rope returns `[]` if the line is empty.
            lines.push("".into())
        }
        lines
    }
}

/// See docs in `Rope`.
#[allow(missing_docs)]
impl RopeCell {
    pub fn new() -> Self {
        default()
    }

    pub fn is_empty(&self) -> bool {
        self.cell.borrow().is_empty()
    }

    pub fn sub(&self, range: impl RangeBounds) -> Rope {
        self.cell.borrow().sub(range)
    }

    pub fn grapheme_count(&self) -> usize {
        self.cell.borrow().grapheme_count()
    }

    pub fn byte_size(&self) -> Byte {
        self.cell.borrow().byte_size()
    }

    pub fn byte_range(&self) -> Range<Byte> {
        self.cell.borrow().byte_range()
    }

    pub fn crop_byte_range(&self, range: impl RangeBounds) -> Range<Byte> {
        self.cell.borrow().crop_byte_range(range)
    }

    pub fn snap_location(&self, location: Location<Byte>) -> Location<Byte> {
        self.cell.borrow().snap_location(location)
    }

    pub fn next_grapheme_offset(&self, offset: Byte) -> Option<Byte> {
        self.cell.borrow().next_grapheme_offset(offset)
    }

    pub fn prev_grapheme_offset(&self, offset: Byte) -> Option<Byte> {
        self.cell.borrow().prev_grapheme_offset(offset)
    }

    pub fn replace(&self, range: impl RangeBounds, text: impl Into<Rope>) {
        self.cell.borrow_mut().replace(range, text)
    }

    pub fn first_line_index(&self) -> Line {
        self.cell.borrow().first_line_index()
    }

    pub fn first_line_byte_offset(&self) -> Byte {
        self.cell.borrow().first_line_byte_offset()
    }

    pub fn first_line_start_location(&self) -> Location<Byte> {
        self.cell.borrow().first_line_start_location()
    }

    pub fn last_line_index(&self) -> Line {
        self.cell.borrow().last_line_index()
    }

    pub fn last_line_byte_offset(&self) -> Byte {
        self.cell.borrow().last_line_byte_offset()
    }

    pub fn last_line_start_location(&self) -> Location<Byte> {
        self.cell.borrow().last_line_start_location()
    }

    pub fn last_line_end_byte_offset(&self) -> Byte {
        self.cell.borrow().last_line_end_byte_offset()
    }

    pub fn last_line_end_location(&self) -> Location<Byte> {
        self.cell.borrow().last_line_end_location()
    }

    pub fn validate_line_index(&self, line: Line) -> Result<Line, BoundsError> {
        self.cell.borrow().validate_line_index(line)
    }

    pub fn validate_byte_offset(&self, offset: Byte) -> Result<Byte, BoundsError> {
        self.cell.borrow().validate_byte_offset(offset)
    }

    pub fn end_byte_offset_of_line_index(&self, line: Line) -> Result<Byte, BoundsError> {
        self.cell.borrow().end_byte_offset_of_line_index(line)
    }

    pub fn line_byte_length(&self, line: Line) -> Byte {
        self.cell.borrow().line_byte_length(line)
    }

    pub fn end_byte_offset_of_line_index_snapped(&self, line: Line) -> Byte {
        self.cell.borrow().end_byte_offset_of_line_index_snapped(line)
    }

    pub fn byte_offset_of_line_index_unchecked(&self, line: Line) -> Byte {
        self.cell.borrow().byte_offset_of_line_index_unchecked(line)
    }

    pub fn byte_offset_of_line_index(&self, line: Line) -> Result<Byte, BoundsError> {
        self.cell.borrow().byte_offset_of_line_index(line)
    }

    pub fn byte_offset_of_line_index_snapped(&self, line: Line) -> Byte {
        self.cell.borrow().byte_offset_of_line_index_snapped(line)
    }

    pub fn byte_offset_of_location(
        &self,
        location: Location<Byte>,
    ) -> Result<Byte, LocationError<Byte>> {
        self.cell.borrow().byte_offset_of_location(location)
    }

    pub fn byte_offset_of_location_snapped(&self, location: Location<Byte>) -> Byte {
        self.cell.borrow().byte_offset_of_location_snapped(location)
    }

    pub fn byte_range_of_line_index(
        &self,
        line: Line,
    ) -> Result<std::ops::Range<Byte>, BoundsError> {
        self.cell.borrow().byte_range_of_line_index(line)
    }

    pub fn byte_range_of_line_index_snapped(&self, line: Line) -> std::ops::Range<Byte> {
        self.cell.borrow().byte_range_of_line_index_snapped(line)
    }

    pub fn line_index_of_byte_offset_unchecked(&self, offset: Byte) -> Line {
        self.cell.borrow().line_index_of_byte_offset_unchecked(offset)
    }

    pub fn line_index_of_byte_offset(&self, offset: Byte) -> Result<Line, BoundsError> {
        self.cell.borrow().line_index_of_byte_offset(offset)
    }

    pub fn line_index_of_byte_offset_snapped(&self, offset: Byte) -> Line {
        self.cell.borrow().line_index_of_byte_offset_snapped(offset)
    }

    pub fn location_of_byte_offset(&self, offset: Byte) -> Result<Location<Byte>, BoundsError> {
        self.cell.borrow().location_of_byte_offset(offset)
    }

    pub fn location_of_byte_offset_snapped(&self, offset: Byte) -> Location<Byte> {
        self.cell.borrow().location_of_byte_offset_snapped(offset)
    }
}



// ==============
// === Change ===
// ==============

/// A single change done to the text content.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Change<Metric = Byte, Str = Rope> {
    /// Range of old text being replaced.
    pub range: Range<Metric>,
    /// The text inserted in place of `range`.
    pub text:  Str,
}


impl<Metric, String> Change<Metric, String> {
    /// Create a change being an insert of the `text` at given `offset` (no text will be removed).
    pub fn inserted(offset: Metric, text: String) -> Self
    where Metric: Copy {
        Self { range: Range::new(offset, offset), text }
    }

    /// Return new [`Change`] with copied range and a reference to self's string.
    pub fn as_ref(&self) -> Change<Metric, &String>
    where Metric: Copy {
        Change { range: self.range, text: &self.text }
    }
}


// === Applying Change ===

impl<S: AsRef<str>> Change<Byte, S> {
    /// Apply the change on the given string.
    pub fn apply(&self, target: &mut String) -> Result<(), BoundsError> {
        let start_byte = self.range.start.value;
        let end_byte = self.range.end.value;
        target.replace_range(start_byte..end_byte, self.text.as_ref());
        Ok(())
    }

    /// Return a new string being a `target` with this change applied.
    pub fn applied(&self, target: &str) -> Result<String, BoundsError> {
        let mut string = target.to_owned();
        self.apply(&mut string)?;
        Ok(string)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn location_of_text_end() {
        struct Case {
            text:     &'static str,
            expected: (usize, usize), // Line and column
        }

        impl Case {
            fn run(&self) {
                let text: Rope = self.text.into();
                let (exp_line, exp_column) = self.expected;
                let expected =
                    Location { line: exp_line.into(), offset: Byte(exp_column.into()) };
                let result = text.location_of_text_end();
                assert_eq!(result, expected, "Wrong text end location in case \"{}\"", text);
            }
        }

        let cases = &[
            Case { text: "", expected: (0, 0) },
            Case { text: "single line", expected: (0, 11) },
            Case { text: "single line with eol\n", expected: (1, 0) },
            Case { text: "\nMany\nLines", expected: (2, 5) },
            Case { text: "Many\nLines\nwith eol\n", expected: (3, 0) },
        ];

        for case in cases {
            case.run()
        }
    }
}
