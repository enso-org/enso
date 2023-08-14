//! The data hold by the text buffer. Under the hood it is implemented as an efficient string rope.

use crate::index::*;
use crate::prelude::*;
use crate::unit::*;

use crate::prelude::fmt::Formatter;
use crate::range::Range;
use crate::range::RangeBounds;
use crate::rope;

use enso_types::min;
use xi_rope::rope::Utf16CodeUnitsMetric;



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
    /// ```
    /// # use enso_text::*;
    /// assert_eq!(Rope::from("").is_empty(), true);
    /// assert_eq!(Rope::from("a").is_empty(), false);
    /// ```
    pub fn is_empty(&self) -> bool {
        self.rope.is_empty()
    }

    /// Length of the text in bytes.
    /// ```
    /// # use enso_text::*;
    /// let str = "🧑🏾";
    /// assert_eq!(Rope::from(str).len(), Bytes(str.len()));
    /// ```
    pub fn len(&self) -> Bytes {
        Bytes(self.rope.len())
    }

    /// Last byte index in text. There is no glyph after this byte index.
    /// ```
    /// # use enso_text::*;
    /// let str = "🧑🏾";
    /// assert_eq!(Rope::from(str).last_byte_index(), Byte(str.len()));
    /// ```
    pub fn last_byte_index(&self) -> Byte {
        Byte(self.rope.len())
    }

    /// Return text narrowed to the given range.
    /// ```
    /// # use enso_text::*;
    /// let s1 = "a";
    /// let s2 = "🧑🏾";
    /// let s3 = "ட்";
    /// let str = format!("{s1}{s2}{s3}");
    /// let range = Byte(s1.len())..Byte(s1.len() + s2.len());
    /// assert_eq!(&Rope::from(str).sub(range).to_string(), s2);
    /// ```
    pub fn sub(&self, range: impl RangeBounds) -> Rope {
        let range = self.crop_byte_range(range);
        let rope = self.rope.subseq(range.into_rope_interval());
        Rope { rope }
    }

    /// The number of grapheme clusters in this text.
    /// ```
    /// # use enso_text::*;
    /// assert_eq!(Rope::from("a🧑🏾ட்").grapheme_count(), 3);
    /// ```
    pub fn grapheme_count(&self) -> usize {
        let mut offset = 0;
        let mut count = 0;
        while let Some(off) = self.rope.next_grapheme_offset(offset) {
            offset = off;
            count += 1;
        }
        count
    }

    /// Range of the text in bytes.
    /// ```
    /// # use enso_text::*;
    /// let str = "a🧑🏾ட்";
    /// assert_eq!(Rope::from(str).byte_range(), Range::from(Byte(0)..Byte(str.len())));
    /// ```
    pub fn byte_range(&self) -> Range<Byte> {
        (..self.last_byte_index()).into()
    }

    /// Return the offset to the next codepoint if any.
    /// ```
    /// # use enso_text::*;
    /// let rope = Rope::from("a🧑🏾ட்");
    /// assert_eq!(rope.next_codepoint_offset(Byte(0)), Some(Byte(1)));
    /// assert_eq!(rope.next_codepoint_offset(Byte(1)), Some(Byte(5)));
    /// assert_eq!(rope.next_codepoint_offset(Byte(5)), Some(Byte(9)));
    /// assert_eq!(rope.next_codepoint_offset(Byte(9)), Some(Byte(12)));
    /// assert_eq!(rope.next_codepoint_offset(Byte(12)), Some(Byte(15)));
    /// assert_eq!(rope.next_codepoint_offset(Byte(15)), None);
    /// assert!(std::panic::catch_unwind(|| rope.next_codepoint_offset(Byte(2))).is_err());
    /// assert!(std::panic::catch_unwind(|| rope.next_codepoint_offset(Byte(16))).is_err());
    /// ```
    pub fn next_codepoint_offset(&self, offset: Byte) -> Option<Byte> {
        debug_assert!(self.rope.is_codepoint_boundary(offset.value));
        self.rope.next_codepoint_offset(offset.value).map(Byte)
    }

    /// Return the offset to the previous codepoint if any.
    /// ```
    /// # use enso_text::*;
    /// let rope = Rope::from("a🧑🏾ட்");
    /// assert_eq!(rope.prev_codepoint_offset(Byte(0)), None);
    /// assert_eq!(rope.prev_codepoint_offset(Byte(1)), Some(Byte(0)));
    /// assert_eq!(rope.prev_codepoint_offset(Byte(5)), Some(Byte(1)));
    /// assert_eq!(rope.prev_codepoint_offset(Byte(9)), Some(Byte(5)));
    /// assert_eq!(rope.prev_codepoint_offset(Byte(12)), Some(Byte(9)));
    /// assert_eq!(rope.prev_codepoint_offset(Byte(15)), Some(Byte(12)));
    /// assert!(std::panic::catch_unwind(|| rope.prev_codepoint_offset(Byte(2))).is_err());
    /// assert!(std::panic::catch_unwind(|| rope.prev_codepoint_offset(Byte(16))).is_err());
    /// ```
    pub fn prev_codepoint_offset(&self, offset: Byte) -> Option<Byte> {
        debug_assert!(self.rope.is_codepoint_boundary(offset.value));
        self.rope.prev_codepoint_offset(offset.value).map(Byte)
    }

    /// Return the offset to the next grapheme if any. See the documentation of the library to
    /// learn more about graphemes.
    /// ```
    /// # use enso_text::*;
    /// let s1 = "a";
    /// let s2 = "🧑🏾";
    /// let s3 = "ட்";
    /// let off1 = Byte(0);
    /// let off2 = Byte(s1.len());
    /// let off3 = Byte(s1.len() + s2.len());
    /// let off4 = Byte(s1.len() + s2.len() + s3.len());
    /// let off5 = Byte(s1.len() + s2.len() + s3.len() + 1);
    /// let rope = Rope::from(format!("{s1}{s2}{s3}"));
    /// for off in off1..off2 {
    ///     assert_eq!(rope.next_grapheme_offset(off), Some(off2));
    /// }
    /// for off in off2..off3 {
    ///     assert_eq!(rope.next_grapheme_offset(off), Some(off3));
    /// }
    /// for off in off3..off4 {
    ///     assert_eq!(rope.next_grapheme_offset(off), Some(off4));
    /// }
    /// assert_eq!(rope.next_grapheme_offset(off4), None);
    /// assert!(std::panic::catch_unwind(|| rope.next_grapheme_offset(off5)).is_err());
    /// ```
    pub fn next_grapheme_offset(&self, offset: Byte) -> Option<Byte> {
        self.rope.next_grapheme_offset(offset.value).map(Byte)
    }

    /// Return the offset to the previous grapheme if any. See the documentation of the library to
    /// learn more about graphemes.
    /// ```
    /// # use enso_text::*;
    /// let s1 = "a";
    /// let s2 = "🧑🏾";
    /// let s3 = "ட்";
    /// let off1 = Byte(0);
    /// let off2 = Byte(s1.len());
    /// let off3 = Byte(s1.len() + s2.len());
    /// let off4 = Byte(s1.len() + s2.len() + s3.len());
    /// let off5 = Byte(s1.len() + s2.len() + s3.len() + 1);
    /// let rope = Rope::from(format!("{s1}{s2}{s3}"));
    /// for off in off1..off2 {
    ///     let next = rope.next_grapheme_offset(off).unwrap();
    ///     assert_eq!(rope.prev_grapheme_offset(next), Some(off1));
    /// }
    /// for off in off2..off3 {
    ///     let next = rope.next_grapheme_offset(off).unwrap();
    ///     assert_eq!(rope.prev_grapheme_offset(next), Some(off2));
    /// }
    /// for off in off3..off4 {
    ///     let next = rope.next_grapheme_offset(off).unwrap();
    ///     assert_eq!(rope.prev_grapheme_offset(next), Some(off3));
    /// }
    /// assert_eq!(rope.prev_grapheme_offset(off1), None);
    /// assert!(std::panic::catch_unwind(|| rope.prev_grapheme_offset(off5)).is_err());
    /// ```
    pub fn prev_grapheme_offset(&self, offset: Byte) -> Option<Byte> {
        self.rope.prev_grapheme_offset(offset.value).map(Byte)
    }

    /// An iterator over the lines of a rope.
    ///
    /// Lines are ended with either Unix (`\n`) or MS-DOS (`\r\n`) style line endings. The line
    /// ending is stripped from the resulting string. The final line ending is optional.
    /// ```
    /// # use enso_text::*;
    /// # use enso_text::prelude::*;
    /// fn test(str: &str, out: Vec<&str>) {
    ///     let lines: Vec<String> = Rope::from(str).lines(..).map(|t| t.into()).collect();
    ///     assert_eq!(lines, out);
    /// }
    /// test("", vec![]);
    /// test("\n", vec![""]);
    /// test("\n\n", vec!["", ""]);
    /// test("a🧑🏾ட்", vec!["a🧑🏾ட்"]);
    /// test("a\n🧑🏾\r\nட்\n", vec!["a", "🧑🏾", "ட்"]); // No newline at end!
    /// test("a\n🧑🏾\r\nட்\n\n", vec!["a", "🧑🏾", "ட்", ""]); // No newline at end!
    /// ```
    pub fn lines<T: rope::IntervalBounds>(&self, range: T) -> rope::Lines {
        self.rope.lines(range)
    }

    /// Replaces the provided range with the provided text.
    /// ```
    /// # use enso_text::*;
    /// let s1 = "a";
    /// let s2 = "🧑🏾";
    /// let s3 = "ட்";
    /// let off1 = Byte(0);
    /// let off2 = Byte(s1.len());
    /// let off3 = Byte(s1.len() + s2.len());
    /// let off4 = Byte(s1.len() + s2.len() + s3.len());
    /// let str = format!("{s1}{s2}{s3}");
    ///
    /// let mut rope = Rope::from(&str);
    /// rope.replace(off1..off2, "x");
    /// assert_eq!(rope.to_string(), "x🧑🏾ட்");
    ///
    /// let mut rope = Rope::from(&str);
    /// rope.replace(off2..off3, "x");
    /// assert_eq!(rope.to_string(), "axட்");
    ///
    /// let mut rope = Rope::from(&str);
    /// rope.replace(off3..off4, "x");
    /// assert_eq!(rope.to_string(), "a🧑🏾x");
    /// ```
    pub fn replace(&mut self, range: impl RangeBounds, text: impl Into<Rope>) {
        let text = text.into();
        let range = self.crop_byte_range(range);
        self.rope.edit(range.into_rope_interval(), text.rope);
    }
}


// === Last Line ===

impl Rope {
    /// The last valid line index in this text. If the text ends with the newline character,
    /// it means that there is an empty last line.
    /// ```
    /// # use enso_text::*;
    /// assert_eq!(Rope::from("").last_line_index(), Line(0));
    /// assert_eq!(Rope::from("\n").last_line_index(), Line(1));
    /// ```
    pub fn last_line_index(&self) -> Line {
        (self.rope.measure::<rope::metric::Lines>()).into()
    }

    /// The last valid line byte offset in this text. If the text ends with the newline character,
    /// it means that there is an empty last line.
    /// ```
    /// # use enso_text::*;
    /// assert_eq!(Rope::from("").last_line_byte_offset(), Byte(0));
    /// assert_eq!(Rope::from("line").last_line_byte_offset(), Byte(0));
    /// assert_eq!(Rope::from("line\nline2").last_line_byte_offset(), Byte(5));
    /// ```
    pub fn last_line_byte_offset(&self) -> Byte {
        self.line_offset_unchecked(self.last_line_index())
    }

    /// The start location of the last line.
    /// ```
    /// # use enso_text::*;
    /// assert_eq!(Rope::from("").last_line_location(), Location(Line(0), Byte(0)));
    /// assert_eq!(Rope::from("line").last_line_location(), Location(Line(0), Byte(0)));
    /// assert_eq!(Rope::from("line\nline2").last_line_location(), Location(Line(1), Byte(0)));
    /// assert_eq!(Rope::from("line\nline2\n").last_line_location(), Location(Line(2), Byte(0)));
    /// ```
    pub fn last_line_location(&self) -> Location<Byte> {
        let line = self.last_line_index();
        let byte_offset = Byte(0);
        Location(line, byte_offset)
    }

    /// The last column number of the last line.
    /// ```
    /// # use enso_text::*;
    /// assert_eq!(Rope::from("").last_line_end_column(), Column(0));
    /// assert_eq!(Rope::from("x🧑🏾ட்").last_line_end_column(), Column(3));
    /// ```
    pub fn last_line_end_column(&self) -> Column {
        self.column_of_byte_offset(self.last_byte_index()).unwrap()
    }

    /// The byte offset of last line end counted from the beginning of the line.
    /// ```
    /// # use enso_text::*;
    /// let line = "x🧑🏾ட்";
    /// let offset = Byte(line.len());
    /// assert_eq!(Rope::from(line).last_line_len(), offset);
    /// assert_eq!(Rope::from(format!("\n{}", line)).last_line_len(), offset);
    /// assert_eq!(Rope::from(format!("\n\n{}", line)).last_line_len(), offset);
    /// assert_eq!(Rope::from(format!("\n\n\n{}", line)).last_line_len(), offset);
    /// assert_eq!(Rope::from(format!("\n\n\n{}\n", line)).last_line_len(), Byte(0));
    /// ```
    pub fn last_line_len(&self) -> Byte {
        self.line_len_up_to_offset(self.last_byte_index()).unwrap()
    }

    /// The byte offset of the end of the last line. Equal to the byte size of the whole text.
    pub fn last_line_end_offset(&self) -> Byte {
        self.last_byte_index()
    }

    /// The location of the last character in the text.
    pub fn last_line_end_location(&self) -> Location<Byte, Line> {
        let line = self.last_line_index();
        let offset = self.last_line_len();
        Location(line, offset)
    }
}


// === Validation ===

impl Rope {
    /// Constraint the provided location, so it will be contained of the range of this data. This
    /// ensures that the resulting location will be valid for operations on this data.
    pub fn snap_location(&self, location: Location<Byte>) -> Location<Byte> {
        use BoundsError::*;
        match self.validate_line_index(location.line) {
            Err(TooSmall) => default(),
            Err(TooBig) => self.last_line_end_location(),
            Ok(line) => {
                let max_byte_offset = self.line_end_offset_unchecked(line);
                let byte_offset = min(location.offset, max_byte_offset);
                Location(line, byte_offset)
            }
        }
    }

    /// Constraint the provided byte range, so it will be contained of the range of this data. This
    /// ensures that the resulting byte range will be valid for operations on this data.
    pub fn crop_byte_range(&self, range: impl RangeBounds) -> Range<Byte> {
        range.with_upper_bound(self.last_byte_index())
    }

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
        } else if offset > self.last_byte_index() {
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
    /// Line end offset counted from the beginning of the text.
    /// ```
    /// # use enso_text::*;
    /// assert_eq!(Rope::from("").line_end_offset_unchecked(Line(0)), Byte(0));
    /// assert_eq!(Rope::from("foo").line_end_offset_unchecked(Line(0)), Byte(3));
    /// assert_eq!(Rope::from("foo\nbar").line_end_offset_unchecked(Line(0)), Byte(3));
    /// assert_eq!(Rope::from("foo\nbar").line_end_offset_unchecked(Line(1)), Byte(7));
    /// assert_eq!(Rope::from("foo\r\nbar").line_end_offset_unchecked(Line(0)), Byte(3));
    /// assert_eq!(Rope::from("foo\r\nbar").line_end_offset_unchecked(Line(1)), Byte(8));
    /// ```
    pub fn line_end_offset_unchecked(&self, line: Line) -> Byte {
        let next_line = line + Line(1);
        let next_line_off = self.line_offset(next_line).ok();
        let next_line_prev = next_line_off.and_then(|next_line_off| {
            self.prev_grapheme_offset(next_line_off).map(|prev1| {
                self.prev_grapheme_offset(prev1)
                    .map(|prev2| {
                        let rn_seq = self.slice_to_cow(prev2.value..prev1.value).starts_with('\r');
                        let off = if rn_seq { prev2 } else { prev1 };
                        off
                    })
                    .unwrap_or(prev1)
            })
        });
        let out = next_line_prev.unwrap_or_else(|| self.last_byte_index());
        out
    }

    /// Line end offset counted from the beginning of the text.
    pub fn line_end_offset(&self, line: Line) -> Result<Byte, BoundsError> {
        self.validate_line_index(line)?;
        Ok(self.line_end_offset_unchecked(line))
    }

    /// Byte length of the given line. Does not include the newline characters.
    /// ```
    /// # use enso_text::*;
    /// let rope = Rope::from("first\r\nsecond\n\nfourth\n");
    /// assert_eq!(rope.line_len(Line(0)), Byte(5));
    /// assert_eq!(rope.line_len(Line(1)), Byte(6));
    /// assert_eq!(rope.line_len(Line(2)), Byte(0));
    /// assert_eq!(rope.line_len(Line(3)), Byte(6));
    /// assert_eq!(rope.line_len(Line(4)), Byte(0));
    /// ```
    pub fn line_len(&self, line: Line) -> Byte {
        let line_start = self.line_offset(line).unwrap();
        let line_end = self.line_end_offset(line).unwrap();
        Byte::try_from(line_end - line_start).unwrap()
    }

    /// Return the offset after the last character of a given line if the line exists. Snapped to
    /// the closest valid value.
    pub fn line_end_offset_snapped(&self, line: Line) -> Byte {
        self.snap_bytes_bounds_result(self.line_end_offset(line))
    }

    /// The line byte offset. Panics in case the line index was invalid.
    /// ```
    /// # use enso_text::*;
    /// let rope = Rope::from("first\r\nsecond\n\nfourth\n");
    /// assert_eq!(rope.line_offset_unchecked(Line(0)), Byte(0));
    /// assert_eq!(rope.line_offset_unchecked(Line(1)), Byte(7));
    /// assert_eq!(rope.line_offset_unchecked(Line(2)), Byte(14));
    /// assert_eq!(rope.line_offset_unchecked(Line(3)), Byte(15));
    /// assert_eq!(rope.line_offset_unchecked(Line(4)), Byte(22));
    /// ```
    pub fn line_offset_unchecked(&self, line: Line) -> Byte {
        self.rope.offset_of_line(line.value).into()
    }

    /// The byte offset of the given line index.
    pub fn line_offset(&self, line: Line) -> Result<Byte, BoundsError> {
        self.validate_line_index(line)?;
        Ok(self.line_offset_unchecked(line))
    }

    /// The byte offset of the given line. Snapped to the closest valid byte offset in case the
    /// line index was invalid.
    pub fn line_offset_snapped(&self, line: Line) -> Byte {
        use BoundsError::*;
        match self.line_offset(line) {
            Ok(offset) => offset,
            Err(TooSmall) => Byte(0),
            Err(TooBig) => self.last_line_byte_offset(),
        }
    }

    /// Byte offset of the given location.
    pub fn location_offset(
        &self,
        location: Location<Byte, Line>,
    ) -> Result<Byte, LocationError<Byte>> {
        let line_offset = self.line_offset(location.line)?;
        self.validate_byte_offset(line_offset + location.offset).map_err(LocationError::from)
    }

    /// Byte offset of the given location. Snapped to the closest valid value.
    pub fn location_offset_snapped(&self, location: Location<Byte>) -> Byte {
        let offset = self.location_offset(location);
        self.snap_bytes_location_result(offset)
    }

    /// Byte range of the given line.
    pub fn line_range(&self, line: Line) -> Result<std::ops::Range<Byte>, BoundsError> {
        let start = self.line_offset(line)?;
        let end = self.line_end_offset(line)?;
        Ok(start..end)
    }

    /// Byte range of the given line. Snapped to the closest valid value.
    pub fn line_range_snapped(&self, line: Line) -> std::ops::Range<Byte> {
        let start = self.line_offset_snapped(line);
        let end = self.line_end_offset_snapped(line);
        start..end
    }
}


// === Into Line Index ===

impl Rope {
    /// The line of a given byte offset. Panics in case the offset was out of valid range. In case
    /// the offset was between `\r` and `\n` of a CRLF sequence, the line before the CRLF sequence
    /// is returned.
    /// ```
    /// # use enso_text::*;
    /// let rope = Rope::from("first\r\nsecond\n\nfourth\n");
    /// for offset in Byte(0)..=Byte(5) {
    ///     assert_eq!(rope.line_unchecked(offset), Line(0));
    /// }
    /// assert_eq!(rope.line_unchecked(Byte(6)), Line(0)); // Between `\r` and `\n`.
    /// for offset in Byte(7)..=Byte(13) {
    ///     assert_eq!(rope.line_unchecked(offset), Line(1));
    /// }
    /// assert_eq!(rope.line_unchecked(Byte(14)), Line(2));
    /// for offset in Byte(15)..=Byte(21) {
    ///     assert_eq!(rope.line_unchecked(offset), Line(3));
    /// }
    /// assert_eq!(rope.line_unchecked(Byte(22)), Line(4));
    /// ```
    pub fn line_unchecked(&self, offset: Byte) -> Line {
        self.rope.line_of_offset(offset.value).into()
    }

    /// The line index of the given byte offset.
    pub fn line(&self, offset: Byte) -> Result<Line, BoundsError> {
        self.validate_byte_offset(offset)?;
        Ok(self.line_unchecked(offset))
    }

    /// The line index of the given byte offset. Snapped to the closest valid line index in case the
    /// byte offset was invalid.
    pub fn line_snapped(&self, offset: Byte) -> Line {
        use BoundsError::*;
        match self.line(offset) {
            Ok(index) => index,
            Err(TooSmall) => Line(0),
            Err(TooBig) => self.last_line_index(),
        }
    }
}


// === Into CodePointIndex ===

impl Rope {
    /// The byte offset between line start and the provided byte offset. If the offset was between
    /// `\r` and `\n` of a CRLF sequence, the offset is assumed to belong to the line before the
    /// CRLF sequence.
    pub fn line_len_up_to_offset(&self, tgt_offset: Byte) -> Result<Byte, BoundsError> {
        self.offset_to_location(tgt_offset).map(|location| location.offset)
    }
}


// === Into Location ===


impl Rope {
    /// The location of the provided byte offset.
    /// ```
    /// # use enso_text::*;
    /// fn test(line: Line, start: usize, end: usize) {
    ///     let rope = Rope::from("first\r\nsecond\n\nfourth\n");
    ///     for i in start..=end {
    ///         let location = Location(line, Byte(i - start));
    ///         assert_eq!(rope.offset_to_location(Byte(i)).unwrap(), location);
    ///     }
    /// }
    /// test(Line(0), 0, 6);
    /// test(Line(1), 7, 13);
    /// test(Line(2), 14, 14);
    /// test(Line(3), 15, 21);
    /// test(Line(4), 22, 22);
    /// ```
    pub fn offset_to_location(
        &self,
        tgt_offset: Byte,
    ) -> Result<Location<Byte, Line>, BoundsError> {
        let line = self.line(tgt_offset)?;
        let line_offset = self.line_offset(line)?;
        let offset = Byte::try_from(tgt_offset - line_offset).unwrap();
        Ok(Location(line, offset))
    }

    /// The location of the provided byte offset. Snapped to the closest valid
    /// value.
    pub fn offset_to_location_snapped(&self, offset: Byte) -> Location<Byte> {
        use BoundsError::*;
        match self.offset_to_location(offset) {
            Ok(location) => location,
            Err(TooSmall) => default(),
            Err(TooBig) => self.last_line_end_location(),
        }
    }


    // === Into Column ===

    /// The last column number of the given line.
    pub fn line_last_column(&self, line: Line) -> Result<Column, BoundsError> {
        let offset = self.line_end_offset(line)?;
        Ok(self.column_of_byte_offset(offset).unwrap())
    }

    /// The column index of the given byte offset.
    pub fn column_of_byte_offset(&self, tgt_offset: Byte) -> Result<Column, LocationError<Column>> {
        use self::BoundsError::*;
        use LocationError::*;
        let line_index = self.line(tgt_offset)?;
        let mut offset = self.line_offset(line_index)?;
        let mut column = Column(0);
        while offset < tgt_offset {
            match self.next_grapheme_offset(offset) {
                None => return Err(BoundsError(TooBig)),
                Some(off) => {
                    offset = off;
                    column += 1;
                }
            }
        }
        if offset != tgt_offset {
            Err(NotClusterBoundary(column))
        } else {
            Ok(column)
        }
    }

    /// The column from line number and byte offset within the line.
    pub fn column_of_line_index_and_in_line_byte_offset(
        &self,
        line: Line,
        in_line_offset: Byte,
    ) -> Result<Column, LocationError<Column>> {
        let offset = self.line_offset(line)?;
        let tgt_offset = offset + in_line_offset;
        let column = self.column_of_byte_offset(tgt_offset)?;
        Ok(column)
    }

    /// The column from line number and byte offset within the line. Snapped to
    /// the closest valid value. In case the offset points inside of a grapheme cluster, it will be
    /// snapped to its right side.
    pub fn column_of_line_index_and_in_line_byte_offset_snapped(
        &self,
        line: Line,
        in_line_offset: Byte,
    ) -> Column {
        let column = self.column_of_line_index_and_in_line_byte_offset(line, in_line_offset);
        self.snap_column_location_result(column)
    }

    /// The column number of the given byte offset. Snapped to the closest valid
    /// value. In case the offset points inside of a grapheme cluster, it will be snapped to its
    /// right side.
    pub fn column_of_byte_offset_snapped(&self, tgt_offset: Byte) -> Column {
        self.snap_column_location_result(self.column_of_byte_offset(tgt_offset))
    }

    /// Snaps the `LocationResult<Column>` to the closest valid column.
    pub fn snap_column_location_result(
        &self,
        result: Result<Column, LocationError<Column>>,
    ) -> Column {
        match result {
            Ok(column) => column,
            Err(err) => self.snap_column_location_error(err),
        }
    }


    // === UTF16 ===

    /// The location of text end in UTF-16 code units.
    pub fn location_of_text_end_utf16_code_unit(&self) -> Location<Utf16CodeUnit> {
        let location = self.last_line_end_location();
        self.utf16_code_unit_location_of_location(location)
    }

    /// ```
    /// # use enso_text::*;
    /// let rope = Rope::from("first_line\n🧑🏾second_line");
    /// let from = Location { line: Line(1), offset: Utf16CodeUnit(5) };
    /// let expected = Location { line: Line(1), offset: Byte(9) };
    /// assert_eq!(rope.location_of_utf16_code_unit_location_snapped(from), expected);
    /// ```
    pub fn location_of_utf16_code_unit_location_snapped(
        &self,
        location: Location<Utf16CodeUnit>,
    ) -> Location<Byte> {
        let line_start = self.line_offset_snapped(location.line);
        let from_line_start = self.rope.slice(line_start.value..);
        let line = location.line;
        let offset =
            Byte(from_line_start.count_base_units::<Utf16CodeUnitsMetric>(location.offset.value));
        Location { line, offset }
    }

    /// ```
    /// # use enso_text::*;
    /// let rope = Rope::from("first_line\n🧑🏾second_line");
    /// let from = Location { line: Line(1), offset: Byte(9) };
    /// let expected = Location { line: Line(1), offset: Utf16CodeUnit(5) };
    /// assert_eq!(rope.utf16_code_unit_location_of_location(from), expected);
    /// ```
    pub fn utf16_code_unit_location_of_location(
        &self,
        location: Location<Byte>,
    ) -> Location<Utf16CodeUnit> {
        let line_start = self.line_offset_snapped(location.line);
        let position = self.location_offset_snapped(location);
        println!("{line_start} {position}");
        let line_fragment_before = self.rope.slice(line_start.value..position.value);
        let line = location.line;
        let offset = Utf16CodeUnit::from(line_fragment_before.measure::<Utf16CodeUnitsMetric>());
        Location { line, offset }
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
    /// Snaps the `LocationError<Column>` to the closest valid column.
    pub fn snap_column_location_error(&self, err: LocationError<Column>) -> Column {
        use self::BoundsError::*;
        use LocationError::*;
        match err {
            BoundsError(TooSmall) => Column(0),
            BoundsError(TooBig) => self.last_line_end_column(),
            LineTooShort(column) => column,
            NotClusterBoundary(column) => column,
        }
    }

    /// Snaps the `LocationError<Byte>` to the closest valid byte offset.
    pub fn snap_bytes_location_error(&self, err: LocationError<Byte>) -> Byte {
        use self::BoundsError::*;
        use LocationError::*;
        match err {
            BoundsError(TooSmall) => Byte(0),
            BoundsError(TooBig) => self.last_line_end_offset(),
            LineTooShort(offset) => offset,
            NotClusterBoundary(offset) => offset,
        }
    }

    /// Snaps the `BoundsError` to the closest valid byte offset.
    pub fn snap_bytes_bounds_error(&self, err: BoundsError) -> Byte {
        use self::BoundsError::*;
        match err {
            TooSmall => Byte(0),
            TooBig => self.last_line_end_offset(),
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

impl From<Rope> for ImString {
    fn from(t: Rope) -> Self {
        ImString::new(t.to_im_string())
    }
}

impl From<&Rope> for ImString {
    fn from(t: &Rope) -> Self {
        t.clone().into()
    }
}

impl From<&&Rope> for ImString {
    fn from(t: &&Rope) -> Self {
        (*t).into()
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

    pub fn len(&self) -> Bytes {
        self.cell.borrow().len()
    }

    pub fn last_byte_index(&self) -> Byte {
        self.cell.borrow().last_byte_index()
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

    pub fn last_line_index(&self) -> Line {
        self.cell.borrow().last_line_index()
    }

    pub fn last_line_byte_offset(&self) -> Byte {
        self.cell.borrow().last_line_byte_offset()
    }

    pub fn last_line_location(&self) -> Location<Byte> {
        self.cell.borrow().last_line_location()
    }

    pub fn last_line_end_offset(&self) -> Byte {
        self.cell.borrow().last_line_end_offset()
    }

    pub fn last_line_end_location(&self) -> Location<Byte> {
        self.cell.borrow().last_line_end_location()
    }

    pub fn line_last_column(&self, line: Line) -> Result<Column, BoundsError> {
        self.cell.borrow().line_last_column(line)
    }

    pub fn validate_line_index(&self, line: Line) -> Result<Line, BoundsError> {
        self.cell.borrow().validate_line_index(line)
    }

    pub fn validate_byte_offset(&self, offset: Byte) -> Result<Byte, BoundsError> {
        self.cell.borrow().validate_byte_offset(offset)
    }

    pub fn line_end_offset(&self, line: Line) -> Result<Byte, BoundsError> {
        self.cell.borrow().line_end_offset(line)
    }

    pub fn line_len(&self, line: Line) -> Byte {
        self.cell.borrow().line_len(line)
    }

    pub fn line_end_offset_snapped(&self, line: Line) -> Byte {
        self.cell.borrow().line_end_offset_snapped(line)
    }

    pub fn line_offset_unchecked(&self, line: Line) -> Byte {
        self.cell.borrow().line_offset_unchecked(line)
    }

    pub fn line_offset(&self, line: Line) -> Result<Byte, BoundsError> {
        self.cell.borrow().line_offset(line)
    }

    pub fn line_offset_snapped(&self, line: Line) -> Byte {
        self.cell.borrow().line_offset_snapped(line)
    }

    pub fn location_offset(&self, location: Location<Byte>) -> Result<Byte, LocationError<Byte>> {
        self.cell.borrow().location_offset(location)
    }

    pub fn location_offset_snapped(&self, location: Location<Byte>) -> Byte {
        self.cell.borrow().location_offset_snapped(location)
    }

    pub fn line_range(&self, line: Line) -> Result<std::ops::Range<Byte>, BoundsError> {
        self.cell.borrow().line_range(line)
    }

    pub fn line_range_snapped(&self, line: Line) -> std::ops::Range<Byte> {
        self.cell.borrow().line_range_snapped(line)
    }

    pub fn line_unchecked(&self, offset: Byte) -> Line {
        self.cell.borrow().line_unchecked(offset)
    }

    pub fn line(&self, offset: Byte) -> Result<Line, BoundsError> {
        self.cell.borrow().line(offset)
    }

    pub fn line_snapped(&self, offset: Byte) -> Line {
        self.cell.borrow().line_snapped(offset)
    }

    pub fn offset_to_location(&self, offset: Byte) -> Result<Location<Byte>, BoundsError> {
        self.cell.borrow().offset_to_location(offset)
    }

    pub fn offset_to_location_snapped(&self, offset: Byte) -> Location<Byte> {
        self.cell.borrow().offset_to_location_snapped(offset)
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

impl Rope {
    /// Apply the given change on the current text.
    ///
    /// See also [`Self::replace`].
    pub fn apply_change(&mut self, change: Change<Byte, impl Into<Rope>>) {
        self.replace(change.range, change.text)
    }
}

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

impl FromInContextSnapped<&Rope, Byte> for Line {
    fn from_in_context_snapped(buffer: &Rope, offset: Byte) -> Self {
        Location::<Byte, Line>::from_in_context_snapped(buffer, offset).line
    }
}


// === Conversions to Byte ===

impl FromInContextSnapped<&Rope, Location<Byte, Line>> for Byte {
    fn from_in_context_snapped(rope: &Rope, location: Location<Byte, Line>) -> Self {
        rope.line_offset(location.line).unwrap() + location.offset
    }
}

impl FromInContextSnapped<&Rope, Location<Column, Line>> for Byte {
    fn from_in_context_snapped(context: &Rope, location: Location) -> Self {
        let location = Location::<Byte, Line>::from_in_context_snapped(context, location);
        Byte::from_in_context_snapped(context, location)
    }
}


// === Conversions to Location<Column, Line> ===

impl FromInContextSnapped<&Rope, Location<Byte, Line>> for Location<Column, Line> {
    fn from_in_context_snapped(rope: &Rope, location: Location<Byte, Line>) -> Self {
        let offset_start = rope.line_offset_snapped(location.line);
        let offset_end = offset_start + location.offset;
        let sub_rope = rope.sub(offset_start..offset_end);
        let mut offset = Byte(0);
        let mut column = Column(0);
        while let Some(next_offset) = sub_rope.next_grapheme_offset(offset) {
            offset = next_offset;
            column += 1;
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
        let offset_start = rope.line_offset_snapped(location.line);
        let offset_end = rope.line_end_offset_snapped(location.line);
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
        let line = rope.line_snapped(offset);
        let line_offset = rope.line_offset_snapped(line);
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



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn getting_utf16_code_unit_location_from_end_of_the_text() {
        let rope = Rope::from("first_line\n🧑🏾second_line");
        let from = Location { line: Line(1), offset: Byte(19) };
        let expected = Location { line: Line(1), offset: Utf16CodeUnit(15) };
        assert_eq!(rope.utf16_code_unit_location_of_location(from), expected);
    }

    #[test]
    fn getting_utf16_code_unit_location_from_out_of_bounds_location() {
        let rope = Rope::from("first_line\n🧑🏾second_line");
        let from = Location { line: Line(1), offset: Byte(20) };
        let expected = Location { line: Line(1), offset: Utf16CodeUnit(15) };
        assert_eq!(rope.utf16_code_unit_location_of_location(from), expected);
    }
}
