//! Definition of strongly typed units, like `LineDiff` or `ByteDiff`. Please note that indexes,
//! such as `Byte` are defined in a separate module.

use crate::index::*;
use crate::prelude::*;

use enso_types::unit;



// ===============
// === Exports ===
// ===============

/// Common traits.
pub mod traits {
    pub use super::byte_diff::Into as TRAIT_byte_diff_into;
    pub use super::bytes::Into as TRAIT_bytes_into;
}
pub use traits::*;



// =============
// === Bytes ===
// =============

unit! {
/// A number of bytes.
Bytes::bytes(usize)
}

impl<T: Into<Bytes>> bytes::Into for Range<T> {
    type Output = Range<Bytes>;
    fn bytes(self) -> Self::Output {
        let start = self.start.into();
        let end = self.end.into();
        Range { start, end }
    }
}

impl Bytes {
    /// Convert the byte length to a byte index.
    pub fn to_byte(self) -> Byte {
        Byte(self.value)
    }

    /// Convert bytes to byte diff.
    pub fn to_diff(self) -> ByteDiff {
        ByteDiff(self.value as i32)
    }
}



// ================
// === ByteDiff ===
// ================

unit! {
/// An offset in the buffer in bytes.
ByteDiff::byte_diff(i32)
}

impl ByteDiff {
    /// Saturating conversion to `usize`.
    pub fn as_usize(self) -> usize {
        self.value.max(0) as usize
    }
}

impl<T: Into<ByteDiff>> byte_diff::Into for Range<T> {
    type Output = Range<ByteDiff>;
    fn byte_diff(self) -> Self::Output {
        let start = self.start.into();
        let end = self.end.into();
        Range { start, end }
    }
}

impl From<usize> for ByteDiff {
    fn from(t: usize) -> Self {
        (t as i32).into()
    }
}

impl From<&usize> for ByteDiff {
    fn from(t: &usize) -> Self {
        (*t as i32).into()
    }
}

impl Sub<Bytes> for ByteDiff {
    type Output = ByteDiff;
    fn sub(self, rhs: Bytes) -> Self::Output {
        let value = self.value - rhs.value as i32;
        ByteDiff(value)
    }
}

impl Add<Bytes> for ByteDiff {
    type Output = ByteDiff;
    fn add(self, rhs: Bytes) -> Self::Output {
        let value = self.value + rhs.value as i32;
        ByteDiff(value)
    }
}

impl AddAssign<Bytes> for ByteDiff {
    fn add_assign(&mut self, rhs: Bytes) {
        *self = *self + rhs
    }
}

impl SubAssign<Bytes> for ByteDiff {
    fn sub_assign(&mut self, rhs: Bytes) {
        *self = *self - rhs
    }
}



// ================
// === LineDiff ===
// ================

/// The difference between lines.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd, From, Into)]
pub struct LineDiff {
    #[allow(missing_docs)]
    pub value: i32,
}

/// Constructor.
#[allow(non_snake_case)]
pub fn LineDiff(value: i32) -> LineDiff {
    LineDiff { value }
}

impl Add for LineDiff {
    type Output = LineDiff;
    fn add(self, rhs: LineDiff) -> Self::Output {
        LineDiff(self.value + rhs.value)
    }
}

impl LineDiff {
    /// Convert the line diff to line and report warning if the diff was negative.
    pub fn to_line(self) -> Line {
        if self.value < 0 {
            warn!("Trying to convert negative line diff to line.");
            Line(0)
        } else {
            Line(self.value as usize)
        }
    }
}



// ==============
// === Column ===
// ==============

unit! {
/// A column index.
Column::column(usize)
}

impl<T: Into<Column>> column::Into for Range<T> {
    type Output = Range<Column>;
    fn column(self) -> Self::Output {
        let start = self.start.into();
        let end = self.end.into();
        Range { start, end }
    }
}

impl AddAssign<usize> for Column {
    fn add_assign(&mut self, rhs: usize) {
        self.value += rhs
    }
}



// ======================
// === UTF16CodeUnit ===
// ======================

unit! {
    /// An offset in the text measured in number of code units in text in UTF-16 representation.
    Utf16CodeUnit::utf16_code_unit(usize)
}



// ================
// === Location ===
// ================

define_not_same_trait!();

mod location {
    use super::*;
    #[doc = " A type representing 2d measurements."]
    #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[allow(missing_docs)]
    pub struct Location<Offset = Column, LineType = Line> {
        pub line:   LineType,
        pub offset: Offset,
    }

    impl<Offset, Line> Location<Offset, Line> {
        /// Line setter.
        pub fn with_line<Line2>(self, line: Line2) -> Location<Offset, Line2> {
            Location { line, offset: self.offset }
        }

        /// CodePointIndex setter.
        pub fn with_offset<Offset2>(self, offset: Offset2) -> Location<Offset2, Line> {
            Location { line: self.line, offset }
        }

        /// Modify the line value.
        pub fn mod_line<Line2>(self, f: impl FnOnce(Line) -> Line2) -> Location<Offset, Line2> {
            let line = f(self.line);
            Location { line, offset: self.offset }
        }

        /// Modify the offset value.
        pub fn mod_offset<Offset2>(
            self,
            f: impl FnOnce(Offset) -> Offset2,
        ) -> Location<Offset2, Line> {
            let offset = f(self.offset);
            Location { line: self.line, offset }
        }

        /// Sets the line to zero.
        pub fn zero_line(self) -> Location<Offset, Line>
        where Line: From<usize> {
            self.with_line(Line::from(0_usize))
        }

        /// Sets the offset to zero.
        pub fn zero_offset(self) -> Location<Offset, Line>
        where Offset: From<usize> {
            self.with_offset(Offset::from(0_usize))
        }

        /// Increment the line index.
        pub fn inc_line(self) -> Location<Offset, <Line as Add<usize>>::Output>
        where Line: Add<usize> + From<usize> {
            self.mod_line(|t| t + 1_usize)
        }

        /// Decrement the line index.
        pub fn dec_line(self) -> Location<Offset, <Line as Sub<usize>>::Output>
        where Line: Sub<usize> + From<usize> {
            self.mod_line(|t| t - 1_usize)
        }

        /// Increment the offset.
        pub fn inc_offset(self) -> Location<<Offset as Add>::Output, Line>
        where Offset: Add + From<usize> {
            self.mod_offset(|t| t + Offset::from(1_usize))
        }

        /// Decrement the offset.
        pub fn dec_offset(self) -> Location<<Offset as Sub>::Output, Line>
        where Offset: Sub + From<usize> {
            self.mod_offset(|t| t - Offset::from(1_usize))
        }
    }

    /// Smart constructor.
    #[doc = " A type representing 2d measurements."]
    #[allow(non_snake_case)]
    pub fn Location<Offset, Line>(line: Line, offset: Offset) -> Location<Offset, Line> {
        Location { line, offset }
    }

    impl<Offset: Copy, Line: Copy> From<&Location<Offset, Line>> for Location<Offset, Line> {
        fn from(t: &Location<Offset, Line>) -> Self {
            *t
        }
    }

    impl<Offset: Copy, Line: Copy> From<&&Location<Offset, Line>> for Location<Offset, Line> {
        fn from(t: &&Location<Offset, Line>) -> Self {
            **t
        }
    }

    #[allow(clippy::needless_update)]
    impl<Offset, Line: Add> Add<Line> for Location<Offset, Line> {
        type Output = Location<Offset, <Line as Add>::Output>;
        fn add(self, rhs: Line) -> Self::Output {
            Location { line: self.line.add(rhs), offset: self.offset }
        }
    }
    #[allow(clippy::needless_update)]
    impl<Offset: Copy, Line: Copy + Add> Add<Line> for &Location<Offset, Line>
    where (Offset, Line): NotSame
    {
        type Output = Location<Offset, <Line as Add>::Output>;
        fn add(self, rhs: Line) -> Self::Output {
            Location { line: self.line.add(rhs), offset: self.offset }
        }
    }

    #[allow(clippy::needless_update)]
    impl<Offset: Add, Line> Add<Offset> for Location<Offset, Line>
    where (Offset, Line): NotSame
    {
        type Output = Location<<Offset as Add>::Output, Line>;
        fn add(self, rhs: Offset) -> Self::Output {
            Location { line: self.line, offset: self.offset.add(rhs) }
        }
    }

    #[allow(clippy::needless_update)]
    impl<Offset: Copy + Add, Line: Copy> Add<Offset> for &Location<Offset, Line>
    where (Offset, Line): NotSame
    {
        type Output = Location<<Offset as Add>::Output, Line>;
        fn add(self, rhs: Offset) -> Self::Output {
            Location { line: self.line, offset: self.offset.add(rhs) }
        }
    }

    #[allow(clippy::needless_update)]
    impl<Offset, Line: SaturatingAdd> SaturatingAdd<Line> for Location<Offset, Line>
    where (Offset, Line): NotSame
    {
        type Output = Location<Offset, <Line as SaturatingAdd>::Output>;
        fn saturating_add(self, rhs: Line) -> Self::Output {
            Location { line: self.line.saturating_add(rhs), offset: self.offset }
        }
    }

    #[allow(clippy::needless_update)]
    impl<Offset: Copy, Line: Copy + SaturatingAdd> SaturatingAdd<Line> for &Location<Offset, Line>
    where (Offset, Line): NotSame
    {
        type Output = Location<Offset, <Line as SaturatingAdd>::Output>;
        fn saturating_add(self, rhs: Line) -> Self::Output {
            Location { line: self.line.saturating_add(rhs), offset: self.offset }
        }
    }

    #[allow(clippy::needless_update)]
    impl<Offset: SaturatingAdd, Line> SaturatingAdd<Offset> for Location<Offset, Line>
    where (Offset, Line): NotSame
    {
        type Output = Location<<Offset as SaturatingAdd>::Output, Line>;
        fn saturating_add(self, rhs: Offset) -> Self::Output {
            Location { line: self.line, offset: self.offset.saturating_add(rhs) }
        }
    }

    #[allow(clippy::needless_update)]
    impl<Offset: Copy + SaturatingAdd, Line: Copy> SaturatingAdd<Offset> for &Location<Offset, Line>
    where (Offset, Line): NotSame
    {
        type Output = Location<<Offset as SaturatingAdd>::Output, Line>;
        fn saturating_add(self, rhs: Offset) -> Self::Output {
            Location { line: self.line, offset: self.offset.saturating_add(rhs) }
        }
    }
}
pub use location::*;
