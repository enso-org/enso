//! Definition of strongly typed units, like `Line`, `Byte`, or `Location`. Used to express type
//! level dependencies in the whole library.

use crate::prelude::*;

use enso_types::unit;
use std::ops::AddAssign;
use std::ops::SubAssign;



// ===============
// === Exports ===
// ===============

/// Common traits.
pub mod traits {
    pub use super::byte::Into as TRAIT_ubytes_into;
    pub use super::byte_diff::Into as TRAIT_byte_diff_into;
}
pub use traits::*;



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

impl From<Byte> for ByteDiff {
    fn from(t: Byte) -> Self {
        (t.value as i32).into()
    }
}

impl From<&Byte> for ByteDiff {
    fn from(t: &Byte) -> Self {
        (t.value as i32).into()
    }
}



// ============
// === Byte ===
// ============

unit! {
/// A byte index.
Byte::byte(usize) NO_SUB
}

impl<T: Into<Byte>> byte::Into for Range<T> {
    type Output = Range<Byte>;
    fn byte(self) -> Self::Output {
        let start = self.start.into();
        let end = self.end.into();
        Range { start, end }
    }
}

impl Sub<Byte> for Byte {
    type Output = ByteDiff;
    fn sub(self, rhs: Byte) -> Self::Output {
        (self.value as i32 - rhs.value as i32).into()
    }
}

impl Sub<ByteDiff> for Byte {
    type Output = Byte;
    fn sub(self, rhs: ByteDiff) -> Self::Output {
        let value = self.value as i32 - rhs.value;
        if value < 0 {
            error!("Subtraction of ByteDiff resulted in negative value.");
            Byte(0)
        } else {
            Byte(value as usize)
        }
    }
}

impl Add<ByteDiff> for Byte {
    type Output = Byte;
    fn add(self, rhs: ByteDiff) -> Self::Output {
        let value = self.value as i32 + rhs.value;
        Byte(value as usize)
    }
}

impl AddAssign<ByteDiff> for Byte {
    fn add_assign(&mut self, rhs: ByteDiff) {
        *self = *self + rhs
    }
}

impl SubAssign<ByteDiff> for Byte {
    fn sub_assign(&mut self, rhs: ByteDiff) {
        *self = *self - rhs
    }
}

impl Byte {
    pub fn to_diff(self) -> ByteDiff {
        ByteDiff(self.value as i32)
    }
}

/// Conversion error.
#[derive(Debug, Clone, Copy)]
pub struct ByteDiffToByteConversionError;

impl TryFrom<ByteDiff> for Byte {
    type Error = ByteDiffToByteConversionError;

    fn try_from(bytes: ByteDiff) -> Result<Self, Self::Error> {
        if bytes.value < 0 {
            Err(ByteDiffToByteConversionError)
        } else {
            Ok(Byte::from(bytes.value as usize))
        }
    }
}



// ============
// === Line ===
// ============

macro_rules! define_line_unit {
    ($name:ident) => {
        /// A line index.
        #[derive(
            Clone, Copy, Debug, Display, Default, Eq, Hash, Ord, PartialEq, PartialOrd, From, Into
        )]
        pub struct $name {
            #[allow(missing_docs)]
            pub value: usize,
        }

        /// Smart constructor.
        #[allow(non_snake_case)]
        pub const fn $name(value: usize) -> $name {
            $name { value }
        }

        impl From<&$name> for $name {
            fn from(t: &$name) -> Self {
                *t
            }
        }

        impl From<&&$name> for $name {
            fn from(t: &&$name) -> Self {
                **t
            }
        }

        impl From<f32> for $name {
            fn from(t: f32) -> Self {
                if t < 0.0 {
                    error!("Negative value used to construct {}.", stringify!($name));
                    $name(0)
                } else {
                    $name(t as usize)
                }
            }
        }

        impl Sub<$name> for $name {
            type Output = LineDiff;
            fn sub(self, rhs: $name) -> Self::Output {
                LineDiff(self.value as i32 - rhs.value as i32)
            }
        }

        impl Sub<$name> for &$name {
            type Output = LineDiff;
            fn sub(self, rhs: $name) -> Self::Output {
                (*self).sub(rhs)
            }
        }

        impl Sub<&$name> for $name {
            type Output = LineDiff;
            fn sub(self, rhs: &$name) -> Self::Output {
                self.sub(*rhs)
            }
        }

        impl Sub<&$name> for &$name {
            type Output = LineDiff;
            fn sub(self, rhs: &$name) -> Self::Output {
                (*self).sub(*rhs)
            }
        }

        impl Sub<usize> for $name {
            type Output = $name;
            fn sub(self, rhs: usize) -> Self::Output {
                if self.value < rhs {
                    error!("Subtraction of {} resulted in negative value.", stringify!($name));
                    $name(0)
                } else {
                    $name(self.value - rhs)
                }
            }
        }

        impl Add<$name> for $name {
            type Output = $name;
            fn add(self, rhs: $name) -> Self::Output {
                let value = self.value.add(rhs.value);
                $name { value }
            }
        }

        impl Add<$name> for &$name {
            type Output = $name;
            fn add(self, rhs: $name) -> Self::Output {
                (*self).add(rhs)
            }
        }

        impl Add<&$name> for $name {
            type Output = $name;
            fn add(self, rhs: &$name) -> Self::Output {
                self.add(*rhs)
            }
        }

        impl Add<&$name> for &$name {
            type Output = $name;
            fn add(self, rhs: &$name) -> Self::Output {
                (*self).add(*rhs)
            }
        }

        impl Add<usize> for $name {
            type Output = $name;
            fn add(self, rhs: usize) -> Self::Output {
                let value = self.value.add(rhs);
                $name { value }
            }
        }

        #[allow(clippy::needless_update)]
        impl AddAssign<$name> for $name {
            fn add_assign(&mut self, rhs: $name) {
                self.value.add_assign(rhs.value)
            }
        }

        #[allow(clippy::needless_update)]
        impl AddAssign<&$name> for $name {
            fn add_assign(&mut self, rhs: &$name) {
                self.add_assign(*rhs)
            }
        }

        impl $name {
            /// Increment the value.
            pub fn inc(self) -> Self {
                self + $name(1)
            }
        }

        impl iter::Step for $name {
            fn steps_between(start: &Self, end: &Self) -> Option<usize> {
                let diff = end.value - start.value;
                Some(diff)
            }

            fn forward_checked(start: Self, count: usize) -> Option<Self> {
                let new_value = start.value.checked_add(count)?;
                Some($name(new_value))
            }

            fn backward_checked(start: Self, count: usize) -> Option<Self> {
                let new_value = start.value.checked_sub(count)?;
                Some($name(new_value))
            }
        }
    };
}

define_line_unit!(Line);
define_line_unit!(ViewLine);



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

impl Add<LineDiff> for Line {
    type Output = Line;
    fn add(self, line_diff: LineDiff) -> Self::Output {
        if -line_diff.value > self.value as i32 {
            error!("Adding of LineDiff to Line resulted in negative value.");
            Line(0)
        } else {
            Line((self.value as i32 + line_diff.value) as usize)
        }
    }
}

impl Add<Line> for LineDiff {
    type Output = Line;
    fn add(self, line: Line) -> Self::Output {
        line + self
    }
}

impl Add<&LineDiff> for Line {
    type Output = Line;
    fn add(self, line_diff: &LineDiff) -> Self::Output {
        self + *line_diff
    }
}

impl Add<LineDiff> for &Line {
    type Output = Line;
    fn add(self, line_diff: LineDiff) -> Self::Output {
        *self + line_diff
    }
}

impl Add<&LineDiff> for &Line {
    type Output = Line;
    fn add(self, line_diff: &LineDiff) -> Self::Output {
        *self + *line_diff
    }
}

impl Add<LineDiff> for ViewLine {
    type Output = ViewLine;
    fn add(self, line_diff: LineDiff) -> Self::Output {
        if -line_diff.value > self.value as i32 {
            error!("Adding of LineDiff to ViewLine resulted in negative value.");
            ViewLine(0)
        } else {
            ViewLine((self.value as i32 + line_diff.value) as usize)
        }
    }
}

impl Add<ViewLine> for LineDiff {
    type Output = ViewLine;
    fn add(self, line: ViewLine) -> Self::Output {
        line + self
    }
}

impl Add<&LineDiff> for ViewLine {
    type Output = ViewLine;
    fn add(self, line_diff: &LineDiff) -> Self::Output {
        self + *line_diff
    }
}

impl Add<LineDiff> for &ViewLine {
    type Output = ViewLine;
    fn add(self, line_diff: LineDiff) -> Self::Output {
        *self + line_diff
    }
}

impl Add<&LineDiff> for &ViewLine {
    type Output = ViewLine;
    fn add(self, line_diff: &LineDiff) -> Self::Output {
        *self + *line_diff
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



// ====================
// === ViewLocation ===
// ====================

/// Alias for [`Location`] with [`ViewLine`] as line index.
pub type ViewLocation<Offset = Column> = Location<Offset, ViewLine>;
