//! Definition of strongly typed indexes, like `Line` or `Byte`. Please note that units, such as
//! `ByteDiff` are defined in a separate module.

use crate::prelude::*;
use crate::unit::*;



// ===============
// === Exports ===
// ===============

/// Common traits.
pub mod traits {
    pub use super::byte::Into as TRAIT_ubytes_into;
}
pub use traits::*;



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
        if value < 0 {
            error!("Addition of ByteDiff resulted in negative value.");
            Byte(0)
        } else {
            Byte(value as usize)
        }
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

impl Sub<Bytes> for Byte {
    type Output = Byte;
    fn sub(self, rhs: Bytes) -> Self::Output {
        let value = self.value as i32 - rhs.value as i32;
        if value < 0 {
            error!("Subtraction of Bytes resulted in negative value.");
            Byte(0)
        } else {
            Byte(value as usize)
        }
    }
}

impl Add<Bytes> for Byte {
    type Output = Byte;
    fn add(self, rhs: Bytes) -> Self::Output {
        let value = self.value + rhs.value;
        Byte(value)
    }
}

impl AddAssign<Bytes> for Byte {
    fn add_assign(&mut self, rhs: Bytes) {
        *self = *self + rhs
    }
}

impl SubAssign<Bytes> for Byte {
    fn sub_assign(&mut self, rhs: Bytes) {
        *self = *self - rhs
    }
}

impl Byte {
    /// Convert byte index to byte diff value.
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

impl iter::Step for Byte {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        let diff = end.value - start.value;
        Some(diff)
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        let new_value = start.value.checked_add(count)?;
        Some(Byte(new_value))
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        let new_value = start.value.checked_sub(count)?;
        Some(Byte(new_value))
    }
}



// ============
// === Line ===
// ============

/// Defines a line-like index unit.
#[macro_export]
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

define_line_unit!(Line);

impl Line {
    /// Convert the line to line diff.
    pub fn to_diff(self) -> LineDiff {
        LineDiff(self.value as i32)
    }
}
