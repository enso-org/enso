use crate::prelude::*;
use crate::text;

use crate::unit::*;

// ==============
// === Traits ===
// ==============

/// Perform conversion between two values. It is just like the [`From`] trait, but it performs the
/// conversion in a "context". The "context" is an object containing additional information required
/// to perform the conversion. For example, the context can be a text, which is needed to convert
/// byte offset to line-column location.
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



// =======================
// === Implementations ===
// =======================

impl FromInContextSnapped<&text::Rope, Line> for Byte {
    fn from_in_context_snapped(text: &text::Rope, line: Line) -> Self {
        text.byte_offset_of_line_index_snapped(line)
    }
}

impl FromInContextSnapped<&text::Rope, Location<Byte, Line>> for Byte {
    fn from_in_context_snapped(text: &text::Rope, location: Location<Byte, Line>) -> Self {
        text.byte_offset_of_line_index(location.line).unwrap() + location.offset
    }
}

impl<T> FromInContextSnapped<&text::Rope, Location<T, Line>> for Line {
    fn from_in_context_snapped(_: &text::Rope, location: Location<T, Line>) -> Self {
        location.line
    }
}

impl FromInContextSnapped<&text::Rope, Byte> for Line {
    fn from_in_context_snapped(buffer: &text::Rope, offset: Byte) -> Self {
        Location::<Byte, Line>::from_in_context_snapped(buffer, offset).line
    }
}

impl FromInContextSnapped<&text::Rope, Byte> for Location<Byte, Line> {
    fn from_in_context_snapped(context: &text::Rope, offset: Byte) -> Self {
        let line = context.line_index_of_byte_offset_snapped(offset);
        let line_offset = context.byte_offset_of_line_index_snapped(line);
        let byte_offset = Byte::try_from(offset - line_offset).unwrap();
        Location(line, byte_offset)
    }
}
