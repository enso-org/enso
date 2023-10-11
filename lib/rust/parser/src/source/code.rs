//! Source code abstraction.

use crate::prelude::*;



// ============
// === Code ===
// ============

/// Wrap a `&str` that is skipped while deserializing; serde doesn't allow a custom deserializer to
/// produce a `&'s str` without borrowing from the input.
#[derive(Debug, Clone, Default, Eq, PartialEq, Deref)]
pub struct StrRef<'s>(pub &'s str);

/// A code representation. It can either be a borrowed source code or a modified owned one.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Reflect, Deserialize, Deref)]
#[allow(missing_docs)]
pub struct Code<'s> {
    #[serde(serialize_with = "crate::serialization::serialize_cow")]
    #[serde(deserialize_with = "crate::serialization::deserialize_cow")]
    #[reflect(as = "crate::serialization::Code", flatten, hide)]
    #[deref]
    pub repr:     StrRef<'s>,
    #[reflect(hide)]
    offset_utf16: u32,
    #[reflect(hide)]
    utf16:        u32,
}

impl<'s> Code<'s> {
    /// Return a code reference from the given source and offset within the document.
    #[inline(always)]
    pub fn from_str_at_offset(repr: &'s str, offset_utf16: u32) -> Self {
        let utf16 = repr.chars().map(|c| c.len_utf16() as u32).sum();
        let repr = StrRef(repr);
        Self { repr, offset_utf16, utf16 }
    }

    /// Return a code reference at the beginning of the document. This can be used in testing, when
    /// accurate code references are not needed.
    #[inline(always)]
    pub fn from_str_without_offset(repr: &'s str) -> Self {
        Self::from_str_at_offset(repr, 0)
    }

    /// Return a copy of this value, and set this value to a 0-length value following the returned
    /// value.
    #[inline(always)]
    pub fn take_as_prefix(&mut self) -> Self {
        let end = self.offset_utf16 + self.utf16;
        Self {
            repr:         mem::take(&mut self.repr),
            offset_utf16: mem::replace(&mut self.offset_utf16, end),
            utf16:        mem::take(&mut self.utf16),
        }
    }

    /// Return a 0-length `Code` located immediately before the start of this `Code`.
    pub fn position_before(&self) -> Self {
        Self { repr: default(), offset_utf16: self.offset_utf16, utf16: default() }
    }

    /// Return a 0-length `Code` located immediately after the end of this `Code`.
    pub fn position_after(&self) -> Self {
        Self {
            repr:         default(),
            offset_utf16: self.offset_utf16 + self.utf16,
            utf16:        default(),
        }
    }

    /// Return the length in UTF-16 code units.
    pub fn len_utf16(&self) -> u32 {
        self.utf16
    }

    /// Split the UTF-8 code at the given byte offset.
    pub fn split_at(&self, offset: usize) -> (Self, Self) {
        let (left, right) = self.repr.split_at(offset);
        let left_utf16 = left.chars().map(|c| c.len_utf16() as u32).sum();
        let right_utf16 = self.utf16 - left_utf16;
        (
            Self {
                repr:         StrRef(left),
                offset_utf16: self.offset_utf16,
                utf16:        left_utf16,
            },
            Self {
                repr:         StrRef(right),
                offset_utf16: self.offset_utf16 + left_utf16,
                utf16:        right_utf16,
            },
        )
    }

    /// Return a reference to an empty string, not associated with any location in the document.
    pub fn empty_without_offset() -> Self {
        Self { repr: StrRef(""), offset_utf16: 0, utf16: 0 }
    }

    /// Return a reference to an empty string.
    pub fn empty(offset: u32) -> Self {
        Self { repr: StrRef(""), offset_utf16: offset, utf16: 0 }
    }

    /// Length of the code in bytes.
    #[inline(always)]
    pub fn len(&self) -> Bytes {
        Bytes(self.repr.len())
    }

    /// Length of the code.
    #[inline(always)]
    pub fn length(&self) -> Length {
        Length { utf8: self.repr.len(), utf16: self.utf16 }
    }

    /// True if the code is the empty string.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.repr.is_empty()
    }

    /// Return this value with its start position removed (set to 0). This can be used to compare
    /// values ignoring offsets.
    pub fn without_offset(&self) -> Self {
        Self { repr: self.repr.clone(), offset_utf16: default(), utf16: self.utf16 }
    }
}

impl<'s> Display for Code<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&*self.repr, f)
    }
}

impl<'a, 'b> PartialEq<&'b str> for Code<'a> {
    #[inline(always)]
    fn eq(&self, other: &&'b str) -> bool {
        self.repr.0.eq(*other)
    }
}

impl AsRef<str> for Code<'_> {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.repr.0
    }
}

impl std::borrow::Borrow<str> for Code<'_> {
    #[inline(always)]
    fn borrow(&self) -> &str {
        &self.repr
    }
}

impl<'s> AddAssign<Code<'s>> for Code<'s> {
    #[inline(always)]
    fn add_assign(&mut self, other: Code<'s>) {
        self.add_assign(&other)
    }
}

impl<'s> AddAssign<&Code<'s>> for Code<'s> {
    #[inline(always)]
    fn add_assign(&mut self, other: &Code<'s>) {
        match (self.is_empty(), other.is_empty()) {
            (false, true) => (),
            (true, true) => {
                // The span builder works by starting with `Span::empty_without_offset()`, and
                // appending to the right side. In order to ensure every span has an offset: When
                // the LHS is empty, take the location from the RHS even if the RHS is also empty.
                self.offset_utf16 = other.offset_utf16;
            }
            (true, false) => {
                *self = other.clone();
            }
            (false, false) => {
                let range = self.repr.as_bytes().as_ptr_range();
                #[allow(unsafe_code)] // See comments in block.
                unsafe {
                    // Combining two slices is sound if:
                    // - They have the same element lifetime (ensured by the type signature).
                    // - The second ends where the first begins (checked below).
                    assert_eq!(range.end, other.repr.as_ptr());
                    let joined =
                        slice::from_raw_parts(range.start, self.repr.len() + other.repr.len());
                    // Concatenating two UTF-8 strings always yields a valid UTF-8 string.
                    self.repr = StrRef(std::str::from_utf8_unchecked(joined));
                }
                self.utf16 += other.utf16;
            }
        }
    }
}


// === Code length ===

/// The length of a [`Code`] object.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Serialize, Reflect, Deserialize)]
pub struct Length {
    #[reflect(skip)]
    #[serde(skip)]
    utf8:  usize,
    utf16: u32,
}

impl Length {
    /// Returns true if the code is empty.
    #[inline(always)]
    pub fn is_zero(&self) -> bool {
        self.utf8 == 0
    }

    /// Return the length in UTF-8 code units (bytes).
    #[inline(always)]
    pub fn utf8_bytes(&self) -> usize {
        self.utf8
    }
}

impl Add for Length {
    type Output = Length;

    #[inline(always)]
    fn add(self, rhs: Self) -> Self::Output {
        let Self { utf8, utf16 } = self;
        Self { utf8: utf8 + rhs.utf8, utf16: utf16 + rhs.utf16 }
    }
}

impl AddAssign for Length {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Display for Length {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.utf8)
    }
}
