//! Source code abstraction.

use crate::prelude::*;



// ============
// === Code ===
// ============

/// Wrap a `&str` that is skipped while deserializing; serde doesn't allow a custom deserializer to
/// produce a `&'s str` without borrowing from the input.
#[derive(Debug, Clone, Default, Eq, PartialEq, Deref)]
pub struct StrRef<'s>(pub &'s str);

/// Identifies a location in source code.
#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    Eq,
    PartialEq,
    Serialize,
    Reflect,
    Deserialize,
    PartialOrd,
    Ord
)]
pub struct Location {
    /// Offset from the beginning, in UTF-8 code units (bytes).
    #[reflect(hide)]
    pub utf8:  u32,
    /// Offset from the beginning, in UTF-16 code units (two-byte words).
    #[reflect(hide)]
    pub utf16: u32,
    /// Line number, starting from 0. The recognized line terminators are CR, LF, or CRLF.
    #[reflect(hide)]
    pub line:  u32,
    /// Offset from start of line, in UTF-16 code units.
    #[reflect(hide)]
    pub col16: u32,
}

impl Add<Length> for Location {
    type Output = Self;

    fn add(self, rhs: Length) -> Self::Output {
        Self {
            utf8:  self.utf8 + rhs.utf8,
            utf16: self.utf16 + rhs.utf16,
            line:  self.line + rhs.newlines,
            col16: if rhs.newlines == 0 { self.col16 } else { 0 } + rhs.line_chars16,
        }
    }
}

/// A code representation.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Reflect, Deserialize, Deref)]
#[allow(missing_docs)]
pub struct Code<'s> {
    /// The borrowed string data.
    #[serde(serialize_with = "crate::serialization::serialize_cow")]
    #[serde(deserialize_with = "crate::serialization::deserialize_cow")]
    #[reflect(as = "crate::serialization::Code", flatten, hide)]
    #[deref]
    pub repr: StrRef<'s>,
    #[reflect(flatten)]
    start:    Location,
    /// The length of the source code.
    #[reflect(flatten)]
    pub len:  Length,
}

impl<'s> Code<'s> {
    /// Return a code reference from the given source and offset within the document.
    #[inline(always)]
    pub fn from_str_at_location(repr: &'s str, location: Location) -> Self {
        let len = Length::of(repr);
        let repr = StrRef(repr);
        Self { repr, start: location, len }
    }

    /// Return a code reference at the beginning of the document. This can be used in testing, when
    /// accurate code references are not needed.
    #[inline(always)]
    pub fn from_str_without_location(repr: &'s str) -> Self {
        Self::from_str_at_location(repr, default())
    }

    /// Return a copy of this value, and set this value to a 0-length value following the returned
    /// value.
    #[inline(always)]
    pub fn take_as_prefix(&mut self) -> Self {
        let end = self.start + self.len;
        Self {
            repr:  mem::take(&mut self.repr),
            start: mem::replace(&mut self.start, end),
            len:   mem::take(&mut self.len),
        }
    }

    /// Return a 0-length `Code` located immediately before the start of this `Code`.
    pub fn position_before(&self) -> Self {
        Self { repr: default(), start: self.start, len: default() }
    }

    /// Return a 0-length `Code` located immediately after the end of this `Code`.
    pub fn position_after(&self) -> Self {
        Self { repr: default(), start: self.start + self.len, len: default() }
    }

    /// Return the start and end of the UTF-16 source code for this element.
    pub fn range(&self) -> Range<Location> {
        self.start..(self.start + self.len)
    }

    /// Split the code at the given location.
    pub fn split_at(&self, split: Length) -> (Self, Self) {
        let (left, right) = self.repr.split_at(usize::try_from(split.utf8).unwrap());
        let right_len = Length {
            utf8:         self.len.utf8 - split.utf8,
            utf16:        self.len.utf16 - split.utf16,
            newlines:     self.len.newlines - split.newlines,
            line_chars16: self.len.line_chars16
                - if split.newlines == 0 { split.line_chars16 } else { 0 },
        };
        (Self { repr: StrRef(left), start: self.start, len: split }, Self {
            repr:  StrRef(right),
            start: self.start + split,
            len:   right_len,
        })
    }

    /// Return a reference to an empty string, not associated with any location in the document.
    pub fn empty_without_location() -> Self {
        Self { repr: StrRef(""), start: default(), len: default() }
    }

    /// Return a reference to an empty string.
    pub fn empty(location: Location) -> Self {
        Self { repr: StrRef(""), start: location, len: default() }
    }

    /// Length of the code in bytes.
    #[inline(always)]
    pub fn len(&self) -> Bytes {
        Bytes(self.repr.len())
    }

    /// Length of the code.
    #[inline(always)]
    pub fn length(&self) -> Length {
        self.len
    }

    /// True if the code is the empty string.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.repr.is_empty()
    }

    /// Return this value with its start position removed (set to 0). This can be used to compare
    /// values ignoring offsets.
    pub fn without_location(&self) -> Self {
        Self { repr: self.repr.clone(), start: default(), len: self.len }
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
                // The span builder works by starting with `Span::empty_without_location()`, and
                // appending to the right side. In order to ensure every span has a location: When
                // the LHS is empty, take the location from the RHS even if the RHS is also empty.
                self.start = other.start;
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
                self.len += other.len;
            }
        }
    }
}


// === Code length ===

/// The length of a [`Code`] object.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Serialize, Reflect, Deserialize)]
pub struct Length {
    /// An offset, in UTF-8 code units (bytes).
    #[reflect(skip)]
    #[serde(skip)]
    pub utf8:         u32,
    /// An offset, in UTF-16 code units (two-byte words).
    pub utf16:        u32,
    /// A difference in line numbers.
    #[reflect(hide)]
    pub newlines:     u32,
    /// If `newlines` is 0, this is the difference in UTF-16 code-unit positions within a line; if
    /// `newlines` is nonzero, this is the position within the line ending the range.
    pub line_chars16: u32,
}

impl Length {
    /// Returns the length of the given input.
    #[inline(always)]
    pub fn of(s: &str) -> Self {
        let mut utf16 = 0;
        let mut newlines = 0;
        let mut line_chars16 = 0;
        let mut prev = None;
        for c in s.chars() {
            let char_len16 = c.len_utf16() as u32;
            utf16 += char_len16;
            line_chars16 += char_len16;
            if c == '\r' || c == '\n' {
                line_chars16 = 0;
            }
            if c == '\r' || (c == '\n' && prev != Some('\r')) {
                newlines += 1;
            }
            prev = Some(c);
        }
        Self { utf8: u32::try_from(s.len()).unwrap(), utf16, newlines, line_chars16 }
    }

    /// Returns true if the code is empty.
    #[inline(always)]
    pub fn is_zero(&self) -> bool {
        self.utf8 == 0
    }

    /// Return the length in UTF-8 code units (bytes).
    #[inline(always)]
    pub fn utf8_bytes(&self) -> u32 {
        self.utf8
    }

    /// Return the length in UTF-16 code units.
    #[inline(always)]
    pub fn utf16_len(&self) -> u32 {
        self.utf16
    }
}

impl Add for Length {
    type Output = Length;

    #[inline(always)]
    fn add(self, rhs: Self) -> Self::Output {
        let Self { utf8, utf16, newlines, line_chars16 } = self;
        Self {
            utf8:         utf8 + rhs.utf8,
            utf16:        utf16 + rhs.utf16,
            newlines:     newlines + rhs.newlines,
            line_chars16: if rhs.newlines == 0 { line_chars16 } else { 0 } + rhs.line_chars16,
        }
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



// ====================
// === Test support ===
// ====================

/// Testing/debugging helpers.
pub mod debug {
    use super::*;
    use std::collections::BTreeMap;

    /// Checks consistency of observed `Location`s. Compares `line:col` values against values found
    /// in an independent scan of the input source code.
    #[derive(Debug, Default)]
    pub struct LocationCheck {
        locations: BTreeMap<u32, Location>,
    }

    impl LocationCheck {
        /// Create a new empty checker.
        pub fn new() -> Self {
            Self::default()
        }

        /// Add the location to the collection waiting to be checked.
        pub fn add(&mut self, location: Location) {
            self.locations.insert(location.utf8, location);
        }

        /// Add multiple locations to the collection waiting to be checked.
        pub fn extend(&mut self, locations: &[Location]) {
            self.locations.extend(locations.iter().map(|loc| (loc.utf8, *loc)));
        }

        /// Check all previously-added locations for consistency with the input.
        pub fn check(mut self, input: &str) {
            let mut pos = Location::default();
            let mut prev = None;
            for (i, c) in input.char_indices() {
                pos.utf8 = i as u32;
                if let Some(loc) = self.locations.remove(&(i as u32)) {
                    assert_eq!(loc, pos);
                }
                let char_len = c.len_utf16() as u32;
                pos.utf16 += char_len;
                pos.col16 += char_len;
                if c == '\r' || c == '\n' {
                    pos.col16 = 0;
                }
                if c == '\r' || (c == '\n' && prev != Some('\r')) {
                    pos.line += 1;
                }
                prev = Some(c);
            }
            if let Some(loc) = self.locations.remove(&(input.len() as u32)) {
                pos.utf8 = input.len() as u32;
                assert_eq!(loc, pos);
            }
            let non_char_boundary_locations: Vec<_> = self.locations.values().cloned().collect();
            assert_eq!(&non_char_boundary_locations, &[]);
        }
    }
}
