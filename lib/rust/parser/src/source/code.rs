//! Source code abstraction.

use crate::prelude::*;



// ============
// === Code ===
// ============

/// A code representation. It can either be a borrowed source code or a modified owned one.
#[derive(Clone, Default, Eq, PartialEq, Serialize, Reflect, Deserialize, Deref)]
#[allow(missing_docs)]
pub struct Code<'s> {
    #[serde(serialize_with = "crate::serialization::serialize_cow")]
    #[serde(deserialize_with = "crate::serialization::deserialize_cow")]
    #[reflect(as = "crate::serialization::Code", flatten, hide)]
    #[deref]
    pub repr:  Cow<'s, str>,
    #[reflect(hide)]
    pub offset_utf16: u32,
    #[reflect(hide)]
    pub utf16: u32,
}

impl<'s> Code<'s> {
    #[inline(always)]
    pub fn from_str_at_offset(repr: impl Into<Cow<'s, str>>, offset_utf16: u32) -> Self {
        let repr = repr.into();
        let utf16 = repr.encode_utf16().count() as u32;
        Self { repr, offset_utf16, utf16 }
    }

    #[inline(always)]
    pub fn from_str_without_offset(repr: impl Into<Cow<'s, str>>) -> Self {
        Self::from_str_at_offset(repr, 0)
    }

    pub fn split_at(&self, offset: usize) -> (Self, Self) {
        let (left, right) = match self.repr {
            Cow::Borrowed(s) => s.split_at(offset),
            Cow::Owned(_) => panic!("Unsupported: Splitting owned cows."),
        };
        let left_utf16 = left.encode_utf16().count() as u32;
        let right_utf16 = self.utf16 - left_utf16;
        (Self {
            repr: Cow::Borrowed(left),
            offset_utf16: self.offset_utf16,
            utf16: left_utf16,
        },
        Self {
             repr: Cow::Borrowed(right),
             offset_utf16: self.offset_utf16 + left_utf16,
             utf16: right_utf16,
         },
        )
    }

    pub fn empty() -> Self {
        Self {
            repr: "".into(),
            offset_utf16: 0,
            utf16: 0,
        }
    }

    /// Length of the code in bytes.
    #[inline(always)]
    pub fn len(&self) -> Bytes {
        Bytes(self.repr.len())
    }

    /// Length of the code.
    #[inline(always)]
    pub fn length(&self) -> Length {
        Length { utf8: self.repr.len(), utf16: self.utf16 as usize }
    }

    /// True if the code is the empty string.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.repr.is_empty()
    }
}

impl<'s> Display for Code<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.repr, f)
    }
}

impl<'s> Debug for Code<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.repr, f)
    }
}

impl<'a, 'b> PartialEq<&'b str> for Code<'a> {
    #[inline(always)]
    fn eq(&self, other: &&'b str) -> bool {
        self.repr.eq(other)
    }
}

impl AsRef<str> for Code<'_> {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        &self.repr
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
        self.repr.add_assign(other.repr);
        self.utf16.add_assign(other.utf16);
    }
}

impl<'s> AddAssign<&Code<'s>> for Code<'s> {
    #[inline(always)]
    fn add_assign(&mut self, other: &Code<'s>) {
        self.repr.add_assign(other.repr.clone());
        self.utf16.add_assign(other.utf16);
    }
}


// === Code length ===

/// The length of a [`Code`] object.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Serialize, Reflect, Deserialize)]
pub struct Length {
    #[reflect(skip)]
    #[serde(skip)]
    utf8:  usize,
    utf16: usize,
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
