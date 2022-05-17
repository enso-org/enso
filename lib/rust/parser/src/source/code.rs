//! Source code abstraction.

use crate::prelude::*;



// ============
// === Code ===
// ============

/// A code representation. It can either be a borrowed source code or a modified owned one.
#[derive(Clone, Default, Eq, PartialEq, From, Into, Shrinkwrap)]
#[shrinkwrap(mutable)]
#[allow(missing_docs)]
pub struct Code<'s> {
    pub repr: Cow<'s, str>,
}

impl<'s> Code<'s> {
    /// Length of the code in bytes.
    #[inline(always)]
    pub fn len(&self) -> Bytes {
        Bytes(self.repr.len())
    }
}

impl<'a> From<&'a str> for Code<'a> {
    #[inline(always)]
    fn from(str: &'a str) -> Self {
        let repr = str.into();
        Self { repr }
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
