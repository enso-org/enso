//! Source code abstraction.

use crate::prelude::*;



// ============
// === Code ===
// ============

/// A code representation. It can either be a borrowed source code or a modified owned one.
#[derive(Clone, Deref, Default, DerefMut, Eq, PartialEq, From, Into)]
pub struct Code<'s> {
    cow: Cow<'s, str>,
}

impl<'s> Code<'s> {
    /// Length of the code in bytes.
    pub fn len(&self) -> Bytes {
        Bytes(self.cow.len())
    }
}

impl<'a> From<&'a str> for Code<'a> {
    fn from(str: &'a str) -> Self {
        let cow = str.into();
        Self { cow }
    }
}

impl<'s> Display for Code<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.cow, f)
    }
}

impl<'s> Debug for Code<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.cow, f)
    }
}

impl AsRef<str> for Code<'_> {
    fn as_ref(&self) -> &str {
        &self.cow
    }
}

impl std::borrow::Borrow<str> for Code<'_> {
    fn borrow(&self) -> &str {
        &self.cow
    }
}

impl<'a, 'b> PartialEq<&'b str> for Code<'a> {
    fn eq(&self, other: &&'b str) -> bool {
        self.cow.eq(other)
    }
}
