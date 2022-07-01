//! Source code abstraction.

use crate::prelude::*;



// ============
// === Code ===
// ============

/// A code representation. It can either be a borrowed source code or a modified owned one.
#[derive(Clone, Default, Eq, PartialEq, From, Into, Shrinkwrap, Serialize, Reflect, Deserialize)]
#[shrinkwrap(mutable)]
#[allow(missing_docs)]
pub struct Code<'s> {
    #[serde(serialize_with = "serialization::cow")]
    #[reflect(as = "serialization::Code", flatten)]
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

impl<'s> std::ops::AddAssign<Code<'s>> for Code<'s> {
    #[inline(always)]
    fn add_assign(&mut self, other: Code<'s>) {
        self.repr.add_assign(other.repr);
    }
}

impl<'s> std::ops::AddAssign<&Code<'s>> for Code<'s> {
    #[inline(always)]
    fn add_assign(&mut self, other: &Code<'s>) {
        self.repr.add_assign(other.repr.clone());
    }
}


// === Serialized Representation ===

mod serialization {
    use crate::prelude::*;

    /// Serialized representation of a source code `Cow`.
    #[derive(Serialize, Reflect)]
    pub(super) struct Code {
        #[reflect(hide)]
        begin: u32,
        #[reflect(hide)]
        len:   u32,
    }

    /// Serde wrapper to serialize a `Cow` as the `Code` representation.
    #[allow(clippy::ptr_arg)] // This is the signature required by serde.
    pub(super) fn cow<S>(cow: &Cow<'_, str>, ser: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        let s = match cow {
            Cow::Borrowed(s) => *s,
            Cow::Owned(_) => panic!(),
        };
        let begin = s.as_ptr() as u32;
        let len = s.len() as u32;
        let serializable = Code { begin, len };
        serializable.serialize(ser)
    }
}
