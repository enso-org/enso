//! Enso language source code related utilities, including a structure attaching source code to
//! other types or an abstraction allowing for getting the representation of an entity, such as
//! [`Token`] (tokens remember the location only, in order to get their representation, the source
//! code needs to be sampled).

use crate::prelude::*;

pub mod span;



// ============
// === With ===
// ============

/// Structure used to keep an element [`T`] associated with source code.
///
/// # Pretty printing
/// Please note, that neither [`Token`] nor [`Ast`] contain sources, it keeps track of the char
/// offsets only. If you want to pretty print it, you should attach sources to it. The easiest way
/// to do it is by using the [`With`] data, for example as:
/// ```text
/// println!("{:#?}", source::With::new(str, &ast));
/// ```
#[derive(Deref, DerefMut, PartialEq)]
#[allow(missing_docs)]
pub struct With<'s, T> {
    #[deref]
    #[deref_mut]
    pub data: T,
    pub code: &'s str,
}

/// Constructor.
pub fn With<'s, T>(code: &'s str, data: T) -> With<'s, T> {
    With { code, data }
}

impl<'s, T: Debug> Debug for With<'s, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[repr: \"{}\"] ", self.code)?;
        Debug::fmt(&self.data, f)
    }
}

impl<'s, T: PartialEq> PartialEq<With<'s, T>> for &With<'s, T> {
    fn eq(&self, other: &With<'s, T>) -> bool {
        <With<'s, T> as PartialEq<With<'s, T>>>::eq(*self, other)
    }
}


impl<'s, T> With<'s, span::With<T>> {
    pub fn split_at(&self, offset: Bytes) -> (With<'s, span::With<()>>, With<'s, span::With<()>>) {
        let (left, right) = self.data.split_at(offset);
        let left = With(&self.code.slice(Bytes::from(0)..offset), left);
        let right = With(&self.code.slice(offset..), right);
        (left, right)
    }

    pub fn with<S>(self, elem: S) -> With<'s, span::With<S>> {
        With(self.code, self.data.with_elem(elem))
    }
}
