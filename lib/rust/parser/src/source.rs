use crate::prelude::*;

use crate::location;



pub mod traits {
    pub use super::HasRepr;
}



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
#[derive(Deref, DerefMut)]
#[allow(missing_docs)]
pub struct With<'s, T> {
    #[deref]
    #[deref_mut]
    pub data:   T,
    pub source: &'s str,
}

impl<'s, T> With<'s, T> {
    /// Constructor.
    pub fn new(source: &'s str, data: T) -> Self {
        Self { source, data }
    }

    /// Attaches the sources to the newly provided data.
    pub fn with_data<S>(&self, data: S) -> With<'s, S> {
        let source = self.source;
        With { source, data }
    }
}

impl<'s, T> Debug for With<'s, &NonEmptyVec<T>>
where for<'t> With<'s, &'t T>: Debug
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter().map(|t| self.with_data(t))).finish()
    }
}



// ===============
// === HasRepr ===
// ===============

/// Abstraction for elements that have source code representation. For example, [`Ast`] nodes that
/// use the [`With`] structure implement [`HasRepr`] because they can be queried for their textual
/// representation.
#[allow(missing_docs)]
pub trait HasRepr<'s> {
    fn repr(&self) -> &'s str;
}

impl<'s, T> HasRepr<'s> for With<'s, location::With<T>> {
    fn repr(&self) -> &'s str {
        self.source_slice(&self.source)
    }
}

impl<'s, T> HasRepr<'s> for With<'s, &location::With<T>> {
    fn repr(&self) -> &'s str {
        self.source_slice(&self.source)
    }
}
