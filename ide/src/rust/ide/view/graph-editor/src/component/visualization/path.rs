//! Definition of name and path which define unique visualization definition location.

use crate::prelude::*;

use crate::data::enso;



// ============
// === Name ===
// ============

im_string_newtype!{
    /// Name of the visualization. You cannot define two visualizations of the same name in the
    /// same library.
    Name
}



// ============
// === Path ===
// ============

/// A fully qualified path of a visualization definition. Contains both the library name and the
/// visualization name.
#[derive(Clone,CloneRef,Debug,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub struct Path {
    pub library : enso::LibraryName,
    pub name    : Name,
}

impl Path {
    /// Constructor.
    pub fn new(library:impl Into<enso::LibraryName>, name:impl Into<Name>) -> Self {
        let library = library.into();
        let name    = name.into();
        Self {library,name}
    }

    /// Constructor for builtin visualizations.
    pub fn builtin(name:impl Into<Name>) -> Self {
        let library = enso::builtin_library();
        Self::new(library,name)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)
    }
}
