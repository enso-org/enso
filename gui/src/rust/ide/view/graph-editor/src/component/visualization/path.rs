//! Definition of name and path which define unique visualization definition location.

use crate::prelude::*;

use crate::data::enso;

use serde::Deserialize;
use serde::Serialize;



// ============
// === Name ===
// ============

im_string_newtype! {
    /// Name of the visualization. You cannot define two visualizations of the same name in the
    /// same library.
    Name
}

/// Identifier to the project owning the visualizaiton.
#[derive(Clone, CloneRef, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub enum Project {
    /// Temporary placeholder for the visualizations embedded in the IDE.
    /// Eventually will be replaced with Standard Library.
    Builtin,
    /// The current project (i.e. the project that user has open in the project manager).
    CurrentProject,
    /// An external library (i.e. the dependency of the current project).
    Library(enso::LibraryName),
}



// ============
// === Path ===
// ============

/// A fully qualified path of a visualization definition. Contains both the library name and the
/// visualization name.
#[derive(Clone, CloneRef, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
#[allow(missing_docs)]
pub struct Path {
    pub project: Project,
    pub name:    Name,
}

impl Path {
    /// Constructor.
    pub fn new(project: Project, name: impl Into<Name>) -> Self {
        let name = name.into();
        Self { project, name }
    }

    /// Constructor for builtin visualizations.
    pub fn builtin(name: impl Into<Name>) -> Self {
        Self::new(Project::Builtin, name)
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)
    }
}

impl From<Path> for String {
    fn from(path: Path) -> Self {
        path.to_string()
    }
}
