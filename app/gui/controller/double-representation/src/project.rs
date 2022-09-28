//! A set of structures describing Project for double representation.

use crate::prelude::*;

use crate::identifier::ReferentName;

use const_format::concatcp;
use serde::Deserialize;
use serde::Serialize;



// =================
// === Constants ===
// =================

/// The namespace of the standard library.
pub const STANDARD_NAMESPACE: &str = "Standard";

/// The name of the project in the [`STANDARD_NAMESPACE`] containing the base standard library.
pub const BASE_LIBRARY_NAME: &str = "Base";

/// The full path of the [`BASE_LIBRARY_NAME`] project in the [`STANDARD_NAMESPACE`].
pub const STANDARD_BASE_LIBRARY_PATH: &str = concatcp!(STANDARD_NAMESPACE, ".", BASE_LIBRARY_NAME);

/// The identifier of the project's main module.
pub fn main_module_id() -> crate::module::Id {
    // We can just assume that `PROJECTS_MAIN_MODULE` is valid. This is verified by a test.
    crate::module::Id::try_new([ast::constants::PROJECTS_MAIN_MODULE]).unwrap()
}

// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail)]
pub enum InvalidQualifiedName {
    #[fail(display = "The qualified name is empty.")]
    EmptyName { source: String },
    #[fail(display = "No namespace in project qualified name.")]
    NoNamespace { source: String },
    #[fail(display = "Invalid namespace in project qualified name.")]
    InvalidNamespace { source: String },
    #[fail(display = "Too many segments in project qualified name.")]
    TooManySegments { source: String },
}


// =====================
// === QualifiedName ===
// =====================

/// The project qualified name has a form of `<namespace_name>.<project_name>`. It serves as
/// a prefix for qualified names of other entities (modules, types, etc.).
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(into = "String")]
#[serde(try_from = "String")]
pub struct QualifiedName {
    /// The name of project's namespace.
    pub namespace: String,
    /// The actual project name.
    pub project:   ReferentName,
}

impl QualifiedName {
    /// Create qualified name from typed components.
    pub fn new(namespace: String, project: ReferentName) -> Self {
        Self { namespace, project }
    }

    /// Create qualified name from string segments. May fail if the segments are invalid.
    pub fn from_segments(
        namespace: impl Into<String>,
        project: impl Into<String>,
    ) -> FallibleResult<Self> {
        let namespace = namespace.into();
        if namespace.is_empty() {
            let source = format!("{}.{}", namespace, project.into());
            Err(InvalidQualifiedName::InvalidNamespace { source }.into())
        } else {
            let project = ReferentName::new(project.into())?;
            Ok(Self { namespace, project })
        }
    }

    /// Create from a text representation. May fail if the text is not valid Qualified.
    pub fn from_text(text: impl Into<String>) -> FallibleResult<Self> {
        let source: String = text.into();
        let all_segments = source.split('.').collect_vec();
        match all_segments.as_slice() {
            [namespace, project] => Self::from_segments(*namespace, *project),
            [] => Err(InvalidQualifiedName::EmptyName { source }.into()),
            [_] => Err(InvalidQualifiedName::NoNamespace { source }.into()),
            _ => Err(InvalidQualifiedName::TooManySegments { source }.into()),
        }
    }

    /// Return the fully qualified name of the [`BASE_LIBRARY_NAME`] project in the
    /// [`STANDARD_NAMESPACE`].
    pub fn standard_base_library() -> Self {
        Self::from_segments(STANDARD_NAMESPACE, BASE_LIBRARY_NAME).unwrap()
    }

    /// The iterator over name's segments: the namespace and project name.
    pub fn segments(&self) -> impl Iterator<Item = &str> {
        std::iter::once(self.namespace.as_ref()).chain(std::iter::once(self.project.as_ref()))
    }
}


// === Conversions ===

impl TryFrom<&str> for QualifiedName {
    type Error = failure::Error;

    fn try_from(text: &str) -> Result<Self, Self::Error> {
        Self::from_text(text)
    }
}

impl TryFrom<String> for QualifiedName {
    type Error = failure::Error;

    fn try_from(text: String) -> Result<Self, Self::Error> {
        Self::from_text(text)
    }
}

impl From<QualifiedName> for String {
    fn from(name: QualifiedName) -> Self {
        String::from(&name)
    }
}

impl From<&QualifiedName> for String {
    fn from(name: &QualifiedName) -> Self {
        name.to_string()
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.namespace, self.project)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn qualified_name_from_string() {
        fn valid_case(text: &str, namespace: &str, project: &str) {
            let qualified_name = QualifiedName::from_text(text).unwrap();
            assert_eq!(qualified_name.namespace, namespace);
            assert_eq!(qualified_name.project, project);
        }

        fn invalid_case(text: &str) {
            assert!(QualifiedName::from_text(text).is_err());
        }

        valid_case("ns.Project", "ns", "Project");
        valid_case("n.Proj", "n", "Proj");

        invalid_case("namespace");
        invalid_case("Project");
        invalid_case("namespace.project");
        invalid_case("namespace.Project.Main");
        invalid_case(".Project");
        invalid_case("namespace.");
        invalid_case(".");
    }

    #[test]
    fn qualified_name_of_standard_base_library_does_not_panic() {
        let _ = QualifiedName::standard_base_library();
    }

    #[test]
    fn main_module_id_test() {
        // Should not panic.
        main_module_id();
    }
}
