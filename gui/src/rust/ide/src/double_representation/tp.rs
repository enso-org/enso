//! Code for type double representation processing.

use crate::prelude::*;

use crate::double_representation::identifier::ReferentName;
use crate::double_representation::module;
use crate::double_representation::project;

use serde::Deserialize;
use serde::Serialize;



// ==============
// === Errors ===
// ==============

#[allow(missing_docs)]
#[derive(Clone, Debug, Fail)]
pub enum InvalidQualifiedName {
    #[fail(display = "The qualified name is empty.")]
    EmptyName { source: String },
    #[fail(display = "The qualified name has no namespace.")]
    NoNamespaceName { source: String },
    #[fail(display = "No module in type qualified name.")]
    NoModuleName { source: String },
}


// =====================
// === QualifiedName ===
// =====================

/// Type's qualified name is used in some of the Language Server's APIs, like
/// `MethodPointer`. It may represent a type defined in a module, or the module itself.
///
/// Qualified name is constructed as follows:
/// `ProjectName.<sequence_of_module_names>.<entity_name>`. The `sequence_of_module_names` may be
/// empty in case of module in project's `src` directory.
///
/// See https://dev.enso.org/docs/distribution/packaging.html for more information about the
/// package structure.
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(into = "String")]
#[serde(try_from = "String")]
pub struct QualifiedName {
    /// The first segment in the full qualified name.
    pub project_name:    project::QualifiedName,
    /// All segments between the project name (the first) and the entity name (the last).
    pub module_segments: Vec<ReferentName>,
    /// The last segment in the full qualified name.
    pub name:            String,
}

impl QualifiedName {
    /// Create from the module's qualified name.
    pub fn from_module(module: module::QualifiedName) -> Self {
        let module::QualifiedName { project_name, id } = module;
        let mut module_segments = id.into_segments();
        // We may unwrap, because the `module::QualifiedName` guarantees segments to be non-empty.
        let name = module_segments.pop().unwrap().into();
        QualifiedName { project_name, module_segments, name }
    }

    /// Create from the module's qualified name.
    pub fn new_module_member(module: module::QualifiedName, name: String) -> Self {
        let module::QualifiedName { project_name, id } = module;
        let module_segments = id.into_segments();
        QualifiedName { project_name, module_segments, name }
    }

    /// Create from a text representation. May fail if the text is not valid Qualified name of any
    /// type.
    pub fn from_text(text: impl Str) -> FallibleResult<Self> {
        use InvalidQualifiedName::*;
        let text: String = text.into();
        let mut all_segments = text.split('.');
        let namespace = all_segments.next().ok_or_else(|| EmptyName { source: text.clone() })?;
        let project_name =
            all_segments.next().ok_or_else(|| NoNamespaceName { source: text.clone() })?;
        let project_name = project::QualifiedName::from_segments(namespace, project_name)?;
        let name_str =
            all_segments.next_back().ok_or_else(|| NoModuleName { source: text.clone() })?;
        let name = name_str.to_owned();
        let mut module_segments = Vec::new();
        for segment in all_segments {
            module_segments.push(ReferentName::new(segment)?);
        }
        Ok(QualifiedName { project_name, module_segments, name })
    }

    /// Check if the name is defined directly in the given module.
    pub fn in_module(&self, module: &module::QualifiedName) -> bool {
        self.project_name == module.project_name && &self.module_segments == module.id.segments()
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

impl From<module::QualifiedName> for QualifiedName {
    fn from(name: module::QualifiedName) -> Self {
        Self::from_module(name)
    }
}

impl From<QualifiedName> for String {
    fn from(name: QualifiedName) -> Self {
        String::from(&name)
    }
}

impl From<&QualifiedName> for String {
    fn from(name: &QualifiedName) -> Self {
        let project_name = name.project_name.segments();
        let segments = name.module_segments.iter().map(AsRef::<str>::as_ref);
        let name = std::iter::once(name.name.as_ref());
        project_name.chain(segments).chain(name).join(".")
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = String::from(self);
        fmt::Display::fmt(&text, f)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    use crate::double_representation::tp::QualifiedName;


    #[test]
    fn qualified_name_from_string() {
        let valid_case =
            |text: &str, ns_name: &str, project_name: &str, segments: Vec<&str>, name: &str| {
                let project_name =
                    project::QualifiedName::from_segments(ns_name, project_name).unwrap();
                let result = QualifiedName::from_text(text).unwrap();
                assert_eq!(result.project_name, project_name);
                assert_eq!(result.module_segments, segments);
                assert_eq!(result.name, name);
            };

        let invalid_case = |text: &str| {
            assert!(QualifiedName::from_text(text).is_err());
        };

        valid_case("local.Project.Main.Test.foo", "local", "Project", vec!["Main", "Test"], "foo");
        valid_case("local.Project.Main.Bar", "local", "Project", vec!["Main"], "Bar");
        valid_case("local.Project.Baz", "local", "Project", vec![], "Baz");

        invalid_case("local");
        invalid_case("local.Project");
        invalid_case("local.Project.module.foo");
        invalid_case("...");
        invalid_case("");
    }
}
