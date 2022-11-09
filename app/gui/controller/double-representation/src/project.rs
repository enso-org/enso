//! A set of structures describing Project for double representation.

use crate::prelude::*;

use const_format::concatcp;
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
    #[fail(display = "No namespace in project qualified name.")]
    NoNamespace { source: String },
    #[fail(display = "Invalid namespace in project qualified name.")]
    InvalidNamespace { source: String },
    #[fail(display = "Too many segments in project qualified name.")]
    TooManySegments { source: String },
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
}
