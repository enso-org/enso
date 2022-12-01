//! Model definitions for [`Project`] and related types.

use enso_prelude::*;

use crate::id;



// ================
// === StateTag ===
// ================

/// Enum representing what state a Cloud backend [`Project`] is in.
///
/// A [`Project`]'s state has a lot of implementation details that are irrelevant to the Cloud
/// frontend. So this enum contains only the tag used to discriminate between different variants.
/// This is returned in API responses from the backend where the content of the state is irrelevant
/// or should not be public, and we're just interested in *which* state the [`Project`] is in.
#[derive(Debug, serde::Deserialize, Eq, serde::Serialize, Clone, Copy, PartialEq)]
#[serde(tag = "type")]
#[allow(missing_docs)]
pub enum StateTag {
    New,
    Created,
    OpenInProgress,
    Opened,
    Closed,
}


// === Trait `impl`s ===

impl Display for StateTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let debug_str = format!("{:?}", self);
        let display_str = crate::separate_pascal_case_str_with_spaces(&debug_str);
        write!(f, "{display_str}")
    }
}



// ===========
// === Ami ===
// ===========

/// Struct that describes Amazon Machine Image identifier.
#[derive(Clone, Debug, serde::Serialize, Into, Display, PartialEq, Eq)]
pub struct Ami(pub String);


// === Trait `impl`s ===

impl FromStr for Ami {
    type Err = crate::Error;

    fn from_str(ami: &str) -> Result<Self, Self::Err> {
        /// A valid ami ID starts with `ami-` prefix.
        const AMI_PREFIX: &str = "ami-";

        if !ami.starts_with(AMI_PREFIX) {
            return Err(format!("Bad Ami format: {}", ami))?;
        };
        Ok(Self(ami.to_string()))
    }
}

impl<'a> serde::Deserialize<'a> for Ami {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: serde::Deserializer<'a> {
        let s = String::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(serde::de::Error::custom)
    }
}



// ===============
// === Project ===
// ===============

/// A representation of a project in the Cloud IDE.
///
/// This is returned as part of responses to cloud API endpoints. It simplified to avoid leaking
/// implementation details via the HTTP API. An example of the implementation details we wish to
/// avoid leaking is the ID of the internal EC2 instance that the project is running on. Users don't
/// need to know these details, as we proxy requests to the correct instance through our API
/// gateway.
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
#[allow(missing_docs)]
pub struct Project {
    pub organization_id: id::OrganizationId,
    pub project_id:      id::ProjectId,
    pub name:            String,
    pub state:           StateTag,
    pub ami:             Option<Ami>,
}
