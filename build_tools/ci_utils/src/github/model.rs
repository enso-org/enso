//! Structures in this module model the types used in GitHub REST API.

use crate::prelude::*;



/// Description of the self-hosted runner, element of the list runners response.
///
/// See:
/// <https://docs.github.com/en/rest/reference/actions#list-self-hosted-runners-for-a-repository>
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Runner {
    pub id:     i32,
    pub name:   String,
    pub os:     String,
    pub status: String,
    pub busy:   bool,
    pub labels: Vec<Label>,
}

/// A label assigned to the self-hosted runner.
///
/// See:
/// <https://docs.github.com/en/rest/reference/actions#list-self-hosted-runners-for-a-repository>
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Label {
    pub id:     i32,
    pub name:   String,
    pub r#type: String,
}

/// See:
/// <https://docs.github.com/en/rest/reference/actions#list-self-hosted-runners-for-a-repository>
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Runners {
    pub runners:     Vec<Runner>,
    pub total_count: i32,
}

/// See:
/// <https://docs.github.com/en/rest/reference/actions#create-a-registration-token-for-a-repository>
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RegistrationToken {
    pub token:      String,
    pub expires_at: chrono::DateTime<chrono::Utc>,
}

impl AsRef<str> for RegistrationToken {
    fn as_ref(&self) -> &str {
        &self.token
    }
}
