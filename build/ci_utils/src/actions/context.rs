#[allow(unused_imports)]
use crate::prelude::*;

use octocrab::models;



/// Corresponds to https://github.com/actions/toolkit/blob/main/packages/github/src/interfaces.ts
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct WebhookPayload {
    pub repository:   Option<models::Repository>,
    pub issue:        Option<models::issues::Issue>,
    pub pull_request: Option<models::pulls::PullRequest>,
    pub sender:       Option<models::User>,
    pub action:       Option<String>,
    pub installation: Option<models::Installation>,
    pub comment:      Option<models::issues::Comment>,
}

/// Corresponds to https://github.com/actions/toolkit/blob/main/packages/github/src/context.ts
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Context {
    pub payload:     WebhookPayload,
    pub event_name:  String,
    pub sha:         String,
    pub r#ref:       String,
    pub workflow:    String,
    pub action:      String,
    pub actor:       String,
    pub job:         String,
    pub run_number:  usize,
    pub run_id:      models::RunId,
    pub api_url:     Url,
    pub server_url:  Url,
    pub graphql_url: Url,
}

impl Context {
    /// Creates a new context from the environment.
    pub fn from_env() -> Result<Self> {
        let payload: WebhookPayload =
            if let Ok(event_path) = crate::actions::env::GITHUB_EVENT_PATH.get() {
                event_path.read_to_json()?
            } else {
                default()
            };
        let event_name = crate::actions::env::GITHUB_EVENT_NAME.get()?;
        let sha = crate::actions::env::GITHUB_SHA.get()?;
        let r#ref = crate::actions::env::GITHUB_REF.get()?;
        let workflow = crate::actions::env::GITHUB_WORKFLOW.get()?;
        let action = crate::actions::env::GITHUB_ACTION.get()?;
        let actor = crate::actions::env::GITHUB_ACTOR.get()?;
        let job = crate::actions::env::GITHUB_JOB.get()?;
        // GitHub Actions defaults run_number and run_id to 10 if they are not set.
        // I am not sure why, for now I chose not to follow this pattern.
        let run_number = crate::actions::env::GITHUB_RUN_NUMBER.get()?;
        let run_id = crate::actions::env::GITHUB_RUN_ID.get()?;
        let api_url = crate::actions::env::GITHUB_API_URL
            .get()
            .or_else(|_| Url::from_str("https://api.github.com"))?;
        let server_url = crate::actions::env::GITHUB_SERVER_URL
            .get()
            .or_else(|_| Url::from_str("https://github.com"))?;
        let graphql_url = crate::actions::env::GITHUB_GRAPHQL_URL
            .get()
            .or_else(|_| Url::from_str("https://api.github.com/graphql"))?;
        Ok(Self {
            payload,
            event_name,
            sha,
            r#ref,
            workflow,
            action,
            actor,
            job,
            run_number,
            run_id,
            api_url,
            server_url,
            graphql_url,
        })
    }
}
