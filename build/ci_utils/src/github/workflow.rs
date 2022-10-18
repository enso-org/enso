use crate::prelude::*;

/// HTTP body payload for the workflow dispatch.
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct RequestBody<S, T> {
    /// Reference to the commit or branch to build. Should be string-like.
    pub r#ref: S,

    /// Inputs to the workflow.
    pub inputs: T,
}

// Function that invokes GitHub API REST API workflow dispatch.
pub async fn dispatch(
    client: &Octocrab,
    repo: &(impl IsRepo + Send + Sync + 'static + ?Sized),
    workflow_id: impl AsRef<str> + Send + Sync + 'static,
    r#ref: impl AsRef<str> + Send + Sync + 'static,
    inputs: &impl Serialize,
) -> Result {
    // Don't use octocrab for this, it has broken error handling!
    // (treating error 404 as Ok)
    let workflow_id = workflow_id.as_ref();
    let name = repo.name();
    let owner = repo.owner();
    let url = client.absolute_url(format!(
        "/repos/{owner}/{name}/actions/workflows/{workflow_id}/dispatches"
    ))?;
    let r#ref = r#ref.as_ref();
    let body = RequestBody { r#ref, inputs };
    let response = client._post(url, Some(&body)).await?;
    let _response = crate::io::web::handle_error_response(response).await?;
    // Nothing interesting in OK response, so we just return empty struct.
    Ok(())
}
