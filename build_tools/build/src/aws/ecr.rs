use crate::prelude::*;

use aws_sdk_ecr::model::Repository;
use ide_ci::programs::docker;


// ==============
// === Export ===
// ==============

pub mod runtime;



/// Lookup the repository by name.
#[instrument(skip(client), err)]
pub async fn resolve_repository(
    client: &aws_sdk_ecr::Client,
    repository_name: &str,
) -> Result<Repository> {
    let repositories =
        client.describe_repositories().repository_names(repository_name).send().await?;
    repositories
        .repositories
        .context("Missing repositories information.")?
        .pop()
        .with_context(|| format!("Cannot find repository {repository_name} in the registry."))
}

/// Generate an authentication token for the repository.
#[instrument(skip(client), err)]
pub async fn get_credentials(client: &aws_sdk_ecr::Client) -> Result<docker::Credentials> {
    let token = client.get_authorization_token().send().await?;
    let auth_data = token
        .authorization_data()
        .context("Missing authorization data.")?
        .first()
        .context("Missing authorization data entry.")?;
    let token_encoded =
        auth_data.authorization_token.as_ref().context("Missing authorization token.")?;
    let token_decoded = base64::decode_config(token_encoded, base64::STANDARD)
        .context("Failed to decode the token.")?;
    let token_decoded = String::from_utf8(token_decoded)?;
    let proxy = auth_data.proxy_endpoint().context("Missing proxy endpoint.")?;
    let fields = token_decoded.split(':').collect_vec();
    let [username, password] = fields.as_slice() else {
        bail!("Invalid token format. Parts: {:?}", fields);
    };
    Ok(docker::Credentials::new(*username, *password, proxy))
}

/// Get a repository URI, that can be used to refer to the repository in the Docker commands.
#[instrument(skip(client), ret)]
pub async fn get_repository_uri(
    client: &aws_sdk_ecr::Client,
    repository_name: &str,
) -> Result<String> {
    let repository = resolve_repository(client, repository_name).await?;
    let repository_uri = repository.repository_uri().context("Missing repository URI.")?;
    Ok(repository_uri.into())
}

/// Create a new ECR client, configured using the environment variables.
pub async fn client_from_env() -> aws_sdk_ecr::Client {
    let config = aws_config::load_from_env().await;
    aws_sdk_ecr::Client::new(&config)
}
