//! Module that allows to create an Enso Cloud compatible credentials file from 
//! a configuration stored in environment variables.

pub mod env;

use aws_config::default_provider::credentials;
use tempfile::NamedTempFile;

use crate::prelude::*;

struct AuthConfig {
    client_id: String,
    username: String,
    password: String,
}

struct Credentials {
    client_id: String,
    access_token: String,
    refresh_token: String,
    refresh_url: String,
    expire_at: String,
}

fn auth_config_from_environment() -> Option<AuthConfig> {
    build_auth_config_from_environment().ok()
}

fn build_auth_config_from_environment() -> Result<AuthConfig> {
    let client_id = env::ci_config::ENSO_CLOUD_COGNITO_CLIENT_ID.get()?;
    let username = env::ci_config::ENSO_CLOUD_TEST_ACCOUNT_USERNAME.get()?;
    let password = env::ci_config::ENSO_CLOUD_TEST_ACCOUNT_PASSWORD.get()?;
    Ok(AuthConfig { client_id, username, password })
}

async fn build_credentials(config: AuthConfig) -> Result<Credentials> {
    if !is_aws_cli_installed().await {
        return Err(anyhow!("AWS CLI is not installed. If you want the build script to generate the Enso Cloud credentials file, you must install the AWS CLI."));
    }

    let mut command = aws_command();
    command
        .args(["cognito-idp", "initiate-auth"])
        .args(["--auth-flow", "USER_PASSWORD_AUTH"])
        .args(["--auth-parameters", &format!("USERNAME={},PASSWORD={}", config.username, config.password)])
        .args(["--client-id", &config.client_id]);


    let stdout = command.run_stdout().await?;
    println!("stdout: {}", stdout);
    // Ok(credentials)
    // TODO
    Err(anyhow!("TODO"))
}

async fn is_aws_cli_installed() -> bool {
    let mut command = aws_command();
    command.arg("--version");
    match command.run_ok().await {
        Ok(_) => true,
        Err(_) => false,
    }
}

fn aws_command() -> Command {
    Command::new("aws")
}

fn save_credentials(credentials: &Credentials, path: &Path) -> Result<()> {
    let json = serde_json::json! {
        {
            "client_id":     credentials.client_id,
            "access_token":  credentials.access_token,
            "refresh_token": credentials.refresh_token,
            "refresh_url":   credentials.refresh_url,
            "expire_at":     credentials.expire_at,
        }
    };
    Err(anyhow!("TODO"))
}

pub async fn prepare_credentials_file() -> Result<Option<NamedTempFile>> {
    let config = match auth_config_from_environment() {
        Some(config) => config,
        None => return Ok(None),
    };

    let credentials = build_credentials(config).await?;
    let credentials_temp_file = NamedTempFile::with_prefix("enso-cloud-credentials")?;
    save_credentials(&credentials, &credentials_temp_file.path())?;
    Ok(Some(credentials_temp_file))
}
