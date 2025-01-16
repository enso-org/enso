//! Module that allows to create an Enso Cloud compatible credentials file from
//! a configuration stored in environment variables.

use crate::prelude::*;

use anyhow::Ok;
use std::fs::File;
use std::io::Write;
use tempfile::NamedTempFile;


// ==============
// === Export ===
// ==============

pub mod env;



pub fn build_auth_config_from_environment() -> Result<AuthConfig> {
    let web_client_id = env::ci_config::ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID.get()?;
    let pool_id = env::ci_config::ENSO_CLOUD_COGNITO_USER_POOL_ID.get()?;
    let region = env::ci_config::ENSO_CLOUD_COGNITO_REGION.get()?;
    let username = env::ci_config::ENSO_CLOUD_TEST_ACCOUNT_USERNAME.get()?;
    let password = env::ci_config::ENSO_CLOUD_TEST_ACCOUNT_PASSWORD.get()?;
    Ok(AuthConfig { web_client_id, user_pool_id: pool_id, region, username, password })
}

pub async fn prepare_credentials_file(auth_config: AuthConfig) -> Result<NamedTempFile> {
    let credentials = build_credentials(auth_config).await?;
    let credentials_temp_file = NamedTempFile::with_prefix("enso-cloud-credentials")?;
    save_credentials(&credentials, credentials_temp_file.path())?;
    Ok(credentials_temp_file)
}

#[derive(Debug)]
pub struct AuthConfig {
    web_client_id: String,
    user_pool_id:  String,
    region:        String,
    username:      String,
    password:      String,
}

struct Credentials {
    client_id:     String,
    access_token:  String,
    refresh_token: String,
    refresh_url:   String,
    expire_at:     String,
}

async fn build_credentials(config: AuthConfig) -> Result<Credentials> {
    if !is_aws_cli_installed().await {
        return Err(anyhow!("AWS CLI is not installed. If you want the build script to generate the Enso Cloud credentials file, you must install the AWS CLI."));
    }

    // We save the timestamp before the authentication, as it's better to say the token expires a
    // bit earlier than to make it expire later than in reality and make downstream user mistakenly
    // use an expired token.
    let now_before_auth = chrono::Utc::now();
    let mut command = aws_command();
    command
        .args(["cognito-idp", "initiate-auth"])
        .args(["--region", &config.region])
        .args(["--auth-flow", "USER_PASSWORD_AUTH"])
        .args([
            "--auth-parameters",
            &format!("USERNAME={},PASSWORD={}", config.username, config.password),
        ])
        .args(["--client-id", &config.web_client_id]);

    let stdout = command.run_stdout().await?;
    let cognito_response = parse_cognito_response(&stdout)?;

    let expire_at = now_before_auth + chrono::Duration::seconds(cognito_response.expires_in);
    let expire_at_str = expire_at.to_rfc3339();
    let refresh_url =
        format!("https://cognito-idp.{}.amazonaws.com/{}", config.region, config.user_pool_id);
    Ok(Credentials {
        client_id: config.web_client_id.to_string(),
        access_token: cognito_response.access_token,
        refresh_token: cognito_response.refresh_token,
        expire_at: expire_at_str,
        refresh_url,
    })
}

async fn is_aws_cli_installed() -> bool {
    let mut command = aws_command();
    command.arg("--version");
    command.run_ok().await.is_ok()
}

fn aws_command() -> Command {
    Command::new("aws")
}

struct CognitoResponse {
    access_token:  String,
    refresh_token: String,
    expires_in:    i64,
}

fn parse_cognito_response(response: &str) -> Result<CognitoResponse> {
    let json: serde_json::Value = serde_json::from_str(response)?;
    let root_mapping = unpack_object(&json)?;
    let authentication_result_mapping =
        unpack_object(get_or_fail(root_mapping, "AuthenticationResult")?)?;
    let token_type = unpack_string(get_or_fail(authentication_result_mapping, "TokenType")?)?;
    if token_type != "Bearer" {
        return Err(anyhow!("Expected token type 'Bearer', but got: {}", token_type));
    }

    let access_token = unpack_string(get_or_fail(authentication_result_mapping, "AccessToken")?)?;
    let refresh_token = unpack_string(get_or_fail(authentication_result_mapping, "RefreshToken")?)?;
    let expires_in = unpack_integer(get_or_fail(authentication_result_mapping, "ExpiresIn")?)?;

    Ok(CognitoResponse {
        access_token: access_token.to_string(),
        refresh_token: refresh_token.to_string(),
        expires_in,
    })
}

fn get_or_fail<'a>(
    mapping: &'a serde_json::Map<String, serde_json::Value>,
    key: &str,
) -> Result<&'a serde_json::Value> {
    match mapping.get(key) {
        Some(value) => Ok(value),
        None => Err(anyhow!("Missing key when deserializing JSON: {}", key)),
    }
}

fn unpack_object(value: &serde_json::Value) -> Result<&serde_json::Map<String, serde_json::Value>> {
    if let serde_json::Value::Object(mapping) = value {
        Ok(mapping)
    } else {
        Err(anyhow!("Expected JSON object, but got: {:?}", value))
    }
}

fn unpack_string(value: &serde_json::Value) -> Result<&String> {
    if let serde_json::Value::String(string) = value {
        Ok(string)
    } else {
        Err(anyhow!("Expected JSON string, but got: {:?}", value))
    }
}

fn unpack_integer(value: &serde_json::Value) -> Result<i64> {
    if let serde_json::Value::Number(number) = value {
        Ok(number.as_i64().ok_or_else(|| anyhow!("Expected JSON integer, but got: {:?}", value))?)
    } else {
        Err(anyhow!("Expected JSON integer, but got: {:?}", value))
    }
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
    let mut file = File::create(path)?;
    file.write_all(json.to_string().as_bytes())?;
    Ok(())
}
