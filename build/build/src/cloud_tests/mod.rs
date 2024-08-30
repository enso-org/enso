//! Module that allows to create an Enso Cloud compatible credentials file from 
//! a configuration stored in environment variables.

use std::env;
use std::error;
use std::process;
use crate::prelude::*;

struct Credentials {
    client_id: String,
    access_token: String,
    refresh_token: String,
    refresh_url: String,
    expire_at: String,
}

fn build_credentials() {
    let mut command = Command::new_program("aws");
        command
            .args(["-products", "*"]);

        let stdout = command.run_stdout().await?;
}

fn is_aws_cli_installed() {
    let mut command = aws_command();
}

fn aws_command() -> Command {
    Command::new_program("aws")
}

fn save_credentials(credentials: &Credentials, path: &Path) -> Result<(), Box<dyn error::Error>> {
    let json = serde_json::json! {
        {
            "client_id":     credentials.client_id,
            "access_token":  credentials.access_token,
            "refresh_token": credentials.refresh_token,
            "refresh_url":   credentials.refresh_url,
            "expire_at":     credentials.expire_at,
        }
    };
}
