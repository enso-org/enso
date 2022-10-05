use crate::prelude::*;

use crate::actions::env;

use std::io::Write;


// ==============
// === Export ===
// ==============

pub mod definition;



/// Check if we are running in an environment that looks like being spawned by GitHub Actions
/// workflow.
pub fn is_in_env() -> bool {
    env::GITHUB_ACTIONS.get().contains(&true)
}

/// Sets an action's output parameter.
///
/// See: <https://docs.github.com/en/actions/learn-github-actions/workflow-commands-for-github-actions#setting-an-output-parameter>
pub fn set_output(name: &str, value: &impl ToString) {
    let value = value.to_string();
    debug!("Setting GitHub Actions step output {name} to {value}");
    println!("::set-output name={name}::{value}");
}

/// Prints a debug message to the log.
///
/// You must create a secret named `ACTIONS_STEP_DEBUG` with the value `true` to see the debug
/// messages set by this command in the log.
///
/// See: <https://docs.github.com/en/actions/learn-github-actions/workflow-commands-for-github-actions#setting-a-debug-message>
pub fn debug(message: &str) {
    println!("::debug::{message}")
}

/// Creates or updates an environment variable for any steps running next in a job.
///
/// This step and all subsequent steps in a job will have access to the variable. Environment
/// variables are case-sensitive and you can include punctuation.
///
/// Just logs and sets variable locally if used under non-GH CI.
pub fn set_env(name: &str, value: &impl ToString) -> Result {
    let value_string = value.to_string();
    debug!("Will try writing Github Actions environment variable: {name}={value_string}");
    std::env::set_var(name, value.to_string());
    if is_in_env() {
        let env_file = env::GITHUB_ENV.get()?;
        let mut file = std::fs::OpenOptions::new().create_new(false).append(true).open(env_file)?;
        writeln!(file, "{name}={value_string}")?;
    }
    Ok(())
}

pub fn mask_text(text: impl AsRef<str>) {
    if is_in_env() {
        println!("::add-mask::{}", text.as_ref())
    }
}

pub fn mask_value(value: impl Display) {
    if is_in_env() {
        println!("::add-mask::{value}")
    }
}

pub fn mask_environment_variable(variable_name: impl AsRef<OsStr>) -> Result {
    mask_value(std::env::var(variable_name)?);
    Ok(())
}

#[derive(Clone, Copy, Debug, strum::Display)]
#[strum(serialize_all = "snake_case")]
pub enum MessageLevel {
    Debug,
    Notice,
    Warning,
    Error,
}

#[derive(Clone, Debug)]
pub struct Message {
    pub level: MessageLevel,
    pub text:  String,
    // TODO title, line, column
}

impl Message {
    pub fn notice(text: impl AsRef<str>) {
        Message { level: MessageLevel::Notice, text: text.as_ref().into() }.send()
    }

    pub fn send(&self) {
        println!("::{} ::{}", self.level, self.text);
    }
}

pub fn message(level: MessageLevel, text: impl AsRef<str>) {
    Message { level, text: text.as_ref().into() }.send()
}
