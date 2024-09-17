/// Windows-specific system tool for copying things.
///
/// See https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/robocopy
use crate::prelude::*;



#[derive(Clone, Copy, Debug)]
pub struct Robocopy;

impl Program for Robocopy {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &'static str {
        "robocopy"
    }

    fn handle_exit_status(status: std::process::ExitStatus) -> Result {
        match status.code() {
            None => default_status_checker(status),
            Some(code) if code >= 8 => bail!("Exit with code {}.", code),
            Some(_) => Ok(()),
        }
    }
}

impl Robocopy {}

pub async fn mirror_directory(source: impl AsRef<Path>, destination: impl AsRef<Path>) -> Result {
    Robocopy
        .cmd()?
        .arg(source.as_ref())
        .arg(destination.as_ref())
        .arg("/mir")
        .arg("/sl")
        .run_ok()
        .await
}
