use crate::prelude::*;

use anyhow::Context;



pub trait OutputExt {
    fn single_line_stdout(&self) -> Result<String>;
    //fn run_ok(&self) -> Result;
    fn describe(&self) -> String;

    fn stdout_as_str(&self) -> Result<&str>;
    fn into_stdout_string(self) -> Result<String>;
}

impl OutputExt for std::process::Output {
    fn single_line_stdout(&self) -> Result<String> {
        // self.run_ok()?;
        let lines = non_empty_lines(&self.stdout)?.collect_vec();
        match lines.as_slice() {
            [line] => Ok(line.to_string()),
            other => bail!("Expected exactly 1 non-empty line. Found: {:?}", other),
        }
    }

    // fn run_ok(&self) -> Result {
    //     self.status.exit_ok().with_context(|| self.describe())
    // }
    fn describe(&self) -> String {
        format!(
            "Stdout:\n{:?}\n\nStderr:\n{:?}\n",
            std::str::from_utf8(&self.stdout).unwrap_or("<INVALID ENCODING>"),
            std::str::from_utf8(&self.stderr).unwrap_or("<INVALID ENCODING>"),
        )
    }

    fn stdout_as_str(&self) -> Result<&str> {
        std::str::from_utf8(&self.stdout).context("The command stdout is not a valid text.")
    }

    fn into_stdout_string(self) -> Result<String> {
        String::from_utf8(self.stdout).context("The command stdout is not a valid text.")
    }
}

pub fn non_empty_lines(bytes: &[u8]) -> Result<impl Iterator<Item = &str>> {
    Ok(std::str::from_utf8(bytes)?.lines().map(str::trim).filter(|line| !line.is_empty()))
}
