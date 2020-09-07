use std::env;
use std::process::{Command, exit};
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;

pub fn wrap_launcher(version: &str) {
    let args: Vec<String> = env::args().collect();
    let path = option_env!("ENSO_LAUNCHER_LOCATION")
        .expect("`ENSO_LAUNCHER_LOCATION` was not set during compilation.");
    let current_exe_path = env::current_exe()
        .expect("Cannot get current executable path.");
    let exe_location = match current_exe_path.to_str() {
        Some(str) => str,
        None => {
            eprintln!("Path {} is invalid.", current_exe_path.to_string_lossy());
            exit(1)
        }
    };
    let parent_directory = current_exe_path.parent()
        .expect("Executable path should have a parent directory.");
    let log_path = parent_directory.join(".launcher_version_log");
    append_to_log(log_path, version).expect("Cannot write to log.");
    let override_args = [
        String::from("--internal-emulate-version"),
        String::from(version),
        String::from("--internal-emulate-location"),
        String::from(exe_location)
    ];
    let modified_args = [&override_args[..], &args[1..]].concat();
    let exit_status =
        Command::new(path).args(modified_args).status();
    let exit_code = match exit_status {
        Ok(status) =>
            if let Some(code) = status.code() {
                code
            } else {
                eprintln!("Process terminated by signal.");
                exit(1)
            }
        Err(error) => {
            eprintln!("{}", error);
            exit(1)
        }
    };
    exit(exit_code)
}

fn append_to_log(path: PathBuf, line: &str) -> io::Result<()> {
    let mut version_log = OpenOptions::new()
        .create(true)
        .write(true)
        .append(true)
        .open(path)?;

    writeln!(version_log, "{}", line)?;
    Ok(())
}
