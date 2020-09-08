use std::env;
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::process::{Command, exit};



// ====================
// === WrapLauncher ===
// ====================

/// Runs the wrapped launcher overriding its reported version to the provided version.
///
/// The launcher's executable locations is also overridden to point to this executable. The launcher
/// is passed all the original arguments plus the arguments that handle the version and location
/// override. The location of the original launcher executable that is wrapped is determined by the
/// environment variable `ENSO_LAUNCHER_LOCATION` that should be set at build-time.
///
/// Additionally, the wrapper appends to a log file called `.launcher_version_log` a line containing
/// the version string that was launched (creating the file if necessary). This can be used by tests
/// to verify the order of launched versions.
pub fn wrap_launcher(version:&str) {
    let args: Vec<String> = env::args().collect();

    let missing_location_message = "`ENSO_LAUNCHER_LOCATION` was not set during compilation.";
    let path = option_env!("ENSO_LAUNCHER_LOCATION").expect(missing_location_message);

    let current_exe_path = env::current_exe().expect("Cannot get current executable path.");
    let exe_location     = match current_exe_path.to_str() {
        Some(str) => str,
        None      => {
            eprintln!("Path {} is invalid.", current_exe_path.to_string_lossy());
            exit(1)
        }
    };

    let missing_directory_message = "Executable path should have a parent directory.";
    let parent_directory          = current_exe_path.parent().expect(missing_directory_message);
    let log_name                  = ".launcher_version_log";
    let log_path                  = parent_directory.join(log_name);
    append_to_log(log_path, version).expect("Cannot write to log.");

    let override_args = [
        String::from("--internal-emulate-version"),
        String::from(version),
        String::from("--internal-emulate-location"),
        String::from(exe_location)
    ];
    let modified_args = [&override_args[..], &args[1..]].concat();

    let exit_status = Command::new(path).args(modified_args).status();
    let exit_code   = match exit_status {
        Ok (status) =>
            if let Some(code) = status.code() {
                code
            } else {
                eprintln!("Process terminated by signal.");
                exit(1)
            }
        Err(error)  => {
            eprintln!("{}",error);
            exit(1)
        }
    };
    exit(exit_code)
}


// === Log ===

/// Appends a line to the file located at the provided path.
pub fn append_to_log(path:PathBuf, line:&str) -> io::Result<()> {
    let mut log_file = OpenOptions::new().create(true).write(true).append(true).open(path)?;
    writeln!(log_file,"{}",line)?;
    Ok(())
}
