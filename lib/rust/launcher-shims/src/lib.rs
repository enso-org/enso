use std::env;
use std::process::{Command, exit};

pub fn wrap_launcher(version: &str) {
    let args: Vec<String> = env::args().collect();
    let path = option_env!("ENSO_LAUNCHER_LOCATION")
        .expect("`ENSO_LAUNCHER_LOCATION` was not set during compilation.");
    let current_exe_result = env::current_exe()
        .expect("Cannot get current executable path.");
    let exe_location = match current_exe_result.to_str() {
        Some(str) => str.clone(),
        None => {
            eprintln!("Path {} is invalid.", current_exe_result.to_string_lossy());
            exit(1)
        }
    };
    let override_args = [
        String::from("--internal-emulate-version"),
        String::from(version),
        String::from("--internal-emulate-location"),
        String::from(exe_location)
    ];
    let modified_args = [&override_args[..], &args[1..]].concat();
    let exit_status =
        Command::new(path).args(modified_args).status();
    match exit_status {
        Ok(status) =>
            if let Some(code) = status.code() {
                exit(code)
            } else {
                eprintln!("Process terminated by signal.");
                exit(1)
            }
        Err(error) => {
            eprintln!("{}", error);
            exit(1)
        }
    }
}
