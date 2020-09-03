use std::env;
use std::process::{Command, exit};

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = match option_env!("ENSO_LAUNCHER_LOCATION") {
        Some(path) => path,
        None => {
            eprintln!("`ENSO_LAUNCHER_LOCATION` was not set during compilation.");
            exit(1)
        }
    };
    let version = match option_env!("ENSO_BUILD_VERSION") {
        Some(version) => version,
        None => {
            eprintln!("`ENSO_BUILD_VERSION` was not set during compilation.");
            exit(1)
        }
    };
    /*let exe_location = match env::current_exe() {
        Ok(path) => {
            let str_opt = path.as_path().to_str();
            match str_opt {
                Some(str) => str.clone(),
                None => {
                    eprintln!("Path {} is invalid.", path.to_string_lossy());
                    exit(1)
                }
            }
        },
        Err(e) => {
            eprintln!("Failed to get path to executable {}", e);
            exit(1)
        }
    };*/
    let override_args = [
        String::from("--internal-emulate-version"),
        String::from(version),
        //String::from("--internal-emulate-location"),
        //String::from(exe_location)
    ];
    let modified_args = [&override_args[..], &args[1..]].concat();
    println!("Final args: {:?}", modified_args);
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
