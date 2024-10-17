#![windows_subsystem = "windows"] // Do not display a console window when running the installer.

use enso_installer::prelude::*;



#[cfg(windows)]
fn main() -> Result {
    enso_installer::win::main()
}

#[cfg(not(windows))]
fn main() -> Result {
    bail!("This installer is only supported on Windows.")
}

#[cfg(test)]
mod tests {
    #[test]
    fn installer_name_matches() {
        assert_eq!(enso_install_config::INSTALLER_NAME, env!("CARGO_PKG_NAME"));
    }
}
