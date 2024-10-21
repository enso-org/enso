#![windows_subsystem = "windows"] // Do not display a console window when running the uninstaller.

use enso_install::prelude::*;


// ==============
// === Export ===
// ==============

#[cfg(windows)]
pub mod win;



#[cfg(windows)]
#[tokio::main]
pub async fn main() -> Result {
    win::main().await
}


#[cfg(not(windows))]
fn main() -> Result {
    bail!("This uninstaller is only supported on Windows.")
}


#[cfg(test)]
mod tests {
    #[test]
    fn uninstaller_name_matches() {
        // Make sure the uninstaller expected filename matches the package name.
        assert_eq!(enso_install_config::UNINSTALLER_NAME, env!("CARGO_PKG_NAME"));
    }
}
