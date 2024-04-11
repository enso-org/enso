// === Features ===
// #![feature(core_intrinsics)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use ide_ci::prelude::*;

use enso_install_config::INSTALLER_PAYLOAD_ID;


// ==============
// === Export ===
// ==============

#[cfg(windows)]
pub mod win;



#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let lock = enso_install::lock()?;
    let _guard = lock.lock()?;
    #[cfg(windows)]
    {
        let config = win::fill_config()?;
        let archive = enso_install::win::resource::get_binary(INSTALLER_PAYLOAD_ID)?;
        let install_dir = enso_install::win::user_program_files()?.join(&config.pretty_name);
        win::install(install_dir, archive, &config)?;
    }
    #[cfg(not(windows))]
    {
        bail!("Unsupported platform.");
    }
    Ok(())
}


#[cfg(test)]
mod tests {
    #[test]
    fn uninstaller_name_matches() {
        assert_eq!(enso_install_config::INSTALLER_NAME, env!("CARGO_PKG_NAME"));
    }
}
