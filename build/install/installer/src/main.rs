// === Features ===
#![feature(core_intrinsics)]
#![feature(default_free_fn)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_build::prelude::*;

#[cfg(windows)]
pub mod win;

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let lock = enso_install::lock()?;
    let _guard = lock.lock()?;
    #[cfg(windows)]
    {
        let archive = enso_install::win::resource::get_binary("ARCHIVE_ID")?;
        let install_dir =
            ide_ci::env::known::win::LOCALAPPDATA.get()?.join("Programs").join("Enso");
        install(install_dir, archive)?;
    }
    Ok(())
}
