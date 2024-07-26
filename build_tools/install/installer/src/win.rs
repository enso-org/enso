use ide_ci::prelude::*;

use crate::access_payload_metadata;
use crate::win::config::Config;
use crate::win::logic::install_with_updates;
use crate::Payload;

use enso_install::win::local_app_data;


// ==============
// === Export ===
// ==============

pub mod app;
pub mod config;
pub mod logic;



/// Retrieve the compiled-in installer payload with metadata.
pub fn access_payload() -> Result<Payload> {
    Ok(Payload {
        data:     enso_install::win::get_installer_payload()?,
        metadata: access_payload_metadata(),
    })
}

/// Spawn a thread that will install the Enso IDE at the given location.
pub fn spawn_installer_thread(
    install_location: impl AsRef<Path>,
    payload: Payload,
    config: Config,
) -> Result<(std::thread::JoinHandle<Result>, std::sync::mpsc::Receiver<crate::InstallerUpdate>)> {
    let install_location = install_location.as_ref().to_path_buf();
    let (sender, receiver) = std::sync::mpsc::channel();
    let handle = std::thread::Builder::new()
        .name("Installer Logic".into())
        .spawn(move || {
            let result = install_with_updates(&install_location, payload, &config, &sender);
            if let Err(err) = result {
                let msg = format!("Installation failed: {err:?}.");
                let _ = sender.send(crate::InstallerUpdate::Finished(Result::Err(err)));
                bail!(msg);
            }
            Ok(())
        })
        .context("Failed to spawn the installer logic thread.")?;
    Ok((handle, receiver))
}


/// Get the default installation directory.
pub fn get_install_dir(pretty_name: &str) -> Result<PathBuf> {
    let programs_dir = enso_install::win::user_program_files()
        .or_else(|e| {
            warn!("Failed to get the user's program files directory: {e:?}");
            // The Windows might refuse to provide the user's program files directory in some cases,
            // like brand-new user accounts that don't have the directory created yet.
            // Thus, we fall back to the default location, as documented in:
            // https://learn.microsoft.com/en-us/windows/win32/shell/knownfolderid
            Result::Ok(local_app_data()?.join("Programs"))
        })
        .context("Failed to get the user's program files directory")?;
    Ok(programs_dir.join(pretty_name))
}

/// The installer's entry point.
pub fn main() -> Result {
    // Note: logging will be set up by the `InstallerApp`. It needs to be responsible for this to be
    // able to show the log file to the user.
    let app = app::InstallerApp::new()?;
    app.run();
    if let Some(result) = app.result.take() {
        info!("Installation finished with result: {result:?}",);
        result
    } else {
        bail!("Installation finished without setting the result.")
    }
}
