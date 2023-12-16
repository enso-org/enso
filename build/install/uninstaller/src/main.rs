use enso_install::config::APPLICATION_SHORTCUT_NAME;
use enso_install::prelude::*;


/// The parent directory of this (uninstaller) executable.
///
/// This is a good candidate for the install directory of Enso.
fn parent_directory() -> Result<PathBuf> {
    let exe_path = ide_ci::env::current_exe()?;
    exe_path.try_parent().map(Into::into)
}

/// Delete the uninstaller executable.
///
/// This uses the `self_replace` crate to delete the executable on Windows. Thanks to this, we can
///
/// delete the executable even if it is currently running.
///
/// This must be invoked before deleting the install directory, if the uninstaller is located in the
/// install directory.
fn self_delete(parent_path: &Path) -> Result {
    self_replace::self_delete_outside_path(&parent_path).with_context(|| {
        format!(
            "Failed to delete the Enso executable. \
            Please delete the file manually: {}",
            parent_path.display()
        )
    })
}

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let install_dir = parent_directory()?;

    // Make sure that Enso.exe is in the same directory as this installer.
    ensure!(
        install_dir.join(enso_install::config::APPLICATION_EXECUTABLE).exists(),
        "Enso.exe not found in the same directory as this installer."
    );

    info!("Removing self (uninstaller) executable.");
    self_delete(&install_dir)?;

    info!("Removing install directory.");
    ide_ci::fs::remove_dir_if_exists(&install_dir)?;

    // Remove prog id but leave file extensions - see https://learn.microsoft.com/en-us/windows/win32/shell/fa-file-types#deleting-registry-information-during-uninstallation
    for prog_id in enso_install::config::PROG_IDS {
        info!("Removing ProgID `{prog_id}`.");
        enso_install::win::prog_id::delete(prog_id)?;
    }

    info!("Removing Start Menu entry.");
    enso_install::win::shortcut::Location::Menu.remove_shortcut(APPLICATION_SHORTCUT_NAME)?;

    info!("Removing Desktop shortcut.");
    enso_install::win::shortcut::Location::Desktop.remove_shortcut(APPLICATION_SHORTCUT_NAME)?;


    Ok(())
}
