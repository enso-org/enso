use enso_install::prelude::*;

use enso_install::is_already_running;
use enso_install::locked_installation_lock;
use enso_install::sanitized_electron_builder_config;



/// The parent directory of this (uninstaller) executable.
///
/// This is a good candidate for the install directory of Enso.
fn parent_directory() -> Result<PathBuf> {
    let exe_path = ide_ci::env::current_exe()?;
    exe_path.try_parent().map(Into::into)
}

/// Delete the uninstaller executable.
///
/// This uses the [`self_replace`] crate to delete the executable on Windows. Running executable
/// cannot be deleted on Windows using the ordinary means.
///
/// This must be invoked before deleting the install directory, if the uninstaller is located in the
/// install directory. Otherwise, the executable makes the directory non-deletable.
fn self_delete(parent_path: &Path) -> Result {
    self_replace::self_delete_outside_path(parent_path).with_context(|| {
        format!(
            "Failed to delete the Enso executable. \
            Please delete the file manually: {}",
            parent_path.display()
        )
    })
}

/// Handle an error, logging it and adding it to the list of errors.
fn handle_error<T>(errors: &mut Vec<anyhow::Error>, result: Result<T>) -> Option<T> {
    match result {
        Err(error) => {
            error!("Encountered an error: {error}.");
            errors.push(error);
            None
        }
        Ok(value) => Some(value),
    }
}

/// Show a dialog with an error message and panic.
fn show_dialog_and_panic(error: &anyhow::Error) -> ! {
    error!("Encountered an error: {error:?}.");
    native_windows_gui::fatal_message(
        "Uninstallation failed",
        &format!("Encountered an error: {error}", error = error),
    );
}

pub async fn main() -> Result {
    let dialog_title = format!("{} installer", sanitized_electron_builder_config().product_name);
    let logfile =
        enso_install::win::ui::setup_logging_or_fatal(env!("CARGO_PKG_NAME"), &dialog_title);

    // Show a yes-no modal dialog.
    let message =
        format!("Do you want to uninstall {}?", sanitized_electron_builder_config().product_name);

    let params = native_windows_gui::MessageParams {
        title:   &dialog_title,
        content: &message,
        buttons: native_windows_gui::MessageButtons::YesNo,
        icons:   native_windows_gui::MessageIcons::Question,
    };
    match native_windows_gui::message(&params) {
        native_windows_gui::MessageChoice::Yes => (),
        native_windows_gui::MessageChoice::No => {
            info!("User chose not to uninstall.");
            return Ok(());
        }
        _ => bail!("Unexpected message box result."),
    }


    let mut errors = vec![];
    let _guard = match locked_installation_lock() {
        Ok(guard) => guard,
        Err(error) => show_dialog_and_panic(&error),
    };

    // Unwrap is safe, because the uninstaller path (being an executable) will never be a root.
    let install_dir = parent_directory().unwrap();

    // Check if there is already running instance of Enso or any of its subprograms.
    let already_running = sysinfo::get_current_pid()
        .map_err(|text| anyhow!("Failed to get current process ID: {text}"))
        .and_then(|my_pid| {
            is_already_running(&install_dir, &[my_pid])
                .context("Failed to check if already running.")
        });
    match already_running {
        Ok(Some(message)) => show_dialog_and_panic(&anyhow!("{message}")),
        Ok(None) => (),
        Err(error) => warn!("Failed to check if there is already running instance: {error:?}"),
    };

    // Make sure that Enso.exe is in the same directory as this uninstaller. This is to prevent
    // situation where just the uninstaller binary is placed by accident elsewhere and ends up
    // deleting the whole directory.
    let executable_filename = enso_install::executable_filename();
    let expected_executable = install_dir.join(&executable_filename);
    let shortcut_name = enso_install::shortcut_name();

    ensure!(
        expected_executable.exists(),
        "{} not found in the presumed install directory {}",
        executable_filename.display(),
        install_dir.display()
    );

    info!("Remove Add/Remove Programs entry.");
    handle_error(
        &mut errors,
        enso_install::win::uninstall::remove_from_registry(enso_install::uninstall_key()),
    );

    info!("Removing self (uninstaller) executable.");
    handle_error(&mut errors, self_delete(&install_dir));

    info!("Removing install directory.");
    handle_error(&mut errors, ide_ci::fs::remove_dir_if_exists(&install_dir));

    // Remove prog id but leave file extensions - see https://learn.microsoft.com/en-us/windows/win32/shell/fa-file-types#deleting-registry-information-during-uninstallation
    for file_association in
        &sanitized_electron_builder_config().extra_metadata.installer.file_associations
    {
        let prog_id = &file_association.prog_id;
        info!("Removing ProgID `{prog_id}`.");
        handle_error(&mut errors, enso_install::win::prog_id::delete(prog_id));
    }

    info!("Removing Start Menu entry.");
    handle_error(
        &mut errors,
        enso_install::win::shortcut::Location::Menu.remove_shortcut(shortcut_name),
    );

    info!("Removing Desktop shortcut.");
    handle_error(
        &mut errors,
        enso_install::win::shortcut::Location::Desktop.remove_shortcut(shortcut_name),
    );

    if !errors.is_empty() {
        error!("Encountered {} errors.", errors.len());
        for error in &errors {
            error!(" * {error:?}");
        }

        let errors_summary = errors.iter().map(|e| format!("{e}")).join("\n *");
        let plain_message = format!(
            "Encountered errors during uninstallation. Some files or \
        registry entries may have been left behind. Please see the log file for details. The file \
        explorer will be opened at the log file location.\n\nErrors:\n{errors_summary}"
        );
        native_windows_gui::error_message("Uninstallation failed", &plain_message);
        let _ = ide_ci::programs::explorer::show_selected(logfile);
        bail!("Uninstallation failed.");
    } else {
        native_windows_gui::simple_message(
            "Uninstallation successful",
            "Enso has been successfully uninstalled.",
        );
        Ok(())
    }
}
