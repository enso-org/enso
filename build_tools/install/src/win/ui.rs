//! UI-related utilities for the Windows installer/uninstaller.

use crate::prelude::*;



/// [Sets up logging](crate::setup_logging). Handles any errors by showing a fatal message box.
///
/// # Panics
/// This function panics if it fails to setup logging. The panic will follow immediately after the
/// error message box is closed.
pub fn setup_logging_or_fatal(app_name: &str, dialog_title: &str) -> PathBuf {
    match crate::setup_logging(app_name) {
        Ok(logfile) => {
            info!("Logging to: {}", logfile.display());
            logfile
        }
        Err(err) => {
            let message = format!("Failed to create a log file: {err:?}");
            native_windows_gui::fatal_message(dialog_title, &message);
        }
    }
}
