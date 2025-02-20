//! Uninstaller information for Windows.
//!
//! These functions allow you to write uninstall information to the registry, and remove it when the
//! application is uninstalled.

use crate::prelude::*;
use winreg::enums::*;



/// The path to the Uninstall subkey of the registry.
const UNINSTALL_KEY_PATH: &str = "Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall";

/// Struct representing the uninstall information stored in the `Uninstall` key.
#[derive(Debug)]
pub struct UninstallInfo {
    /// The name of the application as it appears in the Add/Remove Programs dialog.
    pub display_name: String,

    /// The command that will be executed when the user chooses to uninstall the application.
    pub uninstall_string: String,

    /// The path to the icon that will be displayed next to the application in the Add/Remove
    /// Programs dialog.
    pub display_icon: Option<String>,

    /// The version of the application.
    pub display_version: Option<String>,

    /// The publisher of the application.
    pub publisher: Option<String>,

    /// A URL for the About button in the Add/Remove Programs dialog.
    pub url_info_about: Option<String>,

    /// A URL for the Update button in the Add/Remove Programs dialog.
    pub url_update_info: Option<String>,

    /// A URL for the Help button in the Add/Remove Programs dialog.
    pub help_link: Option<String>,

    /// The installation location of the application.
    pub install_location: Option<String>,

    /// The date the application was installed.
    pub install_date: Option<String>,

    /// The size of the application in kibibytes.
    pub estimated_size_kib: Option<u32>,
}

impl UninstallInfo {
    pub fn new(display_name: impl Into<String>, uninstall_string: impl Into<String>) -> Self {
        Self {
            display_name:       display_name.into(),
            uninstall_string:   uninstall_string.into(),
            display_icon:       None,
            display_version:    None,
            publisher:          None,
            url_info_about:     None,
            url_update_info:    None,
            help_link:          None,
            install_location:   None,
            install_date:       None,
            estimated_size_kib: None,
        }
    }

    #[context("Failed to write '{app_key}' uninstall information to the registry.")]
    pub fn write_to_registry(&self, app_key: &str) -> Result {
        trace!("Writing uninstall information to the registry: {self:#?}");
        let uninstall_key = open_uninstall_key()?;

        // Create a new key for our application
        let (app_uninstall_key, _) = uninstall_key.create_subkey(app_key)?;

        // Set the necessary values
        app_uninstall_key.set_value("DisplayName", &self.display_name)?;
        app_uninstall_key.set_value("UninstallString", &self.uninstall_string)?;

        if let Some(display_icon) = &self.display_icon {
            app_uninstall_key.set_value("DisplayIcon", display_icon)?;
        }
        if let Some(display_version) = &self.display_version {
            app_uninstall_key.set_value("DisplayVersion", display_version)?;
        }
        if let Some(publisher) = &self.publisher {
            app_uninstall_key.set_value("Publisher", publisher)?;
        }
        if let Some(url_info_about) = &self.url_info_about {
            app_uninstall_key.set_value("URLInfoAbout", url_info_about)?;
        }
        if let Some(url_update_info) = &self.url_update_info {
            app_uninstall_key.set_value("URLUpdateInfo", url_update_info)?;
        }
        if let Some(help_link) = &self.help_link {
            app_uninstall_key.set_value("HelpLink", help_link)?;
        }
        if let Some(install_location) = &self.install_location {
            app_uninstall_key.set_value("InstallLocation", install_location)?;
        }
        if let Some(install_date) = &self.install_date {
            app_uninstall_key.set_value("InstallDate", install_date)?;
        }
        if let Some(estimated_size) = self.estimated_size_kib {
            app_uninstall_key.set_value("EstimatedSize", &estimated_size)?;
        }

        Ok(())
    }
}

/// Open the `Uninstall` key in the registry.
pub fn open_uninstall_key() -> Result<RegKey> {
    RegKey::predef(HKEY_CURRENT_USER)
        .open_subkey_with_flags(UNINSTALL_KEY_PATH, KEY_READ | KEY_WRITE)
        .with_context(|| {
            format!("Failed to open the uninstall key in the registry: {UNINSTALL_KEY_PATH}")
        })
}

/// Removes the uninstall information from the registry.
#[context("Failed to remove '{app_key}' uninstall information from the registry.")]
pub fn remove_from_registry(app_key: &str) -> Result {
    let uninstall_key = open_uninstall_key()?;
    uninstall_key.delete_subkey_all(app_key).with_context(|| {
        format!(
            "Failed to delete the '{app_key}' subkey from the '{UNINSTALL_KEY_PATH}' in the registry."
        )
    })
}
