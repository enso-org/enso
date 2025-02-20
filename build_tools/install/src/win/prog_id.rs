//! Code for handling [programmatic identifiers](https://docs.microsoft.com/en-us/windows/win32/com/programmatic-identifiers).
//!
//! Basically, there are two kinds of programmatic identifiers:
//! * File extension associations;
//! * URL protocol associations.
//!
//! Programmatic identifiers are stored in the Windows registry under the:
//! * `HKEY_CURRENT_USER\Software\Classes` key for the current user;
//! * `HKEY_LOCAL_MACHINE\Software\Classes` key for all users.
//!
//! `HKEY_CLASSES_ROOT` is a merged view of these two keys.

use crate::prelude::*;
use winreg::enums::*;

use crate::win::registry::create_subkey;



/// A set of broad categories for file types hard-coded into Windows.
///
/// See: https://learn.microsoft.com/en-us/windows/win32/api/shtypes/ne-shtypes-perceived
#[derive(Debug, Clone, Copy, strum::AsRefStr)]
// [mwu] I have no idea if these are case-sensitive but why would we want to risk it?
#[strum(serialize_all = "snake_case")]
pub enum PerceivedType {
    Folder,
    Text,
    Image,
    Audio,
    Video,
    Compressed,
    Document,
    System,
    Application,
    Gamemedia,
    Contacts,
}

impl PerceivedType {
    /// Deduce `PerceivedType` from MIME type.
    pub fn from_mime_type(mime_type: &str) -> Result<Self> {
        Ok(match mime_type {
            "text/plain" => Self::Text,
            "application/gzip" | "application/zip" => Self::Compressed,
            _ => bail!("MIME type without a corresponding perceived type: '{mime_type}'."),
        })
    }
}

/// The file extension description.
///
/// Apart from some basic information, it directs to the ProgID of the file type.
#[derive(Clone, Debug)]
pub struct FileExtension {
    /// The file extension including the leading dot, e.g. `.enso`.
    pub extension:      String,
    /// The `ProgID` of the file type.
    pub prog_id:        String,
    /// The MIME type of the file.
    pub mime_type:      String,
    /// A broad category for the file type, e.g. `text`.
    pub perceived_type: PerceivedType,
}

impl FileExtension {
    /// Write information about the file extension to the Windows registry.
    ///
    /// Note that this only writes a reference to the ProgID of the file type. The file type
    /// itself must be registered separately using [`FileType::register`].
    pub fn register(&self) -> Result {
        let classes = crate::win::classes_key()?;
        // https://learn.microsoft.com/en-us/windows/win32/shell/fa-file-types#setting-optional-subkeys-and-file-type-extension-attributes
        let (file_ext_key, _disposition) = classes.create_subkey(&self.extension)?;
        file_ext_key.set_value("", &self.prog_id)?;
        file_ext_key.set_value("Content Type", &self.mime_type)?;
        file_ext_key.set_value("PerceivedType", &"text")?;
        Ok(())
    }
}

/// The Programmatic Identifier (ProgID) of a file type.
///
/// These are associated with file extensions in the Windows registry and describe how to
/// open files of that type. More than one file extension can be associated with a single
/// ProgID.
#[derive(Clone, Debug)]
pub struct FileType {
    /// The absolute path to the application executable.
    pub application_path: PathBuf,
    /// The `ProgID` of the file type - a unique identifier for the file type.
    pub prog_id:          String,
    /// The friendly name of the file type.
    pub friendly_name:    String,
    /// The text to display in the info popup when hovering over a file of this type.
    pub info_tip:         String,
}

impl FileType {
    /// Write information about the file type to the Windows registry.
    pub fn register(&self) -> Result {
        let classes = crate::win::classes_key()?;
        let path_str = self.application_path.as_str();

        // https://learn.microsoft.com/en-us/windows/win32/shell/fa-progids
        // Describe the Programmatic Identifier (ProgID) of the file type.
        let (prog_id_key, _disposition) = classes.create_subkey(&self.prog_id)?;
        prog_id_key.set_value("", &self.friendly_name)?;
        prog_id_key.set_value("FriendlyTypeName", &self.friendly_name)?;
        prog_id_key.set_value("InfoTip", &self.info_tip)?;
        // 0 is the index of the icon in the executable (i.e. the first icon).
        // See: https://docs.microsoft.com/en-us/windows/win32/shell/fa-file-types#setting-optional-subkeys-and-file-type-extension-attributes
        prog_id_key.create_subkey("DefaultIcon")?.0.set_value("", &format!(r#""{path_str}",0"#))?;
        prog_id_key
            .create_subkey(r"shell\open\command")?
            .0
            // Note that the quotes around both paths are required for paths with spaces.
            .set_value("", &format!(r#""{path_str}" "%1""#))?;
        Ok(())
    }
}

/// Delete given programmatic identifier from the Windows registry.
#[context("Failed to delete ProgID `{}`.", prog_id)]
pub fn delete(prog_id: &str) -> Result {
    // Must be non-empty, or we will delete the entire `Classes` key.
    ensure!(!prog_id.is_empty(), "Programmatic identifier must not be empty.");
    let classes = crate::win::classes_key()?;
    classes.delete_subkey_all(prog_id)?;
    Ok(())
}

/// Information about a URL protocol.
#[derive(Debug)]
pub struct ProtocolInfo {
    /// The protocol sheme name, e.g. `enso`.
    pub protocol:     String,
    /// The icon for the protocol.
    pub icon:         crate::win::Icon,
    /// The command to run when opening the protocol's URL.
    pub command:      crate::win::PlainOpenCommand,
    /// Display name of the protocol.
    pub display_name: Option<String>,
}

impl ProtocolInfo {
    pub fn new(protocol: impl Into<String>, executable_path: impl Into<PathBuf>) -> Self {
        let protocol = protocol.into();
        let display_name = Some(format!("URL:{protocol} protocol"));
        let executable_path = executable_path.into();
        Self {
            protocol,
            icon: crate::win::Icon::new(&executable_path),
            command: crate::win::PlainOpenCommand::new(executable_path),
            display_name,
        }
    }

    /// Write information about the protocol to the Windows registry.
    pub fn register(&self) -> Result {
        let classes = crate::win::classes_key()?;
        let (protocol_key, _) = create_subkey(&classes, &self.protocol)?;
        protocol_key.set_value("URL Protocol", &"")?;
        if let Some(display_name) = &self.display_name {
            protocol_key.set_value("", display_name)?;
        }
        self.icon.write_default_icon_subkey(&protocol_key)?;
        self.command.set_as_shell_open_command(&protocol_key)?;
        Ok(())
    }
}

/// Set application as the default handler for the given URL protocol, e.g. `enso://`.
///
/// This is necessary for the deep linking to work.
pub fn register_url_protocol(executable_path: &Path, protocol: &str) -> Result {
    // See https://learn.microsoft.com/en-us/windows/win32/shell/app-registration

    // Register the URL protocol.
    let (url_key, _) = RegKey::predef(HKEY_CURRENT_USER)
        .open_subkey_with_flags(r"Software\Classes", KEY_READ | KEY_WRITE)
        .context("Failed to open `HKEY_CURRENT_USER\\Software\\Classes` key.")?
        .create_subkey(protocol)
        .with_context(|| format!(r#"Failed to create subkey for protocol `{protocol}`"#))?;

    url_key.set_value("", &format!("URL:{protocol}"))?;
    url_key.set_value("URL Protocol", &"")?;

    let (command_key, _) = url_key
        .create_subkey(r"shell\open\command")
        .with_context(|| format!(r#"Failed to create subkey for protocol: {protocol}"#))?;
    command_key.set_value("", &format!(r#""{}" "%1""#, executable_path.display()))?;

    Ok(())
}
