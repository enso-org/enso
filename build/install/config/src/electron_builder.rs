//! Rust representation of the `electron-builder` configuration.

use crate::prelude::*;

use serde::Deserialize;
use serde::Serialize;



/// A subset of the configuration options available in the `electron-builder` configuration.
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    /// The application ID, usually in reverse domain name notation. Example: `"org.enso"`.
    pub app_id: String,

    /// The name of the product. Example: `"Enso"`.
    pub product_name: String,

    /// Extra metadata for the application, like version. Example: `{"version": "2023.2.1-dev"}`.
    pub extra_metadata: ExtraMetadata,

    /// Copyright notice of the application. Example: `"Copyright © 2023 New Byte Order sp. z
    /// o.o."`.
    pub copyright: String,

    /// Pattern for naming artifact files. Example: `"enso-${os}-2023.2.1-dev.${ext}"`.
    pub artifact_name: String,

    /// Custom protocol schemes that the application handles. Example: `[{ "name": "Enso url",
    /// "schemes": ["enso"], "role": "Editor" }]`.
    pub protocols: Vec<Protocol>,

    /// Configuration specific to Windows builds.
    pub win: WinConfig,

    /// File associations for the application. Example: `[{ "ext": "enso", "name": "Enso Source
    /// File", "role": "Editor" }, {...}]`.
    pub file_associations: Vec<FileAssociation>,

    /// Configuration for the output directories. Example: `{ "output":
    /// "/home/mwu/Desktop/enso/dist/ide2" }`.
    pub directories: Directories,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExtraMetadata {
    /// Version of the application. Example: `"2023.2.1-dev"`.
    pub version: Version,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Protocol {
    /// Name of the protocol. Example: `"Enso url"`.
    pub name: String,

    /// Schemes associated with the protocol. Example: `["enso"]`.
    pub schemes: Vec<String>,

    /// Role of the application in handling the protocol. Example: `"Editor"`.
    pub role: String,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WinConfig {
    /// Path to the icon file for Windows. Example: `"/tmp/.tmpMPYWpz/icon.ico"`.
    pub icon: PathBuf,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FileAssociation {
    /// File extension to associate. Example: `"enso"`.
    pub ext: String,

    /// Name of the file type. Example: `"Enso Source File"`.
    pub name: String,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Directories {
    /// Output directory for the build. Example: `"/home/mwu/Desktop/enso/dist/ide2"`.
    pub output: PathBuf,
}
