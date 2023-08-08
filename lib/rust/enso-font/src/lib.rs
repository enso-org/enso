//! The Enso Font. This crate supports downloading and unpacking the font family, as well as
//! constructing a reduced font family from a subset of the fonts.

// === Features ===
#![feature(let_chains)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]

use ide_ci::prelude::*;

use enso_font::NonVariableDefinition;
use enso_font::NonVariableFaceHeader;


// ==============
// === Export ===
// ==============

pub use owned_ttf_parser as ttf;



// =================
// === Constants ===
// =================

/// The name of the Enso font family.
pub const ENSO_FONT_FAMILY_NAME: &str = "enso";

const ENSO_FONT_FAMILY_FONTS: &[(&str, ttf::Weight)] = &[
    ("Black", ttf::Weight::Black),
    ("Bold", ttf::Weight::Bold),
    ("ExtraBold", ttf::Weight::ExtraBold),
    ("ExtraLight", ttf::Weight::ExtraLight),
    ("Light", ttf::Weight::Light),
    ("Medium", ttf::Weight::Medium),
    ("Regular", ttf::Weight::Normal),
    ("SemiBold", ttf::Weight::SemiBold),
    ("Thin", ttf::Weight::Thin),
];

/// The URL for the Enso Font package, excluding the final component.
pub const PACKAGE_BASE_URL: &str = "https://github.com/enso-org/font/releases/download/1.0/";
/// The final (filename) component of the URL for the Enso Font package.
pub const PACKAGE_FILE: &str = "enso-font-1.0.tar.gz";
const PACKAGE_FONTS_PREFIX: &str = "ttf";



// =================
// === Enso Font ===
// =================

/// Returns the Enso Font.
pub fn enso_font() -> NonVariableDefinition {
    ENSO_FONT_FAMILY_FONTS
        .iter()
        .map(|(name, weight)| {
            let file = format!("Enso-{name}.ttf");
            let header = NonVariableFaceHeader {
                weight: *weight,
                width:  ttf::Width::Normal,
                style:  ttf::Style::Normal,
            };
            (header, file)
        })
        .collect()
}

/// Extract the fonts from the given archive file, and write them in the given directory.
pub async fn extract_fonts(
    fonts: &NonVariableDefinition,
    archive_file: impl Read,
    out_dir: impl AsRef<Path>,
) -> Result {
    let tar = flate2::read::GzDecoder::new(archive_file);
    let mut archive = tar::Archive::new(tar);
    let mut files_expected: HashSet<_> = fonts.files().collect();
    let entries = archive.entries()?;
    for entry in entries {
        let mut entry = entry?;
        let file_path = entry.path()?;
        if let Ok(file_path) = file_path.strip_prefix(PACKAGE_FONTS_PREFIX)
            && let Some(file_path_str) = file_path.to_str()
            && files_expected.remove(file_path_str) {
            entry.unpack(out_dir.as_ref().join(file_path))?;
        }
    }
    ensure!(files_expected.is_empty(), "Required fonts not found in archive: {files_expected:?}.");
    Ok(())
}
