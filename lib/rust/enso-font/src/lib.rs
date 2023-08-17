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

/// The URL for the Enso Font package.
pub const PACKAGE_URL: &str =
    "https://github.com/enso-org/font/releases/download/1.0/enso-font-1.0.tar.gz";
const PACKAGE_FONTS_PREFIX: &str = "ttf";

/// Font features.
pub mod feature {
    /// The flag identifying the ligature feature in this font.
    pub const LIGATURES: &str = "liga";
}



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
#[context("Failed to extract fonts from archive: {}", package.as_ref().display())]
pub async fn extract_fonts(
    fonts: &NonVariableDefinition,
    package: impl AsRef<Path>,
    out_dir: impl AsRef<Path>,
) -> Result {
    let mut files_expected: HashSet<_> = fonts.files().collect();
    ide_ci::archive::tar::Archive::open_tar_gz(&package)
        .await?
        .extract_files(|path_in_archive| {
            path_in_archive
                .strip_prefix(PACKAGE_FONTS_PREFIX)
                .ok()
                .filter(|path| path.to_str().map_or(false, |path| files_expected.remove(path)))
                .map(|path| out_dir.as_ref().join(path))
        })
        .await?;
    ensure!(files_expected.is_empty(), "Required fonts not found in archive: {files_expected:?}.");
    Ok(())
}
