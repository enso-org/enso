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
pub use owned_ttf_parser as ttf;



// =================
// === Constants ===
// =================

const ENSO_FONT_FAMILY_NAME: &str = "enso";

const ENSO_FONT_FAMILY_FONTS: &[Font] = &[
    Font { name: "Black", weight: ttf::Weight::Black },
    Font { name: "Bold", weight: ttf::Weight::Bold },
    Font { name: "ExtraBold", weight: ttf::Weight::ExtraBold },
    Font { name: "ExtraLight", weight: ttf::Weight::ExtraLight },
    Font { name: "Light", weight: ttf::Weight::Light },
    Font { name: "Medium", weight: ttf::Weight::Medium },
    Font { name: "Regular", weight: ttf::Weight::Normal },
    Font { name: "SemiBold", weight: ttf::Weight::SemiBold },
    Font { name: "Thin", weight: ttf::Weight::Thin },
];

/// The URL for the Enso Font package, excluding the final component.
pub const PACKAGE_BASE_URL: &str = "https://github.com/enso-org/font/releases/download/1.0/";
/// The final (filename) component of the URL for the Enso Font package.
pub const PACKAGE_FILE: &str = "enso-font-1.0.tar.gz";
const PACKAGE_FONTS_PREFIX: &str = "ttf";



// ===================
// === Font Family ===
// ===================

/// A collection of fonts.
#[derive(Debug)]
pub struct FontFamily {
    fonts: Vec<Font>,
}

impl FontFamily {
    /// Returns the Enso Font.
    pub fn enso() -> Self {
        FontFamily { fonts: ENSO_FONT_FAMILY_FONTS.to_vec() }
    }

    /// This font family's name.
    pub fn name(&self) -> &'static str {
        ENSO_FONT_FAMILY_NAME
    }

    /// Extract the fonts from the given archive file, and write them in the given directory.
    pub async fn extract_fonts(
        &self,
        archive_file: impl Read,
        out_dir: impl AsRef<Path>,
    ) -> Result {
        let tar = flate2::read::GzDecoder::new(archive_file);
        let mut archive = tar::Archive::new(tar);
        let mut files_expected: HashSet<_> = self.fonts.iter().map(|v| v.filename()).collect();
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
        ensure!(
            files_expected.is_empty(),
            "Required fonts not found in archive: {files_expected:?}."
        );
        Ok(())
    }

    /// Returns an iterator of the fonts in this family.
    pub fn fonts(&self) -> impl Iterator<Item = &Font> {
        self.fonts.iter()
    }

    /// Creates a [`SubfamilyBuilder`], which can be used to construct a new [`FontFamily`]
    /// containing a subset of the fonts from this family.
    pub fn subfamily_builder(&self) -> SubfamilyBuilder {
        SubfamilyBuilder::new(self)
    }

    /// Return the font in this family that exactly matches the given parameters, if any is found.
    pub fn get(&self, width: ttf::Width, weight: ttf::Weight, style: ttf::Style) -> Option<&Font> {
        self.fonts
            .iter()
            .find(|font| font.width() == width && font.weight() == weight && font.style() == style)
    }
}



// =========================
// === Subfamily Builder ===
// =========================

/// Supports construct a [`FontFamily`] containing a subset of the fonts from a parent family.
#[derive(Debug)]
pub struct SubfamilyBuilder<'a> {
    base:       &'a FontFamily,
    referenced: HashSet<&'static str>,
}

impl<'a> SubfamilyBuilder<'a> {
    /// Mark the font meeting the given specifications to be included in the subfamily, and return a
    /// reference to it.
    pub fn require(
        &mut self,
        width: ttf::Width,
        weight: ttf::Weight,
        style: ttf::Style,
    ) -> Option<&Font> {
        let font = self.base.get(width, weight, style);
        if let Some(font) = font {
            self.referenced.insert(font.name);
        }
        font
    }

    /// Create a new font family containing the fonts in the base family that were referenced by
    /// [`require`].
    pub fn finish(self) -> FontFamily {
        let fonts_referenced =
            self.base.fonts.iter().filter(|font| self.referenced.contains(font.name));
        let fonts = fonts_referenced.cloned().collect();
        FontFamily { fonts }
    }
}

impl<'a> SubfamilyBuilder<'a> {
    fn new(base: &'a FontFamily) -> Self {
        Self { base, referenced: Default::default() }
    }
}



// ============
// === Font ===
// ============

/// A font, for rendering text with specific parameters (e.g. weight), in the style of a
/// [`FontFamily`].
#[derive(Debug, Clone, Copy)]
pub struct Font {
    name:   &'static str,
    weight: ttf::Weight,
}

impl Font {
    /// Return the font's filename.
    pub fn filename(&self) -> String {
        format!("Enso-{}.ttf", self.name)
    }

    /// Return the font's width.
    pub fn width(&self) -> ttf::Width {
        Default::default()
    }

    /// Return the font's weight.
    pub fn weight(&self) -> ttf::Weight {
        self.weight
    }

    /// Return the font's style.
    pub fn style(&self) -> ttf::Style {
        Default::default()
    }
}
