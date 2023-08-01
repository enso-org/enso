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
use owned_ttf_parser as ttf;

use enso_build::ide::web::download::get_file_from_cache_or_download;



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

const PACKAGE_BASE_URL: &str = "https://github.com/enso-org/font/releases/download/1.0/";
const PACKAGE_FILE: &str = "enso-font-1.0.tar.gz";
const PACKAGE_FONTS_PREFIX: &str = "ttf";



// ===================
// === Font Family ===
// ===================

#[derive(Debug)]
pub struct FontFamily {
    fonts: Vec<Font>,
}

impl FontFamily {
    pub fn enso() -> Self {
        FontFamily { fonts: ENSO_FONT_FAMILY_FONTS.iter().cloned().collect() }
    }

    pub fn name(&self) -> &'static str {
        ENSO_FONT_FAMILY_NAME
    }

    pub async fn download_fonts(&self, out_dir: impl AsRef<Path>) -> Result {
        let archive_file = Self::get_archive().await?;
        self.extract_fonts(archive_file, out_dir)
    }

    pub fn fonts(&self) -> impl Iterator<Item=&Font> {
        self.fonts.iter()
    }
}

impl FontFamily {
    async fn get_archive() -> Result<std::fs::File> {
        let octocrab = ide_ci::github::setup_octocrab().await?;
        let cache = ide_ci::cache::Cache::new_default().await?;
        let url = format!("{}{}", PACKAGE_BASE_URL, PACKAGE_FILE);
        get_file_from_cache_or_download(Path::new(PACKAGE_FILE), &cache, octocrab, url).await
    }

    fn extract_fonts(&self, archive_file: impl Read, out_dir: impl AsRef<Path>) -> Result {
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
}



// ============
// === Font ===
// ============

#[derive(Debug, Clone, Copy)]
pub struct Font {
    name:   &'static str,
    weight: ttf::Weight,
}

impl Font {
    pub fn filename(&self) -> String {
        format!("Enso-{}.ttf", self.name)
    }

    pub fn width(&self) -> ttf::Width {
        Default::default()
    }

    pub fn weight(&self) -> ttf::Weight {
        self.weight
    }

    pub fn style(&self) -> ttf::Style {
        Default::default()
    }
}
