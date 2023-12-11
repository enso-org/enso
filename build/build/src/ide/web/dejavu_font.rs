//! Downloading DejaVu Fonts.

use crate::prelude::*;

use enso_font::NonVariableDefinition;
use enso_font::NonVariableFaceHeader;
use ide_ci::cache::Cache;



// =================
// === Constants ===
// =================

pub const PACKAGE_URL: &str = "hhttps://github.com/dejavu-fonts/dejavu-fonts/releases/download/version_2_37/dejavu-fonts-ttf-2.37.zip";

const PACKAGE_FONTS_PREFIX: &str = "dejavu-fonts-2.37/ttf/DejaVu";

const PACKAGE_SANS_MONO_PREFIX: &str = "SansMono";

const DEJAVU_SANS_MONO_FONT_FAMILY_FONTS: &[(&str, enso_font::Weight)] =
    &[("-Bold", enso_font::Weight::Bold), ("", enso_font::Weight::Normal)];



// ===================
// === DejaVu Font ===
// ===================

pub async fn download_dejavu_sans_mono_font(
    cache: &Cache,
    octocrab: &Octocrab,
    output_path: impl AsRef<Path>,
    css_output_path: impl AsRef<Path>,
) -> Result {
    let output_path = output_path.as_ref();
    let html_fonts: Vec<_> = [
        NonVariableFaceHeader { weight: enso_font::Weight::Normal, ..default() },
        NonVariableFaceHeader { weight: enso_font::Weight::Bold, ..default() },
    ]
    .into_iter()
    .collect();
    let html_font_definitions = enso_enso_font::enso_font()
        .variations()
        .filter(|v| html_fonts.contains(&v.header))
        .collect();
    let get_font_files = async {
        let package = get_dejavu_font_package_(cache, octocrab).await?;
        enso_enso_font::extract_fonts(&html_font_definitions, package, output_path).await
    };
    let make_css_file = async {
        let mut css = String::new();
        let family = "Enso";
        let url = ".";
        for header in html_fonts {
            use std::fmt::Write;
            let def = html_font_definitions.get(header);
            let def = def.ok_or_else(|| {
                anyhow!(
                    "Required font not found in Enso Font package. \
                  Expected a font matching: {header:?}."
                )
            })?;
            let file = def.file;
            let weight = def.header.weight.to_number();
            writeln!(&mut css, "@font-face {{")?;
            writeln!(&mut css, "  font-family: '{family}';")?;
            writeln!(&mut css, "  src: url('{url}/{file}');")?;
            writeln!(&mut css, "  font-weight: {weight};")?;
            writeln!(&mut css, "  font-style: normal;")?;
            writeln!(&mut css, "}}")?;
            writeln!(&mut css, "")?;
        }
        ide_ci::fs::tokio::write(css_output_path, css).await?;
        Ok(())
    };
    try_join!(get_font_files, make_css_file)?;
    Ok(())
}

/// Returns the DejaVu Font.
pub fn dejavu_sans_mono_font() -> NonVariableDefinition {
    DEJAVU_SANS_MONO_FONT_FAMILY_FONTS
        .iter()
        .map(|(name, weight)| {
            let file = format!("{PACKAGE_FONTS_PREFIX}{PACKAGE_SANS_MONO_PREFIX}{name}.ttf");
            let header = NonVariableFaceHeader {
                weight: *weight,
                width:  enso_font::Width::Normal,
                style:  enso_font::Style::Normal,
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
    ide_ci::archive::zip::open(&package)
        .ok()
        .map(|archive| {
            archive.extract_file(
                ide_ci::archive::zip::read::ZipFile {}, /*  | path_in_archive | {
                                                         * files_expected
                                                         * .remove(path_in_archive)
                                                         * .ok()
                                                         * .filter(|path| {
                                                         * path.to_str().map_or(false, |path|
                                                         * files_expected.remove(path))
                                                         * })
                                                         * .map(|path|
                                                         * out_dir.as_ref().join(path))
                                                         * }, */
            )
        })
        .await?;
    ensure!(files_expected.is_empty(), "Required fonts not found in archive: {files_expected:?}.");
    Ok(())
}

/// Download the DejaVu Font package, with caching and GitHub authentication.
pub async fn get_dejavu_font_package() -> Result<Box<Path>> {
    let cache = Cache::new_default().await?;
    let octocrab = ide_ci::github::setup_octocrab().await?;
    get_dejavu_font_package_(&cache, &octocrab).await
}

async fn get_dejavu_font_package_(cache: &Cache, octocrab: &Octocrab) -> Result<Box<Path>> {
    Ok(cache
        .get(ide_ci::cache::download::DownloadFile {
            client: octocrab.client.clone(),
            key:    ide_ci::cache::download::Key {
                url:                PACKAGE_URL.parse().unwrap(),
                additional_headers: reqwest::header::HeaderMap::from_iter([(
                    reqwest::header::ACCEPT,
                    reqwest::header::HeaderValue::from_static(
                        mime::APPLICATION_OCTET_STREAM.as_ref(),
                    ),
                )]),
            },
        })
        .await?
        .into_boxed_path())
}
