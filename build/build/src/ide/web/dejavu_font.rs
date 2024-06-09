//! Definitions for DejaVu fonts, and functions for downloading and installing them.

use crate::prelude::*;

use enso_font as font;
use enso_font::NonVariableDefinition;
use enso_font::NonVariableFaceHeader;
use ide_ci::cache::Cache;



// =================
// === Constants ===
// =================

pub const PACKAGE_URL: &str = "https://github.com/dejavu-fonts/dejavu-fonts/releases/download/version_2_37/dejavu-fonts-ttf-2.37.zip";

const FONT_FAMILY: &str = "DejaVu Sans Mono";

const FILE_PREFIX: &str = "DejaVu";

const FILE_SANS_MONO_PREFIX: &str = "SansMono";

const SANS_MONO_FONT_FAMILY_FONTS: &[(&str, font::Weight)] =
    &[("-Bold", font::Weight::Bold), ("", font::Weight::Normal)];



// ===================
// === DejaVu Font ===
// ===================

/// Internal helper function to download the DejaVu Sans Mono font. Exposed via thin wrapper
/// functions.
async fn install_sans_mono_internal(
    cache: &Cache,
    octocrab: &Octocrab,
    output_path: impl AsRef<Path>,
    css_output_info: Option<(&str, impl AsRef<Path>)>,
) -> Result {
    let output_path = output_path.as_ref();
    let font = font();
    let faces = faces();
    let font = crate::ide::web::fonts::filter_font(&font, &faces);
    let package = download(cache, octocrab).await?;
    let get_font_files = extract_fonts(&font, package, output_path);
    let make_css_file = crate::ide::web::fonts::write_css_file_if_required(
        FONT_FAMILY,
        &font,
        &faces,
        css_output_info,
    );
    try_join!(get_font_files, make_css_file)?;
    Ok(())
}

/// Install DejaVu Sans Mono, without an auto-generated CSS file.
pub async fn install_sans_mono(
    cache: &Cache,
    octocrab: &Octocrab,
    output_path: impl AsRef<Path>,
) -> Result {
    install_sans_mono_internal(cache, octocrab, output_path, None::<(&str, &str)>).await
}

/// Install DejaVu Sans Mono, including an auto-generated CSS file.
pub async fn install_sans_mono_with_css(
    cache: &Cache,
    octocrab: &Octocrab,
    css_basepath: &str,
    output_path: impl AsRef<Path>,
    css_output_path: impl AsRef<Path>,
) -> Result {
    install_sans_mono_internal(cache, octocrab, output_path, Some((css_basepath, css_output_path)))
        .await
}

/// The DejaVu Sans Mono Font.
pub fn font() -> NonVariableDefinition {
    SANS_MONO_FONT_FAMILY_FONTS
        .iter()
        .map(|(name, weight)| {
            let file = format!("{FILE_PREFIX}{FILE_SANS_MONO_PREFIX}{name}.ttf");
            let header = NonVariableFaceHeader {
                weight: *weight,
                width:  font::Width::Normal,
                style:  font::Style::Normal,
            };
            (header, file)
        })
        .collect()
}

/// All font faces contained in this font.
pub fn faces() -> [NonVariableFaceHeader; 2] {
    [NonVariableFaceHeader { weight: font::Weight::Normal, ..default() }, NonVariableFaceHeader {
        weight: font::Weight::Bold,
        ..default()
    }]
}

/// Extract the fonts from the given archive file, and write them in the given directory.
pub async fn extract_fonts(
    fonts: &NonVariableDefinition,
    package: impl AsRef<Path>,
    out_dir: impl AsRef<Path>,
) -> Result {
    let mut archive = ide_ci::archive::zip::open(&package)?;
    crate::ide::web::fonts::extract_fonts(&mut archive, fonts, package, out_dir, &mut |path| {
        let mut iter = path.iter();
        for _ in iter.by_ref().take(2) {}
        Box::from(iter.as_str())
    })
    .await
}

/// Download the DejaVu Font package, with caching and GitHub authentication.
pub async fn download(cache: &Cache, octocrab: &Octocrab) -> Result<Box<Path>> {
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
