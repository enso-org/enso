//! Downloading DejaVu Fonts.

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

const PACKAGE_FONTS_PREFIX: &str = "DejaVu";

const PACKAGE_SANS_MONO_PREFIX: &str = "SansMono";

const DEJAVU_SANS_MONO_FONT_FAMILY_FONTS: &[(&str, font::Weight)] =
    &[("-Bold", font::Weight::Bold), ("", font::Weight::Normal)];



// ===================
// === DejaVu Font ===
// ===================

pub async fn download_dejavu_sans_mono_font_internal(
    cache: &Cache,
    octocrab: &Octocrab,
    output_path: impl AsRef<Path>,
    css_output_info: Option<(&str, impl AsRef<Path>)>,
) -> Result {
    let output_path = output_path.as_ref();
    let html_fonts: Vec<_> = [
        NonVariableFaceHeader { weight: font::Weight::Normal, ..default() },
        NonVariableFaceHeader { weight: font::Weight::Bold, ..default() },
    ]
    .into_iter()
    .collect();
    let html_font_definitions =
        dejavu_sans_mono_font().variations().filter(|v| html_fonts.contains(&v.header)).collect();
    let get_font_files = async {
        let package = get_dejavu_font_package_(cache, octocrab).await?;
        extract_fonts(&html_font_definitions, package, output_path).await
    };
    let make_css_file = async {
        if let Some((css_basepath, css_output_path)) = css_output_info {
            let contents = crate::ide::web::fonts::generate_css_file(
                css_basepath,
                FONT_FAMILY,
                &html_font_definitions,
                html_fonts.iter(),
            )
            .await?;
            ide_ci::fs::tokio::write(css_output_path, contents).await?;
            Ok(())
        } else {
            Ok(())
        }
    };
    try_join!(get_font_files, make_css_file)?;
    Ok(())
}

pub async fn download_dejavu_sans_mono_font(
    cache: &Cache,
    octocrab: &Octocrab,
    output_path: impl AsRef<Path>,
) -> Result {
    download_dejavu_sans_mono_font_internal(cache, octocrab, output_path, None::<(&str, &str)>)
        .await
}

pub async fn download_dejavu_sans_mono_font_with_css(
    cache: &Cache,
    octocrab: &Octocrab,
    css_basepath: &str,
    output_path: impl AsRef<Path>,
    css_output_path: impl AsRef<Path>,
) -> Result {
    download_dejavu_sans_mono_font_internal(
        cache,
        octocrab,
        output_path,
        Some((css_basepath, css_output_path)),
    )
    .await
}

/// Returns the DejaVu Font.
pub fn dejavu_sans_mono_font() -> NonVariableDefinition {
    DEJAVU_SANS_MONO_FONT_FAMILY_FONTS
        .iter()
        .map(|(name, weight)| {
            let file = format!("{PACKAGE_FONTS_PREFIX}{PACKAGE_SANS_MONO_PREFIX}{name}.ttf");
            let header = NonVariableFaceHeader {
                weight: *weight,
                width:  font::Width::Normal,
                style:  font::Style::Normal,
            };
            (header, file)
        })
        .collect()
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
