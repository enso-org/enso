//! Definitions for Enso Font, and functions for downloading and installing them.

use crate::prelude::*;

use enso_enso_font::ttf;
use enso_font::NonVariableFaceHeader;
use ide_ci::cache::Cache;


// =================
// === Constants ===
// =================

const FONT_FAMILY: &str = "Enso";



// =================
// === Enso Font ===
// =================

pub async fn install_for_html(
    cache: &Cache,
    octocrab: &Octocrab,
    output_path: impl AsRef<Path>,
) -> Result {
    let output_path = output_path.as_ref();
    let html_fonts: HashMap<_, _> = [
        (NonVariableFaceHeader { weight: ttf::Weight::Normal, ..default() }, "Regular"),
        (NonVariableFaceHeader { weight: ttf::Weight::ExtraBold, ..default() }, "Bold"),
    ]
    .into_iter()
    .collect();
    let html_font_definitions = enso_enso_font::font()
        .variations()
        .filter(|v| html_fonts.contains_key(&v.header))
        .collect();
    let get_font_files = async {
        let package = download(cache, octocrab).await?;
        enso_enso_font::extract_fonts(&html_font_definitions, package, output_path).await
    };
    let make_css_file = async {
        let mut css = String::new();
        let url = ".";
        for (header, variant) in html_fonts {
            use std::fmt::Write;
            let def = html_font_definitions.get(header);
            let def = def.ok_or_else(|| {
                anyhow!(
                    "Required font not found in Enso Font package. \
                  Expected a font matching: {header:?}."
                )
            })?;
            let file = &def.file;
            // Note that this cannot use `generate_css_file`, as it specifies a different font
            // family for each variant.
            writeln!(&mut css, "@font-face {{")?;
            writeln!(&mut css, "  font-family: '{FONT_FAMILY}{variant}';")?;
            writeln!(&mut css, "  src: url('{url}/{file}');")?;
            writeln!(&mut css, "  font-weight: normal;")?;
            writeln!(&mut css, "  font-style: normal;")?;
            writeln!(&mut css, "}}")?;
        }
        let css_path = output_path.join("ensoFont.css");
        ide_ci::fs::tokio::write(css_path, css).await?;
        Ok(())
    };
    try_join!(get_font_files, make_css_file)?;
    Ok(())
}

pub async fn install_with_css(
    cache: &Cache,
    octocrab: &Octocrab,
    css_basepath: &str,
    output_path: impl AsRef<Path>,
    css_output_path: impl AsRef<Path>,
) -> Result {
    let output_path = output_path.as_ref();
    let font = enso_enso_font::font();
    let faces = enso_enso_font::faces();
    let font = crate::ide::web::fonts::filter_font(&font, &faces);
    let package = download(cache, octocrab).await?;
    let get_font_files = enso_enso_font::extract_fonts(&font, package, output_path);
    let css_output_info = Some((css_basepath, css_output_path));
    let make_css_file = crate::ide::web::fonts::write_css_file_if_required(
        FONT_FAMILY,
        &font,
        &faces,
        css_output_info,
    );
    try_join!(get_font_files, make_css_file)?;
    Ok(())
}

/// Download the Enso Font package, with caching and GitHub authentication.
pub async fn download(cache: &Cache, octocrab: &Octocrab) -> Result<Box<Path>> {
    Ok(cache
        .get(ide_ci::cache::download::DownloadFile {
            client: octocrab.client.clone(),
            key:    ide_ci::cache::download::Key {
                url:                enso_enso_font::PACKAGE_URL.parse().unwrap(),
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
