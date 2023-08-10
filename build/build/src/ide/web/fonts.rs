use crate::prelude::*;

use crate::ide::web::google_font;

use enso_enso_font::ttf;
use enso_font::NonVariableFaceHeader;
use ide_ci::cache::Cache;



// =========================
// === HTML Font Support ===
// =========================

pub async fn install_html_fonts(
    cache: &Cache,
    octocrab: &Octocrab,
    output_path: impl AsRef<Path>,
) -> Result {
    let output_path = output_path.as_ref();
    google_font::download_google_font(cache, octocrab, "mplus1", output_path).await?;
    install_enso_font_for_html(cache, octocrab, output_path).await?;
    Ok(())
}

pub async fn install_enso_font_for_html(
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
    let html_font_definitions = enso_enso_font::enso_font()
        .variations()
        .filter(|v| html_fonts.contains_key(&v.header))
        .collect();
    let get_font_files = async {
        let package = get_enso_font_package_(cache, octocrab).await?;
        enso_enso_font::extract_fonts(&html_font_definitions, package, output_path).await
    };
    let make_css_file = async {
        let mut css = String::new();
        let family = "Enso";
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
            writeln!(&mut css, "@font-face {{")?;
            writeln!(&mut css, "  font-family: '{family}{variant}';")?;
            writeln!(&mut css, "    src: url('{url}/{file}');")?;
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



// =================
// === Enso Font ===
// =================

/// Download the Enso Font package, with caching and GitHub authentication.
pub async fn get_enso_font_package() -> Result<Box<Path>> {
    let cache = Cache::new_default().await?;
    let octocrab = ide_ci::github::setup_octocrab().await?;
    get_enso_font_package_(&cache, &octocrab).await
}

async fn get_enso_font_package_(cache: &Cache, octocrab: &Octocrab) -> Result<Box<Path>> {
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
