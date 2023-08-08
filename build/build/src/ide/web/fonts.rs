use crate::prelude::*;

use crate::ide::web::download::get_file_from_cache_or_download;
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
        let archive = get_enso_font_package_(cache, octocrab.clone()).await?;
        enso_enso_font::extract_fonts(&html_font_definitions, archive, output_path).await
    };
    let make_css_file = async {
        let mut css = String::new();
        let family = "Enso";
        let url = ".";
        for (header, variant) in html_fonts {
            use std::fmt::Write;
            let def = html_font_definitions.get(header);
            let def = def.ok_or(anyhow!("Required font not found in package."))?;
            let file = &def.file;
            writeln!(&mut css, "@font-face {{")?;
            writeln!(&mut css, "  font-family: '{family}{variant}';")?;
            writeln!(&mut css, "    src: url('{url}/{file}');")?;
            writeln!(&mut css, "  font-weight: normal;")?;
            writeln!(&mut css, "  font-style: normal;")?;
            writeln!(&mut css, "}}")?;
        }
        let css_path = output_path.join("ensoFont.css");
        let mut css_file = ide_ci::fs::tokio::create(css_path).await?;
        css_file.write_all(css.as_bytes()).await?;
        Ok(())
    };
    try_join!(get_font_files, make_css_file)?;
    Ok(())
}



// =================
// === Enso Font ===
// =================

/// Download the Enso Font package, with caching and GitHub authentication.
pub async fn get_enso_font_package() -> Result<std::fs::File> {
    let cache = Cache::new_default().await?;
    let octocrab = ide_ci::github::setup_octocrab().await?;
    get_enso_font_package_(&cache, octocrab).await
}

async fn get_enso_font_package_(cache: &Cache, octocrab: Octocrab) -> Result<std::fs::File> {
    let url = format!("{}{}", enso_enso_font::PACKAGE_BASE_URL, enso_enso_font::PACKAGE_FILE);
    let path = Path::new(enso_enso_font::PACKAGE_FILE);
    get_file_from_cache_or_download(path, cache, octocrab, url).await
}
