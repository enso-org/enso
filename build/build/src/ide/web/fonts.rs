use crate::prelude::*;
use enso_enso_font::SubfamilyBuilder;

use crate::ide::web::download::get_file_from_cache_or_download;
use crate::ide::web::google_font;
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
    let archive = get_enso_font_package_(cache, octocrab.clone()).await?;
    let all_fonts = enso_enso_font::FontFamily::enso();
    // Acquire the fonts required by [`HtmlFontFamily`].
    let mut subfamily = all_fonts.subfamily_builder();
    let html_fonts = HtmlFontFamily::new(&mut subfamily)?;
    let fonts_needed = subfamily.finish();
    fonts_needed.extract_fonts(archive, output_path).await?;
    // Create a CSS file for the [`HtmlFontFamily`].
    let css = html_fonts.css("Enso", ".");
    let css_path = output_path.join("ensoFont.css");
    let mut css_file = ide_ci::fs::tokio::create(css_path).await?;
    css_file.write_all(css.as_bytes()).await?;
    Ok(())
}

struct HtmlFontFamily {
    regular: String,
    bold:    String,
}

impl HtmlFontFamily {
    fn new(fonts: &mut SubfamilyBuilder) -> Result<Self> {
        const MISSING_FONT: &str = "Required font not found in package.";
        Self::new_(fonts).ok_or(anyhow!("{}", MISSING_FONT))
    }

    fn new_(fonts: &mut SubfamilyBuilder) -> Option<Self> {
        use enso_enso_font::ttf;
        let regular = fonts.require(default(), ttf::Weight::Normal, default())?.filename();
        let bold = fonts.require(default(), ttf::Weight::ExtraBold, default())?.filename();
        Some(Self { regular, bold })
    }

    fn css(&self, family: &str, url: &str) -> String {
        let mut css = String::new();
        Self::make_face(&mut css, family, "Regular", url, &self.regular).unwrap();
        Self::make_face(&mut css, family, "Bold", url, &self.bold).unwrap();
        css
    }

    fn make_face(
        mut css: impl std::fmt::Write,
        family: &str,
        variant: &str,
        url: &str,
        file: &str,
    ) -> Result {
        writeln!(css, "@font-face {{")?;
        writeln!(css, "  font-family: '{family}{variant}';")?;
        writeln!(css, "    src: url('{url}/{file}');")?;
        writeln!(css, "  font-weight: normal;")?;
        writeln!(css, "  font-style: normal;")?;
        writeln!(css, "}}")?;
        Ok(())
    }
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
