use crate::prelude::*;

use enso_font::NonVariableDefinition;
use enso_font::NonVariableFaceHeader;
use ide_ci::archive::extract_files::ExtractFiles;
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
    crate::ide::web::google_font::install(cache, octocrab, "mplus1", output_path).await?;
    crate::ide::web::enso_font::install_for_html(cache, octocrab, output_path).await?;
    Ok(())
}

/// A CSS font style that is displayed as a CSS [`@font-face`] [`font-style`] value.
///
/// [`@font-face`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face
/// [`font-style`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face/font-style
#[derive(Debug, Display, Copy, Clone)]
pub enum FontStyle {
    #[display(fmt = "normal")]
    Normal,
    #[display(fmt = "italic")]
    Italic,
    #[display(fmt = "oblique")]
    Oblique,
    /// Angle is in degrees, between -90 and 90.
    #[display(fmt = "oblique {_0}deg")]
    ObliqueWithAngle(f64),
    /// Angles are in degrees, between -90 and 90.
    #[display(fmt = "oblique {_0}deg {_1}deg")]
    ObliqueWithAngleRange(f64, f64),
}

/// A CSS font face that is displayed as a CSS [`@font-face`] declaration.
///
/// [`@font-face`]: https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face
#[derive(Debug, Clone)]
pub struct FontFace<'a> {
    family: Cow<'a, str>,
    path:   Cow<'a, str>,
    weight: Option<u16>,
    style:  Option<FontStyle>,
}

impl Display for FontFace<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let family = &self.family;
        let path = &self.path;
        writeln!(f, "@font-face {{")?;
        writeln!(f, "  font-family: '{family}';")?;
        writeln!(f, "  src: url('{path}');")?;
        if let Some(weight) = self.weight {
            writeln!(f, "  font-weight: {weight};")?;
        }
        if let Some(style) = self.style {
            writeln!(f, "  font-style: {style};")?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

/// Generate a CSS file containing the given font files. Does not include font-weight, so use this
/// only if the font weights are not known - prefer [`generate_css_file`] in all other cases.
pub fn generate_css_file_from_paths<AsRefStr>(
    basepath: &str,
    family: &str,
    paths: impl Iterator<Item = AsRefStr>,
) -> Result<String>
where
    AsRefStr: AsRef<str>,
{
    let mut css = String::new();
    for path in paths {
        use std::fmt::Write;
        let path = format!("{basepath}/{}", path.as_ref());
        let font_face = FontFace {
            family: Cow::Borrowed(family),
            path:   Cow::Borrowed(path.as_str()),
            weight: None,
            style:  None,
        };
        writeln!(&mut css, "{font_face}")?;
    }
    Ok(css)
}

/// Generate a CSS file containing the given font family, including only the given font variations.
pub fn generate_css_file<'a>(
    basepath: &str,
    family: &str,
    definitions: &NonVariableDefinition,
    fonts: impl Iterator<Item = &'a NonVariableFaceHeader>,
) -> Result<String> {
    let mut css = String::new();
    for header in fonts {
        use std::fmt::Write;
        let def = definitions.get(*header);
        let def = def.ok_or_else(|| {
            anyhow!(
                "Required font not found in {family} Font package. \
                    Expected a font matching: {header:?}."
            )
        })?;
        let path = format!("{basepath}/{}", def.file);
        let weight = def.header.weight.to_number();
        let font_face = FontFace {
            family: Cow::Borrowed(family),
            path:   Cow::Borrowed(path.as_str()),
            weight: Some(weight),
            style:  None,
        };
        writeln!(&mut css, "{font_face}")?;
    }
    Ok(css)
}



// ===================
// === Filter Font ===
// ===================

pub fn filter_font(
    font: &NonVariableDefinition,
    faces: &[NonVariableFaceHeader],
) -> NonVariableDefinition {
    font.variations().filter(|v| faces.contains(&v.header)).collect()
}



// =====================
// === Make CSS File ===
// =====================

pub async fn write_css_file_if_required(
    font_family: &str,
    font: &NonVariableDefinition,
    faces: &[NonVariableFaceHeader],
    css_output_info: Option<(&str, impl AsRef<Path>)>,
) -> Result {
    if let Some((css_basepath, css_output_path)) = css_output_info {
        let contents = generate_css_file(css_basepath, font_family, font, faces.iter())?;
        ide_ci::fs::tokio::write(css_output_path, contents).await?;
        Ok(())
    } else {
        Ok(())
    }
}



// =====================
// === Extract Fonts ===
// =====================

/// Extract the fonts from the given archive file, and write them in the given directory.
#[context("Failed to extract fonts from archive {}", package.as_ref().display())]
pub async fn extract_fonts(
    archive: impl ExtractFiles,
    fonts: &NonVariableDefinition,
    package: impl AsRef<Path>,
    out_dir: impl AsRef<Path>,
    normalize_path: &mut impl FnMut(&Path) -> Box<str>,
) -> Result {
    ide_ci::fs::tokio::create_dir_if_missing(out_dir.as_ref()).await?;
    let mut files_expected: HashSet<_> = fonts.files().collect();
    archive
        .extract_files(|path_in_archive| {
            let stripped_path = normalize_path(path_in_archive);
            if files_expected.remove(stripped_path.as_ref()) {
                Some(out_dir.as_ref().join(stripped_path.as_ref()))
            } else {
                None
            }
        })
        .await?;
    ensure!(files_expected.is_empty(), "Required fonts not found in archive: {files_expected:?}.");
    Ok(())
}
