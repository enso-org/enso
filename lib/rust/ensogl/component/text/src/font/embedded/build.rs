//! Downloader of fonts considered as "embedded" into the application.

// === Features ===
#![feature(const_trait_impl)]

use ide_ci::prelude::*;

use ide_ci::log::setup_logging;
use owned_ttf_parser::AsFaceRef;
use owned_ttf_parser::OwnedFace;
use std::fmt::Write as FmtWrite;



// =============
// === Utils ===
// =============

/// Add a new code line to the string buffer.
macro_rules! ln {
    ($ident:expr, $out:expr, $($ts:tt)*) => {
        writeln!($out, "{}", format!("{}{}", "    ".repeat($ident), format!($($ts)*))).ok();
    };
}



// =====================
// === CodeGenerator ===
// =====================

/// Generated file with code filling embedded fonts map.
#[derive(Debug, Default)]
pub struct CodeGenerator {
    embeds:      String,
    definitions: String,
}

impl CodeGenerator {
    fn add_font_data(&mut self, file_name: &str) {
        ln!(1, &mut self.embeds, "map.insert(\"{file_name}\", include_bytes!(\"{file_name}\"));");
    }

    fn add_variable_font_definition(&mut self, family: &str, file: &str) {
        let key = format!("\"{family}\".into()");
        let family_def = format!("family::VariableDefinition::new(\"{file}\")");
        let value = format!("family::Definition::Variable({family_def})");
        ln!(1, &mut self.definitions, "map.insert({key},{value});");
    }

    fn add_non_variable_font_definition(&mut self, family_name: &str, def: &str) {
        ln!(1, &mut self.definitions, "map.insert(\"{family_name}\".into(), {def});");
    }

    fn body(&self) -> String {
        let mut body = String::new();
        ln!(0, body, "/// Mapping between file name and embedded fonts data.");
        ln!(0, body, "pub fn embedded_fonts_data() -> HashMap<&'static str, &'static [u8]> {{");
        ln!(1, body, "let mut map = HashMap::<&'static str, &'static [u8]>::new();");
        write!(body, "{}", self.embeds).ok();
        ln!(1, body, "map");
        ln!(0, body, "}}");
        ln!(0, body, "");
        ln!(0, body, "/// Definitions of embedded font families.");
        ln!(0, body, "pub fn embedded_family_definitions()");
        ln!(0, body, "-> HashMap<family::Name, family::Definition> {{");
        ln!(1, body, "let mut map = HashMap::new();");
        write!(body, "{}", self.definitions).ok();
        ln!(1, body, "map");
        ln!(0, body, "}}");
        body
    }
}



// ===================
// === DejaVu Font ===
// ===================

mod deja_vu {
    use super::*;

    use crate::CodeGenerator;
    use enso_build_utilities::GithubRelease;

    pub const PACKAGE: GithubRelease<&str> = GithubRelease {
        project_url: "https://github.com/dejavu-fonts/dejavu-fonts/",
        version:     "version_2_37",
        filename:    "dejavu-fonts-ttf-2.37.zip",
    };

    pub const PACKAGE_FONTS_PREFIX: &str = "dejavu-fonts-ttf-2.37/ttf";

    const FILE_NAMES: [&str; 4] =
        ["DejaVuSans.ttf", "DejaVuSans-Bold.ttf", "DejaVuSansMono.ttf", "DejaVuSansMono-Bold.ttf"];

    pub fn extract_all_fonts(package_path: &Path) -> Result {
        let archive_file = ide_ci::fs::open(package_path)?;
        let mut archive = zip::ZipArchive::new(archive_file).unwrap();
        for file_name in FILE_NAMES {
            let font_in_package_path = format!("{PACKAGE_FONTS_PREFIX}/{file_name}");
            let mut input_stream = archive.by_name(&font_in_package_path).with_context(|| {
                format!(
                    "Cannot find font file {} in the package {}",
                    file_name,
                    package_path.display()
                )
            })?;
            let output_path = package_path.with_file_name(file_name);
            let mut output_stream = ide_ci::fs::create(&output_path)?;
            std::io::copy(&mut input_stream, &mut output_stream).with_context(|| {
                format!("Cannot extract font file {} to {}", file_name, output_path.display())
            })?;
        }
        Ok(())
    }

    pub async fn download_and_extract_all_fonts(out_dir: &Path) -> Result {
        let package_path = ide_ci::io::download_to_dir(PACKAGE.url()?, out_dir).await?;
        extract_all_fonts(package_path.as_path())
    }

    pub fn add_entries_to_fill_map_rs(file: &mut CodeGenerator) {
        for file_name in FILE_NAMES {
            file.add_font_data(file_name);
        }
    }
}



// ====================
// === Google Fonts ===
// ====================

mod google_fonts {
    use super::*;
    use crate::CodeGenerator;

    use enso_build::ide::web::google_font::download_google_font;

    #[derive(Debug)]
    pub struct FaceDefinition {
        file_name: String,
        face:      OwnedFace,
    }

    /// A description of downloaded file.
    #[derive(Debug, Clone)]
    pub struct DownloadedFile {
        /// Path relative to the output directory.
        name: String,
    }

    pub async fn download_files(
        name: impl AsRef<str>,
        out_dir: &Path,
    ) -> Result<Vec<DownloadedFile>> {
        let octocrab = ide_ci::github::setup_octocrab().await?;
        let cache = ide_ci::cache::Cache::new_default().await?;
        let result = download_google_font(&cache, &octocrab, name.as_ref(), out_dir).await?;
        result
            .into_iter()
            .map(|font| Ok(DownloadedFile { name: font.try_file_name()?.as_str().into() }))
            .try_collect()
    }

    pub async fn load(out_dir: &Path, buffer: &mut CodeGenerator, family_name: &str) -> Result {
        let files = download_files(family_name, out_dir).await?;

        for file in &files {
            buffer.add_font_data(&file.name)
        }

        let font_faces: Vec<FaceDefinition> = files.into_iter().try_map(|file| {
            let file_name = file.name;
            let path = out_dir.join(&file_name);
            let bytes = ide_ci::fs::read(&path)?;
            let face = OwnedFace::from_vec(bytes, 0)
                .with_context(|| format!("Cannot load font file {}.", path.display()))?;
            Ok(FaceDefinition { file_name, face })
        })?;

        ensure!(!font_faces.is_empty(), "No font files were downloaded for family {family_name}.",);
        if font_faces.len() == 1 && font_faces[0].face.as_face_ref().is_variable() {
            let file_name = &font_faces[0].file_name;
            buffer.add_variable_font_definition(family_name, file_name);
        } else {
            if font_faces.iter().any(|def| def.face.as_face_ref().is_variable()) {
                let err1 = "is a variable font with multiple source files.";
                let err2 = "This is intentionally not supported.";
                let err3 = "CSS does not support it either,";
                let err4 = "see: https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face";
                bail!("Family {} {} {} {} {}", family_name, err1, err2, err3, err4);
            }
            let mut code = String::new();
            let fam_def = "family::Definition::NonVariable";
            ln!(1, code, "{}(family::NonVariableDefinition::from_iter([", fam_def);
            for def in font_faces {
                let file_name = &def.file_name;
                let face_ref = def.face.as_face_ref();
                ln!(2, code, "(");
                ln!(3, code, "family::NonVariableFaceHeader::new(");
                ln!(4, code, "family::Width::{:?},", face_ref.width());
                ln!(4, code, "family::Weight::{:?},", face_ref.weight());
                ln!(4, code, "family::Style::{:?},", face_ref.style());
                ln!(3, code, "),");
                ln!(3, code, "\"{file_name}\".to_string(),");
                ln!(2, code, "),");
            }
            ln!(1, code, "]))");
            buffer.add_non_variable_font_definition(family_name, &code);
        };
        Ok(())
    }
}



// ============
// === Main ===
// ============
#[tokio::main]
async fn main() -> Result {
    println!("cargo:rerun-if-changed=build.rs");
    setup_logging()?;
    let out_dir = ide_ci::programs::cargo::build_env::OUT_DIR.get()?;
    deja_vu::download_and_extract_all_fonts(&out_dir).await?;

    let mut code_gen = CodeGenerator::default();
    google_fonts::load(&out_dir, &mut code_gen, "mplus1").await?;
    google_fonts::load(&out_dir, &mut code_gen, "mplus1p").await?;

    deja_vu::add_entries_to_fill_map_rs(&mut code_gen);

    let body = code_gen.body();
    let out_path = out_dir.join("embedded_fonts_data.rs");
    ide_ci::fs::tokio::write(&out_path, body).await?;
    Ok(())
}
