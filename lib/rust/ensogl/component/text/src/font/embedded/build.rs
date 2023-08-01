//! Downloader of fonts considered as "embedded" into the application.

// === Features ===
#![feature(const_trait_impl)]
#![feature(let_chains)]

use ide_ci::prelude::*;

use ide_ci::log::setup_logging;
use owned_ttf_parser as ttf;
use owned_ttf_parser::AsFaceRef;
use owned_ttf_parser::OwnedFace;
use std::fmt::Write as FmtWrite;



// =================
// === Constants ===
// =================

/// The name of the Rust source file that will be generated from the downloaded font data.
const GENERATED_SOURCE_FILE_NAME: &str = "embedded_fonts_data.rs";



// =============
// === Utils ===
// =============

/// Add a new code line to the string buffer.
macro_rules! ln {
    ($ident:expr, $out:expr, $($ts:tt)*) => {
        writeln!($out, "{}", format!("{}{}", "    ".repeat($ident), format!($($ts)*))).ok();
    };
}



// ====================================
// === Non variable font definition ===
// ====================================

struct NonVariableFontDefinition {
    variations: Vec<NonVariableVariation>,
}

impl NonVariableFontDefinition {
    fn files(&self) -> impl Iterator<Item=&str> {
        self.variations.iter().map(|v| v.file.as_str())
    }
}

struct NonVariableVariation {
    file:   String,
    header: NonVariableFaceHeader,
}

#[derive(Default)]
struct NonVariableFaceHeader {
    width:  ttf::Width,
    weight: ttf::Weight,
    style:  ttf::Style,
}

impl Display for NonVariableFontDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let fam_def = "family::Definition::NonVariable";
        ln!(1, f, "{}(family::NonVariableDefinition::from_iter([", fam_def);
        for variation in &self.variations {
            let file_name = &variation.file;
            let header = &variation.header;
            ln!(2, f, "(");
            ln!(3, f, "family::NonVariableFaceHeader::new(");
            ln!(4, f, "family::Width::{:?},", &header.width);
            ln!(4, f, "family::Weight::{:?},", &header.weight);
            ln!(4, f, "family::Style::{:?},", &header.style);
            ln!(3, f, "),");
            ln!(3, f, "\"{file_name}\".to_string(),");
            ln!(2, f, "),");
        }
        ln!(1, f, "]))");
        Ok(())
    }
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



// =================
// === Enso Font ===
// =================

mod enso_font {
    use super::*;
    use crate::CodeGenerator;

    pub async fn load(out_dir: impl AsRef<Path>, code_gen: &mut CodeGenerator) -> Result {
        let font_family = enso_enso_font::FontFamily::enso();
        font_family.download_fonts(&out_dir).await?;
        add_entries_to_fill_map_rs(&font_family, code_gen);
        Ok(())
    }

    fn add_entries_to_fill_map_rs(family: &enso_enso_font::FontFamily, code_gen: &mut CodeGenerator) {
        let variations: Vec<NonVariableVariation> = family
            .fonts()
            .map(|variant| {
                let file = variant.filename();
                let header = NonVariableFaceHeader {
                    width: variant.width(),
                    weight: variant.weight(),
                    style: variant.style(),
                };
                NonVariableVariation { file, header }
            })
            .collect();
        let definition = NonVariableFontDefinition { variations };
        let def_code = definition.to_string();
        code_gen.add_non_variable_font_definition(family.name(), &def_code);
        for file in definition.files() {
            code_gen.add_font_data(file);
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

    impl From<FaceDefinition> for NonVariableVariation {
        fn from(def: FaceDefinition) -> Self {
            let face = def.face.as_face_ref();
            Self {
                file:   def.file_name,
                header: NonVariableFaceHeader {
                    width:  face.width(),
                    weight: face.weight(),
                    style:  face.style(),
                },
            }
        }
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
            let variations = font_faces.into_iter().map(|def| def.into()).collect();
            let code = NonVariableFontDefinition { variations }.to_string();
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
    let mut code_gen = CodeGenerator::default();

    google_fonts::load(&out_dir, &mut code_gen, "mplus1p").await?;

    enso_font::load(&out_dir, &mut code_gen).await?;

    let body = code_gen.body();
    let out_path = out_dir.join(GENERATED_SOURCE_FILE_NAME);
    ide_ci::fs::tokio::write(&out_path, body).await?;
    Ok(())
}
