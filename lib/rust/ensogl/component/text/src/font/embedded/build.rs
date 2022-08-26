//! Downloader of fonts considered as "embedded" into the application.

// === Features ===
#![feature(const_trait_impl)]

use owned_ttf_parser::AsFaceRef;
use owned_ttf_parser::OwnedFace;
use std::env;
use std::fmt::Write;
use std::fs;
use std::fs::File;
use std::io;
use std::io::BufReader;
use std::io::Read;
use std::io::Write as IoWrite;
use std::path;



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
        let value = format!("family::Definition::Variable({})", family_def);
        ln!(1, &mut self.definitions, "map.insert({},{});", key, value);
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

    fn write<P: AsRef<path::Path>>(&self, path: P) -> io::Result<()> {
        let mut file = fs::File::create(path)?;
        writeln!(file, "{}", self.body())
    }
}



// ===================
// === DejaVu Font ===
// ===================

mod deja_vu {
    use crate::CodeGenerator;

    use enso_build_utilities::GithubRelease;
    use std::path;

    pub const PACKAGE: GithubRelease<&str> = GithubRelease {
        project_url: "https://github.com/dejavu-fonts/dejavu-fonts/",
        version:     "version_2_37",
        filename:    "dejavu-fonts-ttf-2.37.zip",
    };

    pub const PACKAGE_FONTS_PREFIX: &str = "dejavu-fonts-ttf-2.37/ttf";

    pub fn extract_font(package_path: &path::Path, file_name: &str) {
        let font_in_package_path = format!("{}/{}", PACKAGE_FONTS_PREFIX, file_name);
        let package_dir = package_path.parent().unwrap();
        let output_path = package_dir.join(file_name);

        let archive_file = std::fs::File::open(package_path).unwrap();
        let mut archive = zip::ZipArchive::new(archive_file).unwrap();
        let mut input_stream = archive.by_name(font_in_package_path.as_str()).unwrap();
        let mut output_stream = std::fs::File::create(output_path).unwrap();
        std::io::copy(&mut input_stream, &mut output_stream).unwrap();
    }

    const FILE_NAMES: [&str; 4] =
        ["DejaVuSans.ttf", "DejaVuSans-Bold.ttf", "DejaVuSansMono.ttf", "DejaVuSansMono-Bold.ttf"];

    pub fn extract_all_fonts(package_path: &path::Path) {
        for file_name in FILE_NAMES {
            extract_font(package_path, file_name);
        }
    }

    pub fn download_and_extract_all_fonts(out_dir: &path::Path) {
        let package_path = out_dir.join(PACKAGE.filename);
        PACKAGE.download(out_dir);
        extract_all_fonts(package_path.as_path());
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

    use enso_build_utilities::GithubFile;
    use enso_build_utilities::GoogleFontsRelease;
    use std::path;

    #[derive(Debug)]
    pub struct FaceDefinition {
        file_name: String,
        face:      OwnedFace,
    }

    pub fn download_files(name: impl AsRef<str>, out_dir: &path::Path) -> Vec<GithubFile> {
        GoogleFontsRelease::download(name.as_ref(), out_dir)
    }

    pub fn load(out_dir: &path::Path, buffer: &mut CodeGenerator, family_name: &str) {
        let files = download_files(family_name, out_dir);

        for file in &files {
            buffer.add_font_data(&file.name)
        }

        let font_faces: Vec<FaceDefinition> = files
            .into_iter()
            .map(|file| {
                let file_name = file.name;
                let path = out_dir.join(&file_name);
                let err = |action: &str| format!("Cannot {} file {:?}", action, path);
                let handle = File::open(&path).unwrap_or_else(|_| panic!("{}", err("read")));
                let mut reader = BufReader::new(handle);
                let mut bytes = Vec::new();
                reader.read_to_end(&mut bytes).unwrap_or_else(|_| panic!("{}", err("read")));
                let face = OwnedFace::from_vec(bytes, 0);
                let face = face.unwrap_or_else(|_| panic!("{}", err("parse")));
                FaceDefinition { file_name, face }
            })
            .collect();

        if font_faces.is_empty() {
            panic!("No font faces found for family {}.", family_name);
        } else if font_faces.len() == 1 && font_faces[0].face.as_face_ref().is_variable() {
            let file_name = &font_faces[0].file_name;
            buffer.add_variable_font_definition(family_name, file_name);
        } else {
            if font_faces.iter().any(|def| def.face.as_face_ref().is_variable()) {
                let err1 = "is a variable font with multiple source files.";
                let err2 = "This is intentionally not supported.";
                let err3 = "CSS does not support it either,";
                let err4 = "see: https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face";
                panic!("Family {} {} {} {} {}", family_name, err1, err2, err3, err4);
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
        }
    }
}



// ============
// === Main ===
// ============

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let out = env::var("OUT_DIR").unwrap();
    let out_dir = path::Path::new(&out);
    deja_vu::download_and_extract_all_fonts(out_dir);

    let mut code_gen = CodeGenerator::default();
    google_fonts::load(out_dir, &mut code_gen, "mplus1");
    google_fonts::load(out_dir, &mut code_gen, "mplus1p");

    let out_path = out_dir.join("embedded_fonts_data.rs");
    deja_vu::add_entries_to_fill_map_rs(&mut code_gen);
    code_gen.write(out_path).unwrap();
}
