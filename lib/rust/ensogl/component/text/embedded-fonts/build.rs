//! Downloader of fonts considered as "embedded" into the application.

// === Features ===
#![feature(const_trait_impl)]

use std::env;
use std::fmt::Write;
use std::fs;
use std::io;
use std::io::Write as IoWrite;
use std::path;



// =====================
// === FillMapRsFile ===
// =====================

/// Generated file with code filling embedded fonts map
#[derive(Debug, Default)]
pub struct FillMapRsFile {
    embedded_fonts_map_body: String,
    // file: fs::File,
}

impl FillMapRsFile {
    fn write<P: AsRef<path::Path>>(&self, path: P) -> io::Result<()> {
        let mut file = fs::File::create(path)?;
        writeln!(file, "pub fn embedded_fonts_data() -> HashMap<&'static str, &'static [u8]> {{")?;
        writeln!(file, "    let mut map = HashMap::<&'static str, &'static [u8]>::new();")?;
        write!(file, "{}", self.embedded_fonts_map_body)?;
        writeln!(file, "    map")?;
        writeln!(file, "}}")?;
        Ok(())
    }

    fn add_font_inserting_line(&mut self, font_name: &str, font_file: &str) {
        writeln!(
            &mut self.embedded_fonts_map_body,
            "    map.insert(\"{font_name}\", include_bytes!(\"{font_file}\"));",
        )
        .ok();
    }

    fn code(&self) -> String {
        let mut out = String::new();
        writeln!(
            &mut out,
            "pub fn embedded_fonts_data() -> HashMap<&'static str, &'static [u8]> {{"
        );
        writeln!(&mut out, "{}", &self.embedded_fonts_map_body);
        writeln!(&mut out, "}}");
        out
    }
}



// ===================
// === DejaVu Font ===
// ===================

mod deja_vu {
    use crate::FillMapRsFile;

    use enso_build_utilities::GithubRelease;
    // use ensogl_text_embedded_fonts_names::DejaVuSans;
    use std::path;

    pub const PACKAGE: GithubRelease<&str> = GithubRelease {
        project_url: "https://github.com/dejavu-fonts/dejavu-fonts/",
        version:     "version_2_37",
        filename:    "dejavu-fonts-ttf-2.37.zip",
    };

    pub const PACKAGE_FONTS_PREFIX: &str = "dejavu-fonts-ttf-2.37/ttf";

    pub fn extract_font(package_path: &path::Path, file_name: &str) {
        // println!("cargo:warning={:?}",package_path);
        let font_in_package_path = format!("{}/{}", PACKAGE_FONTS_PREFIX, file_name);
        let package_dir = package_path.parent().unwrap();
        let output_path = package_dir.join(file_name);

        let archive_file = std::fs::File::open(package_path).unwrap();
        let mut archive = zip::ZipArchive::new(archive_file).unwrap();
        let mut input_stream = archive.by_name(font_in_package_path.as_str()).unwrap();
        // println!("cargo:warning={:?}",output_path);
        let mut output_stream = std::fs::File::create(output_path).unwrap();
        std::io::copy(&mut input_stream, &mut output_stream).unwrap();
    }

    // const deja_vu: DejaVuSans = DejaVuSans;

    const FILE_NAMES: [&str; 2] = ["DejaVuSans.ttf", "DejaVuSans-Bold.ttf"];

    pub fn extract_all_fonts(package_path: &path::Path) {
        for file_name in FILE_NAMES {
            extract_font(package_path, &file_name);
        }
    }

    pub fn download_and_extract_all_fonts(out_dir: &path::Path) {
        let package_path = out_dir.join(PACKAGE.filename);
        PACKAGE.download(out_dir);
        extract_all_fonts(package_path.as_path());
    }

    pub fn add_entries_to_fill_map_rs(file: &mut FillMapRsFile) {
        for file_name in FILE_NAMES {
            file.add_font_inserting_line(file_name, file_name);
        }
    }
}


mod google_fonts {
    use crate::FillMapRsFile;

    use enso_build_utilities::GithubFile;
    use enso_build_utilities::GoogleFontsRelease;
    use std::path;


    pub fn download_font(name: impl Into<String>, out_dir: &path::Path) -> Vec<GithubFile> {
        let name = name.into();
        let release = GoogleFontsRelease { name };
        release.download(out_dir)
    }

    pub fn add_entries_to_fill_map_rs(out: &mut FillMapRsFile, files: &[GithubFile]) {
        for file in files {
            out.add_font_inserting_line(&file.name, &file.name)
        }
    }
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let out = env::var("OUT_DIR").unwrap();
    // println!("cargo:warning=out dir {:?}",out);
    let out_dir = path::Path::new(&out);
    deja_vu::download_and_extract_all_fonts(out_dir);
    let files = google_fonts::download_font("mplus1", out_dir);
    let fill_map_rs_path = out_dir.join("embedded_fonts_data.rs");
    let mut fill_map_rs_file = FillMapRsFile::default();
    deja_vu::add_entries_to_fill_map_rs(&mut fill_map_rs_file);
    google_fonts::add_entries_to_fill_map_rs(&mut fill_map_rs_file, &files);
    fill_map_rs_file.write(fill_map_rs_path).unwrap();
}
