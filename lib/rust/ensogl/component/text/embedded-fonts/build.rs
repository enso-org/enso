//! Downloader of fonts considered as "embedded" into the application.

// === Features ===
#![feature(const_trait_impl)]

use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::path;



// =====================
// === FillMapRsFile ===
// =====================

/// Generated file with code filling embedded fonts map
pub struct FillMapRsFile {
    file: fs::File,
}

impl FillMapRsFile {
    fn create<P: AsRef<path::Path>>(path: P) -> io::Result<FillMapRsFile> {
        let mut file = fs::File::create(path)?;
        writeln!(file, "{{")?;
        Ok(FillMapRsFile { file })
    }

    fn add_font_inserting_line(&mut self, font_name: &str, font_file: &str) -> io::Result<()> {
        writeln!(
            self.file,
            "   font_data_by_name.insert(\"{font_name}\", include_bytes!(\"{font_file}\"));",
            font_name = font_name,
            font_file = font_file
        )
    }

    fn close_block(&mut self) -> io::Result<()> {
        writeln!(self.file, "}}")
    }
}



// ===================
// === DejaVu Font ===
// ===================

mod deja_vu {
    use crate::FillMapRsFile;

    use enso_build_utilities::GithubRelease;
    use ensogl_text_embedded_fonts_names::DejaVuSans;
    use ensogl_text_embedded_fonts_names::FontFamily;
    use std::path;

    pub const PACKAGE: GithubRelease<&str> = GithubRelease {
        project_url: "https://github.com/dejavu-fonts/dejavu-fonts/",
        version:     "version_2_37",
        filename:    "dejavu-fonts-ttf-2.37.zip",
    };

    pub const PACKAGE_FONTS_PREFIX: &str = "dejavu-fonts-ttf-2.37/ttf";

    pub fn font_file_from_font_name(font_name: &str) -> String {
        return format!("{}.ttf", font_name);
    }

    pub fn extract_font(package_path: &path::Path, font_name: &str) {
        let font_file = font_file_from_font_name(font_name);
        let font_in_package_path = format!("{}/{}", PACKAGE_FONTS_PREFIX, font_file);
        let package_dir = package_path.parent().unwrap();
        let output_path = package_dir.join(font_file);

        let archive_file = std::fs::File::open(package_path).unwrap();
        let mut archive = zip::ZipArchive::new(archive_file).unwrap();
        let mut input_stream = archive.by_name(font_in_package_path.as_str()).unwrap();
        let mut output_stream = std::fs::File::create(output_path).unwrap();
        std::io::copy(&mut input_stream, &mut output_stream).unwrap();
    }

    pub const FONTS_TO_EXTRACT: &[&str] =
        &[DejaVuSans::regular(), DejaVuSans::bold(), DejaVuSans::mono(), DejaVuSans::mono_bold()];

    pub fn extract_all_fonts(package_path: &path::Path) {
        for font_name in FONTS_TO_EXTRACT {
            extract_font(package_path, font_name);
        }
    }

    pub fn download_and_extract_all_fonts(out_dir: &path::Path) {
        let package_path = out_dir.join(PACKAGE.filename);
        PACKAGE.download(out_dir);
        extract_all_fonts(package_path.as_path());
    }

    pub fn add_entries_to_fill_map_rs(file: &mut FillMapRsFile) {
        for font_name in FONTS_TO_EXTRACT {
            let font_file = font_file_from_font_name(font_name);
            file.add_font_inserting_line(font_name, font_file.as_str()).unwrap();
        }
    }
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let out = env::var("OUT_DIR").unwrap();
    let out_dir = path::Path::new(&out);
    deja_vu::download_and_extract_all_fonts(out_dir);
    let fill_map_rs_path = out_dir.join("fill_map.rs");
    let mut fill_map_rs_file = FillMapRsFile::create(fill_map_rs_path).unwrap();
    deja_vu::add_entries_to_fill_map_rs(&mut fill_map_rs_file);
    fill_map_rs_file.close_block().unwrap();
}
