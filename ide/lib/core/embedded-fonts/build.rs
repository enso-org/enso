use std::{path, env, fs, io};
use std::io::Write;

// =====================
// === FillMapRsFile ===
// =====================

/// Generated file with code filling embedded fonts map
pub struct FillMapRsFile {
    file : fs::File
}

impl FillMapRsFile {
    fn create<P: AsRef<path::Path>>(path : P) -> io::Result<FillMapRsFile> {
        let mut file = fs::File::create(path)?;
        writeln!(file, "{{")?;
        Ok(FillMapRsFile{ file })
    }

    fn add_font_inserting_line(&mut self, font_name : &str, font_file : &str)
        -> io::Result<()> {
        writeln!(
            self.file,
            "   fonts_by_name.insert(\"{}\", include_bytes!(\"{}\"));",
            font_name,
            font_file
        )
    }

    fn close_block(&mut self) -> io::Result<()> {
        writeln!(self.file, "}}")
    }
}

// ====================
// === DejaVu fonts ===
// ====================

mod deja_vu {
    use std::path;
    use basegl_build_utilities::github_download;
    use crate::FillMapRsFile;

    pub const PACKAGE_NAME     : &str = "dejavu-fonts-ttf-2.37.zip";
    pub const PACKAGE_VERSION  : &str = "version_2_37";
    pub const PROJECT_URL      : &str =
        "https://github.com/dejavu-fonts/dejavu-fonts/";

    pub const PACKAGE_FONTS_PREFIX: &str = "dejavu-fonts-ttf-2.37/ttf";

    pub fn font_file_from_font_name(font_name : &str) -> String {
        return format!("{}.ttf", font_name);
    }

    pub fn extract_font(package_path : &path::Path, font_name : &str) {
        let font_file = font_file_from_font_name(font_name);
        let font_package_path = format!("{}/{}",
            PACKAGE_FONTS_PREFIX,
            font_file
        );

        let mut archive = zip::ZipArchive::new(
            std::fs::File::open(package_path).unwrap()
        ).unwrap();
        let mut input = archive.by_name(
            font_package_path.as_str()
        ).unwrap();
        let mut output = std::fs::File::create(
            package_path.parent().unwrap().join(font_file)
        ).unwrap();
        std::io::copy(&mut input, &mut output).unwrap();
    }

    pub const FONTS_TO_EXTRACT : &[&str] = &[
        "DejaVuSans",
        "DejaVuSans-ExtraLight",
        "DejaVuSansMono",
        "DejaVuSansMono-Bold",
        "DejaVuSansMono-Oblique",
        "DejaVuSansCondensed",
        "DejaVuSerif",
        "DejaVuSerifCondensed",
    ];

    pub fn extract_all_fonts(package_path : &path::Path) {
        for font_name in FONTS_TO_EXTRACT {
            extract_font(package_path, font_name);
        }
    }

    pub fn download_and_extract_all_fonts(out_dir : &path::Path) {
        github_download(
            PROJECT_URL,
            PACKAGE_VERSION,
            PACKAGE_NAME,
            &out_dir
        );

        let package_path = out_dir.join(PACKAGE_NAME);
        extract_all_fonts(package_path.as_path());
    }

    pub fn add_entries_to_fill_map_rs(file : &mut FillMapRsFile) {
        for font_name in FONTS_TO_EXTRACT {
            let font_file = font_file_from_font_name(font_name);
            file.add_font_inserting_line(
                font_name,
                font_file.as_str()
            ).unwrap();
        }
    }
}

fn main() {
    let out = env::var("OUT_DIR").unwrap();
    let out_dir = path::Path::new(&out);
    let fill_map_rs_path = out_dir.join("fill_map.rs");

    let mut fill_map_rs_file =
        FillMapRsFile::create(fill_map_rs_path).unwrap();

    deja_vu::download_and_extract_all_fonts(out_dir);
    deja_vu::add_entries_to_fill_map_rs(&mut fill_map_rs_file);

    fill_map_rs_file.close_block().unwrap();

    println!("cargo:rerun-if-changed=build.rs");
}