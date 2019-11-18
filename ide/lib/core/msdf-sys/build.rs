extern crate download_lp;

use std::{fs, path};

/// Download the release package from github
pub fn github_download(
    project_url     : &str,
    version         : &str,
    filename        : &str,
    destination_dir : &path::Path
) {
    let url = format!(
        "{project}/releases/download/{version}/{filename}",
        project  = project_url,
        version  = version,
        filename = filename
    );

    let destination_file = path::Path::new(destination_dir)
        .join(filename);

    if destination_file.exists() {
        fs::remove_file(&destination_file).unwrap();
    }

    download_lp::download(
        url.as_str(),
        destination_dir.to_str().unwrap()
    ).unwrap();
}

mod msdfgen_wasm {
    use crate::github_download;
    use std::path;

    pub const VERSION     : &str = "v1.0";
    pub const FILENAME    : &str = "msdfgen_wasm.js";
    pub const PROJECT_URL : &str = "https://github.com/luna/msdfgen-wasm";

    pub fn download() {
        github_download(
            PROJECT_URL,
            VERSION,
            FILENAME,
            path::Path::new(".") // Note [Downloading to src dir]
        );
    }

    /* Note [Downloading to src dir]
     * In theory, build.rs scripts should create and modify files in OUT_DIR
     * only, but I haven't found any way to make #[wasm_bindgen(module="")]
     * taking a file from OUT_DIR (except by providing a full system path,
     * which is obviously awful)
     *
     * Thanks for your understanding
     *
     * If you find and implement a better way to downloading js snippets, please
     * remember to remove msdfgen_wasm.js entry from .gitignore
     */
}

mod fonts {
    use crate::github_download;
    use std::{path, env};

    pub const PACKAGE_NAME         : &str = "dejavu-fonts-ttf-2.37.zip";
    pub const VERSION              : &str = "version_2_37";
    pub const PROJECT_URL          : &str =
        "https://github.com/dejavu-fonts/dejavu-fonts/";
    pub const PACKAGE_FONTS_PREFIX : &str = "dejavu-fonts-ttf-2.37/ttf";
    pub const FONTS_TO_EXTRACT     : &[&str] = &["DejaVuSansMono-Bold"];

    /// Extract font file from official DejaVu zip package
    ///
    /// The font file extracted to package's directory
    fn extract_dejavu_font(
        package : &std::path::Path,
        font_name : &str,
    ) {
        let font_file = format!("{}.ttf", font_name);
        let font_package_path = format!("{}/{}",
            PACKAGE_FONTS_PREFIX,
            font_file
        );

        let mut archive = zip::ZipArchive::new(
            std::fs::File::open(package).unwrap()
        ).unwrap();
        let mut input = archive.by_name(
            font_package_path.as_str()
        ).unwrap();
        let mut output = std::fs::File::create(
            package.parent().unwrap().join(font_file)
        ).unwrap();
        std::io::copy(&mut input, &mut output).unwrap();
    }

    pub fn download_and_unzip() {
        let out_dir = env::var("OUT_DIR").unwrap();
        let destination_dir = path::Path::new(&out_dir);
        let package_path = destination_dir.join(PACKAGE_NAME);

        github_download(
            PROJECT_URL,
            VERSION,
            PACKAGE_NAME,
            destination_dir
        );

        for font in FONTS_TO_EXTRACT {
            extract_dejavu_font(package_path.as_path(), &font);
        }
    }
}

fn main() {
    msdfgen_wasm::download();
    fonts::download_and_unzip();
    println!("cargo:rerun-if-changed=build.rs");
}
