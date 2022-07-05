//! Downloader and patch for msdfgen library.



mod msdfgen_wasm {
    use enso_build_utilities::GithubRelease;

    use std::fs;
    use std::io::Write;
    use std::path;

    pub const PACKAGE: GithubRelease<&str> = GithubRelease {
        project_url: "https://github.com/enso-org/msdfgen-wasm",
        version:     "v1.1",
        filename:    "msdfgen_wasm.js",
    };

    /// Downloads the msdfgen package.
    ///
    /// **Note**
    /// In theory, build.rs scripts should create and modify files in OUT_DIR only, but we haven't
    /// found any way to make `#[wasm_bindgen(module="...")]` taking a file from OUT_DIR (except by
    /// providing a full system path, which is obviously awful).
    ///
    /// If you find and implement a better way to downloading js snippets, please
    /// remember to remove msdfgen_wasm.js entry from the .gitignore file.
    pub fn download() {
        PACKAGE.download(path::Path::new("."))
    }

    const PATCH_LINE: &str = "; export { ccall, getValue, _msdfgen_getKerning,\
        _msdfgen_generateAutoframedMSDF, _msdfgen_result_getMSDFData,\
        _msdfgen_result_getAdvance, _msdfgen_result_getTranslation,\
        _msdfgen_result_getScale, _msdfgen_freeResult, _msdfgen_freeFont,\
        addInitializationCb, isInitialized }";

    /// Patches downloaded msdfgen_wasm.js file.
    ///
    /// For some reason, for wasm-bindgen test on browsers the function must be explicitly exported.
    /// Examples works without this perfectly.
    pub fn patch_for_wasm_bindgen_test() {
        let path = path::Path::new(&PACKAGE.filename);
        let mut open_options = fs::OpenOptions::new();
        open_options.append(true);
        let mut file = open_options.open(path).unwrap();
        let file_content = fs::read_to_string(path).unwrap();
        if !file_content.ends_with(PATCH_LINE) {
            file.write_all(PATCH_LINE.as_bytes()).unwrap();
        }
    }
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    msdfgen_wasm::download();
    msdfgen_wasm::patch_for_wasm_bindgen_test();
}
