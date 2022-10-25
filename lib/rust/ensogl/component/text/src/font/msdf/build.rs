//! Downloader and patch for msdfgen library.

use ide_ci::prelude::*;

use enso_build_utilities::GithubRelease;
use ide_ci::log::setup_logging;



pub const PACKAGE: GithubRelease<&str> = GithubRelease {
    project_url: "https://github.com/enso-org/msdfgen-wasm",
    version:     "v1.4",
    filename:    "msdfgen_wasm.js",
};

const PATCH_LINE: &str =
    "; export { ccall, getValue, _msdfgen_getKerning, _msdfgen_setVariationAxis,\
    _msdfgen_generateAutoframedMSDF, _msdfgen_generateAutoframedMSDFByIndex, \
    _msdfgen_result_getMSDFData, _msdfgen_result_getAdvance, _msdfgen_result_getTranslation,\
    _msdfgen_result_getScale, _msdfgen_freeResult, _msdfgen_freeFont,\
    addInitializationCb, isInitialized }";

#[tokio::main]
async fn main() -> Result {
    println!("cargo:rerun-if-changed=build.rs");
    setup_logging()?;

    // Downloads the msdfgen package.
    //
    // **Note**
    // In theory, build.rs scripts should create and modify files in OUT_DIR only, but we haven't
    // found any way to make `#[wasm_bindgen(module="...")]` taking a file from OUT_DIR (except by
    // providing a full system path, which is obviously awful).
    //
    // If you find and implement a better way to downloading js snippets, please
    // remember to remove msdfgen_wasm.js entry from the .gitignore file.
    let mut file = ide_ci::fs::tokio::create(PACKAGE.filename).await?;
    let mut stream = ide_ci::io::web::download_reader(PACKAGE.url()?).await?;
    tokio::io::copy(&mut stream, &mut file)
        .await
        .with_context(|| format!("Failed to stream download to file {}.", PACKAGE.filename))?;

    // Patch downloaded msdfgen_wasm.js file.
    //
    // For some reason, for wasm-bindgen test on browsers the function must be explicitly
    // exported. Examples work without this perfectly.
    file.write_all(PATCH_LINE.as_bytes())
        .await
        .with_context(|| format!("Failed to write to file {}", PACKAGE.filename))?;
    Ok(())
}
