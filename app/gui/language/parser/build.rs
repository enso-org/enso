//! This build script is responsible for ensuring that if parser targets wasm,
//! the JS Parser package is available at the expected location for
//! `wasm_bindgen` tool.

// === Features ===
#![feature(option_result_contains)]

use ide_ci::prelude::*;



// =========================
// == Hardcoded constants ==
// =========================

/// Where the crate expects to find file with compiled parser.
/// Path relative to the crate directory.
const PARSER_PATH: &str = "./pkg/scala-parser.js";

/// Commit from `enso` repository that will be used to obtain parser from.
const PARSER_COMMIT: &str = "649fe33ccf148d47deb6ba6a06f3babc48078e3e";

/// Magic code that needs to be prepended to ScalaJS generated parser due to:
/// https://github.com/scala-js/scala-js/issues/3677/
const PARSER_PREAMBLE: &str = "var __ScalaJSEnv = { global: window };";

/// Obtains a URL where this parser version can be downloaded.
pub fn parser_url(version: &ParserVersion) -> reqwest::Url {
    let url_string = format!(
        "https://packages.luna-lang.org/parser-js/nightly/{}/scala-parser.js",
        version.commit
    );
    let invalid_url_msg = format!("{} is an invalid URL.", url_string);
    reqwest::Url::parse(&url_string).expect(&invalid_url_msg)
}



// ===================
// == ParserVersion ==
// ===================

/// Parser version described as commit hash from `enso` repository.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParserVersion {
    pub commit: String,
}

impl ParserVersion {
    /// Create a version described by given commit hash.
    pub fn from_commit(commit: String) -> ParserVersion {
        ParserVersion { commit }
    }

    /// The JS parser version required for this crate.
    pub fn required() -> ParserVersion {
        ParserVersion { commit: PARSER_COMMIT.into() }
    }
}



// ========================
// == Downloading parser ==
// ========================

/// Stores information which parser version should be provided where.
///
/// Implementation provides methods that download desired parser version, patch it and store to the
/// file, so parser can be consumed by `wasm_bindgen`.
struct ParserProvider {
    /// Required parser version.
    version:     ParserVersion,
    /// The path where JS file needs to be provided.
    parser_path: PathBuf,
}

impl ParserProvider {
    /// Creates a provider that obtains given parser version to a given path.
    pub fn new(version: ParserVersion, parser_path: impl AsRef<Path>) -> ParserProvider {
        let parser_path = PathBuf::from(parser_path.as_ref());
        ParserProvider { version, parser_path }
    }

    /// Downloads contents of JS parser into memory.
    pub async fn download(&self) -> Result<Bytes> {
        let url = parser_url(&self.version);
        ide_ci::io::download_all(url.clone()).await.context("Failed to download the parser.")
    }

    /// Stores JS parser into file, after patching with a `PARSER_PREAMBLE`.
    pub async fn patch_and_store(&self, js_parser: bytes::Bytes) -> Result {
        ide_ci::fs::tokio::write_iter(&self.parser_path, [
            PARSER_PREAMBLE.as_bytes(),
            js_parser.as_ref(),
        ])
        .await
    }

    /// Places required parser version in the target location.
    pub async fn run(&self) -> Result {
        let fingerprint = self.parser_path.with_file_name("parser.fingerprint");
        let opt_version = ide_ci::fs::tokio::read_to_string(&fingerprint).await;
        let changed = match opt_version {
            Err(_) => true,
            Ok(hash) => hash != PARSER_COMMIT,
        };
        if changed {
            let parser_js = self.download().await?;
            self.patch_and_store(parser_js).await?;
            ide_ci::fs::tokio::write(&fingerprint, PARSER_COMMIT).await?;
        }
        Ok(())
    }
}



// ==========
// == main ==
// ==========

#[tokio::main]
async fn main() -> Result {
    if ide_ci::programs::cargo::build_env::targeting_wasm() {
        let required_version = ParserVersion::required();
        let parser_path = Path::new(PARSER_PATH).absolutize()?;
        let provider = ParserProvider::new(required_version, &parser_path);
        provider.run().await?;
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", PARSER_PATH);

    Ok(())
}
