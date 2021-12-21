//! This build script is responsible for ensuring that if parser targets wasm,
//! the JS Parser package is available at the expected location for
//! `wasm_bindgen` tool.

#![feature(option_result_contains)]

use enso_build_utilities::absolute_path;
use enso_build_utilities::targeting_wasm;
use enso_build_utilities::PathRef;

use std::fs;
use std::fs::create_dir_all;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;



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
#[derive(Clone, Debug, PartialEq)]
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
    pub fn new(version: ParserVersion, parser_path: impl PathRef) -> ParserProvider {
        let parser_path = PathBuf::from(parser_path.as_ref());
        ParserProvider { version, parser_path }
    }

    /// Downloads contents of JS parser into memory.
    pub async fn download(&self) -> bytes::Bytes {
        let url = parser_url(&self.version);
        let get_error = format!("Failed to get response from {}.", url);
        let download_error = format!("Failed to download contents of {}.", url);
        let server_error = format!("Server replied with error when getting {}.", url);
        let response = reqwest::get(url).await.expect(&get_error);
        let response = response.error_for_status().expect(&server_error);
        response.bytes().await.expect(&download_error)
    }

    /// Stores JS parser into file, after patching with a `PARSER_PREAMBLE`.
    pub fn patch_and_store(&self, js_parser: bytes::Bytes) {
        let display_path = self.parser_path.display();
        let open_error = format!("Failed to open {}.", display_path);
        let write_error = format!("Failed to write {}.", display_path);
        let flush_error = format!("Failed to flush {}.", display_path);

        let mut file = File::create(&self.parser_path).expect(&open_error);
        file.write_all(PARSER_PREAMBLE.as_bytes()).expect(&write_error);
        file.write_all(&js_parser).expect(&write_error);
        file.flush().expect(&flush_error);
    }

    /// Ensures that target's parent directory exists.
    pub fn prepare_target_location(&self) {
        let parent_directory =
            self.parser_path.parent().expect("Unable to access parent directory.");
        let create_dir_error =
            format!("Failed to create directory: {}.", parent_directory.display());
        create_dir_all(parent_directory).expect(&create_dir_error);
    }

    /// Places required parser version in the target location.
    pub async fn run(&self) {
        self.prepare_target_location();
        let parent_directory =
            self.parser_path.parent().expect("Unable to access parent directory.");
        let fingerprint = parent_directory.join("parser.fingerprint");
        let opt_version = fs::read_to_string(&fingerprint);
        let changed = match opt_version {
            Err(_) => true,
            Ok(hash) => hash != PARSER_COMMIT,
        };
        if changed {
            let parser_js = self.download().await;
            self.patch_and_store(parser_js);
            fs::write(&fingerprint, PARSER_COMMIT).expect("Unable to write parser fingerprint.");
        }
    }
}



// ==========
// == main ==
// ==========

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    if targeting_wasm() {
        let required_version = ParserVersion::required();
        let parser_path = absolute_path(PARSER_PATH)?;
        let provider = ParserProvider::new(required_version, &parser_path);
        provider.run().await;
    }
    println!("cargo:rerun-if-changed=build.rs");
    Ok(())
}
