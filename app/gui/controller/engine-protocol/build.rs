use std::io::prelude::*;

use std::env;
use std::fs;
use std::fs::File;
use std::path::PathBuf;



// =========================
// == Hardcoded constants ==
// =========================

/// The name of zip containing engine interface files.
const ZIP_NAME: &str = "fbs-schema.zip";

/// The directory structure inside downloaded engine interface folder.
const ZIP_CONTENT: &str = "fbs-upload/fbs-schema/";

/// Commit from `enso` repository that will be used to obtain artifacts from.
/// If you change this commit manually, you must have `flatc` installed to regenerate interface
/// files. Run `cargo build` to do so, before creating a commit.
///
/// Follow to `contribution.md` for more guidance about setting up the development environment.
const COMMIT: &str = "0b363e3e85215aa0468f7ce8c17882f60f9284d9";

/// Currently the flatc-generated files updating shall work purely on opt-in basis. This script
/// should do nothing if the following environment variable has not been defined.
const ENABLE_ENV_VAR_NAME: &str = "ENSO_IDE_ENABLE_FLATC";

/// An URL pointing to engine interface files.
pub fn interface_description_url() -> reqwest::Url {
    let url =
        format!("https://packages.luna-lang.org/fbs-schema/nightly/{}/fbs-schema.zip", COMMIT);
    let err = format!("{} is an invalid URL.", url);
    reqwest::Url::parse(&url).expect(&err)
}



// ===================================
// == Download Engine Api Artifacts ==
// ===================================

/// Struct for downloading engine artifacts.
struct ApiProvider {
    /// The path where downloaded artifacts will be stored.
    out_dir: PathBuf,
}

impl ApiProvider {
    /// Creates a provider that can download engine artifacts.
    pub fn new() -> ApiProvider {
        let out_dir = env::var("OUT_DIR").expect("OUT_DIR isn't environment variable").into();
        ApiProvider { out_dir }
    }

    /// Downloads api artifacts into memory.
    pub async fn download(&self) -> bytes::Bytes {
        let url = interface_description_url();
        let get_error = format!("Failed to get response from {}", &url);
        let download_error = format!("Failed to download contents of {}", &url);
        let response = reqwest::get(url).await.expect(&get_error);
        response.bytes().await.expect(&download_error)
    }

    /// Saves unzipped artifacts into file.
    pub fn unzip(&self, artifacts: bytes::Bytes) {
        let zip_path = self.out_dir.join(ZIP_NAME);
        let display_path = zip_path.display();
        let open_error = format!("Failed to open {}", display_path);
        let write_error = format!("Failed to write {}", display_path);
        let flush_error = format!("Failed to flush {}", display_path);
        let unzip_error = format!("Failed to unzip {}", display_path);

        let mut file = File::create(&zip_path).expect(&open_error);
        file.write_all(&artifacts).expect(&write_error);
        file.flush().expect(&flush_error);

        let file = File::open(&zip_path).expect(&open_error);
        let mut archive = zip::ZipArchive::new(&file).expect(&open_error);
        archive.extract(&self.out_dir).expect(&unzip_error);
    }


    /// Generates rust files from FlatBuffers schemas.
    pub fn generate_files(&self) {
        let fbs_dir = self.out_dir.join(ZIP_CONTENT);
        for entry in fs::read_dir(fbs_dir).expect("Could not read content of dir") {
            let path = entry.expect("Invalid content of dir").path();
            let result = flatc_rust::run(flatc_rust::Args {
                inputs: &[&path],
                out_dir: &PathBuf::from("./src/generated"),
                ..Default::default()
            });
            if result.is_err() {
                println!(
                    "cargo:info=Engine API files were not regenerated because `flatc` isn't \
                         installed."
                );
                break;
            }
        }
    }

    /// Places required artifacts in the target location.
    pub async fn run(&self) {
        let fingerprint = self.out_dir.join("engine.api.fingerprint");
        let unchanged = match fs::read_to_string(&fingerprint) {
            Ok(commit) => commit == COMMIT,
            Err(_) => false,
        };
        if unchanged {
            return;
        }

        println!("cargo:info=Engine API artifacts version changed. Rebuilding.");
        let artifacts = self.download().await;
        self.unzip(artifacts);
        self.generate_files();
        fs::write(&fingerprint, COMMIT).expect("Unable to write artifacts fingerprint.");
    }
}



// ==========
// == main ==
// ==========

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // Regenerating bindings is now strictly opt-in, see: https://github.com/enso-org/ide/issues/644
    if env::var(ENABLE_ENV_VAR_NAME).is_ok() {
        let provider = ApiProvider::new();
        provider.run().await;
    } else {
        println!(
            "cargo:info=Will not try updating flatc-generated files. Define `{}` environment \
        variable to enable regeneration of the Engine API flatc bindings.",
            ENABLE_ENV_VAR_NAME
        );
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed={}", ENABLE_ENV_VAR_NAME);
    Ok(())
}
