// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_build_cli::prelude::*;

use enso_build::setup_octocrab;
use ide_ci::github::RepoRef;
use ide_ci::io::web::handle_error_response;
use ide_ci::log::setup_logging;



const REPO: RepoRef = RepoRef { owner: "enso-org", name: "enso" };

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    info!("Removing draft releases from GitHub.");
    let octo = setup_octocrab().await?;
    if let Err(e) = octo.current().user().await {
        bail!(
            "Failed to authenticate: {}.\nBeing authenticated is necessary to see draft releases.",
            e
        );
    }
    let repo = REPO.handle(&octo);
    info!("Fetching all releases.");
    let releases = repo.all_releases().await?;
    let draft_releases = releases.into_iter().filter(|r| r.draft).collect_vec();
    info!("Found {} draft releases.", draft_releases.len());
    for release in draft_releases {
        let id = release.id;

        let route = format!("{}repos/{repo}/releases/{id}", octo.base_url);
        info!("Will delete {}: {route}.", release.name.unwrap_or_default());
        let response = octo._delete(route, Option::<&()>::None).await?;
        handle_error_response(response).await?;
    }

    info!("Done.");
    Ok(())
}
