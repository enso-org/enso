// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_build_cli::prelude::*;

use enso_build::setup_octocrab;
use ide_ci::github::Repo;
use ide_ci::io::web::handle_error_response;
use ide_ci::log::setup_logging;



#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let octo = setup_octocrab().await?;
    let repo = Repo::from_str("enso-org/enso")?.handle(&octo);

    let releases = repo.all_releases().await?;
    let draft_releases = releases.into_iter().filter(|r| r.draft);
    for release in draft_releases {
        let id = release.id;

        let route = format!("{}repos/{repo}/releases/{id}", octo.base_url);
        info!("Will delete {}: {route}.", release.name.unwrap_or_default());
        let response = octo._delete(route, Option::<&()>::None).await?;
        handle_error_response(response).await?;
    }


    Ok(())
}
