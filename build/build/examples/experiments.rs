use enso_build::prelude::*;

use enso_build::setup_octocrab;
use ide_ci::github::Repo;
use octocrab::models::ReleaseId;



#[tokio::main]
async fn main() -> Result {
    let octo = setup_octocrab().await?;
    let repo = Repo::from_str("enso-org/enso-staging")?;
    let handler = repo.repos(&octo);
    let releases = handler.releases();

    let release = releases.get_by_id(ReleaseId(59585385)).await?;
    dbg!(&release);

    Ok(())
}
