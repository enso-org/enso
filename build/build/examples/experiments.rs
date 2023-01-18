use enso_build::prelude::*;

use ide_ci::github::setup_octocrab;
use ide_ci::github::Repo;
use octocrab::models::ReleaseId;



#[tokio::main]
async fn main() -> Result {
    let octo = setup_octocrab().await?;
    let repo = Repo::from_str("enso-org/enso-staging")?;
    let handler = repo.handle(&octo);
    let repos = handler.repos();
    let releases = repos.releases();

    let release = releases.get_by_id(ReleaseId(59585385)).await?;
    dbg!(&release);

    Ok(())
}
