// === Features ===
#![feature(default_free_fn)]

use enso_build::prelude::*;

use ide_ci::actions::artifacts;
use ide_ci::actions::artifacts::download::ArtifactDownloader;
use ide_ci::actions::artifacts::run_session::SessionClient;
use tempfile::TempDir;



#[tokio::main]
async fn main() -> Result {
    let dir = std::env::current_exe()?.parent().unwrap().to_owned();

    debug!("Will upload {}", dir.display());
    let provider = artifacts::discover_recursive(dir);
    artifacts::upload(provider, "MyPrecious", default()).await?;


    let file = std::env::current_exe()?;
    debug!("Will upload {}", file.display());
    let artifact_name = file.file_name().unwrap().to_str().unwrap();
    let provider = artifacts::single_file_provider(file.clone())?;
    artifacts::upload(provider, artifact_name, default()).await?;
    debug!("Upload done!");
    // artifacts::upload_single_file(file, )

    let context = artifacts::context::Context::new_from_env()?;
    let session = SessionClient::new(&context)?;

    // debug!("Checking artifacts through official API");
    // let octocrab = setup_octocrab()?;
    // let run_id = ide_ci::actions::env::run_id()?;
    // debug!("Got run ID {run_id}");
    // let run = octocrab.workflows("enso-org", "ci-build").get(run_id).await?;
    // debug!("Got run information {run:?}");
    // let artifacts = octocrab
    //     .actions()
    //     .list_workflow_run_artifacts("enso-org", "ci-build", run_id)
    //     .send()
    //     .await?;
    // debug!("Got artifacts information {artifacts:?}");


    debug!("Checking artifacts through runtime API");
    let list = session.list_artifacts().await?;
    dbg!(&list);

    let relevant_entry = list
        .iter()
        .find(|artifact| artifact.name == artifact_name)
        .ok_or_else(|| anyhow!("Failed to find artifact by name {artifact_name}."))?;

    dbg!(&relevant_entry);

    let items = ide_ci::actions::artifacts::raw::endpoints::get_container_items(
        &context.json_client()?,
        relevant_entry.file_container_resource_url.clone(),
        &relevant_entry.name,
    )
    .await?;
    dbg!(&items);

    let temp = TempDir::new()?;
    let downloader = ArtifactDownloader::new(session.clone(), artifact_name).await?;
    downloader.download_all_to(temp.path()).await?;

    let expected_path = temp.path().join(artifact_name);
    assert_eq!(std::fs::read(expected_path)?, std::fs::read(&file)?);
    Ok(())
}
