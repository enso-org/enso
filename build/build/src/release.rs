use crate::prelude::*;

use crate::context::BuildContext;
use crate::paths::generated;
use crate::paths::TargetTriple;
use crate::paths::EDITION_FILE_ARTIFACT_NAME;
use crate::project;

use ide_ci::github;
use ide_ci::programs::Docker;
use octocrab::models::repos::Release;
use tempfile::tempdir;



pub async fn create_release(context: &BuildContext) -> Result<Release> {
    let versions = &context.triple.versions;
    let commit = ide_ci::actions::env::GITHUB_SHA.get()?;

    let changelog_contents = ide_ci::fs::read_to_string(&context.repo_root.changelog_md)?;
    let latest_changelog_body =
        crate::changelog::Changelog(&changelog_contents).top_release_notes()?;

    debug!("Preparing release {} for commit {}", versions.version, commit);
    let release = context
        .remote_repo
        .repos(&context.octocrab)
        .releases()
        .create(&versions.tag())
        .target_commitish(&commit)
        .name(&versions.pretty_name())
        .body(&latest_changelog_body.contents)
        .prerelease(true)
        .draft(true)
        .send()
        .await?;

    crate::env::ReleaseId.emit(&release.id)?;
    Ok(release)
}

pub async fn publish_release(context: &BuildContext) -> Result {
    let BuildContext { inner: project::Context { octocrab, .. }, remote_repo, triple, .. } =
        context;

    let release_id = crate::env::ReleaseId.fetch()?;

    debug!("Looking for release with id {release_id} on github.");
    let release = remote_repo.repos(octocrab).releases().get_by_id(release_id).await?;
    ensure!(release.draft, "Release has been already published!");

    debug!("Found the target release, will publish it.");
    remote_repo.repos(octocrab).releases().update(release.id.0).draft(false).send().await?;
    debug!("Done. Release URL: {}", release.url);

    let temp = tempdir()?;
    let edition_file_path = crate::paths::generated::RepoRootDistributionEditions::new_root(
        temp.path(),
        triple.versions.edition_name(),
    )
    .edition_yaml;


    ide_ci::actions::artifacts::download_single_file_artifact(
        EDITION_FILE_ARTIFACT_NAME,
        &edition_file_path,
    )
    .await?;

    debug!("Updating edition in the AWS S3.");
    crate::aws::update_manifest(remote_repo, &edition_file_path).await?;

    Ok(())
}

pub async fn deploy_to_ecr(context: &BuildContext, repository: String) -> Result {
    let octocrab = &context.octocrab;
    let release_id = crate::env::ReleaseId.fetch()?;

    let linux_triple = TargetTriple { os: OS::Linux, ..context.triple.clone() };
    let package_name =
        generated::RepoRootBuiltDistribution::new_root(".", linux_triple.to_string())
            .enso_engine_triple
            .file_name()
            .context("Failed to get Engine Package name.")?
            .as_str()
            .to_string();

    let release = context.remote_repo.find_release_by_id(octocrab, release_id).await?;
    let asset = github::find_asset_by_text(&release, &package_name)?;


    let temp_for_archive = tempdir()?;
    let downloaded_asset = context
        .remote_repo
        .download_asset_to(octocrab, asset, temp_for_archive.path().to_owned())
        .await?;

    let temp_for_extraction = tempdir()?;
    ide_ci::archive::extract_to(&downloaded_asset, &temp_for_extraction).await?;

    let engine_package = generated::EnginePackage::new_under(
        &temp_for_extraction,
        context.triple.versions.version.to_string(),
    );


    let config = &aws_config::load_from_env().await;
    let client = aws_sdk_ecr::Client::new(config);
    let repository_uri = crate::aws::ecr::get_repository_uri(&client, &repository).await?;
    let tag = format!("{}:{}", repository_uri, context.triple.versions.version);
    let _image = crate::aws::ecr::runtime::build_runtime_image(
        context.repo_root.tools.ci.docker.clone(),
        engine_package,
        tag.clone(),
    )
    .await?;

    let credentials = crate::aws::ecr::get_credentials(&client).await?;
    Docker.while_logged_in(credentials, || async move { Docker.push(&tag).await }).await?;
    Ok(())
}

pub async fn dispatch_cloud_image_build_action(octocrab: &Octocrab, version: &Version) -> Result {
    let input = serde_json::json!({
        "version": version.to_string(),
    });
    octocrab
        .actions()
        .create_workflow_dispatch("enso-org", "cloud-v2", "build-image.yaml", "main")
        .inputs(input)
        .send()
        .await
        .context("Failed to dispatch the cloud image build action.")
}
