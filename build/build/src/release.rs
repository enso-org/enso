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

const BUCKET_FOR_GUI: &str = "ensocdn";


pub async fn draft_a_new_release(context: &BuildContext) -> Result<Release> {
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

    ide_ci::actions::env::set_and_emit(&crate::env::ENSO_RELEASE_ID, &release.id)?;
    Ok(release)
}

pub async fn publish_release(context: &BuildContext) -> Result {
    let BuildContext { inner: project::Context { octocrab, .. }, remote_repo, triple, .. } =
        context;

    let release_id = crate::env::ENSO_RELEASE_ID.get()?;

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

/// Download the Enso Engine distribution from the GitHub release.
pub async fn get_engine_package(
    octocrab: &Octocrab,
    repo: &(impl IsRepo + Send + Sync + 'static),
    output: impl AsRef<Path>,
    triple: &TargetTriple,
) -> Result<generated::EnginePackage> {
    let release_id = crate::env::ENSO_RELEASE_ID.get()?;
    let package_name = generated::RepoRootBuiltDistribution::new_root(".", triple.to_string())
        .enso_engine_triple
        .file_name()
        .context("Failed to get Engine Package name.")?
        .as_str()
        .to_string();

    let release = repo.find_release_by_id(octocrab, release_id).await?;
    let asset = github::find_asset_by_text(&release, &package_name)?;
    let temp_for_archive = tempdir()?;
    let downloaded_asset =
        repo.download_asset_to(octocrab, asset, temp_for_archive.path().to_owned()).await?;

    ide_ci::archive::extract_to(&downloaded_asset, output.as_ref()).await?;

    let engine_package =
        generated::EnginePackage::new_under(output.as_ref(), triple.versions.version.to_string());
    Ok(engine_package)
}

/// Download the Enso Engine distribution from the GitHub release and build Runtime Docker image
/// from it.
pub async fn generate_runtime_image(
    context: &BuildContext,
    tag: impl Into<String>,
) -> Result<ide_ci::programs::docker::ImageId> {
    // Our runtime images always target Linux.
    let linux_triple = TargetTriple { os: OS::Linux, ..context.triple.clone() };
    let temp_for_extraction = tempdir()?;
    let engine_package = get_engine_package(
        &context.octocrab,
        &context.remote_repo,
        temp_for_extraction.path(),
        &linux_triple,
    )
    .await?;
    crate::aws::ecr::runtime::build_runtime_image(
        context.repo_root.tools.ci.docker.clone(),
        engine_package,
        tag.into(),
    )
    .await
}

/// Perform deploy of the backend to the ECR.
///
/// Downloads the Engine package from the release, builds the runtime image from it and pushes it
/// to our ECR.
pub async fn deploy_to_ecr(context: &BuildContext, repository: String) -> Result {
    let client = crate::aws::ecr::client_from_env().await;
    let repository_uri = crate::aws::ecr::get_repository_uri(&client, &repository).await?;
    let tag = format!("{}:{}", repository_uri, context.triple.versions.version);
    // We don't care about the image ID, we will refer to it by the tag.
    let _image_id = generate_runtime_image(context, &tag).await?;
    let credentials = crate::aws::ecr::get_credentials(&client).await?;
    Docker.while_logged_in(credentials, || async move { Docker.push(&tag).await }).await?;
    Ok(())
}

/// Upload GUI to the cloud (AWS S3).
pub async fn upload_gui_to_cloud(
    assets: &crate::paths::generated::RepoRootDistGuiAssets,
    version: &Version,
) -> Result {
    let path = format!("ide/{version}");
    let bucket = crate::aws::s3::BucketContext {
        bucket:     BUCKET_FOR_GUI.into(),
        upload_acl: aws_sdk_s3::model::ObjectCannedAcl::PublicRead,
        client:     crate::aws::s3::client_from_env().await,
        key_prefix: path,
    };

    let files_to_upload = [
        assets.ide_wasm.as_path(),
        assets.index_js.as_path(),
        assets.style_css.as_path(),
        assets.wasm_imports_js.as_path(),
    ];

    for file in files_to_upload.iter() {
        bucket.put_file(file).await?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn upload_gui() -> Result {
        setup_logging()?;
        let assets = crate::paths::generated::RepoRootDistGuiAssets::new_root(
            r"H:\NBO\enso4\dist\gui\assets",
        );
        let version = "2022.1.1-dev.provisional.test.1".parse2()?;
        upload_gui_to_cloud(&assets, &version).await?;

        let body = json!({
            "version_number": version.to_string(),
            "version_type": "Ide"
        });
        let response = reqwest::Client::new()
            .post("https://uhso47ta0i.execute-api.eu-west-1.amazonaws.com/versions")
            .header("x-enso-organization-id", "org-2BqGX0q2yCdONdmx3Om1MVZzmv3")
            .header("Content-Type", "application/json")
            .json(&body)
            .send()
            .await?;
        trace!("{:?}", response);
        Ok(())
    }
}
