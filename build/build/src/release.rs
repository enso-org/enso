//! Support code for managing Enso releases.

use crate::prelude::*;

use crate::changelog::Changelog;
use crate::context::BuildContext;
use crate::env::ENSO_ADMIN_TOKEN;
use crate::paths::generated;
use crate::paths::TargetTriple;
use crate::paths::EDITION_FILE_ARTIFACT_NAME;
use crate::project;
use crate::version;
use crate::version::promote::Designation;
use crate::version::Versions;

use ide_ci::github;
use ide_ci::io::web::handle_error_response;
use ide_ci::programs::Docker;
use ide_ci::programs::SevenZip;
use octocrab::models::repos::Release;
use octocrab::params::repos::Reference;
use reqwest::Response;
use serde_json::json;
use tempfile::tempdir;


// ==============
// === Export ===
// ==============

pub mod manifest;



/// Generate placeholders for the release notes.
pub fn release_body_placeholders(
    context: &BuildContext,
) -> Result<BTreeMap<&'static str, serde_json::Value>> {
    let mut ret = BTreeMap::new();
    ret.insert(
        "is_stable",
        (!version::Kind::deduce(&context.triple.versions.version)?.is_prerelease()).into(),
    );
    ret.insert("version", context.triple.versions.version.to_string().into());
    ret.insert("edition", context.triple.versions.edition_name().into());
    ret.insert("repo", serde_json::to_value(&context.remote_repo)?);
    ret.insert(
        "assets",
        serde_json::to_value(manifest::Assets::new(
            &context.remote_repo,
            &context.triple.versions.version,
        ))?,
    );

    // Generate the release notes.
    let changelog_contents = ide_ci::fs::read_to_string(&context.repo_root.changelog_md)?;
    let latest_changelog_body = Changelog(&changelog_contents).top_release_notes()?;
    ret.insert("changelog", latest_changelog_body.contents.into());
    Ok(ret)
}

/// Generate the release notes.
///
/// They are generated from the template file `release-body.md` by replacing the placeholders.
pub async fn generate_release_body(context: &BuildContext) -> Result<String> {
    let available_placeholders = release_body_placeholders(context)?;

    let handlebars = handlebars::Handlebars::new();
    let template = include_str!("../release-body.md");
    let body = handlebars.render_template(template, &available_placeholders)?;
    Ok(body)
}

/// Create a new release draft on GitHub.
///
/// As it is a draft, it will not be visible to the public until it is published.
#[context("Failed to draft a new release {} from the commit {}.", context.triple.versions.tag(),
commit)]
pub async fn draft_a_new_release(context: &BuildContext, commit: &str) -> Result<Release> {
    let versions = &context.triple.versions;
    let tag = versions.tag();

    // We don't require being run from a git repository context, but if we do happen to be, then
    // the local HEAD is required to be the same as the commit we read from environment.
    if let Ok(git) = context.git().await {
        let head_hash = git.head_hash().await?;
        ensure!(
            head_hash == commit,
            "Local HEAD {} is not the same as the commit {} we were ordered to build.",
            head_hash,
            commit
        );
    }

    // Make sure that this version has not been released yet.
    let repo = &context.remote_repo_handle();
    if let Ok(colliding_release) = repo.find_release_by_tag(&tag).await {
        ensure!(colliding_release.draft, "Release {} has already been published.", tag);
        warn!(
            "Release {tag} has already been drafted: {}. Creating a new one.",
            colliding_release.html_url
        );
    }

    // Check if the remote repository contains a conflicting tag.
    let colliding_remote_tag = repo.get_ref(&Reference::Tag(tag.clone())).await;
    if let Ok(colliding_remote_tag) = colliding_remote_tag {
        bail!("Tag {} already exists on remote as {:?}.", tag, colliding_remote_tag.object);
    }

    let is_prerelease = version::Kind::deduce(&versions.version)?.is_prerelease();

    debug!("Preparing release {} for commit {}", versions.version, commit);
    let release = context
        .remote_repo_handle()
        .repos()
        .releases()
        .create(&versions.tag())
        .target_commitish(&commit)
        .name(&versions.pretty_name())
        .body(&generate_release_body(context).await?)
        .prerelease(is_prerelease)
        .draft(true)
        .send()
        .await?;

    ide_ci::actions::workflow::set_output(&crate::env::ENSO_RELEASE_ID, &release.id).await?;
    Ok(release)
}

/// Publish a previously drafted release and update edition file in the cloud.
///
/// Requires the release ID to be set in the environment variable `ENSO_RELEASE_ID`.
pub async fn publish_release(context: &BuildContext) -> Result {
    let remote_repo = context.remote_repo_handle();
    let BuildContext { inner: project::Context { .. }, triple, .. } = context;

    let release_id = crate::env::ENSO_RELEASE_ID.get()?;
    let release_handle = remote_repo.release_handle(release_id);

    debug!("Looking for release with id {release_id} on github.");
    let release = release_handle.get().await?;
    ensure!(release.draft, "Release has been already published!");

    debug!("Found the target release, will publish it.");
    release_handle.publish().await?;
    debug!("Done. Release URL: {}", release.url);

    let temp = tempdir()?;
    let edition_file_path = generated::RepoRootDistributionEditions::new_root(
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
    crate::aws::update_manifest(&remote_repo, &edition_file_path).await?;

    // Add assets manifest.
    let manifest = manifest::Assets::new(&remote_repo, &triple.versions.version);
    let tempdir = tempdir()?;
    let manifest_path = tempdir.path().join(manifest::ASSETS_MANIFEST_FILENAME);
    ide_ci::fs::write_json(&manifest_path, &manifest)?;
    release_handle.upload_asset_file(&manifest_path).await?;

    // The validation step is performed to enable issue reporting and enhance issue visibility.
    // Currently, even if the validation fails, the release will not be retracted.
    validate_release(release_handle).await?;

    Ok(())
}

/// Perform basic check if the release contains advertised assets.
///
/// This should be run only on a published (non-draft) release, as asset download URLs change after
/// publishing.
#[context("Failed to validate release: {release:?}")]
pub async fn validate_release(release: github::release::Handle) -> Result {
    let info = release.get().await?;
    ensure!(!info.draft, "Release is a draft.");
    let version = Version::from_str(&info.tag_name)?;
    let manifest_url = github::release::download_asset(
        &release.repo,
        &version,
        manifest::ASSETS_MANIFEST_FILENAME,
    );
    let manifest = ide_ci::io::download_all(manifest_url)
        .await
        .context("Failed to download assets manifest.")?;
    let manifest: manifest::Assets =
        serde_json::from_slice(&manifest).context("Failed to parse assets manifest.")?;
    for asset in manifest.assets() {
        let response = reqwest::Client::new().get(asset.url.clone()).send().await?;
        ensure!(response.status().is_success(), "Failed to download asset: {}", asset.url);
    }
    Ok(())
}

/// Download the Enso Engine distribution from the GitHub release.
pub async fn get_engine_package<R: IsRepo>(
    repo: &github::repo::Handle<R>,
    output: impl AsRef<Path>,
    triple: &TargetTriple,
) -> Result<generated::EnginePackage> {
    let release_id = crate::env::ENSO_RELEASE_ID.get()?;
    let package_name =
        generated::RepoRootBuiltDistributionEnsoEngineTriple::segment_name(triple.to_string());
    let release = repo.find_release_by_id(release_id).await?;
    let asset = github::find_asset_by_text(&release, &package_name)?;
    let temp_for_archive = tempdir()?;
    let downloaded_asset =
        repo.download_asset_to(asset, temp_for_archive.path().to_owned()).await?;

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
        &context.remote_repo_handle(),
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

/// Packs given files with `gzip` and uploads them to the S3 bucket.
///
/// The files are uploaded with the same name, but with `.gz` extension.
pub async fn put_files_gzipping(
    bucket: &crate::aws::s3::BucketContext,
    files: impl IntoIterator<Item = impl AsRef<Path>>,
) -> Result {
    let temp_for_gzipping = tempdir()?;
    for file in files {
        let gzipped_file = file.with_parent(&temp_for_gzipping).with_appended_extension("gz");
        SevenZip.pack(&gzipped_file, [file]).await?;
        bucket.put_file(&gzipped_file).await?;
    }
    Ok(())
}

/// Deploy the built GUI (frontend) to the cloud.
#[context("Failed to notify the cloud about GUI upload in version {}.", version)]
pub async fn notify_cloud_about_gui(version: &Version) -> Result<Response> {
    let body = json!({
        "versionNumber": version.to_string(),
        "versionType": "Ide"
    });
    let response = reqwest::Client::new()
        .post("https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com/versions")
        .header("x-enso-organization-id", "org-2BqGX0q2yCdONdmx3Om1MVZzmv3")
        .header("x-enso-admin-token", ENSO_ADMIN_TOKEN.get()?)
        .header("Content-Type", "application/json")
        .json(&body)
        .send()
        .await?;
    debug!("Response code from the cloud: {}.", response.status());
    handle_error_response(response).await
}

/// Generate a new version number of a requested kind.
pub async fn resolve_version_designation(
    context: &BuildContext,
    designation: Designation,
) -> Result<Version> {
    let git = context.git().await?;
    let version_set = version::promote::Releases::from_remote(&git).await?;
    version_set.generate_version(designation)
}

/// Generate a new version and make it visible to CI.
///
/// Sets `ENSO_VERSION`, `ENSO_EDITION` and `ENSO_RELEASE_MODE` environment variables.
#[instrument]
pub async fn promote_release(context: &BuildContext, version_designation: Designation) -> Result {
    let version = resolve_version_designation(context, version_designation).await?;
    let versions = Versions::new(version);
    versions.publish().await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use ide_ci::cache::Cache;
    use ide_ci::github::setup_octocrab;

    #[tokio::test]
    #[ignore]
    async fn notify_cloud() -> Result {
        setup_logging().ok();
        let version = Version::from_str("2022.1.1-rc.2")?;
        notify_cloud_about_gui(&version).await?;
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn release_assets() -> Result {
        setup_logging().ok();

        let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let repo_root = crate_dir.parent().unwrap().parent().unwrap();
        let version = Version::from_str("2024.1.1-nightly.2024.3.26")?;
        let triple = TargetTriple::new(Versions::new(version.clone()));
        let context = BuildContext {
            inner:       project::Context {
                repo_root: crate::paths::new_repo_root(repo_root, &triple),
                octocrab:  setup_octocrab().await?,
                cache:     Cache::new_default().await?,
            },
            remote_repo: github::Repo::new("enso-org", "enso"),
            triple:      TargetTriple::new(Versions::new(version.clone())),
        };

        let release_body = generate_release_body(&context).await?;
        debug!("Release body: {}", release_body);

        let manifest = manifest::Assets::new(&context.remote_repo, &version);
        let manifest_json = serde_json::to_string_pretty(&manifest)?;
        debug!("Manifest: {}", manifest_json);

        Ok(())
    }
}
