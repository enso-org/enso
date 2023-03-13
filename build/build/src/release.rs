//! Support code for managing Enso releases.

use crate::prelude::*;

use crate::changelog::Changelog;
use crate::context::BuildContext;
use crate::paths::generated;
use crate::paths::TargetTriple;
use crate::paths::EDITION_FILE_ARTIFACT_NAME;
use crate::project;
use crate::project::gui;
use crate::project::Gui;
use crate::project::IsTarget;
use crate::source::ExternalSource;
use crate::source::FetchTargetJob;
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
use std::env::consts::EXE_SUFFIX;
use tempfile::tempdir;



/// Get the prefix of URL of the release's asset in GitHub.
///
/// By joining it with the asset name, we can get the URL of the asset.
///
/// ```
/// # use enso_build::prelude::*;
/// # use enso_build::release::download_asset_prefix;
/// # use ide_ci::github::RepoRef;
/// let repo = RepoRef::new("enso-org", "enso");
/// let version = Version::from_str("2020.1.1").unwrap();
/// let prefix = download_asset_prefix(&repo, &version);
/// assert_eq!(prefix, "https://github.com/enso-org/enso/releases/download/2020.1.1");
/// ```
pub fn download_asset_prefix(repo: &impl IsRepo, version: &Version) -> String {
    format!("https://github.com/{repo}/releases/download/{version}",)
}

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
        "download_prefix",
        format!(
            "https://github.com/{}/releases/download/{}",
            context.remote_repo, context.triple.versions.version
        )
        .into(),
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
    crate::aws::update_manifest(&remote_repo, &edition_file_path).await?;

    Ok(())
}

/// Download the Enso Engine distribution from the GitHub release.
pub async fn get_engine_package<R: IsRepo>(
    repo: &github::repo::Handle<R>,
    output: impl AsRef<Path>,
    triple: &TargetTriple,
) -> Result<generated::EnginePackage> {
    let release_id = crate::env::ENSO_RELEASE_ID.get()?;
    let package_name = generated::RepoRootBuiltDistribution::new_root(
        ".",
        triple.versions.edition_name(),
        EXE_SUFFIX,
        triple.to_string(),
        triple.versions.version.to_string(),
    )
    .enso_engine_triple
    .file_name()
    .context("Failed to get Engine Package name.")?
    .as_str()
    .to_string();

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

/// Download the GUI artifacts from the current CI run artifacts.
pub async fn get_gui_from_current_ci_run(
    context: &BuildContext,
    out_dir: impl Into<PathBuf>,
) -> Result<gui::Artifact> {
    let target = Gui;
    let source = ExternalSource::new_ongoing_ci_run(target.artifact_name());
    let fetch_job = FetchTargetJob { destination: out_dir.into(), inner: source };
    target.get_external(context.inner.clone(), fetch_job).await
}

/// Upload GUI to the cloud (AWS S3).
pub async fn upload_gui_to_cloud_good(context: &BuildContext) -> Result {
    let temp = tempdir()?;
    let gui = get_gui_from_current_ci_run(context, temp.path()).await?;
    upload_gui_to_cloud(&gui.assets, &context.triple.versions.version).await?;
    notify_cloud_about_gui(&context.triple.versions.version).await?;
    Ok(())
}

/// Upload GUI to the cloud (AWS S3).
pub async fn upload_gui_to_cloud(
    assets: &crate::paths::generated::RepoRootDistGuiAssets,
    version: &Version,
) -> Result {
    let bucket = crate::aws::s3::gui::context(version).await?;

    // Some file we upload as-is, some gzipped. This seems somewhat arbitrary now.
    let files_to_upload = [
        assets.pkg_opt_wasm.as_path(),
        assets.style_css.as_path(),
        assets.dynamic_assets.as_path(),
    ];
    let files_to_upload_gzipped = [assets.index_js.as_path(), assets.pkg_js.as_path()];

    for file in files_to_upload.iter() {
        bucket.put_item(file).await?;
    }

    put_files_gzipping(&bucket, &files_to_upload_gzipped).await?;

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

    #[tokio::test]
    #[ignore]
    async fn upload_gui() -> Result {
        setup_logging()?;
        let assets = crate::paths::generated::RepoRootDistGuiAssets::new_root(
            r"H:\NBO\enso4\dist\gui\assets",
        );
        let version = "2023.1.1-dev.cloud.test".parse2()?;
        upload_gui_to_cloud(&assets, &version).await?;
        notify_cloud_about_gui(&version).await?;
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn notify_cloud() -> Result {
        setup_logging()?;
        let version = Version::from_str("2022.1.1-rc.2")?;
        notify_cloud_about_gui(&version).await?;
        Ok(())
    }
}
