//! Code supporting dealing with GitHub releases and their assets.

use crate::prelude::*;

use crate::github::Repo;

use mime::Mime;
use octocrab::models::repos::Asset;
use octocrab::models::repos::Release;
use octocrab::models::ReleaseId;
use reqwest::Body;
use tracing::instrument;


// ==============
// === Export ===
// ==============

pub use octocrab::models::ReleaseId as Id;



/// Extension of the preferred archive format for release assets on the current platform.
pub fn archive_extension() -> &'static str {
    archive_extension_for(TARGET_OS)
}

/// Get archive format extension for release assets targeting the given operating system.
///
/// - For Windows, we use `.zip` because it has built-in support in Windows Explorer.
/// - For all other operating systems, we use `.tar.gz` as the default.
pub fn archive_extension_for(os: OS) -> &'static str {
    match os {
        OS::Windows => "zip",
        _ => "tar.gz",
    }
}

/// Get the prefix of URL of the release's asset in GitHub.
///
/// By joining it with the asset name, we can get the URL of the asset.
///
/// ```
/// # use ide_ci::prelude::*;
/// # use ide_ci::github::release::download_asset_prefix;
/// # use ide_ci::github::RepoRef;
/// let repo = RepoRef::new("enso-org", "enso");
/// let version = Version::from_str("2020.1.1").unwrap();
/// let prefix = download_asset_prefix(&repo, &version);
/// assert_eq!(prefix.as_str(), "https://github.com/enso-org/enso/releases/download/2020.1.1/");
/// ```
pub fn download_asset_prefix(repo: &impl IsRepo, release_tag: &impl Display) -> Url {
    let text = format!("https://github.com/{repo}/releases/download/{release_tag}/",);
    Url::from_str(&text).expect("Failed to parse the URL.")
}

/// Get the URL for downloading the asset from the GitHub release.
///
/// ```
/// # use ide_ci::prelude::*;
/// # use ide_ci::github::release::download_asset;
/// # use ide_ci::github::RepoRef;
/// let repo = RepoRef::new("enso-org", "enso");
/// let version = Version::from_str("2020.1.1").unwrap();
/// let name = "foo.zip";
/// let url = download_asset(&repo, &version, name);
/// assert_eq!(url.as_str(), "https://github.com/enso-org/enso/releases/download/2020.1.1/foo.zip");
/// ```
pub fn download_asset(
    repo: &impl IsRepo,
    release_tag: &impl Display,
    asset_name: impl AsRef<str>,
) -> Url {
    let prefix = download_asset_prefix(repo, release_tag);
    prefix
        .join(asset_name.as_ref())
        .with_context(|| {
            format!(
                "Failed to join the prefix {} with the asset name {}.",
                prefix,
                asset_name.as_ref()
            )
        })
        .expect("Failed to join the URL.")
}

/// Types that uniquely identify a release and can be used to fetch it from GitHub.
pub trait IsRelease: Debug {
    /// The release ID.
    fn id(&self) -> ReleaseId;

    /// The repository where the release is located.
    fn repo(&self) -> Repo;

    /// Client used to perform GitHub API operations.
    fn octocrab(&self) -> &Octocrab;
}

/// Information about release that we can provide from the [`IsRelease`] trait.
#[async_trait]
pub trait IsReleaseExt: IsRelease + Sync {
    /// Upload a new asset to the release.
    fn upload_asset(
        &self,
        asset_name: impl AsRef<str>,
        content_type: Mime,
        content_length: u64,
        body: impl Into<Body>,
    ) -> BoxFuture<'static, Result<Asset>> {
        let asset_name = asset_name.as_ref().to_string();
        let upload_url = format!(
            "https://uploads.github.com/repos/{repo}/releases/{release_id}/assets",
            repo = self.repo(),
            release_id = self.id(),
        );
        let body = body.into();
        let request = self
            .octocrab()
            .client
            .post(&upload_url)
            .query(&[("name", &asset_name)])
            .header(reqwest::header::ACCEPT, "application/vnd.github.v3+json")
            .header(reqwest::header::CONTENT_TYPE, content_type.to_string())
            .header(reqwest::header::CONTENT_LENGTH, content_length)
            .body(body);

        async move {
            ensure!(content_length > 0, "Release asset file cannot be empty.");
            crate::io::web::execute(request)
                .await?
                .json()
                .await
                .context("Failed to deserialize the response from the GitHub API to an asset.")
        }
        .with_context(move || {
            format!(
                "Failed to upload an asset to the {upload_url}. Asset name: {asset_name}. \
                Content type: {content_type}. Content length: {content_length}."
            )
        })
        .boxed()
    }

    /// Upload a new asset to the release from a given file.
    ///
    /// Given closure `f` is used to transform the filename.
    #[instrument(skip_all, fields(source = %path.as_ref().display()), err)]
    async fn upload_asset_file_with_custom_name(
        &self,
        path: impl AsRef<Path> + Send,
        f: impl FnOnce(String) -> String + Send,
    ) -> Result<Asset> {
        let error_msg =
            format!("Failed to upload an asset from the file under {}.", path.as_ref().display());
        let filename = path.as_ref().try_file_name().map(|filename| f(filename.as_str().into()));
        async move { self.upload_asset_file_as(path, &filename?).await }.await.context(error_msg)
    }

    /// Upload a new asset to the release from a given file.
    ///
    /// The filename will be used to name the asset and deduce MIME content type.
    #[instrument(skip_all, fields(source = %path.as_ref().display()), err)]
    async fn upload_asset_file(&self, path: impl AsRef<Path> + Send) -> Result<Asset> {
        let error_msg =
            format!("Failed to upload an asset from the file under {}.", path.as_ref().display());
        async move {
            let filename = path.try_file_name()?.to_owned();
            self.upload_asset_file_as(path, filename).await
        }
        .await
        .context(error_msg)
    }

    /// Upload a new asset to the release from a given file with a custom name.
    #[instrument(skip_all, fields(source = %path.as_ref().display(), asset = %asset_filename.as_ref().display()), err)]
    async fn upload_asset_file_as(
        &self,
        path: impl AsRef<Path> + Send,
        asset_filename: impl AsRef<Path> + Send,
    ) -> Result<Asset> {
        let error_msg = format!(
            "Failed to upload an asset from the file under {} as {}.",
            path.as_ref().display(),
            asset_filename.as_ref().display()
        );
        async move {
            let path = path.as_ref();
            let asset_name = asset_filename.as_ref();
            let content_type = new_mime_guess::from_path(path).first_or_octet_stream();
            let metadata = crate::fs::tokio::metadata(path).await?;
            trace!("File metadata: {metadata:#?}.");
            let file_size = metadata.len();
            crate::io::retry(|| async {
                let file = crate::fs::tokio::open_stream(path).await?;
                let body = Body::wrap_stream(file);
                self.upload_asset(asset_name.as_str(), content_type.clone(), file_size, body).await
            })
            .await
        }
        .await
        .context(error_msg)
    }

    /// Upload given directory as a release asset.
    ///
    /// The given filename will be used, with appended [platform-specific
    /// extension](ARCHIVE_EXTENSION).
    async fn upload_compressed_dir_as(
        &self,
        dir_to_upload: impl AsRef<Path> + Send,
        custom_name: impl AsRef<Path> + Send,
    ) -> Result<Asset> {
        let dir_to_upload = dir_to_upload.as_ref();
        let temp_dir = tempfile::tempdir()?;
        let archive_path =
            custom_name.with_parent(temp_dir.path()).with_appended_extension(archive_extension());
        crate::archive::create(&archive_path, [&dir_to_upload]).await?;
        self.upload_asset_file(archive_path).await
    }

    /// Upload given directory as a release asset.
    ///
    /// The archive name will be deduced from the directory name.
    async fn upload_compressed_dir(&self, path: impl AsRef<Path> + Send) -> Result<Asset> {
        let output_filename_stem = path.try_file_name()?.to_owned();
        self.upload_compressed_dir_as(path, output_filename_stem).await
    }

    /// Get the information about the release.
    async fn get(&self) -> Result<Release> {
        Ok(self
            .octocrab()
            .repos(self.repo().owner(), self.repo().name())
            .releases()
            .get_by_id(self.id())
            .await?)
    }

    async fn publish(&self) -> Result<Release> {
        self.octocrab()
            .repos(self.repo().owner(), self.repo().name())
            .releases()
            .update(self.id().0)
            .draft(false)
            .send()
            .await
            .with_context(|| format!("Failed to publish the release {}.", self.id()))
    }
}

impl<T> IsReleaseExt for T where T: IsRelease + Sync {}

/// A release on GitHub.
#[derive(Clone)]
#[derive_where(Debug)]
pub struct Handle {
    pub repo:     Repo,
    pub id:       ReleaseId,
    #[derive_where(skip)]
    pub octocrab: Octocrab,
}

impl IsRelease for Handle {
    fn id(&self) -> ReleaseId {
        self.id
    }

    fn repo(&self) -> Repo {
        self.repo.clone()
    }

    fn octocrab(&self) -> &Octocrab {
        &self.octocrab
    }
}

impl Handle {
    pub fn new(octocrab: &Octocrab, repo: impl Into<Repo>, id: ReleaseId) -> Self {
        let repo = repo.into();
        Self { repo, id, octocrab: octocrab.clone() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use reqwest::header::HeaderMap;

    #[tokio::test]
    #[ignore]
    pub async fn create_release() -> Result {
        let pat = std::env::var("GITHUB_TOKEN").unwrap();

        let octocrab = Octocrab::builder().personal_token(pat.clone()).build()?;
        let repo = octocrab.repos("enso-org", "ci-build");
        let release = if let Ok(release) = repo.releases().get_latest().await {
            release
        } else {
            repo.releases()
                .create("release_tag_name")
                .name("release_name")
                .target_commitish("4ba61ded019a9d4919e2b22a0ed3746ff2a6c088")
                .send()
                .await?
        };


        dbg!(&release);

        let mut header_map = HeaderMap::new();
        header_map.append(reqwest::header::AUTHORIZATION, format!("Bearer {pat}").parse()?);
        let client = reqwest::Client::builder()
            .user_agent("enso-build")
            .default_headers(header_map)
            .build()?;

        // TODO label?


        let upload_url_string = release.upload_url.to_string();
        dbg!(&upload_url_string);

        let upload_url = format!(
            "https://uploads.github.com/repos/{}/{}/releases/{}/assets",
            "enso-org", "ci-build", release.id
        );
        let file_to_upload = PathBuf::from(r"D:\mask_bin.png");
        let mime = new_mime_guess::from_path(&file_to_upload).first_or_octet_stream();
        let file = tokio::fs::File::open(&file_to_upload).await?;
        let file_size = file.metadata().await?.len();
        let file_contents_stream = tokio_util::io::ReaderStream::new(file);
        let body = Body::wrap_stream(file_contents_stream);
        let request = client
            .post(upload_url)
            .query(&[("name", "testowyasset2.png")])
            .header(reqwest::header::ACCEPT, "application/vnd.github.v3+json")
            .header(reqwest::header::CONTENT_TYPE, mime.to_string())
            .header(reqwest::header::CONTENT_LENGTH, file_size)
            .body(body)
            .build()?;

        dbg!(&request);
        let response = client.execute(request).await?;


        dbg!(&response);
        // debug!("{}", response.text().await?);
        response.error_for_status()?;
        Ok(())
    }
}
