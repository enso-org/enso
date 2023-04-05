//! Code supporting dealing with GitHub releases and their assets.

use crate::prelude::*;

use crate::github::Repo;

use mime::Mime;
use octocrab::models::repos::Asset;
use octocrab::models::repos::Release;
use octocrab::models::ReleaseId;
use reqwest::Body;
use tracing::instrument;



/// The extensions that will be used for the archives in the GitHub release assets.
///
/// On Windows we use `.zip`, because it has out-of-the-box support in the Explorer.
/// On other platforms we use `.tar.gz`, because it is a good default.
pub const ARCHIVE_EXTENSION: &str = match TARGET_OS {
    OS::Windows => "zip",
    _ => "tar.gz",
};

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
            crate::io::web::execute(request).await?.json().await.with_context(|| {
                format!("Failed to deserialize the response from the GitHub API to an asset.")
            })
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
    /// The filename will be used to name the asset and deduce MIME content type.
    #[instrument(skip_all, fields(source = %path.as_ref().display()), err)]
    async fn upload_asset_file(&self, path: impl AsRef<Path> + Send) -> Result<Asset> {
        let error_msg =
            format!("Failed to upload an asset from the file under {}.", path.as_ref().display());
        async move {
            let path = path.as_ref().to_path_buf();
            let asset_name = path.try_file_name()?;
            let content_type = new_mime_guess::from_path(&path).first_or_octet_stream();
            let metadata = crate::fs::tokio::metadata(&path).await?;
            trace!("File metadata: {metadata:#?}.");
            let file_size = metadata.len();
            let file = crate::fs::tokio::open_stream(&path).await?;
            let body = Body::wrap_stream(file);
            self.upload_asset(asset_name.as_str(), content_type, file_size, body).await
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
            custom_name.with_parent(temp_dir.path()).with_appended_extension(ARCHIVE_EXTENSION);
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
        self.octocrab()
            .repos(self.repo().owner(), self.repo().name())
            .releases()
            .get_by_id(self.id())
            .await
            .anyhow_err()
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
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct Handle {
    #[derivative(Debug(format_with = "std::fmt::Display::fmt"))]
    pub repo:     Repo,
    pub id:       ReleaseId,
    #[derivative(Debug = "ignore")]
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
    use reqwest::Body;

    #[tokio::test]
    #[ignore]
    pub async fn create_release() -> Result {
        let pat = std::env::var("GITHUB_TOKEN").unwrap();

        let octocrab = octocrab::Octocrab::builder().personal_token(pat.clone()).build()?;
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
