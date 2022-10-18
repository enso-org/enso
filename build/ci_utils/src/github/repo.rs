use crate::cache::download::DownloadFile;
use crate::github;
use crate::github::model;
use crate::github::MAX_PER_PAGE;
use crate::prelude::*;
use headers::HeaderMap;
use headers::HeaderValue;
use octocrab::models::repos::Asset;
use octocrab::models::repos::Release;
use octocrab::models::workflows::WorkflowListArtifact;
use octocrab::models::ArtifactId;
use octocrab::models::AssetId;
use octocrab::models::ReleaseId;
use octocrab::models::RunId;
use octocrab::params::actions::ArchiveFormat;
use reqwest::Response;



/// Data denoting a specific GitHub repository.
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize, derive_more::Display)]
#[display(fmt = "{}/{}", owner, name)]
pub struct Repo {
    /// Owner - an organization's or user's name.
    pub owner: String,
    pub name:  String,
}

impl IsRepo for Repo {
    fn owner(&self) -> &str {
        &self.owner
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// Parse from strings in format "owner/name". Opposite of `Display`.
impl std::str::FromStr for Repo {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        RepoRef::from_str(s).map(|repo| repo.into())
    }
}

impl<'a> From<RepoRef<'a>> for Repo {
    fn from(repo: RepoRef<'a>) -> Self {
        Repo { owner: repo.owner.to_owned(), name: repo.name.to_owned() }
    }
}

impl Repo {
    pub fn new(owner: impl Into<String>, name: impl Into<String>) -> Self {
        Self { owner: owner.into(), name: name.into() }
    }
}


/// Non-owning reference to a GitHub repository.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize, derive_more::Display)]
#[display(fmt = "{}/{}", owner, name)]
pub struct RepoRef<'a> {
    /// Owner - an organization's or user's name.
    pub owner: &'a str,
    pub name:  &'a str,
}

impl<'a> IsRepo for RepoRef<'a> {
    fn owner(&self) -> &str {
        self.owner
    }

    fn name(&self) -> &str {
        self.name
    }
}

impl<'a> RepoRef<'a> {
    pub const fn new<T1, T2>(owner: &'a T1, name: &'a T2) -> Self
    where
        T1: ~const AsRef<str> + ?Sized,
        T2: ~const AsRef<str> + ?Sized, {
        Self { owner: owner.as_ref(), name: name.as_ref() }
    }

    pub fn from_str(s: &'a (impl AsRef<str> + ?Sized)) -> Result<Self> {
        let s = s.as_ref();
        match s.split('/').collect_vec().as_slice() {
            [owner, name] => Ok(Self { owner, name }),
            slice => bail!("Failed to parse string '{}': Splitting by '/' should yield exactly 2 pieces, found: {}", s, slice.len()),
        }
    }
}

/// Entity that uniquely identifies a GitHub-hosted repository.
#[async_trait]
pub trait IsRepo: Display {
    fn owner(&self) -> &str;
    fn name(&self) -> &str;

    /// Generate a token that can be used to register a new runner for this repository.
    async fn generate_runner_registration_token(
        &self,
        octocrab: &Octocrab,
    ) -> Result<model::RegistrationToken> {
        let path =
            iformat!("/repos/{self.owner()}/{self.name()}/actions/runners/registration-token");
        let url = octocrab.absolute_url(path)?;
        octocrab.post(url, EMPTY_REQUEST_BODY).await.context(format!(
            "Failed to generate a runner registration token for the {self} repository."
        ))
    }

    /// The repository's URL.
    fn url(&self) -> Result<Url> {
        let url_text = iformat!("https://github.com/{self.owner()}/{self.name()}");
        Url::parse(&url_text)
            .context(format!("Failed to generate an URL for the {self} repository."))
    }

    fn repos<'a>(&'a self, client: &'a Octocrab) -> octocrab::repos::RepoHandler<'a> {
        client.repos(self.owner(), self.name())
    }

    async fn all_releases(&self, client: &Octocrab) -> Result<Vec<Release>> {
        github::get_all(client, self.repos(client).releases().list().per_page(MAX_PER_PAGE).send())
            .await
            .context(format!("Failed to list all releases in the {self} repository."))
    }

    async fn latest_release(&self, client: &Octocrab) -> Result<Release> {
        self.repos(client)
            .releases()
            .get_latest()
            .await
            .context(format!("Failed to get the latest release in the {self} repository."))
    }

    async fn find_release_by_id(
        &self,
        client: &Octocrab,
        release_id: ReleaseId,
    ) -> Result<Release> {
        let repo_handler = self.repos(client);
        let releases_handler = repo_handler.releases();
        releases_handler
            .get_by_id(release_id)
            .await
            .context(format!("Failed to find release by id `{release_id}` in `{self}`."))
    }

    #[tracing::instrument(skip(client), fields(%self, %text), err)]
    async fn find_release_by_text(&self, client: &Octocrab, text: &str) -> anyhow::Result<Release> {
        self.all_releases(client)
            .await?
            .into_iter()
            .find(|release| release.tag_name.contains(text))
            .inspect(|release| info!("Found release at: {} (id={}).", release.html_url, release.id))
            .context(format!("No release with tag matching `{text}` in {self}."))
    }

    #[tracing::instrument(skip(client), fields(%self, %run_id, %name), err, ret)]
    async fn find_artifact_by_name(
        &self,
        client: &Octocrab,
        run_id: RunId,
        name: &str,
    ) -> Result<WorkflowListArtifact> {
        let artifacts = client
            .actions()
            .list_workflow_run_artifacts(self.owner(), self.name(), run_id)
            .per_page(100)
            .send()
            .await
            .context(format!("Failed to list artifacts of run {run_id} in {self}."))?
            .value
            .context("Failed to find any artifacts.")?;

        artifacts
            .into_iter()
            .find(|artifact| artifact.name == name)
            .context(format!("Failed to find artifact by name '{name}'."))
    }

    async fn download_artifact(&self, client: &Octocrab, artifact_id: ArtifactId) -> Result<Bytes> {
        client
            .actions()
            .download_artifact(self.owner(), self.name(), artifact_id, ArchiveFormat::Zip)
            .await
            .context(format!("Failed to download artifact with ID={artifact_id}."))
    }

    async fn download_and_unpack_artifact(
        &self,
        client: &Octocrab,
        artifact_id: ArtifactId,
        output_dir: &Path,
    ) -> Result {
        let bytes = self.download_artifact(client, artifact_id).await?;
        crate::archive::zip::extract_bytes(bytes, output_dir)?;
        Ok(())
    }

    #[tracing::instrument(name="Get the asset information.", skip(client), fields(self=%self), err)]
    async fn asset(&self, client: &Octocrab, asset_id: AssetId) -> Result<Asset> {
        self.repos(client).releases().get_asset(asset_id).await.anyhow_err()
    }

    fn download_asset_job(&self, octocrab: &Octocrab, asset_id: AssetId) -> DownloadFile {
        let path = iformat!("/repos/{self.owner()}/{self.name()}/releases/assets/{asset_id}");
        // Unwrap will work, because we are appending relative URL constant.
        let url = octocrab.absolute_url(path).unwrap();
        DownloadFile {
            client: octocrab.client.clone(),
            key:    crate::cache::download::Key {
                url,
                additional_headers: HeaderMap::from_iter([(
                    reqwest::header::ACCEPT,
                    HeaderValue::from_static(mime::APPLICATION_OCTET_STREAM.as_ref()),
                )]),
            },
        }
    }

    #[tracing::instrument(name="Download the asset.", skip(client), fields(self=%self), err)]
    async fn download_asset(&self, client: &Octocrab, asset_id: AssetId) -> Result<Response> {
        self.download_asset_job(client, asset_id).send_request().await
    }

    #[tracing::instrument(name="Download the asset to a file.", skip(client, output_path), fields(self=%self, dest=%output_path.as_ref().display()), err)]
    async fn download_asset_as(
        &self,
        client: &Octocrab,
        asset_id: AssetId,
        output_path: impl AsRef<Path> + Send + Sync + 'static,
    ) -> Result {
        let response = self.download_asset(client, asset_id).await?;
        crate::io::web::stream_response_to_file(response, &output_path).await
    }

    #[tracing::instrument(name="Download the asset to a directory.",
        skip(client, output_dir, asset),
        fields(self=%self, dest=%output_dir.as_ref().display(), id = %asset.id),
        err)]
    async fn download_asset_to(
        &self,
        client: &Octocrab,
        asset: &Asset,
        output_dir: impl AsRef<Path> + Send + Sync + 'static,
    ) -> Result<PathBuf> {
        let output_path = output_dir.as_ref().join(&asset.name);
        self.download_asset_as(client, asset.id, output_path.clone()).await?;
        Ok(output_path)
    }
}
