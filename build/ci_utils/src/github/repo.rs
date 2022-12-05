use crate::prelude::*;

use crate::cache::download::DownloadFile;
use crate::github;
use crate::github::model;
use crate::github::MAX_PER_PAGE;

use headers::HeaderMap;
use headers::HeaderValue;
use octocrab::models::repos::Asset;
use octocrab::models::repos::Ref;
use octocrab::models::repos::Release;
use octocrab::models::workflows::WorkflowListArtifact;
use octocrab::models::ArtifactId;
use octocrab::models::AssetId;
use octocrab::models::ReleaseId;
use octocrab::models::RunId;
use octocrab::params::actions::ArchiveFormat;
use octocrab::params::repos::Reference;
use reqwest::Response;



/// Owned data denoting a specific GitHub repository.
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
        RepoRef::try_from(s).map(Into::into)
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


/// Non-owning equivalent of `Repo`.
///
/// Particularly useful for defining `const` repositories.
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
}

/// Note that we chose to implemend `TryFrom` rather than `FromStr` for `RepoRef` because
/// `FromStr` requires the parsed value to be owned (or at least lifetime-independent from input),
/// which is not the case for `RepoRef`.
impl<'a> TryFrom<&'a str> for RepoRef<'a> {
    type Error = anyhow::Error;

    fn try_from(value: &'a str) -> std::result::Result<Self, Self::Error> {
        match value.split('/').collect_vec().as_slice() {
            [owner, name] => Ok(Self { owner, name }),
            slice => bail!("Failed to parse string '{}': Splitting by '/' should yield exactly 2 pieces, found: {}", value, slice.len()),
        }
    }
}

/// Any entity that uniquely identifies a GitHub-hosted repository.
#[async_trait]
pub trait IsRepo: Display {
    fn owner(&self) -> &str;
    fn name(&self) -> &str;

    /// The repository's URL.
    fn url(&self) -> Result<Url> {
        let url_text = iformat!("https://github.com/{self.owner()}/{self.name()}");
        Url::parse(&url_text)
            .context(format!("Failed to generate an URL for the {self} repository."))
    }

    fn handle(&self, octocrab: &Octocrab) -> Handle<Self>
    where Self: Clone + Sized {
        Handle { repo: self.clone(), octocrab: octocrab.clone() }
    }
}

/// A handle to a specific GitHub repository.
///
/// It includes a client (so also an authentication token) and a repository.
#[derive(Debug, Clone)]
pub struct Handle<Repo> {
    pub octocrab: Octocrab,
    pub repo:     Repo,
}

impl<R: Display> Display for Handle<R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.repo)
    }
}

impl<R: IsRepo> IsRepo for Handle<R> {
    fn owner(&self) -> &str {
        self.repo.owner()
    }

    fn name(&self) -> &str {
        self.repo.name()
    }
}

impl<R: IsRepo> Handle<R> {
    /// Create a new handle.
    pub fn new(octocrab: &Octocrab, repo: R) -> Self {
        Self { octocrab: octocrab.clone(), repo }
    }

    /// Generate a token that can be used to register a new runner for this repository.
    pub async fn generate_runner_registration_token(&self) -> Result<model::RegistrationToken> {
        let path =
            iformat!("/repos/{self.owner()}/{self.name()}/actions/runners/registration-token");
        let url = self.octocrab.absolute_url(path)?;
        self.octocrab.post(url, EMPTY_REQUEST_BODY).await.context(format!(
            "Failed to generate a runner registration token for the {self} repository."
        ))
    }

    pub fn repos(&self) -> octocrab::repos::RepoHandler {
        self.octocrab.repos(self.owner(), self.name())
    }

    pub async fn all_releases(&self) -> Result<Vec<Release>> {
        github::get_all(
            &self.octocrab,
            self.repos().releases().list().per_page(MAX_PER_PAGE).send(),
        )
        .await
        .context(format!("Failed to list all releases in the {self} repository."))
    }

    pub async fn latest_release(&self) -> Result<Release> {
        self.repos()
            .releases()
            .get_latest()
            .await
            .context(format!("Failed to get the latest release in the {self} repository."))
    }

    pub async fn find_release_by_id(&self, release_id: ReleaseId) -> Result<Release> {
        let repo_handler = self.repos();
        let releases_handler = repo_handler.releases();
        releases_handler
            .get_by_id(release_id)
            .await
            .context(format!("Failed to find release by id `{release_id}` in `{self}`."))
    }

    #[tracing::instrument(fields(%self), skip(predicate), err)]
    pub async fn find_release_if(&self, predicate: impl Fn(&Release) -> bool) -> Result<Release> {
        let releases = self.all_releases().await?;
        let release = releases.into_iter().find(predicate);
        release.context("Failed to find a release that satisfies the predicate.")
    }

    #[tracing::instrument(fields(%self, %text), err)]
    pub async fn find_release_by_text(&self, text: &str) -> anyhow::Result<Release> {
        self.find_release_if(|release| release.tag_name.contains(text))
            .await
            .inspect(|release| info!("Found release at: {} (id={}).", release.html_url, release.id))
            .with_context(|| format!("No release with tag matching `{text}` in {self}."))
    }

    #[tracing::instrument(fields(%self, %text), err)]
    pub async fn find_release_by_tag(&self, text: &str) -> anyhow::Result<Release> {
        self.find_release_if(|release| release.tag_name == text)
            .await
            .inspect(|release| info!("Found release at: {} (id={}).", release.html_url, release.id))
            .with_context(|| format!("No release with tag equal to `{text}` in {self}."))
    }

    pub async fn get_ref(&self, r#ref: &Reference) -> Result<Ref> {
        self.repos().get_ref(r#ref).await.context(format!("Failed to get ref `{ref}` in {self}."))
    }

    #[tracing::instrument(fields(%self, %run_id, %name), err, ret)]
    pub async fn find_artifact_by_name(
        &self,
        run_id: RunId,
        name: &str,
    ) -> Result<WorkflowListArtifact> {
        let artifacts = self
            .octocrab
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

    pub async fn download_artifact(&self, artifact_id: ArtifactId) -> Result<Bytes> {
        self.octocrab
            .actions()
            .download_artifact(self.owner(), self.name(), artifact_id, ArchiveFormat::Zip)
            .await
            .context(format!("Failed to download artifact with ID={artifact_id}."))
    }

    pub async fn download_and_unpack_artifact(
        &self,
        artifact_id: ArtifactId,
        output_dir: &Path,
    ) -> Result {
        let bytes = self.download_artifact(artifact_id).await?;
        crate::archive::zip::extract_bytes(bytes, output_dir)?;
        Ok(())
    }

    #[tracing::instrument(name="Get the asset information.", fields(self=%self), err)]
    pub async fn asset(&self, asset_id: AssetId) -> Result<Asset> {
        self.repos().releases().get_asset(asset_id).await.anyhow_err()
    }

    pub fn download_asset_job(&self, asset_id: AssetId) -> DownloadFile {
        let path = iformat!("/repos/{self.owner()}/{self.name()}/releases/assets/{asset_id}");
        // Unwrap will work, because we are appending relative URL constant.
        let url = self.octocrab.absolute_url(path).unwrap();
        DownloadFile {
            client: self.octocrab.client.clone(),
            key:    crate::cache::download::Key {
                url,
                additional_headers: HeaderMap::from_iter([(
                    reqwest::header::ACCEPT,
                    HeaderValue::from_static(mime::APPLICATION_OCTET_STREAM.as_ref()),
                )]),
            },
        }
    }

    #[tracing::instrument(name="Download the asset.", fields(self=%self), err)]
    pub async fn download_asset(&self, asset_id: AssetId) -> Result<Response> {
        self.download_asset_job(asset_id).send_request().await
    }

    #[tracing::instrument(name="Download the asset to a file.", skip(output_path), fields(self=%self, dest=%output_path.as_ref().display()), err)]
    pub async fn download_asset_as(
        &self,
        asset_id: AssetId,
        output_path: impl AsRef<Path> + Send + Sync + 'static,
    ) -> Result {
        let response = self.download_asset(asset_id).await?;
        crate::io::web::stream_response_to_file(response, &output_path).await
    }

    #[tracing::instrument(name="Download the asset to a directory.",
    skip(output_dir, asset),
    fields(self=%self, dest=%output_dir.as_ref().display(), id = %asset.id),
    err)]
    pub async fn download_asset_to(
        &self,
        asset: &Asset,
        output_dir: impl AsRef<Path> + Send + Sync + 'static,
    ) -> Result<PathBuf> {
        let output_path = output_dir.as_ref().join(&asset.name);
        self.download_asset_as(asset.id, output_path.clone()).await?;
        Ok(output_path)
    }

    /// Get the repository information.
    pub async fn get(&self) -> Result<octocrab::models::Repository> {
        self.repos()
            .get()
            .await
            .with_context(|| format!("Failed to get the infomation for the {self} repository."))
    }

    /// Get the name of the default branch.
    pub async fn default_branch(&self) -> Result<String> {
        self.get().await?.default_branch.with_context(|| {
            format!(
                "Failed to get the default branch of the {} repository. Missing field: `default_branch`.",
                self
            )
        })
    }

    /// Dispatches the workflow using the head of the default branch.
    pub async fn dispatch_workflow(
        &self,
        workflow_id: impl AsRef<str> + Send + Sync + 'static,
        inputs: &impl Serialize,
    ) -> Result {
        let default_branch = self.default_branch().await?;
        crate::github::workflow::dispatch(self, workflow_id, default_branch, inputs).await
    }
}
