use crate::prelude::*;

use crate::define_env_var;

use octocrab::models::repos::Asset;
use octocrab::models::repos::Release;


// ==============
// === Export ===
// ==============

pub mod model;
pub mod release;
pub mod repo;
pub mod workflow;

pub use repo::Repo;
pub use repo::RepoRef;



/// Maximum number of items per page in the GitHub API.
const MAX_PER_PAGE: u8 = 100;

define_env_var! {
    /// GitHub Personal Access Token, used for authentication in GutHub API.
    ///
    /// Can be [created using GitHub web UI](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
    GITHUB_TOKEN, String;
}

/// Tries to retrieve the GitHub Personal Access Token from the environment.
pub fn retrieve_github_access_token() -> Result<String> {
    fn get_token_from_file() -> Result<String> {
        let path =
            dirs::home_dir().context("Failed to locate home directory.")?.join("GITHUB_TOKEN");
        debug!("Looking for GitHub token in the file {}", path.display());
        let content = crate::fs::read_to_string(path)?;
        Ok(content.trim().into())
    }

    GITHUB_TOKEN
        .get()
        .inspect(|_| debug!("Will use {GITHUB_TOKEN} environment variable."))
        .inspect_err(|e| debug!("Failed to retrieve GitHub authentication from environment: {e}"))
        .or_else(|_| get_token_from_file())
}


/// Prepare the octocrab (GitHub API client) using the authentication token from the environment.
#[context("Failed to setup GitHub API client.")]
pub async fn setup_octocrab() -> Result<Octocrab> {
    let builder = octocrab::OctocrabBuilder::new();
    let octocrab = if let Ok(access_token) = retrieve_github_access_token() {
        let octocrab = builder.personal_token(access_token).build()?;
        let username = octocrab
            .current()
            .user()
            .await
            .inspect_err(|e| warn!("Failed to retrieve GitHub username: {e}"))
            .map_or_else(|_| "N/A".to_string(), |user| user.login);
        info!("Using GitHub API with personal access token. Authenticated as {username}.",);
        octocrab
    } else {
        info!("No GitHub Personal Access Token found. Will use anonymous API access.");
        warn!(
            "Anonymous GitHub API access is rate-limited. If you are experiencing issues, please \
        set the GITHUB_TOKEN environment variable."
        );
        warn!(
            "Additionally some APIs may not be available to anonymous users. This primarily \
        pertains the release-related APIs."
        );
        builder.build()?
    };

    // LPrint rate limit. This both helps debugging related issues and allows to validate the
    // GitHub access token.
    octocrab
        .ratelimit()
        .get()
        .await
        .inspect(|rate| {
            info!(
                "GitHub API rate limit: {}/{}.",
                rate.resources.core.used, rate.resources.core.limit
            )
        })
        .context("Failed to get rate limit info. GitHub Personal Access Token might be invalid")?;
    Ok(octocrab)
}

/// Goes over all the pages and returns result.
///
/// We prefer taking a future page result rather than page itself to be able to easily wrap both
/// actions with a single Result context.
// TODO [mwu]: Yielding a Stream that fetches pages as-needed would be better.
pub async fn get_all<T: DeserializeOwned>(
    client: &Octocrab,
    f: impl Future<Output = octocrab::Result<octocrab::Page<T>>>,
) -> octocrab::Result<Vec<T>> {
    let first_page = f.await?;
    client.all_pages(first_page).await
}

/// Utility functions for dealing with organization-specific GitHub API.
#[async_trait]
pub trait IsOrganization {
    /// Organization name.
    fn name(&self) -> &str;

    /// Generate a token that can be used to register a new runner for this repository.
    async fn generate_runner_registration_token(
        &self,
        octocrab: &Octocrab,
    ) -> anyhow::Result<model::RegistrationToken> {
        let name = self.name();
        let path = format!("/orgs/{name}/actions/runners/registration-token");
        let url = octocrab.absolute_url(path)?;
        octocrab.post(url, EMPTY_REQUEST_BODY).await.with_context(|| {
            format!("Failed to generate runner registration token for organization {name}.")
        })
    }

    /// The organization's URL.
    fn url(&self) -> Result<Url> {
        let url_text = format!("https://github.com/{}", self.name());
        Url::from_str(&url_text)
    }
}

/// Get the biggest asset containing given text.
#[instrument(skip(release), fields(id = %release.id, url = %release.url), err)]
pub fn find_asset_by_text<'a>(release: &'a Release, text: &str) -> Result<&'a Asset> {
    release
        .assets
        .iter()
        .filter(|asset| asset.name.contains(text))
        .max_by_key(|asset| asset.size)
        .ok_or_else(|| {
            anyhow!("Cannot find release asset by string {} in the release {}.", text, release.url)
        })
        .inspect(|asset| trace!("Found asset: {:#?}", asset))
}

/// Get the biggest asset containing given text.
#[instrument(skip(release), fields(id = %release.id, url = %release.url), ret(Display), err)]
pub fn find_asset_url_by_text<'a>(release: &'a Release, text: &str) -> Result<&'a Url> {
    let matching_asset = find_asset_by_text(release, text)?;
    Ok(&matching_asset.browser_download_url)
}

/// Obtain URL to an archive with the latest runner package for a given system.
///
/// Octocrab client does not need to bo authorized with a PAT for this. However, being authorized
/// will help with GitHub API query rate limits.
pub async fn latest_runner_url(octocrab: &Octocrab, os: OS) -> Result<Url> {
    let latest_release = octocrab.repos("actions", "runner").releases().get_latest().await?;

    let os_name = match os {
        OS::Linux => "linux",
        OS::Windows => "win",
        OS::MacOS => "osx",
        other_os => unimplemented!("System `{}` is not yet supported!", other_os),
    };

    let arch_name = match TARGET_ARCH {
        Arch::X86_64 => "x64",
        Arch::Arm => "arm",
        Arch::AArch64 => "arm64",
        other_arch => unimplemented!("Architecture `{}` is not yet supported!", other_arch),
    };

    let platform_name = format!("{os_name}-{arch_name}");
    find_asset_url_by_text(&latest_release, &platform_name).cloned()
}

/// Download and extract latest GitHub Actions runner package for a given system.
pub async fn fetch_runner(octocrab: &Octocrab, os: OS, output_dir: impl AsRef<Path>) -> Result {
    let url = latest_runner_url(octocrab, os).await?;
    crate::io::download_and_extract(url, output_dir).await
}

/// Sometimes octocrab is just not enough.
///
/// Client has set the authorization header.
pub fn create_client(pat: impl AsRef<str>) -> Result<reqwest::Client> {
    let mut header_map = reqwest::header::HeaderMap::new();
    header_map.append(reqwest::header::AUTHORIZATION, format!("Bearer {}", pat.as_ref()).parse()?);
    reqwest::Client::builder()
        .user_agent("enso-build")
        .default_headers(header_map)
        .build()
        .anyhow_err()
}
