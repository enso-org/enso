use crate::prelude::*;

use octocrab::models::repos::Asset;
use octocrab::models::repos::Release;



const MAX_PER_PAGE: u8 = 100;

pub mod model;
pub mod release;
pub mod repo;
pub mod workflow;

pub use repo::Repo;
pub use repo::RepoRef;


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

#[async_trait]
pub trait IsOrganization {
    /// Organization name.
    fn name(&self) -> &str;

    /// Generate a token that can be used to register a new runner for this repository.
    async fn generate_runner_registration_token(
        &self,
        octocrab: &Octocrab,
    ) -> anyhow::Result<model::RegistrationToken> {
        let path = iformat!("/orgs/{self.name()}/actions/runners/registration-token");
        let url = octocrab.absolute_url(path)?;
        octocrab.post(url, EMPTY_REQUEST_BODY).await.map_err(Into::into)
    }

    /// The organization's URL.
    fn url(&self) -> Result<Url> {
        let url_text = iformat!("https://github.com/{self.name()}");
        Url::parse(&url_text).map_err(Into::into)
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

    let platform_name = format!("{}-{}", os_name, arch_name);
    find_asset_url_by_text(&latest_release, &platform_name).cloned()
}

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
