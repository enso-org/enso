//! Downloading Google Fonts.

use crate::prelude::*;

use ide_ci::cache::Cache;
use ide_ci::cache::Storable;
use ide_ci::github;
use ide_ci::github::RepoRef;
use octocrab::models::repos;



// =================
// === Constants ===
// =================

/// Google Fonts repository.
pub const GOOGLE_FONTS_REPOSITORY: RepoRef = RepoRef { owner: "google", name: "fonts" };

/// Path to the directory on the Google Fonts repository where we get the fonts from.
///
/// The directory name denotes the license of the fonts. In our case this is SIL OPEN FONT LICENSE
/// Version 1.1, commonly known as OFL.
pub const GOOGLE_FONT_DIRECTORY: &str = "ofl";

/// We keep dependency to a fixed commit, so we can safely cache it.
///
/// There are no known reasons not to bump this.
pub const GOOGLE_FONT_SHA1: &str = "ea893a43af7c5ab5ccee189fc2720788d99887ed";


// ==============
// === Family ===
// ==============

/// Identifies uniquely a source of font family download.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Family {
    /// Remote repository with fonts.
    pub repo:  github::Repo,
    /// Which commit we want to be downloaded.
    pub r#ref: String,
    /// Font family. It corresponds to the subdirectories names (under the top-level
    /// license-denoting directories).
    pub name:  String,
}

impl Family {
    /// List content items in the repository that contain TTF files for the given font family.
    pub async fn list_ttf(
        &self,
        handle: github::repo::Handle<impl IsRepo>,
    ) -> Result<Vec<repos::Content>> {
        let path = format!("{GOOGLE_FONT_DIRECTORY}/{}", self.name);
        let files = handle.repos().get_content().r#ref(&self.r#ref).path(path).send().await?;
        Ok(files.items.into_iter().filter(|file| file.name.ends_with(".ttf")).collect())
    }
}


// ====================
// === DownloadFont ===
// ====================

/// Description of the job to download the fonts.
#[derive(Derivative, Clone)]
#[derivative(Debug)]
pub struct DownloadFont {
    pub family:   Family,
    /// Possible authentication to GitHub (to get bigger rate limit).
    #[derivative(Debug = "ignore")]
    pub octocrab: Octocrab,
}

impl DownloadFont {
    /// Get a handle to the remote repository with the fonts.
    pub fn handle(&self) -> github::repo::Handle<impl IsRepo> {
        self.family.repo.handle(&self.octocrab)
    }

    /// Download the font family to the given directory. They will be placed in the output
    /// directory. The function returns relative paths to the downloaded files.
    pub async fn download(&self, output_path: impl AsRef<Path>) -> Result<Vec<PathBuf>> {
        let files = self.family.list_ttf(self.handle()).await?;
        let mut ret = Vec::new();
        for file in &files {
            let destination_file = output_path.as_ref().join(&file.name);
            let url = file.download_url.as_ref().context("Missing 'download_url' in the reply.")?;
            let reply = ide_ci::io::web::client::download(&self.octocrab.client, url).await?;
            ide_ci::io::web::stream_to_file(reply, &destination_file).await?;
            ret.push(file.name.as_str().into());
        }
        Ok(ret)
    }
}

impl Storable for DownloadFont {
    /// In metadata form we just store paths relative to the store.
    type Metadata = Vec<PathBuf>;
    /// Here paths are absolute.
    type Output = Vec<PathBuf>;
    type Key = Family;

    fn generate(
        &self,
        _cache: Cache,
        store: PathBuf,
    ) -> BoxFuture<'static, Result<Self::Metadata>> {
        let this = self.clone();
        async move {
            let fonts = this.download(&store).await?;
            Ok(fonts)
        }
        .boxed()
    }

    fn adapt(
        &self,
        cache: PathBuf,
        mut metadata: Self::Metadata,
    ) -> BoxFuture<'static, Result<Self::Output>> {
        async move {
            for font in &mut metadata {
                *font = cache.join(&font);
            }
            Ok(metadata)
        }
        .boxed()
    }

    fn key(&self) -> Self::Key {
        self.family.clone()
    }
}

// ===================
// === Entry Point ===
// ===================

pub async fn download_google_font(
    cache: &Cache,
    octocrab: &Octocrab,
    family: &str,
    output_path: impl AsRef<Path>,
) -> Result<Vec<PathBuf>> {
    let family = Family {
        repo:  GOOGLE_FONTS_REPOSITORY.into(),
        r#ref: GOOGLE_FONT_SHA1.into(),
        name:  family.into(),
    };
    let font = DownloadFont { family, octocrab: octocrab.clone() };
    let cached_fonts = cache.get(font).await?;
    let copy_futures =
        cached_fonts.into_iter().map(|font| ide_ci::fs::tokio::copy_to(font, &output_path));
    let result = futures::future::join_all(copy_futures).await.into_iter().try_collect()?;
    Ok(result)
}

// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore]
    async fn new_download() -> Result {
        setup_logging()?;
        let path = r"C:\temp\google_fonts2";
        let octocrab = ide_ci::github::setup_octocrab().await?;
        let cache = Cache::new_default().await?;
        let aaa = download_google_font(&cache, &octocrab, "mplus1", path).await?;
        dbg!(aaa);
        Ok(())
    }
}
