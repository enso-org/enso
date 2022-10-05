use crate::prelude::*;

use crate::paths::TargetTriple;

use derivative::Derivative;
use ide_ci::models::config::RepoContext;
use ide_ci::programs::Git;
use octocrab::models::repos::Release;
use octocrab::models::ReleaseId;



/// The basic, common information available in this application.
#[derive(Clone, Derivative, derive_more::Deref)]
#[derivative(Debug)]
pub struct BuildContext {
    #[deref]
    pub inner: crate::project::Context,

    /// Version to be built.
    ///
    /// Note that this affects only targets that are being built. If project parts are provided by
    /// other means, their version might be different.
    pub triple: TargetTriple,

    /// Remote repository is used for release-related operations. This also includes deducing a new
    /// version number.
    pub remote_repo: RepoContext,
}

impl BuildContext {
    pub fn commit(&self) -> BoxFuture<'static, Result<String>> {
        let root = self.repo_root.to_path_buf();
        async move {
            match ide_ci::actions::env::GITHUB_SHA.get() {
                Ok(commit) => Ok(commit),
                Err(_e) => Git::new(root).await?.head_hash().await,
            }
        }
        .boxed()
    }

    #[tracing::instrument]
    pub fn resolve_release_designator(
        &self,
        designator: String,
    ) -> BoxFuture<'static, Result<Release>> {
        let repository = self.remote_repo.clone();
        let octocrab = self.octocrab.clone();
        let designator_cp = designator.clone();
        async move {
            let release = if let Ok(id) = designator.parse2::<ReleaseId>() {
                repository.find_release_by_id(&octocrab, id).await?
            } else {
                match designator.as_str() {
                    "latest" => repository.latest_release(&octocrab).await?,
                    "nightly" =>
                        crate::version::latest_nightly_release(&octocrab, &repository).await?,
                    tag => repository.find_release_by_text(&octocrab, tag).await?,
                }
            };
            Ok(release)
        }
        .map_err(move |e: anyhow::Error| {
            e.context(format!("Failed to resolve release designator `{designator_cp}`."))
        })
        .boxed()
    }
}
