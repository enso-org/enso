use crate::prelude::*;

use crate::paths::TargetTriple;

use derivative::Derivative;
use ide_ci::github;
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
    pub remote_repo: ide_ci::github::Repo,
}

impl BuildContext {
    /// Get the current commit hash.
    ///
    /// If there is GITHUB_SHA environment variable, it is used. Otherwise, the current commit hash
    /// is determined using `git` command.
    pub fn commit(&self) -> BoxFuture<'static, Result<String>> {
        let git = self.git();
        async move {
            match ide_ci::actions::env::GITHUB_SHA.get() {
                Ok(commit) => Ok(commit),
                Err(_e) => git.await?.head_hash().await,
            }
        }
        .boxed()
    }

    #[tracing::instrument]
    pub fn resolve_release_designator(
        &self,
        designator: String,
    ) -> BoxFuture<'static, Result<Release>> {
        let repository = self.remote_repo_handle();
        let designator_cp = designator.clone();
        async move {
            let release = if let Ok(id) = designator.parse2::<ReleaseId>() {
                repository.find_release_by_id(id).await?
            } else {
                match designator.as_str() {
                    "latest" => repository.latest_release().await?,
                    "nightly" => crate::version::latest_nightly_release(&repository).await?,
                    tag => repository.find_release_by_text(tag).await?,
                }
            };
            Result::Ok(release)
        }
        .with_context(move || format!("Failed to resolve release designator `{designator_cp}`"))
        .boxed()
    }

    pub fn remote_repo_handle(&self) -> github::repo::Handle<github::Repo> {
        github::repo::Handle::new(&self.octocrab, self.remote_repo.clone())
    }
}
