use enso_build::repo::deduce_repository_path;
use enso_build_cli::prelude::*;
use ide_ci::actions::workflow::MessageLevel;
use ide_ci::log::setup_logging;
use ide_ci::programs::Git;

/// Name of the label that is used to mark the PRs that should not require changelog entry.
///
/// It should be defined in the `.github/settings.yml` file of the repository.
const NO_CHECK_LABEL: &str = "CI: No changelog needed";

/// Name of the remote source in the working copy.
const REMOTE_NAME: &str = "origin";

/// Check if a given label is the one that indicates that the changelog check should be skipped.
pub fn is_skip_changelog_label(label: &octocrab::models::Label) -> bool {
    label.name == NO_CHECK_LABEL
}

/// Check if the given PR has the changelog check skipped.
pub fn has_check_disabled(pull_request: &octocrab::models::pulls::PullRequest) -> bool {
    if pull_request.labels.iter().flatten().any(is_skip_changelog_label) {
        info!("Skipping changelog check because the PR has the label {}.", NO_CHECK_LABEL);
        false
    } else {
        true
    }
}

/// Check if we are in context where the changelog check should be performed.
pub fn is_check_needed(context: &ide_ci::actions::context::Context) -> bool {
    if let Some(pr) = context.payload.pull_request.as_ref() {
        info!("Checking if changelog is up to date for PR #{}.", pr.number);
        has_check_disabled(pr)
    } else {
        info!("Not a pull request, skipping the check.");
        false
    }
}

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let repo_path = deduce_repository_path()?;
    let context = ide_ci::actions::context::Context::from_env()?;
    if !is_check_needed(&context) {
        return Ok(());
    };

    trace!("The context is {context:#?}.");
    // TODO: we should check the absolute path rather than just filename.
    let changelog = enso_build::paths::generated::RepoRootChangelogMd::new(&repo_path).path;
    let repository = context.payload.repository.context("Missing repository information.")?;
    let default_branch =
        repository.default_branch.context("Missing default branch information.")?;
    let git = Git::new(&repo_path).await?;
    let remote_base = format!("{REMOTE_NAME}/{default_branch}");
    let files_changed = git.diff_against(remote_base).await?;
    let changelog_was_changed = files_changed.iter().contains(&changelog);
    if !changelog_was_changed {
        let message = format!(
            "{} was not updated. Either update it or add the label {} to the PR.",
            enso_build::paths::generated::RepoRootChangelogMd::segment_name(),
            NO_CHECK_LABEL
        );
        ide_ci::actions::workflow::message(MessageLevel::Error, &message);
        bail!(message);
    }
    Ok(())
}
