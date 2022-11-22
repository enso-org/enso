use crate::prelude::*;

use crate::ci::labels::NO_CHANGELOG_CHECK;
use crate::paths::generated::RepoRoot;

use ide_ci::actions::workflow::MessageLevel;
use ide_ci::programs::git;



/// Name of the remote source in the working copy.
const REMOTE_NAME: &str = "origin";

/// Check if a given label is the one that indicates that the changelog check should be skipped.
pub fn is_skip_changelog_label(label: &octocrab::models::Label) -> bool {
    label.name == NO_CHANGELOG_CHECK
}

/// Check if the given PR has the changelog check skipped.
pub fn has_check_disabled(pull_request: &octocrab::models::pulls::PullRequest) -> bool {
    if pull_request.labels.iter().flatten().any(is_skip_changelog_label) {
        info!("Skipping changelog check because the PR has the label {}.", NO_CHANGELOG_CHECK);
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

#[instrument("Checking if the changelog has been updated.", fields(%repo_path), skip(context), err)]
pub async fn check(repo_path: RepoRoot, context: ide_ci::actions::Context) -> Result {
    trace!("The context is {context:#?}.");
    if !is_check_needed(&context) {
        debug!("No changelog check necessary.");
        return Ok(());
    };

    let changelog = crate::paths::generated::RepoRootChangelogMd::new(&repo_path).path;
    let repository = context.payload.repository.context("Missing repository information.")?;
    let default_branch =
        repository.default_branch.context("Missing default branch information.")?;
    let git = git::Context::new(&repo_path).await?;
    git.fetch_branch(REMOTE_NAME, &default_branch).await?;
    let remote_base = format!("{REMOTE_NAME}/{default_branch}");
    let files_changed = git.diff_against(remote_base).await?;
    debug!("Files changed: {files_changed:#?}.");
    let changelog_was_changed = files_changed.iter().contains(&changelog);
    if !changelog_was_changed {
        let message = format!(
            "{} was not updated. Either update it or add the '{}' label to the PR.",
            crate::paths::generated::RepoRootChangelogMd::segment_name(),
            NO_CHANGELOG_CHECK
        );
        ide_ci::actions::workflow::message(MessageLevel::Error, &message);
        bail!(message);
    }
    Ok(())
}
