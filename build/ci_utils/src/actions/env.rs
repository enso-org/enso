//! See: https://docs.github.com/en/actions/learn-github-actions/environment-variables

use crate::prelude::*;

use crate::define_env_var;
use crate::env::new::RawVariable;
use crate::env::new::TypedVariable;
use crate::github::Repo;



define_env_var! {
    /// Always set to true when being run under GitHub Actions runner. Also, this is often set on
    /// other CI systems.
    CI, bool;

    /// The name of the action currently running, or the id of a step. For example, for an action,
    /// `__repo-owner_name-of-action-repo`.
    GITHUB_ACTION, String;

    /// The path where an action is located. This property is only supported in composite actions.
    /// You can use this path to access files located in the same repository as the action.
    /// For example, `/home/runner/work/_actions/repo-owner/name-of-action-repo/v1`.
    GITHUB_ACTION_PATH, PathBuf;

    /// For a step executing an action, this is the owner and repository name of the action.
    /// For example, `actions/checkout`.
    GITHUB_ACTION_REPOSITORY, Repo;

    /// Always set to true when GitHub Actions is running the workflow.  You can use this variable
    /// to differentiate when tests are being run locally or by GitHub Actions.
    GITHUB_ACTIONS, bool;

    /// The name of the person or app that initiated the workflow. For example, `octocat`.
    GITHUB_ACTOR, String;

    /// Returns the API URL. For example: https://api.github.com.
    GITHUB_API_URL, Url;

    /// The name of the base ref or target branch of the pull request in a workflow run. This is
    /// only set when the event that triggers a workflow run is either `pull_request` or
    /// `pull_request_target`. For example, `main`.
    GITHUB_BASE_REF, String;

    /// The path on the runner to the file that sets environment variables from workflow commands.
    /// This file is unique to the current step and changes for each step in a job. For example,
    /// `/home/runner/work/_temp/_runner_file_commands/set_env_87406d6e-4979-4d42-98e1-3dab1f48b13a`.
    GITHUB_ENV, PathBuf;

    /// Path to the environment file with step's output parameters. This file is unique to the
    /// current step and changes for each step in a job.
    GITHUB_OUTPUT, PathBuf;

    /// The name of the event that triggered the workflow. For example, `workflow_dispatch`.
    GITHUB_EVENT_NAME, String;

    /// The path to the file on the runner that contains the full event webhook payload.
    /// For example, `/github/workflow/event.json`.
    GITHUB_EVENT_PATH, PathBuf;

    /// Returns the GraphQL API URL. For example: https://api.github.com/graphql.
    GITHUB_GRAPHQL_URL, Url;

    /// The head ref or source branch of the pull request in a workflow run. This property is only
    /// set when the event that triggers a workflow run is either `pull_request` or
    /// `pull_request_target`. For example, `feature-branch-1`.
    GITHUB_HEAD_REF, String;

    /// The job_id of the current job. For example, greeting_job.
    GITHUB_JOB, String;

    /// The path on the runner to the file that sets system PATH variables from workflow commands.
    /// This file is unique to the current step and changes for each step in a job. For example,
    /// /home/runner/work/_temp/_runner_file_commands/add_path_899b9445-ad4a-400c-aa89-249f18632cf5.
    GITHUB_PATH, PathBuf;

    /// The fully-formed ref of the branch or tag that triggered the workflow run. For workflows
    /// triggered by push, this is the branch or tag ref that was pushed. For workflows triggered
    /// by pull_request, this is the pull request merge branch. For workflows triggered by release,
    /// this is the release tag created. For other triggers, this is the branch or tag ref that
    /// triggered the workflow run. This is only set if a branch or tag is available for the event
    /// type. The ref given is fully-formed, meaning that for branches the format is
    /// `refs/heads/<branch_name>`, for pull requests it is `refs/pull/<pr_number>/merge`, and for
    /// tags it is `refs/tags/<tag_name>`. For example, `refs/heads/feature-branch-1`.
    GITHUB_REF, String;

    /// The short ref name of the branch or tag that triggered the workflow run. This value matches
    ///  the branch or tag name shown on GitHub. For example, `feature-branch-1`.
    GITHUB_REF_NAME, String;

    /// true if branch protections are configured for the ref that triggered the workflow run.
    GITHUB_REF_PROTECTED, bool;

    /// The type of ref that triggered the workflow run. Valid values are `branch` or `tag`.
    GITHUB_REF_TYPE, String;

    /// The owner and repository name. For example, octocat/Hello-World.
    GITHUB_REPOSITORY, Repo;

    /// The repository owner's name. For example, octocat.
    GITHUB_REPOSITORY_OWNER, String;

    /// The number of days that workflow run logs and artifacts are kept. For example, 90.
    GITHUB_RETENTION_DAYS, usize;

    /// A unique number for each attempt of a particular workflow run in a repository. This number
    /// begins at 1 for the workflow run's first attempt, and increments with each re-run. For
    /// example, 3.
    GITHUB_RUN_ATTEMPT, usize;

    /// A unique number for each workflow run within a repository. This number does not change if
    /// you re-run the workflow run. For example, 1658821493.
    GITHUB_RUN_ID, octocrab::models::RunId;

    /// A unique number for each run of a particular workflow in a repository. This number begins
    /// at 1 for the workflow's first run, and increments with each new run. This number does not
    /// change if you re-run the workflow run. For example, 3.
    GITHUB_RUN_NUMBER, usize;

    /// The URL of the GitHub server. For example: https://github.com.
    GITHUB_SERVER_URL, Url;

    /// The commit SHA that triggered the workflow. The value of this commit SHA depends on the
    /// event that triggered the workflow. For more information, see "Events that trigger
    /// workflows." For example, `ffac537e6cbbf934b08745a378932722df287a53`.
    GITHUB_SHA, String;

    /// The path on the runner to the file that contains job summaries from workflow commands.
    /// This file is unique to the current step and changes for each step in a job. For example,
    /// `/home/rob/runner/_layout/_work/_temp/_runner_file_commands/step_summary_1cb22d7f-5663-41a8-9ffc-13472605c76c`.
    /// For more information, see "Workflow commands for GitHub Actions."
    GITHUB_STEP_SUMMARY, PathBuf;

    /// The name of the workflow. For example, `My test workflow`. If the workflow file doesn't
    /// specify a name, the value of this variable is the full path of the workflow file in the
    /// repository.
    GITHUB_WORKFLOW, String;

    /// The default working directory on the runner for steps, and the default location of your
    /// repository when using the checkout action. For example,
    /// `/home/runner/work/my-repo-name/my-repo-name`.
    GITHUB_WORKSPACE, PathBuf;

    /// The architecture of the runner executing the job.
    /// Possible values are `X86`, `X64`, `ARM`, or `ARM64`.
    RUNNER_ARCH, String;

    /// This is set only if debug logging is enabled, and always has the value of 1. It can be
    /// useful as an indicator to enable additional debugging or verbose logging in your own job
    /// steps.
    RUNNER_DEBUG, usize;

    /// The name of the runner executing the job. For example, `Hosted Agent`
    RUNNER_NAME, String;

    /// The operating system of the runner executing the job. Possible values are `Linux`,
    /// `Windows`, or `macOS`. For example, `Windows`.
    RUNNER_OS, String;

    /// The path to a temporary directory on the runner. This directory is emptied at the beginning
    /// and end of each job. Note that files will not be removed if the runner's user account does
    /// not have permission to delete them. For example, `D:\a\_temp`
    RUNNER_TEMP, PathBuf;

    /// The path to the directory containing preinstalled tools for GitHub-hosted runners.
    /// For example, `C:\hostedtoolcache\windows`
    RUNNER_TOOL_CACHE, PathBuf;
}

/// Fails when called outside of GitHub Actions environment,
pub fn is_self_hosted() -> Result<bool> {
    let name = RUNNER_NAME.get_raw()?;
    Ok(!name.starts_with("GitHub Actions"))
}

pub async fn set_and_emit<V>(var: &V, value: &V::Borrowed) -> Result
where V: TypedVariable {
    let value_raw = var.generate(value)?;
    crate::actions::workflow::set_env(var.name(), &value_raw).await
}
