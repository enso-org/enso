use crate::prelude::*;

use crate::ci_gen::runs_on;
use crate::ci_gen::secret;
use crate::ci_gen::step;
use ide_ci::actions::workflow::definition::cancel_workflow_action;
use ide_ci::actions::workflow::definition::checkout_repo_step;
use ide_ci::actions::workflow::definition::Job;
use ide_ci::actions::workflow::definition::JobArchetype;
use ide_ci::actions::workflow::definition::RunnerLabel;
use ide_ci::actions::workflow::definition::Step;
use ide_ci::actions::workflow::definition::Strategy;
use std::convert::identity;



pub trait RunsOn {
    fn strategy(&self) -> Option<Strategy> {
        None
    }
    fn runs_on(&self) -> Vec<RunnerLabel>;
    fn os_name(&self) -> Option<String> {
        None
    }
}

impl RunsOn for OS {
    fn runs_on(&self) -> Vec<RunnerLabel> {
        runs_on(*self)
    }
    fn os_name(&self) -> Option<String> {
        Some(self.to_string())
    }
}

impl RunsOn for Strategy {
    fn strategy(&self) -> Option<Strategy> {
        Some(self.clone())
    }

    fn runs_on(&self) -> Vec<RunnerLabel> {
        vec![RunnerLabel::MatrixOs]
    }
}

pub fn plain_job(
    runs_on_info: &impl RunsOn,
    name: impl AsRef<str>,
    command_line: impl AsRef<str>,
) -> Job {
    plain_job_customized(runs_on_info, name, command_line, identity)
}

pub fn plain_job_customized(
    runs_on_info: &impl RunsOn,
    name: impl AsRef<str>,
    command_line: impl AsRef<str>,
    f: impl FnOnce(Step) -> Step,
) -> Job {
    let name = if let Some(os_name) = runs_on_info.os_name() {
        format!("{} ({})", name.as_ref(), os_name)
    } else {
        name.as_ref().to_string()
    };
    let steps = crate::ci_gen::setup_customized_script_steps(command_line, f);
    let runs_on = runs_on_info.runs_on();
    let strategy = runs_on_info.strategy();
    Job { name, runs_on, steps, strategy, ..default() }
}

#[derive(Clone, Copy, Debug)]
pub struct AssertChangelog;
impl JobArchetype for AssertChangelog {
    fn job(os: OS) -> Job {
        let changed_files = r#"
git fetch
list=`git diff --name-only origin/develop HEAD | tr '\n' ' '`
echo $list
echo "::set-output name=list::'$list'"
"#
        .trim()
        .to_string();

        let changed_files_id = "changed_files";
        let changelog_was_changed =
            format!("contains(steps.{changed_files_id}.outputs.list,'CHANGELOG.md')");
        let omit_in_commit_msg =
            "contains(github.event.head_commit.message,'[ci no changelog needed]')";
        let omit_in_pr_body = "contains(github.event.pull_request.body,'[ci no changelog needed]')";
        let is_dependabot = "github.event.pull_request.user.login == 'dependabot'";

        let steps = {
            let mut steps = vec![];
            steps.extend(checkout_repo_step());
            steps.push(Step {
                id: Some(changed_files_id.into()),
                run: Some(changed_files),
                ..default()
            });
            steps.push(                Step {
                run: Some(format!("if [[ ${{{{ {changelog_was_changed} || {omit_in_commit_msg} || {omit_in_pr_body} || {is_dependabot} }}}} == false ]]; then exit 1; fi")),
                r#if: Some("github.base_ref == 'develop' || github.base_ref == 'unstable' || github.base_ref == 'stable'".into()),
                ..default()
            });
            steps
        };

        Job {
            name: "Assert if CHANGELOG.md was updated (on pull request)".into(),
            runs_on: runs_on(os),
            steps,
            ..default()
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CancelWorkflow;
impl JobArchetype for CancelWorkflow {
    fn job(_os: OS) -> Job {
        Job {
            name: "Cancel Previous Runs".into(),
            // It is important that this particular job runs pretty much everywhere (we use x64,
            // as all currently available GH runners have this label). If we limited it only to
            // our self-hosted machines (as we usually do), it'd be enqueued after other jobs
            // and wouldn't be able to cancel them.
            runs_on: vec![RunnerLabel::X64],
            steps: vec![cancel_workflow_action()],
            ..default()
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Lint;
impl JobArchetype for Lint {
    fn job(os: OS) -> Job {
        plain_job(&os, "Lint", "lint")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NativeTest;
impl JobArchetype for NativeTest {
    fn job(os: OS) -> Job {
        plain_job(&os, "Native GUI tests", "wasm test --no-wasm")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WasmTest;
impl JobArchetype for WasmTest {
    fn job(os: OS) -> Job {
        plain_job(&os, "WASM GUI tests", "wasm test --no-native")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct IntegrationTest;
impl JobArchetype for IntegrationTest {
    fn job(os: OS) -> Job {
        plain_job(
            &os,
            "IDE integration tests",
            "ide integration-test --backend-source current-ci-run",
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BuildWasm;
impl JobArchetype for BuildWasm {
    fn job(os: OS) -> Job {
        plain_job(
            &os,
            "Build GUI (WASM)",
            " --upload-artifacts ${{ runner.os == 'Linux' }} wasm build",
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BuildBackend;
impl JobArchetype for BuildBackend {
    fn job(os: OS) -> Job {
        plain_job(&os, "Build Backend", "backend get")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UploadBackend;
impl JobArchetype for UploadBackend {
    fn job(os: OS) -> Job {
        plain_job(&os, "Upload Backend", "backend upload")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UploadRuntimeToEcr;
impl JobArchetype for UploadRuntimeToEcr {
    fn job(os: OS) -> Job {
        plain_job_customized(&os, "Upload Runtime to ECR", "release deploy-to-ecr", |step| {
            step.with_env("ENSO_BUILD_ECR_REPOSITORY", enso_build::aws::ecr::runtime::NAME)
                .with_secret_exposed_as(secret::ECR_PUSH_RUNTIME_ACCESS_KEY_ID, "AWS_ACCESS_KEY_ID")
                .with_secret_exposed_as(
                    secret::ECR_PUSH_RUNTIME_SECRET_ACCESS_KEY,
                    "AWS_SECRET_ACCESS_KEY",
                )
                .with_env("AWS_DEFAULT_REGION", enso_build::aws::ecr::runtime::REGION)
        })
    }
}

pub fn expose_os_specific_signing_secret(os: OS, step: Step) -> Step {
    match os {
        OS::Windows => step
            .with_secret_exposed_as(
                secret::WINDOWS_CERT_PATH,
                &enso_build::ide::web::env::WIN_CSC_LINK,
            )
            .with_secret_exposed_as(
                secret::WINDOWS_CERT_PASSWORD,
                &enso_build::ide::web::env::WIN_CSC_KEY_PASSWORD,
            ),
        OS::MacOS => step
            .with_secret_exposed_as(secret::APPLE_CODE_SIGNING_CERT, "CSC_LINK")
            .with_secret_exposed_as(secret::APPLE_CODE_SIGNING_CERT_PASSWORD, "CSC_KEY_PASSWORD")
            .with_secret_exposed_as(secret::APPLE_NOTARIZATION_USERNAME, "APPLEID")
            .with_secret_exposed_as(secret::APPLE_NOTARIZATION_PASSWORD, "APPLEIDPASS")
            .with_env("CSC_IDENTITY_AUTO_DISCOVERY", "true"),
        _ => step,
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PackageIde;
impl JobArchetype for PackageIde {
    fn job(os: OS) -> Job {
        plain_job_customized(
            &os,
            "Package IDE",
            "ide build --wasm-source current-ci-run --backend-source current-ci-run",
            |step| expose_os_specific_signing_secret(os, step),
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CiCheckBackend;
impl JobArchetype for CiCheckBackend {
    fn job(os: OS) -> Job {
        let mut ret = plain_job(&os, "Engine", "backend ci-check");
        ret.steps.push(step::test_reporter(os));
        ret
    }
}
