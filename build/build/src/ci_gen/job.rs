use crate::prelude::*;

use crate::ci_gen::runs_on;
use crate::ci_gen::secret;
use crate::ci_gen::step;

use ide_ci::actions::workflow::definition::cancel_workflow_action;
use ide_ci::actions::workflow::definition::Job;
use ide_ci::actions::workflow::definition::JobArchetype;
use ide_ci::actions::workflow::definition::RunnerLabel;
use ide_ci::actions::workflow::definition::Step;
use ide_ci::actions::workflow::definition::Strategy;



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
    plain_job_customized(runs_on_info, name, command_line, |s| vec![s])
}

pub fn plain_job_customized(
    runs_on_info: &impl RunsOn,
    name: impl AsRef<str>,
    command_line: impl AsRef<str>,
    f: impl FnOnce(Step) -> Vec<Step>,
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
pub struct CancelWorkflow;
impl JobArchetype for CancelWorkflow {
    fn job(&self, _os: OS) -> Job {
        Job {
            name: "Cancel Previous Runs".into(),
            // It is important that this particular job runs pretty much everywhere (we use x64,
            // as all currently available GH runners have this label). If we limited it only to
            // our self-hosted machines (as we usually do), it'd be enqueued after other jobs
            // and wouldn't be able to cancel them.
            runs_on: vec![RunnerLabel::LinuxLatest],
            steps: vec![cancel_workflow_action()],
            ..default()
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Lint;
impl JobArchetype for Lint {
    fn job(&self, os: OS) -> Job {
        plain_job(&os, "Lint", "lint")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NativeTest;
impl JobArchetype for NativeTest {
    fn job(&self, os: OS) -> Job {
        plain_job(&os, "Native GUI tests", "wasm test --no-wasm")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WasmTest;
impl JobArchetype for WasmTest {
    fn job(&self, os: OS) -> Job {
        plain_job(&os, "WASM GUI tests", "wasm test --no-native")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct IntegrationTest;
impl JobArchetype for IntegrationTest {
    fn job(&self, os: OS) -> Job {
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
    fn job(&self, os: OS) -> Job {
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
    fn job(&self, os: OS) -> Job {
        plain_job(&os, "Build Backend", "backend get")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UploadBackend;
impl JobArchetype for UploadBackend {
    fn job(&self, os: OS) -> Job {
        plain_job(&os, "Upload Backend", "backend upload")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DeployRuntime;
impl JobArchetype for DeployRuntime {
    fn job(&self, os: OS) -> Job {
        plain_job_customized(&os, "Upload Runtime to ECR", "release deploy-runtime", |step| {
            let step = step
                .with_secret_exposed_as("CI_PRIVATE_TOKEN", "GITHUB_TOKEN")
                .with_env("crate_ECR_REPOSITORY", crate::aws::ecr::runtime::NAME)
                .with_secret_exposed_as(secret::ECR_PUSH_RUNTIME_ACCESS_KEY_ID, "AWS_ACCESS_KEY_ID")
                .with_secret_exposed_as(
                    secret::ECR_PUSH_RUNTIME_SECRET_ACCESS_KEY,
                    "AWS_SECRET_ACCESS_KEY",
                )
                .with_env("AWS_DEFAULT_REGION", crate::aws::ecr::runtime::REGION);
            vec![step]
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DeployGui;
impl JobArchetype for DeployGui {
    fn job(&self, os: OS) -> Job {
        plain_job_customized(&os, "Upload GUI to S3", "release deploy-gui", |step| {
            let step = step
                .with_secret_exposed_as("CI_PRIVATE_TOKEN", "GITHUB_TOKEN")
                .with_secret_exposed_as(secret::ARTEFACT_S3_ACCESS_KEY_ID, "AWS_ACCESS_KEY_ID")
                .with_secret_exposed_as(
                    secret::ARTEFACT_S3_SECRET_ACCESS_KEY,
                    "AWS_SECRET_ACCESS_KEY",
                );
            vec![step]
        })
    }
}

pub fn expose_os_specific_signing_secret(os: OS, step: Step) -> Step {
    match os {
        OS::Windows => step
            .with_secret_exposed_as(secret::WINDOWS_CERT_PATH, &crate::ide::web::env::WIN_CSC_LINK)
            .with_secret_exposed_as(
                secret::WINDOWS_CERT_PASSWORD,
                &crate::ide::web::env::WIN_CSC_KEY_PASSWORD,
            ),
        OS::MacOS => step
            .with_secret_exposed_as(
                secret::APPLE_CODE_SIGNING_CERT,
                &crate::ide::web::env::CSC_LINK,
            )
            .with_secret_exposed_as(
                secret::APPLE_CODE_SIGNING_CERT_PASSWORD,
                &crate::ide::web::env::CSC_KEY_PASSWORD,
            )
            .with_secret_exposed_as(
                secret::APPLE_NOTARIZATION_USERNAME,
                &crate::ide::web::env::APPLEID,
            )
            .with_secret_exposed_as(
                secret::APPLE_NOTARIZATION_PASSWORD,
                &crate::ide::web::env::APPLEIDPASS,
            )
            .with_env(&crate::ide::web::env::CSC_IDENTITY_AUTO_DISCOVERY, "true"),
        _ => step,
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PackageIde;
impl JobArchetype for PackageIde {
    fn job(&self, os: OS) -> Job {
        plain_job_customized(
            &os,
            "Package IDE",
            "ide build --wasm-source current-ci-run --backend-source current-ci-run",
            |step| vec![expose_os_specific_signing_secret(os, step)],
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CiCheckBackend;
impl JobArchetype for CiCheckBackend {
    fn job(&self, os: OS) -> Job {
        plain_job_customized(&os, "Engine", "backend ci-check", |main_step| {
            vec![main_step, step::engine_test_reporter(os), step::stdlib_test_reporter(os)]
        })
    }
}
