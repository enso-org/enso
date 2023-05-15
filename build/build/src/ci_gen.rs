use crate::prelude::*;

use crate::ci_gen::job::expose_os_specific_signing_secret;
use crate::ci_gen::job::plain_job;
use crate::ci_gen::job::plain_job_customized;
use crate::ci_gen::job::RunsOn;
use crate::version::promote::Designation;
use crate::version::ENSO_EDITION;
use crate::version::ENSO_RELEASE_MODE;
use crate::version::ENSO_VERSION;

use ide_ci::actions::workflow::definition::checkout_repo_step;
use ide_ci::actions::workflow::definition::is_non_windows_runner;
use ide_ci::actions::workflow::definition::is_windows_runner;
use ide_ci::actions::workflow::definition::run;
use ide_ci::actions::workflow::definition::setup_artifact_api;
use ide_ci::actions::workflow::definition::setup_conda;
use ide_ci::actions::workflow::definition::setup_wasm_pack_step;
use ide_ci::actions::workflow::definition::wrap_expression;
use ide_ci::actions::workflow::definition::Branches;
use ide_ci::actions::workflow::definition::Concurrency;
use ide_ci::actions::workflow::definition::Event;
use ide_ci::actions::workflow::definition::Job;
use ide_ci::actions::workflow::definition::JobArchetype;
use ide_ci::actions::workflow::definition::JobSecrets;
use ide_ci::actions::workflow::definition::PullRequest;
use ide_ci::actions::workflow::definition::PullRequestActivityType;
use ide_ci::actions::workflow::definition::Push;
use ide_ci::actions::workflow::definition::RunnerLabel;
use ide_ci::actions::workflow::definition::Schedule;
use ide_ci::actions::workflow::definition::Step;
use ide_ci::actions::workflow::definition::Workflow;
use ide_ci::actions::workflow::definition::WorkflowCall;
use ide_ci::actions::workflow::definition::WorkflowDispatch;
use ide_ci::actions::workflow::definition::WorkflowDispatchInput;
use ide_ci::actions::workflow::definition::WorkflowDispatchInputType;
use ide_ci::actions::workflow::definition::WorkflowToWrite;
use strum::IntoEnumIterator;


// ==============
// === Export ===
// ==============

pub mod job;
pub mod step;



#[derive(Clone, Copy, Debug)]
pub struct DeluxeRunner;

#[derive(Clone, Copy, Debug)]
pub struct BenchmarkRunner;

pub const PRIMARY_OS: OS = OS::Linux;

pub const TARGETED_SYSTEMS: [OS; 3] = [OS::Windows, OS::Linux, OS::MacOS];

pub const DEFAULT_BRANCH_NAME: &str = "develop";

pub const RELEASE_CONCURRENCY_GROUP: &str = "release";

pub const DESIGNATOR_INPUT_NAME: &str = "designator";

pub const PROMOTE_WORKFLOW_PATH: &str = "./.github/workflows/promote.yml";

pub const RELEASE_WORKFLOW_PATH: &str = "./.github/workflows/release.yml";

/// Secrets set up in our organization.
///
/// To manage, see: https://github.com/organizations/enso-org/settings/secrets/actions
pub mod secret {
    // === AWS S3 deploy (release list) ===
    pub const ARTEFACT_S3_ACCESS_KEY_ID: &str = "ARTEFACT_S3_ACCESS_KEY_ID";
    pub const ARTEFACT_S3_SECRET_ACCESS_KEY: &str = "ARTEFACT_S3_SECRET_ACCESS_KEY";


    // === AWS ECR deployment (runtime release to cloud) ===
    pub const ECR_PUSH_RUNTIME_SECRET_ACCESS_KEY: &str = "ECR_PUSH_RUNTIME_SECRET_ACCESS_KEY";
    pub const ECR_PUSH_RUNTIME_ACCESS_KEY_ID: &str = "ECR_PUSH_RUNTIME_ACCESS_KEY_ID";


    // === Apple Code Signing & Notarization ===
    pub const APPLE_CODE_SIGNING_CERT: &str = "APPLE_CODE_SIGNING_CERT";
    pub const APPLE_CODE_SIGNING_CERT_PASSWORD: &str = "APPLE_CODE_SIGNING_CERT_PASSWORD";
    pub const APPLE_NOTARIZATION_USERNAME: &str = "APPLE_NOTARIZATION_USERNAME";
    pub const APPLE_NOTARIZATION_PASSWORD: &str = "APPLE_NOTARIZATION_PASSWORD";

    // === Windows Code Signing ===
    /// Name of the GitHub Actions secret that stores path to the Windows code signing certificate
    /// within the runner.
    pub const WINDOWS_CERT_PATH: &str = "MICROSOFT_CODE_SIGNING_CERT";

    /// Name of the GitHub Actions secret that stores password to the Windows code signing
    /// certificate.
    pub const WINDOWS_CERT_PASSWORD: &str = "MICROSOFT_CODE_SIGNING_CERT_PASSWORD";

    // === Github Token ===
    /// A token created for the `enso-ci` user.
    pub const CI_PRIVATE_TOKEN: &str = "CI_PRIVATE_TOKEN";
}

pub fn release_concurrency() -> Concurrency {
    Concurrency::new(RELEASE_CONCURRENCY_GROUP)
}

/// Get expression that gets input from the workflow dispatch. See:
/// <https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows#providing-inputs>
pub fn get_input_expression(name: impl Into<String>) -> String {
    wrap_expression(format!("inputs.{}", name.into()))
}

impl RunsOn for DeluxeRunner {
    fn runs_on(&self) -> Vec<RunnerLabel> {
        vec![RunnerLabel::MwuDeluxe]
    }
    fn os_name(&self) -> Option<String> {
        None
    }
}

impl RunsOn for BenchmarkRunner {
    fn runs_on(&self) -> Vec<RunnerLabel> {
        vec![RunnerLabel::Benchmark]
    }
    fn os_name(&self) -> Option<String> {
        None
    }
}

pub fn on_default_branch_push() -> Push {
    Push { inner_branches: Branches::new([DEFAULT_BRANCH_NAME]), ..default() }
}

pub fn runs_on(os: OS) -> Vec<RunnerLabel> {
    match os {
        OS::Windows => vec![RunnerLabel::SelfHosted, RunnerLabel::Windows, RunnerLabel::Engine],
        OS::Linux => vec![RunnerLabel::SelfHosted, RunnerLabel::Linux, RunnerLabel::Engine],
        OS::MacOS => vec![RunnerLabel::MacOSLatest],
        _ => todo!("Not supported"),
    }
}

pub fn setup_script_steps() -> Vec<Step> {
    let mut ret = vec![setup_conda(), setup_wasm_pack_step(), setup_artifact_api()];
    ret.extend(checkout_repo_step());
    ret.push(run("--help").with_name("Build Script Setup"));
    ret
}

pub fn list_everything_on_failure() -> impl IntoIterator<Item = Step> {
    let win = Step {
        name: Some("List files if failed (Windows)".into()),
        r#if: Some(format!("failure() && {}", is_windows_runner())),
        run: Some("Get-ChildItem -Force -Recurse".into()),
        ..default()
    };

    let non_win = Step {
        name: Some("List files if failed (non-Windows)".into()),
        r#if: Some(format!("failure() && {}", is_non_windows_runner())),
        run: Some("ls -lAR".into()),
        ..default()
    };

    [win, non_win]
}

/// The `f` is applied to the step that does an actual script invocation.
pub fn setup_customized_script_steps(
    command_line: impl AsRef<str>,
    customize: impl FnOnce(Step) -> Vec<Step>,
) -> Vec<Step> {
    use crate::ci::labels::CLEAN_BUILD_REQUIRED;
    // Check if the pull request has a "Clean required" label.
    let pre_clean_condition =
        format!("contains(github.event.pull_request.labels.*.name, '{CLEAN_BUILD_REQUIRED}')",);
    let post_clean_condition = format!("always() && {pre_clean_condition}");

    let mut steps = setup_script_steps();
    let clean_step = run("git-clean").with_if(&pre_clean_condition).with_name("Clean before");
    steps.push(clean_step.clone());
    steps.extend(customize(run(command_line)));
    steps.extend(list_everything_on_failure());
    steps.push(
        clean_step.with_if(format!("always() && {post_clean_condition}")).with_name("Clean after"),
    );
    steps
}

pub fn setup_script_and_steps(command_line: impl AsRef<str>) -> Vec<Step> {
    setup_customized_script_steps(command_line, |s| vec![s])
}

#[derive(Clone, Copy, Debug)]
pub struct DraftRelease;
impl JobArchetype for DraftRelease {
    fn job(&self, os: OS) -> Job {
        let name = "Create a release draft.".into();

        let prepare_step = run("release create-draft").with_id(Self::PREPARE_STEP_ID);
        let mut steps = setup_script_steps();
        steps.push(prepare_step);

        let mut ret = Job { name, runs_on: runs_on(os), steps, ..default() };
        self.expose_outputs(&mut ret);
        ret
    }

    fn outputs(&self) -> BTreeMap<String, Vec<String>> {
        let mut ret = BTreeMap::new();
        ret.insert(Self::PREPARE_STEP_ID.into(), vec![
            "ENSO_VERSION".into(),
            "ENSO_RELEASE_ID".into(),
        ]);
        ret
    }
}

impl DraftRelease {
    pub const PREPARE_STEP_ID: &'static str = "prepare";
}

#[derive(Clone, Copy, Debug)]
pub struct PublishRelease;
impl JobArchetype for PublishRelease {
    fn job(&self, os: OS) -> Job {
        let mut ret = plain_job(&os, "Publish release", "release publish");
        ret.expose_secret_as(secret::ARTEFACT_S3_ACCESS_KEY_ID, "AWS_ACCESS_KEY_ID");
        ret.expose_secret_as(secret::ARTEFACT_S3_SECRET_ACCESS_KEY, "AWS_SECRET_ACCESS_KEY");
        ret.env("AWS_REGION", "us-west-1");
        ret
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UploadIde;
impl JobArchetype for UploadIde {
    fn job(&self, os: OS) -> Job {
        plain_job_customized(&os, "Build IDE", "ide upload --wasm-source current-ci-run --backend-source release --backend-release ${{env.ENSO_RELEASE_ID}}", |step| 
            vec![expose_os_specific_signing_secret(os, step)]
        )
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PromoteReleaseJob;
impl JobArchetype for PromoteReleaseJob {
    fn job(&self, os: OS) -> Job {
        let command = format!("release promote {}", get_input_expression(DESIGNATOR_INPUT_NAME));
        let mut job = plain_job_customized(&os, "Promote release", command, |step| {
            vec![step.with_id(Self::PROMOTE_STEP_ID)]
        });
        self.expose_outputs(&mut job);
        job
    }

    fn outputs(&self) -> BTreeMap<String, Vec<String>> {
        let mut ret = BTreeMap::new();
        ret.insert(Self::PROMOTE_STEP_ID.into(), vec![
            ENSO_VERSION.name.to_string(),
            ENSO_EDITION.name.to_string(),
            ENSO_RELEASE_MODE.name.to_string(),
        ]);
        ret
    }
}
impl PromoteReleaseJob {
    pub const PROMOTE_STEP_ID: &'static str = "promote";
}

/// Generate a workflow that checks if the changelog has been updated (if needed).
pub fn changelog() -> Result<Workflow> {
    use PullRequestActivityType::*;
    let mut ret = Workflow::new("Changelog");
    ret.on.pull_request(PullRequest::default().with_types([
        Labeled,
        Unlabeled,
        Synchronize,
        Opened,
        Reopened,
    ]));
    ret.add_job(Job {
        name: "Changelog".into(),
        runs_on: vec![RunnerLabel::X64],
        steps: setup_script_and_steps("changelog-check"),
        ..default()
    });
    Ok(ret)
}

pub fn nightly() -> Result<Workflow> {
    let on = Event {
        workflow_dispatch: Some(default()),
        // 2am (UTC) every day.
        schedule: vec![Schedule::new("0 2 * * *")?],
        ..default()
    };

    let mut workflow = Workflow { on, name: "Nightly Release".into(), ..default() };

    let job = workflow_call_job("Promote nightly", PROMOTE_WORKFLOW_PATH)
        .with_with(DESIGNATOR_INPUT_NAME, Designation::Nightly.as_ref());
    workflow.add_job(job);
    Ok(workflow)
}

fn add_release_steps(workflow: &mut Workflow) -> Result {
    let prepare_job_id = workflow.add(PRIMARY_OS, DraftRelease);
    let build_wasm_job_id = workflow.add(PRIMARY_OS, job::BuildWasm);
    let mut packaging_job_ids = vec![];

    // Assumed, because Linux is necessary to deploy ECR runtime image.
    assert!(TARGETED_SYSTEMS.contains(&OS::Linux));
    for os in TARGETED_SYSTEMS {
        let backend_job_id = workflow.add_dependent(os, job::UploadBackend, [&prepare_job_id]);
        let build_ide_job_id = workflow.add_dependent(os, UploadIde, [
            &prepare_job_id,
            &backend_job_id,
            &build_wasm_job_id,
        ]);
        packaging_job_ids.push(build_ide_job_id.clone());

        // Deploying our release to cloud needs to be done only once.
        // We could do this on any platform, but we choose Linux, because it's most easily
        // available and performant.
        if os == OS::Linux {
            let runtime_requirements = [&prepare_job_id, &backend_job_id];
            let upload_runtime_job_id =
                workflow.add_dependent(os, job::DeployRuntime, runtime_requirements);
            packaging_job_ids.push(upload_runtime_job_id);

            let gui_requirements = [build_ide_job_id];
            let deploy_gui_job_id = workflow.add_dependent(os, job::DeployGui, gui_requirements);
            packaging_job_ids.push(deploy_gui_job_id);
        }
    }

    let publish_deps = {
        packaging_job_ids.push(prepare_job_id);
        packaging_job_ids
    };


    let _publish_job_id = workflow.add_dependent(PRIMARY_OS, PublishRelease, publish_deps);
    workflow.env("RUST_BACKTRACE", "full");
    Ok(())
}

pub fn workflow_call_job(name: impl Into<String>, path: impl Into<String>) -> Job {
    Job {
        name: name.into(),
        uses: Some(path.into()),
        secrets: Some(JobSecrets::Inherit),
        ..default()
    }
}

pub fn call_release_job(version_input_expr: &str) -> Job {
    workflow_call_job("Release", RELEASE_WORKFLOW_PATH).with_with("version", version_input_expr)
}

pub fn release() -> Result<Workflow> {
    let version_input = WorkflowDispatchInput::new_string(
        "What version number this release should get.",
        true,
        None::<String>,
    );
    let workflow_dispatch = WorkflowDispatch::default().with_input("version", version_input);
    let workflow_call = WorkflowCall::try_from(workflow_dispatch.clone())?;
    let on = Event {
        workflow_dispatch: Some(workflow_dispatch),
        workflow_call: Some(workflow_call),
        ..default()
    };

    let mut workflow = Workflow {
        on,
        name: "Release".into(),
        concurrency: Some(release_concurrency()),
        ..default()
    };

    add_release_steps(&mut workflow)?;
    let version_input_expression = get_input_expression("version");
    workflow.env(ENSO_EDITION.name, &version_input_expression);
    workflow.env(ENSO_VERSION.name, &version_input_expression);
    Ok(workflow)
}


pub fn promote() -> Result<Workflow> {
    let designator = WorkflowDispatchInput::new_choice(
        "What kind of release should be promoted.",
        true,
        Designation::iter().map(|d| d.as_ref().to_string()),
        None::<String>,
    )?;
    let workflow_dispatch =
        WorkflowDispatch::default().with_input(DESIGNATOR_INPUT_NAME, designator);
    let on = Event {
        workflow_call: Some(WorkflowCall::try_from(workflow_dispatch.clone())?),
        workflow_dispatch: Some(workflow_dispatch),
        ..default()
    };
    let mut workflow = Workflow { on, name: "Generate a new version".into(), ..default() };
    let promote_job_id = workflow.add(PRIMARY_OS, PromoteReleaseJob);


    let version_input = format!("needs.{promote_job_id}.outputs.{ENSO_VERSION}");
    let mut release_job = call_release_job(&wrap_expression(version_input));
    release_job.needs(&promote_job_id);
    workflow.add_job(release_job);

    Ok(workflow)
}

pub fn typical_check_triggers() -> Event {
    Event {
        pull_request: Some(default()),
        workflow_dispatch: Some(default()),
        push: Some(on_default_branch_push()),
        ..default()
    }
}

pub fn gui() -> Result<Workflow> {
    let on = typical_check_triggers();
    let mut workflow = Workflow { name: "GUI CI".into(), on, ..default() };
    workflow.add(PRIMARY_OS, job::CancelWorkflow);
    workflow.add(PRIMARY_OS, job::Lint);
    workflow.add(PRIMARY_OS, job::WasmTest);
    workflow.add(PRIMARY_OS, job::NativeTest);

    // FIXME: Integration tests are currently always failing.
    //        The should be reinstated when fixed.
    // workflow.add_customized::<job::IntegrationTest>(PRIMARY_OS, |job| {
    //     job.needs.insert(job::BuildBackend::key(PRIMARY_OS));
    // });

    // Because WASM upload happens only for the Linux build, all other platforms needs to depend on
    // it.
    let wasm_job_linux = workflow.add(OS::Linux, job::BuildWasm);
    for os in TARGETED_SYSTEMS {
        if os != OS::Linux {
            // Linux was already added above.
            let _wasm_job = workflow.add(os, job::BuildWasm);
        }
        let project_manager_job = workflow.add(os, job::BuildBackend);
        workflow.add_customized(os, job::PackageIde, |job| {
            job.needs.insert(wasm_job_linux.clone());
            job.needs.insert(project_manager_job);
        });
    }
    Ok(workflow)
}

pub fn backend() -> Result<Workflow> {
    let on = typical_check_triggers();
    let mut workflow = Workflow { name: "Engine CI".into(), on, ..default() };
    workflow.add(PRIMARY_OS, job::CancelWorkflow);
    for os in TARGETED_SYSTEMS {
        workflow.add(os, job::CiCheckBackend);
    }
    Ok(workflow)
}

pub fn benchmark() -> Result<Workflow> {
    let just_check_input_name = "just-check";
    let just_check_input = WorkflowDispatchInput {
        r#type: WorkflowDispatchInputType::Boolean{default: Some(false)},
        ..WorkflowDispatchInput::new("If set, benchmarks will be only checked to run correctly, not to measure actual performance.", true)
    };
    let on = Event {
        workflow_dispatch: Some(
            WorkflowDispatch::default().with_input(just_check_input_name, just_check_input),
        ),
        schedule: vec![Schedule::new("0 0 * * *")?],
        ..default()
    };
    let mut workflow = Workflow { name: "Benchmark Engine".into(), on, ..default() };
    // Note that we need to use `true == input` instead of `input` because that interprets input as
    // `false` rather than empty string. Empty string is not falsy enough.
    workflow.env(
        "ENSO_BUILD_MINIMAL_RUN",
        wrap_expression(format!("true == inputs.{just_check_input_name}")),
    );

    let mut benchmark_job =
        plain_job(&BenchmarkRunner, "Benchmark Engine", "backend benchmark runtime");
    benchmark_job.timeout_minutes = Some(60 * 8);
    workflow.add_job(benchmark_job);
    Ok(workflow)
}

/// Generate workflows for the CI.
pub fn generate(
    repo_root: &crate::paths::generated::RepoRootGithubWorkflows,
) -> Result<Vec<WorkflowToWrite>> {
    let workflows = [
        (repo_root.changelog_yml.to_path_buf(), changelog()?),
        (repo_root.nightly_yml.to_path_buf(), nightly()?),
        (repo_root.scala_new_yml.to_path_buf(), backend()?),
        (repo_root.gui_yml.to_path_buf(), gui()?),
        (repo_root.benchmark_yml.to_path_buf(), benchmark()?),
        (repo_root.release_yml.to_path_buf(), release()?),
        (repo_root.promote_yml.to_path_buf(), promote()?),
    ];
    let workflows = workflows
        .into_iter()
        .map(|(path, workflow)| WorkflowToWrite { workflow, path, source: module_path!().into() })
        .collect();
    Ok(workflows)
}
