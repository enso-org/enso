use crate::prelude::*;

use crate::ci::labels::CLEAN_BUILD_REQUIRED;
use crate::ci_gen::job::plain_job;
use crate::ci_gen::job::with_packaging_steps;
use crate::ci_gen::job::RunsOn;
use crate::version::promote::Designation;
use crate::version::ENSO_EDITION;
use crate::version::ENSO_RELEASE_MODE;
use crate::version::ENSO_VERSION;

use ide_ci::actions::workflow::definition::checkout_repo_step;
use ide_ci::actions::workflow::definition::get_input_expression;
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


    // === AWS S3 Standard Library Tests ===
    pub const ENSO_LIB_S3_AWS_ACCESS_KEY_ID: &str = "ENSO_LIB_S3_AWS_ACCESS_KEY_ID";
    pub const ENSO_LIB_S3_AWS_REGION: &str = "ENSO_LIB_S3_AWS_REGION";
    pub const ENSO_LIB_S3_AWS_SECRET_ACCESS_KEY: &str = "ENSO_LIB_S3_AWS_SECRET_ACCESS_KEY";


    // === AWS ECR deployment (runtime release to cloud) ===
    pub const ECR_PUSH_RUNTIME_SECRET_ACCESS_KEY: &str = "ECR_PUSH_RUNTIME_SECRET_ACCESS_KEY";
    pub const ECR_PUSH_RUNTIME_ACCESS_KEY_ID: &str = "ECR_PUSH_RUNTIME_ACCESS_KEY_ID";

    // === Enso Cloud deployment ===
    /// Static token for admin requests on our Lambdas.
    pub const ENSO_ADMIN_TOKEN: &str = "ENSO_ADMIN_TOKEN";


    // === Apple Code Signing & Notarization ===
    pub const APPLE_CODE_SIGNING_CERT: &str = "APPLE_CODE_SIGNING_CERT";
    pub const APPLE_CODE_SIGNING_CERT_PASSWORD: &str = "APPLE_CODE_SIGNING_CERT_PASSWORD";
    pub const APPLE_NOTARIZATION_USERNAME: &str = "APPLE_NOTARIZATION_USERNAME";
    pub const APPLE_NOTARIZATION_PASSWORD: &str = "APPLE_NOTARIZATION_PASSWORD";
    pub const APPLE_NOTARIZATION_TEAM_ID: &str = "APPLE_NOTARIZATION_TEAM_ID";

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

impl RunsOn for BenchmarkRunner {
    fn runs_on(&self) -> Vec<RunnerLabel> {
        vec![RunnerLabel::Benchmark]
    }
    fn job_name_suffix(&self) -> Option<String> {
        None
    }
}


/// Condition under which the runner should be cleaned.
#[derive(Clone, Copy, Debug, Default, PartialOrd, Ord, PartialEq, Eq)]
pub enum CleaningCondition {
    /// Always clean, even if the job was canceled or failed.
    Always,
    /// Clean only if the "Clean required" label is present on the pull request.
    #[default]
    OnLabel,
}

impl CleaningCondition {
    /// Pretty print (for GH Actions) the `if` condition for the cleaning step.
    pub fn format(self) -> String {
        // Note that we need to use `always() &&` to make this condition evaluate on failed and
        // canceled runs. See: https://docs.github.com/en/actions/learn-github-actions/expressions#always
        //
        // Using `always() &&` is not a no-op like `true &&` would be.
        match self {
            Self::Always => "always()".into(),
            Self::OnLabel => format!(
                "contains(github.event.pull_request.labels.*.name, '{CLEAN_BUILD_REQUIRED}')"
            ),
        }
    }

    /// Format condition as `if` expression.
    ///
    /// All the conditions are joined with `&&`.
    pub fn format_conjunction(conditions: impl IntoIterator<Item = Self>) -> Option<String> {
        let conditions = conditions.into_iter().collect::<BTreeSet<_>>();
        if conditions.is_empty() {
            None
        } else {
            Some(conditions.into_iter().map(Self::format).join(" && "))
        }
    }
}


/// Create a step that cleans the runner if the conditions are met.
pub fn cleaning_step(
    name: impl Into<String>,
    conditions: impl IntoIterator<Item = CleaningCondition>,
) -> Step {
    let mut ret = run("git-clean").with_name(name);
    ret.r#if = CleaningCondition::format_conjunction(conditions);
    ret
}

/// Data needed to generate a typical sequence of CI steps invoking `./run` script.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct RunStepsBuilder {
    /// The command passed to `./run` script.
    pub run_command: String,
    /// Condition under which the runner should be cleaned before and after the run.
    pub cleaning:    CleaningCondition,
    /// Customize the step that runs the command.
    ///
    /// Allows replacing the run step with one or more custom steps.
    #[derivative(Debug = "ignore")]
    pub customize:   Option<Box<dyn FnOnce(Step) -> Vec<Step>>>,
}

impl RunStepsBuilder {
    /// Create a builder with the given command.
    pub fn new(run_command: impl Into<String>) -> Self {
        Self { run_command: run_command.into(), cleaning: default(), customize: default() }
    }

    /// Set the cleaning condition.
    pub fn cleaning(mut self, cleaning: CleaningCondition) -> Self {
        self.cleaning = cleaning;
        self
    }

    /// Customize the step that runs the command.
    pub fn customize(mut self, customize: impl FnOnce(Step) -> Vec<Step> + 'static) -> Self {
        self.customize = Some(Box::new(customize));
        self
    }

    /// Build the steps.
    pub fn build(self) -> Vec<Step> {
        let clean_before = cleaning_step("Clean before", [self.cleaning]);
        let clean_after = cleaning_step("Clean after", [CleaningCondition::Always, self.cleaning]);
        let run_step = run(self.run_command);
        let run_steps = match self.customize {
            Some(customize) => customize(run_step),
            None => vec![run_step],
        };
        let mut steps = setup_script_steps();
        steps.push(clean_before);
        steps.extend(run_steps);
        steps.extend(list_everything_on_failure());
        steps.push(clean_after);
        steps
    }

    pub fn job_builder(self, name: impl Into<String>, runs_on: impl RunsOn) -> RunJobBuilder {
        RunJobBuilder::new(self, name, runs_on)
    }

    pub fn build_job(self, name: impl Into<String>, runs_on: impl RunsOn) -> Job {
        self.job_builder(name, runs_on).build()
    }
}

/// Data needed to generate a job that invokes `./run` script.
#[derive(Debug)]
pub struct RunJobBuilder {
    /// Data to generate the steps.
    pub inner:   RunStepsBuilder,
    /// Name of the job. Might be modified to include the runner info.
    pub name:    String,
    /// The runners on which the job should run.
    pub runs_on: Box<dyn RunsOn>,
}

impl RunJobBuilder {
    pub fn new(
        build_steps: RunStepsBuilder,
        name: impl Into<String>,
        runs_on: impl RunsOn + 'static,
    ) -> Self {
        Self { name: name.into(), runs_on: Box::new(runs_on), inner: build_steps }
    }

    pub fn build(self) -> Job {
        let name = if let Some(os_name) = self.runs_on.job_name_suffix() {
            format!("{} ({})", self.name, os_name)
        } else {
            self.name
        };
        let steps = self.inner.build();
        let runs_on = self.runs_on.runs_on();
        let strategy = self.runs_on.strategy();
        Job { name, runs_on, steps, strategy, ..default() }
    }
}

/// Trigger the workflow on push to the default branch.
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

/// Initial CI job steps: check out the source code and set up the environment.
pub fn setup_script_steps() -> Vec<Step> {
    let mut ret = vec![setup_conda(), setup_wasm_pack_step(), setup_artifact_api()];
    ret.extend(checkout_repo_step());
    // We run `./run --help` so:
    // * The build-script is build in a separate step. This allows us to monitor its build-time and
    //   not affect timing of the actual build.
    // * The help message is printed to the log, including environment-dependent flag defaults.
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
        let mut ret = plain_job(os, "Publish release", "release publish");
        ret.expose_secret_as(secret::ARTEFACT_S3_ACCESS_KEY_ID, crate::aws::env::AWS_ACCESS_KEY_ID);
        ret.expose_secret_as(
            secret::ARTEFACT_S3_SECRET_ACCESS_KEY,
            crate::aws::env::AWS_SECRET_ACCESS_KEY,
        );
        ret.env(crate::aws::env::AWS_REGION, "us-west-1");
        ret
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UploadIde;
impl JobArchetype for UploadIde {
    fn job(&self, os: OS) -> Job {
        RunStepsBuilder::new("ide upload --wasm-source current-ci-run --backend-source release --backend-release ${{env.ENSO_RELEASE_ID}}")
            .customize(with_packaging_steps(os))
            .build_job("Build Old IDE", os)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UploadIde2;
impl JobArchetype for UploadIde2 {
    fn job(&self, os: OS) -> Job {
        RunStepsBuilder::new(
            "ide2 upload --backend-source release --backend-release ${{env.ENSO_RELEASE_ID}}",
        )
        .customize(with_packaging_steps(os))
        .build_job("Build New IDE", os)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PromoteReleaseJob;
impl JobArchetype for PromoteReleaseJob {
    fn job(&self, os: OS) -> Job {
        let command = format!("release promote {}", get_input_expression(DESIGNATOR_INPUT_NAME));
        let mut job = RunStepsBuilder::new(command)
            .customize(|step| vec![step.with_id(Self::PROMOTE_STEP_ID)])
            .build_job("Promote release", os);
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
    ret.add_job(RunStepsBuilder::new("changelog-check").build_job("Changelog", RunnerLabel::X64));
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

        let build_ide2_job_id =
            workflow.add_dependent(os, UploadIde2, [&prepare_job_id, &backend_job_id]);
        packaging_job_ids.push(build_ide2_job_id.clone());

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
    workflow.add(PRIMARY_OS, job::NewGuiTest);

    // FIXME: Integration tests are currently always failing.
    //        The should be reinstated when fixed.
    // workflow.add_customized::<job::IntegrationTest>(PRIMARY_OS, |job| {
    //     job.needs.insert(job::BuildBackend::key(PRIMARY_OS));
    // });

    for os in TARGETED_SYSTEMS {
        let project_manager_job = workflow.add(os, job::BuildBackend);
        workflow.add_customized(os, job::PackageNewIde, |job| {
            job.needs.insert(project_manager_job.clone());
        });
        workflow.add(os, job::NewGuiBuild);
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

pub fn engine_benchmark() -> Result<Workflow> {
    benchmark("Benchmark Engine", "backend benchmark runtime", Some(4 * 60))
}

pub fn std_libs_benchmark() -> Result<Workflow> {
    benchmark("Benchmark Standard Libraries", "backend benchmark enso-jmh", Some(4 * 60))
}

fn benchmark(name: &str, command_line: &str, timeout_minutes: Option<u32>) -> Result<Workflow> {
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
    let mut workflow = Workflow { name: name.into(), on, ..default() };
    // Note that we need to use `true == input` instead of `input` because that interprets input as
    // `false` rather than empty string. Empty string is not falsy enough.
    workflow.env(
        "ENSO_BUILD_MINIMAL_RUN",
        wrap_expression(format!("true == inputs.{just_check_input_name}")),
    );

    let mut benchmark_job = RunStepsBuilder::new(command_line)
        .cleaning(CleaningCondition::Always)
        .build_job(name, BenchmarkRunner);
    benchmark_job.timeout_minutes = timeout_minutes;
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
        (repo_root.engine_benchmark_yml.to_path_buf(), engine_benchmark()?),
        (repo_root.std_libs_benchmark_yml.to_path_buf(), std_libs_benchmark()?),
        (repo_root.release_yml.to_path_buf(), release()?),
        (repo_root.promote_yml.to_path_buf(), promote()?),
    ];
    let workflows = workflows
        .into_iter()
        .map(|(path, workflow)| WorkflowToWrite { workflow, path, source: module_path!().into() })
        .collect();
    Ok(workflows)
}
