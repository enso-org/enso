//! Model of a workflow definition and related utilities.

use crate::prelude::*;

use crate::convert_case::ToKebabCase;
use crate::env::accessor::RawVariable;

use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::convert::identity;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;



/// Operating system and architecture.
///
/// While this should be enough of a target information for a Job definition, it is not enough
/// for a runner labels: self-hosted and GitHub-hosted runners use different label schemes.
pub type Target = (OS, Arch);

/// Default timeout for a job.
///
/// We use a very long timeout because we want to avoid cancelling jobs that are just slow.
pub const DEFAULT_TIMEOUT_IN_MINUTES: u32 = 360;

/// The name of the field in the matrix strategy that we use by convention for different
/// runner labels (OS-es, but also runner-identifying labels).
const MATRIX_STRATEGY_OS: &str = "os";

/// Wraps a given expression in GitHub Actions into `{{ ... }}`.
///
/// Expressions must be wrapped in this way to be evaluated by GitHub Actions, except for some
/// special cases, e.g. `if` expressions.
pub fn wrap_expression(expression: impl AsRef<str>) -> String {
    format!("${{{{ {} }}}}", expression.as_ref())
}

/// An expression that accesses a secret with a given name.
///
/// /// # Example
///
/// ```
/// use ide_ci::actions::workflow::definition::secret_expression;
///
/// let secret_name = "MY_SECRET";
/// let secret_expr = secret_expression(secret_name);
/// assert_eq!(secret_expr, "${{ secrets.MY_SECRET }}");
/// ```
pub fn secret_expression(secret_name: impl AsRef<str>) -> String {
    wrap_expression(format!("secrets.{}", secret_name.as_ref()))
}

/// An expression that accesses a variable with a given name.
pub fn variable_expression(secret_name: impl AsRef<str>) -> String {
    wrap_expression(format!("vars.{}", secret_name.as_ref()))
}

/// An expression that accesses an environment variable with a given name.
pub fn env_expression(environment_variable: &impl RawVariable) -> String {
    wrap_expression(format!("env.{}", environment_variable.name()))
}

/// Get expression that gets input from the workflow dispatch. See:
/// <https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows#providing-inputs>
pub fn get_input_expression(name: impl Into<String>) -> String {
    wrap_expression(format!("inputs.{}", name.into()))
}

/// GH Actions expression piece that evaluates to `true` if run on a GitHub-hosted runner.
pub fn is_github_hosted() -> String {
    "startsWith(runner.name, 'GitHub Actions') || startsWith(runner.name, 'Hosted Agent')".into()
}

pub fn setup_wasm_pack_step() -> Step {
    Step {
        name: Some("Installing wasm-pack".into()),
        uses: Some("jetli/wasm-pack-action@v0.4.0".into()),
        with: Some(step::Argument::Other(BTreeMap::from_iter([(
            "version".into(),
            "v0.12.1".into(),
        )]))),
        r#if: Some(is_github_hosted()),
        ..default()
    }
}

/// Step that executes a given [GitHub Script](https://github.com/actions/github-script).
pub fn github_script_step(name: impl Into<String>, script: impl Into<String>) -> Step {
    Step {
        name: Some(name.into()),
        uses: Some("actions/github-script@v7".into()),
        with: Some(step::Argument::GitHubScript { script: script.into() }),
        ..default()
    }
}

/// Export environment needed by our [Artifact API wrappers](crate::actions::artifacts).
pub fn setup_artifact_api() -> Step {
    let script = r#"
    core.exportVariable("ACTIONS_RUNTIME_TOKEN", process.env["ACTIONS_RUNTIME_TOKEN"])
    core.exportVariable("ACTIONS_RUNTIME_URL", process.env["ACTIONS_RUNTIME_URL"])
    core.exportVariable("GITHUB_RETENTION_DAYS", process.env["GITHUB_RETENTION_DAYS"])
    console.log(context)
    "#;
    github_script_step("Expose Artifact API and context information.", script)
}

/// An expression piece that evaluates to `true` if the current runner runs on Windows.
pub fn is_windows_runner() -> String {
    "runner.os == 'Windows'".into()
}

/// An expression piece that evaluates to `true` if the current runner *does not* run on Windows.
pub fn is_non_windows_runner() -> String {
    "runner.os != 'Windows'".into()
}

pub fn shell(command_line: impl Into<String>) -> Step {
    Step { run: Some(command_line.into()), env: once(github_token_env()).collect(), ..default() }
}

/// Invoke our entry point to the build scripts, i.e. the `./run` script.
pub fn run(run_args: impl AsRef<str>) -> Step {
    shell(format!("./run {}", run_args.as_ref()))
}

pub fn cancel_workflow_action() -> Step {
    Step {
        name: Some("Cancel Previous Runs".into()),
        uses: Some("styfle/cancel-workflow-action@0.12.1".into()),
        with: Some(step::Argument::Other(BTreeMap::from_iter([(
            "access_token".into(),
            "${{ github.token }}".into(),
        )]))),
        ..default()
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct JobId(String);

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", untagged)]
pub enum Concurrency {
    Plain(String),
    #[serde(rename_all = "kebab-case")]
    Map {
        group:              String,
        cancel_in_progress: String,
    },
}

impl Concurrency {
    pub fn new(group_name: impl Into<String>) -> Self {
        Self::Plain(group_name.into())
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "kebab-case")]
pub enum Permission {
    Actions,
    Checks,
    Contents,
    Deployments,
    IdToken,
    Issues,
    Discussions,
    Packages,
    Pages,
    PullRequests,
    RepositoryProjects,
    SecurityEvents,
    Statuses,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Access {
    Read,
    Write,
    None,
}

/// Models a GitHub Actions workflow definition.
///
/// See: <https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions>.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Workflow {
    pub name:        String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub on:          Event,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub permissions: BTreeMap<Permission, Access>,
    // No additional clause, as the jobs must be non-empty.
    pub jobs:        BTreeMap<String, Job>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub env:         BTreeMap<String, String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub concurrency: Option<Concurrency>,
}

impl Default for Workflow {
    fn default() -> Self {
        let mut ret = Self {
            name:        default(),
            permissions: default(),
            description: default(),
            on:          default(),
            jobs:        default(),
            env:         default(),
            concurrency: default(),
        };
        // By default CI should never check program versions.
        ret.env("ENSO_BUILD_SKIP_VERSION_CHECK", "true");
        ret
    }
}

impl Workflow {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into(), ..Default::default() }
    }

    /// Expose all outputs of one job to another.
    ///
    /// Note that while a source job must have been already added to the workflow, the consumer job
    /// must not have been added yet. This step is meant to be a part of the consumer job building.
    pub fn expose_outputs(&self, source_job_id: impl AsRef<str>, consumer_job: &mut Job) {
        let source_job = self.jobs.get(source_job_id.as_ref()).unwrap();
        consumer_job.use_job_outputs(source_job_id.as_ref(), source_job);
    }
}

impl Workflow {
    pub fn add_job(&mut self, job: Job) -> String {
        let key = job.name.to_kebab_case();
        if self.jobs.insert(key.clone(), job).is_some() {
            warn!("Job with name {key} already exists.");
        }
        key
    }

    pub fn add(&mut self, target: Target, job: impl JobArchetype) -> String {
        self.add_customized(target, job, |_| {})
    }

    pub fn add_customized(
        &mut self,
        target: Target,
        job: impl JobArchetype,
        f: impl FnOnce(&mut Job),
    ) -> String {
        let (key, mut job) = job.entry(target);
        f(&mut job);
        self.jobs.insert(key.clone(), job);
        key
    }

    pub fn add_dependent(
        &mut self,
        target: Target,
        job: impl JobArchetype,
        needed: impl IntoIterator<Item: AsRef<str>>,
    ) -> String {
        let (key, mut job) = job.entry(target);
        for needed in needed {
            self.expose_outputs(needed.as_ref(), &mut job);
        }
        self.jobs.insert(key.clone(), job);
        key
    }

    pub fn env(&mut self, var_name: impl Into<String>, var_value: impl Into<String>) {
        self.env.insert(var_name.into(), var_value.into());
    }

    /// Apply custom permissions to this job.
    pub fn set_permission(&mut self, permission: Permission, access: Access) {
        self.permissions.insert(permission, access);
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Push {
    #[serde(flatten)]
    pub inner_branches: Branches,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub tags:           Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub tags_ignore:    Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub paths:          Vec<PathBuf>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub paths_ignore:   Vec<PathBuf>,
}

/// Common branch-related fields between some event triggers.
///
/// See: <https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#onpull_requestpull_request_targetbranchesbranches-ignore>
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Branches {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub branches:        Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub branches_ignore: Vec<String>,
}

impl Branches {
    pub fn new(branches: impl IntoIterator<Item: Into<String>>) -> Self {
        Self { branches: branches.into_iter().map(Into::into).collect(), ..default() }
    }
}

/// See: <https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows#pull_request>
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PullRequestActivityType {
    Assigned,
    Unassigned,
    Labeled,
    Unlabeled,
    Opened,
    Edited,
    Closed,
    Reopened,
    Synchronize,
    ConvertedToDraft,
    ReadyForReview,
    Locked,
    Unlocked,
    ReviewRequested,
    ReviewRequestRemoved,
    AutoMergeEnabled,
    AutoMergeDisabled,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct PullRequest {
    #[serde(flatten)]
    pub inner_branches: Branches,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub types:          Vec<PullRequestActivityType>,
}

impl PullRequest {
    pub fn with_types(
        mut self,
        types: impl IntoIterator<Item: Into<PullRequestActivityType>>,
    ) -> Self {
        self.types.extend(types.into_iter().map(Into::into));
        self
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Schedule {
    pub cron: String,
}

impl Schedule {
    pub fn new(cron_text: impl Into<String>) -> Result<Self> {
        let cron = cron_text.into();
        // Check if the given string is a valid cron expression.
        // let _ = cron::Schedule::from_str(cron_text.as_str())?;
        Ok(Self { cron })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type")]
pub enum WorkflowDispatchInputType {
    String {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<String>,
    },
    Choice {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<String>,
        options: Vec<String>, // should be non-empty
    },
    Boolean {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<bool>,
    },
    Environment {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<String>,
    },
}

impl Default for WorkflowDispatchInputType {
    fn default() -> Self {
        Self::String { default: None }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkflowDispatchInput {
    /// A string description of the input parameter.
    pub description:         String,
    /// A string shown to users using the deprecated input.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub deprecation_message: Option<String>,
    /// A boolean to indicate whether the action requires the input parameter. Set to true when the
    /// parameter is required.
    pub required:            bool,
    /// A string representing the type of the input.
    #[serde(flatten)]
    pub r#type:              WorkflowDispatchInputType,
}

impl WorkflowDispatchInput {
    pub fn new(description: impl Into<String>, required: bool) -> Self {
        Self {
            description: description.into(),
            deprecation_message: None,
            required,
            r#type: Default::default(),
        }
    }

    pub fn new_string(
        description: impl Into<String>,
        required: bool,
        default: Option<impl Into<String>>,
    ) -> Self {
        Self {
            r#type: WorkflowDispatchInputType::String { default: default.map(Into::into) },
            ..Self::new(description, required)
        }
    }

    pub fn new_choice(
        description: impl Into<String>,
        required: bool,
        options: impl IntoIterator<Item: Into<String>>,
        default: Option<impl Into<String>>,
    ) -> Result<Self> {
        let options = options.into_iter().map(Into::into).collect_vec();
        ensure!(!options.is_empty(), "The options for a choice input must not be empty.");
        let default = default.map(Into::into);
        if let Some(default) = &default {
            ensure!(options.contains(default), "The default value  must be one of the options.");
        }
        Ok(Self {
            r#type: WorkflowDispatchInputType::Choice { default, options },
            ..Self::new(description, required)
        })
    }

    pub fn new_boolean(description: impl Into<String>, required: bool, default: bool) -> Self {
        Self {
            r#type: WorkflowDispatchInputType::Boolean { default: Some(default) },
            ..Self::new(description, required)
        }
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct WorkflowDispatch {
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub inputs: BTreeMap<String, WorkflowDispatchInput>,
}

impl WorkflowDispatch {
    pub fn add_input(
        &mut self,
        name: impl Into<String>,
        input: WorkflowDispatchInput,
    ) -> &mut Self {
        self.inputs.insert(name.into(), input);
        self
    }

    pub fn with_input<S: Into<String>>(mut self, name: S, input: WorkflowDispatchInput) -> Self {
        self.inputs.insert(name.into(), input);
        self
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "type")]
pub enum WorkflowCallInputType {
    String {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<String>,
    },
    Boolean {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<bool>,
    },
    Number {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<f64>,
    },
}

impl Default for WorkflowCallInputType {
    fn default() -> Self {
        Self::String { default: None }
    }
}

impl TryFrom<WorkflowDispatchInputType> for WorkflowCallInputType {
    type Error = anyhow::Error;

    fn try_from(value: WorkflowDispatchInputType) -> Result<Self> {
        Ok(match value {
            WorkflowDispatchInputType::String { default } => Self::String { default },
            WorkflowDispatchInputType::Boolean { default } => Self::Boolean { default },
            WorkflowDispatchInputType::Choice { default, .. } => Self::String { default },
            WorkflowDispatchInputType::Environment { .. } =>
                bail!("Environment is not supported for workflow call inputs!"),
        })
    }
}

/// Input to the workflow_call trigger.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct WorkflowCallInput {
    /// A string description of the input parameter.
    pub description: String,
    /// A boolean to indicate whether the action requires the input parameter.
    pub required:    bool,
    /// A string representing the type of the input.
    #[serde(flatten)]
    pub r#type:      WorkflowCallInputType,
}

impl TryFrom<WorkflowDispatchInput> for WorkflowCallInput {
    type Error = anyhow::Error;

    fn try_from(value: WorkflowDispatchInput) -> Result<Self> {
        Ok(Self {
            description: value.description,
            required:    value.required,
            r#type:      value.r#type.try_into()?,
        })
    }
}

/// A workflow call output.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct WorkflowCallOutput {
    /// A string description of the output parameter.
    pub description: String,
    /// Expression to defining the output parameter.
    pub value:       String,
}

/// A workflow call secret parameter.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WorkflowCallSecret {
    /// A string description of the secret parameter.
    pub description: String,
    /// A boolean specifying whether the secret must be supplied.
    pub required:    bool,
}

/// A workflow call trigger.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct WorkflowCall {
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub inputs:  BTreeMap<String, WorkflowCallInput>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub outputs: BTreeMap<String, WorkflowCallOutput>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub secrets: BTreeMap<String, WorkflowCallSecret>,
}

impl TryFrom<WorkflowDispatch> for WorkflowCall {
    type Error = anyhow::Error;

    fn try_from(value: WorkflowDispatch) -> Result<Self> {
        Ok(Self {
            inputs: value
                .inputs
                .into_iter()
                .map(|(k, v)| Result::Ok((k, v.try_into()?)))
                .try_collect()?,
            ..Default::default()
        })
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Event {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub push:              Option<Push>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pull_request:      Option<PullRequest>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub schedule:          Vec<Schedule>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workflow_dispatch: Option<WorkflowDispatch>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workflow_call:     Option<WorkflowCall>,
}

impl Event {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, push: Push) -> &mut Self {
        self.push = Some(push);
        self
    }

    pub fn pull_request(&mut self, pull_request: PullRequest) -> &mut Self {
        self.pull_request = Some(pull_request);
        self
    }

    pub fn schedule(&mut self, schedule: Schedule) -> &mut Self {
        self.schedule.push(schedule);
        self
    }

    pub fn workflow_dispatch(&mut self, workflow_dispatch: WorkflowDispatch) -> &mut Self {
        self.workflow_dispatch = Some(workflow_dispatch);
        self
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum JobSecrets {
    Inherit,
    Map(BTreeMap<String, String>),
}

/// Job is a top-level building block of a workflow.
///
/// It is scheduled to run on a specific runner. A workflow can have multiple jobs, they will run in
/// parallel by default.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Job {
    pub name:            String,
    #[serde(skip_serializing_if = "BTreeSet::is_empty")]
    pub needs:           BTreeSet<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#if:            Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub runs_on:         Vec<RunnerLabel>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub steps:           Vec<Step>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub concurrency:     Option<Concurrency>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub outputs:         BTreeMap<String, String>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub env:             BTreeMap<String, String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub strategy:        Option<Strategy>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout_minutes: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uses:            Option<String>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub with:            BTreeMap<String, String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub secrets:         Option<JobSecrets>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub permissions:     BTreeMap<Permission, Access>,
}

impl Job {
    /// Create a new job definition.
    pub fn new(
        name: impl Into<String>,
        runs_on: impl IntoIterator<Item: Into<RunnerLabel>>,
    ) -> Self {
        Self {
            name: name.into(),
            timeout_minutes: Some(DEFAULT_TIMEOUT_IN_MINUTES),
            runs_on: runs_on.into_iter().map(Into::into).collect(),
            ..Default::default()
        }
    }

    /// Add a step to this job, while exposing the step's outputs as job outputs.
    pub fn add_step_with_output(
        &mut self,
        mut step: Step,
        outputs: impl IntoIterator<Item: Into<String>>,
    ) {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        // A step must have an unique id if we want to access its output.
        let id =
            step.id.unwrap_or_else(|| format!("step_{}", COUNTER.fetch_add(1, Ordering::SeqCst)));
        step.id = Some(id.clone());
        self.steps.push(step);
        for output in outputs {
            self.expose_output(&id, output);
        }
    }

    /// Expose a step's output as a job output.
    pub fn expose_output(&mut self, step_id: impl AsRef<str>, output_name: impl Into<String>) {
        let step = step_id.as_ref();
        let output = output_name.into();
        let value = format!("${{{{ steps.{step}.outputs.{output} }}}}");
        if let Some(old_value) = self.outputs.insert(output.clone(), value) {
            warn!(
                "Overwriting output `{}` of job `{}` with value `{}`",
                output, self.name, old_value
            );
        }
    }

    /// Define an environment variable for this job, it will be available to all steps.
    pub fn env(&mut self, name: impl Into<String>, value: impl Into<String>) {
        self.env.insert(name.into(), value.into());
    }

    /// Expose a secret as environment variable for this job.
    pub fn expose_secret_as(&mut self, secret: impl AsRef<str>, given_name: impl Into<String>) {
        self.env(given_name, format!("${{{{ secrets.{} }}}}", secret.as_ref()));
    }

    /// Expose outputs of another job as environment variables in this job.
    ///
    /// This also adds a dependency on the other job.
    pub fn use_job_outputs(&mut self, job_id: impl Into<String>, job: &Job) {
        let job_id = job_id.into();
        for output_name in job.outputs.keys() {
            let reference = wrap_expression(format!("needs.{job_id}.outputs.{output_name}"));
            self.env.insert(output_name.into(), reference);
        }
        self.needs(job_id);
    }

    /// Add a dependency on another job.
    pub fn needs(&mut self, job_id: impl Into<String>) {
        self.needs.insert(job_id.into());
    }

    /// Set an input for the invoked reusable workflow.
    ///
    /// This is only valid if the job uses a reusable workflow.
    pub fn with(&mut self, name: impl Into<String>, value: impl Into<String>) {
        self.with.insert(name.into(), value.into());
    }

    /// Like [`with`](Self::with), but self-consuming.
    pub fn with_with(mut self, name: impl Into<String>, value: impl Into<String>) -> Self {
        self.with(name, value);
        self
    }

    /// Apply custom permissions to this job.
    pub fn with_permission(mut self, permission: Permission, access: Access) -> Self {
        self.permissions.insert(permission, access);
        self
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Strategy {
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub matrix:    BTreeMap<String, serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fail_fast: Option<bool>,
}

impl Strategy {
    pub fn new(
        matrix_entries: impl IntoIterator<
            Item = (impl Into<String>, impl IntoIterator<Item: Serialize>),
        >,
    ) -> Result<Self> {
        let mut ret = Self::default();
        for (key, value) in matrix_entries {
            ret.insert_to_matrix(key, value)?;
        }
        Ok(ret)
    }

    pub fn fail_fast(mut self, fail_fast: bool) -> Self {
        self.fail_fast = Some(fail_fast);
        self
    }

    pub fn insert_to_matrix(
        &mut self,
        name: impl Into<String>,
        values: impl IntoIterator<Item: Serialize>,
    ) -> Result<&mut Self> {
        let values = values.into_iter().map(serde_json::to_value).try_collect()?;
        self.matrix.insert(name.into(), serde_json::Value::Array(values));
        Ok(self)
    }

    pub fn new_os(values: impl IntoIterator<Item: Serialize>) -> Result<Strategy> {
        let mut matrix = Self { fail_fast: Some(false), ..default() };
        matrix.insert_to_matrix(MATRIX_STRATEGY_OS, values)?;
        Ok(matrix)
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Step {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id:                Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#if:              Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name:              Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uses:              Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub run:               Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub shell:             Option<Shell>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub with:              Option<step::Argument>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub env:               BTreeMap<String, String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub continue_on_error: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout_minutes:   Option<u32>,
}

impl Step {
    /// Expose a secret as an environment variable with the same name.
    pub fn with_secret_exposed(self, secret: impl AsRef<str>) -> Self {
        let secret_name = secret.as_ref();
        let env_name = secret_name.to_owned();
        self.with_secret_exposed_as(secret_name, env_name)
    }

    /// Expose [a variable](https://docs.github.com/en/actions/learn-github-actions/variables) as an environment variable with the same name.
    pub fn with_variable_exposed(self, variable: impl AsRef<str>) -> Self {
        let variable_name = variable.as_ref();
        let env_name = variable_name.to_owned();
        self.with_variable_exposed_as(variable_name, env_name)
    }

    /// Expose [a variable](https://docs.github.com/en/actions/learn-github-actions/variables) as an environment variable with a given name.
    pub fn with_variable_exposed_as(
        self,
        variable: impl AsRef<str>,
        given_name: impl Into<String>,
    ) -> Self {
        let variable_expr = variable_expression(variable);
        self.with_env(given_name, variable_expr)
    }

    /// Expose a secret as an environment variable with a given name.
    pub fn with_input_exposed_as(
        self,
        input: impl AsRef<str>,
        given_name: impl Into<String>,
    ) -> Self {
        let input_expr = get_input_expression(input.as_ref());
        self.with_env(given_name, input_expr)
    }

    pub fn with_secret_exposed_as(
        self,
        secret: impl AsRef<str>,
        given_name: impl Into<String>,
    ) -> Self {
        let secret_expr = secret_expression(&secret);
        self.with_env(given_name, secret_expr)
    }

    pub fn with_env(mut self, name: impl Into<String>, value: impl Into<String>) -> Self {
        let name = name.into();
        let value = value.into();
        let entry = self.env.entry(name);
        if let Entry::Occupied(mut entry) = entry {
            warn!(
                "Overriding environment variable `{}` with value `{}` (old value was `{}`)",
                entry.key(),
                value,
                entry.get(),
            );
            *entry.get_mut() = value;
        } else {
            entry.or_insert(value);
        }
        self
    }

    pub fn with_if(mut self, condition: impl Into<String>) -> Self {
        self.r#if = Some(condition.into());
        self
    }

    pub fn with_id(mut self, id: impl Into<String>) -> Self {
        self.id = Some(id.into());
        self
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn with_custom_argument(
        mut self,
        name: impl Into<String>,
        value: impl Into<serde_yaml::Value>,
    ) -> Self {
        match &mut self.with {
            Some(step::Argument::Other(map)) => {
                map.insert(name.into(), value.into());
            }
            _ => {
                if let Some(previous) = self.with {
                    warn!("Dropping previous step argument: {:?}", previous);
                }
                self.with = Some(step::Argument::new_other(name, value));
            }
        }
        self
    }
}

pub fn github_token_env() -> (String, String) {
    ("GITHUB_TOKEN".into(), "${{ secrets.GITHUB_TOKEN }}".into())
}

impl IntoIterator for Step {
    type Item = Step;
    type IntoIter = std::iter::Once<Self>;
    fn into_iter(self) -> Self::IntoIter {
        once(self)
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Shell {
    /// Command Prompt.
    Cmd,
    Bash,
    /// Power Shell.
    Pwsh,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum CheckoutArgumentSubmodules {
    True,
    False,
    Recursive,
}

pub mod step {
    use super::*;
    use crate::github;


    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(rename_all = "kebab-case")]
    #[serde(untagged)]
    pub enum Argument {
        #[serde(rename_all = "kebab-case")]
        Checkout {
            #[serde(
                skip_serializing_if = "Option::is_none",
                with = "crate::serde::via_string_opt"
            )]
            repository: Option<github::Repo>,
            #[serde(skip_serializing_if = "Option::is_none")]
            clean:      Option<bool>,
            #[serde(skip_serializing_if = "Option::is_none")]
            submodules: Option<CheckoutArgumentSubmodules>,
        },
        #[serde(rename_all = "kebab-case")]
        GitHubScript {
            script: String,
        },
        Other(BTreeMap<String, serde_yaml::Value>),
    }

    impl Argument {
        pub fn new_other(name: impl Into<String>, value: impl Into<serde_yaml::Value>) -> Self {
            Argument::Other(BTreeMap::from_iter([(name.into(), value.into())]))
        }
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum RunnerLabel {
    #[serde(rename = "self-hosted")]
    SelfHosted,
    #[serde(rename = "macOS")]
    MacOS,
    #[serde(rename = "Linux")]
    Linux,
    #[serde(rename = "Windows")]
    Windows,
    #[serde(rename = "engine")]
    Engine,
    #[serde(rename = "macos-13")]
    MacOS13,
    #[serde(rename = "macos-latest")]
    MacOSLatest,
    #[serde(rename = "ubuntu-latest")]
    LinuxLatest,
    #[serde(rename = "windows-latest")]
    WindowsLatest,
    #[serde(rename = "X64")]
    X64,
    #[serde(rename = "ARM64")]
    Arm64,
    #[serde(rename = "benchmark")]
    Benchmark,
    #[serde(rename = "metarunner")]
    Metarunner,
    #[serde(rename = "${{ matrix.os }}")] // Must be in sync with [`MATRIX_STRATEGY_OS`].
    MatrixOs,
}

pub fn checkout_repo_step_customized(f: impl FnOnce(Step) -> Step) -> Vec<Step> {
    let actual_checkout = Step {
        name: Some("Checking out the repository".into()),
        uses: Some("actions/checkout@v4".into()),
        with: Some(step::Argument::Checkout {
            repository: None,
            clean:      Some(false),
            submodules: Some(CheckoutArgumentSubmodules::Recursive),
        }),
        ..default()
    };
    // Apply customization.
    let actual_checkout = f(actual_checkout);
    vec![actual_checkout]
}

/// See [`checkout_repo_step_customized`].
pub fn checkout_repo_step() -> impl IntoIterator<Item = Step> {
    checkout_repo_step_customized(identity)
}

pub trait JobArchetype {
    fn id_key_base(&self) -> String {
        std::any::type_name::<Self>().to_kebab_case()
    }

    fn key(&self, (os, arch): Target) -> String {
        format!("{}-{os}-{arch}", self.id_key_base())
    }

    fn job(&self, target: Target) -> Job;

    fn entry(&self, target: Target) -> (String, Job) {
        (self.key(target), self.job(target))
    }

    // [Step ID] => [variable names]
    fn outputs(&self) -> BTreeMap<String, Vec<String>> {
        default()
    }

    fn expose_outputs(&self, job: &mut Job) {
        for (step_id, outputs) in self.outputs() {
            for output in outputs {
                job.expose_output(&step_id, output);
            }
        }
    }
}

/// Describes the workflow to be stored on the disk.
#[derive(Clone, Debug)]
pub struct WorkflowToWrite {
    /// The workflow to be stored.
    pub workflow: Workflow,
    /// The path where the workflow should be stored.
    pub path:     PathBuf,
    /// Who generated this workflow.
    pub source:   String,
}
