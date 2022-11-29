use crate::prelude::*;

use crate::env::new::RawVariable;

use heck::ToKebabCase;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::collections::BTreeSet;



pub const DEFAULT_TIMEOUT_IN_MINUTES: u32 = 360;

pub fn wrap_expression(expression: impl AsRef<str>) -> String {
    format!("${{{{ {} }}}}", expression.as_ref())
}

pub fn env_expression(environment_variable: &impl RawVariable) -> String {
    wrap_expression(format!("env.{}", environment_variable.name()))
}


pub fn is_github_hosted() -> String {
    "startsWith(runner.name, 'GitHub Actions') || startsWith(runner.name, 'Hosted Agent')".into()
}

pub fn setup_conda() -> Step {
    // use crate::actions::workflow::definition::step::CondaChannel;
    Step {
        name: Some("Setup conda (GH runners only)".into()),
        uses: Some("s-weigand/setup-conda@v1.0.5".into()),
        r#if: Some(is_github_hosted()),
        with: Some(step::Argument::SetupConda {
            update_conda:   Some(false),
            conda_channels: Some("anaconda, conda-forge".into()),
        }),
        ..default()
    }
}

pub fn setup_wasm_pack_step() -> Step {
    Step {
        name: Some("Installing wasm-pack".into()),
        uses: Some("jetli/wasm-pack-action@v0.3.0".into()),
        with: Some(step::Argument::Other(BTreeMap::from_iter([(
            "version".into(),
            "v0.10.2".into(),
        )]))),
        r#if: Some(is_github_hosted()),
        ..default()
    }
}

pub fn github_script_step(name: impl Into<String>, script: impl Into<String>) -> Step {
    Step {
        name: Some(name.into()),
        uses: Some("actions/github-script@v6".into()),
        with: Some(step::Argument::GitHubScript { script: script.into() }),
        ..default()
    }
}

pub fn setup_artifact_api() -> Step {
    let script = r#"
    core.exportVariable("ACTIONS_RUNTIME_TOKEN", process.env["ACTIONS_RUNTIME_TOKEN"])
    core.exportVariable("ACTIONS_RUNTIME_URL", process.env["ACTIONS_RUNTIME_URL"])
    core.exportVariable("GITHUB_RETENTION_DAYS", process.env["GITHUB_RETENTION_DAYS"])
    console.log(context)
    "#;
    github_script_step("Expose Artifact API and context information.", script)
}

pub fn is_windows_runner() -> String {
    "runner.os == 'Windows'".into()
}

pub fn is_non_windows_runner() -> String {
    "runner.os != 'Windows'".into()
}

pub fn shell_os(os: OS, command_line: impl Into<String>) -> Step {
    Step {
        run: Some(command_line.into()),
        env: once(github_token_env()).collect(),
        r#if: Some(format!("runner.os {} 'Windows'", if os == OS::Windows { "==" } else { "!=" })),
        shell: Some(if os == OS::Windows { Shell::Pwsh } else { Shell::Bash }),
        ..default()
    }
}

pub fn shell(command_line: impl Into<String>) -> Step {
    Step {
        run: Some(command_line.into()),
        env: once(github_token_env()).collect(),
        timeout_minutes: Some(DEFAULT_TIMEOUT_IN_MINUTES),
        ..default()
    }
}

/// Invoke our entry point to the build scripts, i.e. the `./run` script.
pub fn run(run_args: impl AsRef<str>) -> Step {
    shell(format!("./run {}", run_args.as_ref()))
}

pub fn cancel_workflow_action() -> Step {
    Step {
        name: Some("Cancel Previous Runs".into()),
        uses: Some("styfle/cancel-workflow-action@0.9.1".into()),
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
    Map { group: String, cancel_in_progress: bool },
}

impl Concurrency {
    pub fn new(group_name: impl Into<String>) -> Self {
        Self::Plain(group_name.into())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Workflow {
    pub name:        String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub on:          Event,
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

    pub fn expose_outputs(&self, source_job_id: impl AsRef<str>, consumer_job: &mut Job) {
        let source_job = self.jobs.get(source_job_id.as_ref()).unwrap();
        consumer_job.use_job_outputs(source_job_id.as_ref(), source_job);
    }
}

impl Workflow {
    pub fn add_job(&mut self, job: Job) -> String {
        let key = job.name.to_kebab_case();
        self.jobs.insert(key.clone(), job);
        key
    }

    pub fn add<J: JobArchetype>(&mut self, os: OS) -> String {
        self.add_customized::<J>(os, |_| {})
    }

    pub fn add_customized<J: JobArchetype>(&mut self, os: OS, f: impl FnOnce(&mut Job)) -> String {
        let (key, mut job) = J::entry(os);
        f(&mut job);
        self.jobs.insert(key.clone(), job);
        key
    }

    pub fn add_dependent<J: JobArchetype>(
        &mut self,
        os: OS,
        needed: impl IntoIterator<Item: AsRef<str>>,
    ) -> String {
        let (key, mut job) = J::entry(os);
        for needed in needed {
            self.expose_outputs(needed.as_ref(), &mut job);
        }
        self.jobs.insert(key.clone(), job);
        key
    }

    pub fn env(&mut self, var_name: impl Into<String>, var_value: impl Into<String>) {
        self.env.insert(var_name.into(), var_value.into());
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
/// See: https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#onpull_requestpull_request_targetbranchesbranches-ignore
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

/// See: https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows#pull_request
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
        choices: Vec<String>, // should be non-empty
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
        default: impl Into<String>,
    ) -> Self {
        Self {
            r#type: WorkflowDispatchInputType::String { default: Some(default.into()) },
            ..Self::new(description, required)
        }
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

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct Event {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub push:              Option<Push>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pull_request:      Option<PullRequest>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub schedule:          Vec<Schedule>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workflow_dispatch: Option<WorkflowDispatch>,
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

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Job {
    pub name:            String,
    #[serde(skip_serializing_if = "BTreeSet::is_empty")]
    pub needs:           BTreeSet<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#if:            Option<String>,
    pub runs_on:         Vec<RunnerLabel>,
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
}

impl Job {
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into(), timeout_minutes: Some(DEFAULT_TIMEOUT_IN_MINUTES), ..default() }
    }

    pub fn expose_output(&mut self, step_id: impl AsRef<str>, output_name: impl Into<String>) {
        let step = step_id.as_ref();
        let output = output_name.into();
        let value = format!("${{{{ steps.{step}.outputs.{output} }}}}");
        self.outputs.insert(output, value);
    }

    pub fn env(&mut self, name: impl Into<String>, value: impl Into<String>) {
        self.env.insert(name.into(), value.into());
    }

    pub fn expose_secret_as(&mut self, secret: impl AsRef<str>, given_name: impl Into<String>) {
        self.env(given_name, format!("${{{{ secrets.{} }}}}", secret.as_ref()));
    }

    pub fn use_job_outputs(&mut self, job_id: impl Into<String>, job: &Job) {
        let job_id = job_id.into();
        for output_name in job.outputs.keys() {
            let reference = format!("${{{{needs.{}.outputs.{}}}}}", job_id, output_name);
            self.env.insert(output_name.into(), reference);
        }
        self.needs(job_id);
    }

    pub fn needs(&mut self, job_id: impl Into<String>) {
        self.needs.insert(job_id.into());
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
        let values = values.into_iter().map(serde_json::to_value).try_collect_vec()?;
        self.matrix.insert(name.into(), serde_json::Value::Array(values));
        Ok(self)
    }

    pub fn new_os(labels: impl Serialize) -> Strategy {
        let oses = serde_json::to_value(labels).unwrap();
        Strategy {
            fail_fast: Some(false),
            matrix:    [("os".to_string(), oses)].into_iter().collect(),
        }
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
    pub fn with_secret_exposed_as(
        self,
        secret: impl AsRef<str>,
        given_name: impl Into<String>,
    ) -> Self {
        let secret_expr = wrap_expression(format!("secrets.{}", secret.as_ref()));
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


    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(rename_all = "kebab-case")]
    #[serde(untagged)]
    pub enum Argument {
        #[serde(rename_all = "kebab-case")]
        Checkout {
            #[serde(skip_serializing_if = "Option::is_none")]
            clean:      Option<bool>,
            #[serde(skip_serializing_if = "Option::is_none")]
            submodules: Option<CheckoutArgumentSubmodules>,
        },
        #[serde(rename_all = "kebab-case")]
        SetupConda {
            #[serde(skip_serializing_if = "Option::is_none")]
            update_conda:   Option<bool>,
            #[serde(skip_serializing_if = "Option::is_none")]
            conda_channels: Option<String>, // conda_channels: Vec<CondaChannel>
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
    #[serde(rename = "macos-latest")]
    MacOSLatest,
    #[serde(rename = "ubuntu-latest")]
    LinuxLatest,
    #[serde(rename = "windows-latest")]
    WindowsLatest,
    #[serde(rename = "X64")]
    X64,
    #[serde(rename = "mwu-deluxe")]
    MwuDeluxe,
    #[serde(rename = "benchmark")]
    Benchmark,
    #[serde(rename = "${{ matrix.os }}")]
    MatrixOs,
}

pub fn checkout_repo_step() -> impl IntoIterator<Item = Step> {
    // This is a workaround for a bug in GH actions/checkout. If a submodule is added and removed,
    // it effectively breaks any future builds of this repository on a given self-hosted runner.
    // The workaround step below comes from:
    // https://github.com/actions/checkout/issues/590#issuecomment-970586842
    //
    // As an exception to general rule, we use here bash even on Windows. As the bash us the one
    // coming from a git installation, we can assume that git works nicely with it.
    // Having this rewritten to github-script might have been nicer but it does not seem
    // effort-worthy.
    //
    // See:
    // https://github.com/actions/checkout/issues/590
    // https://github.com/actions/checkout/issues/788
    // and many other duplicate reports.
    let git_bash_command = "git checkout -f $(git -c user.name=x -c user.email=x@x commit-tree $(git hash-object -t tree /dev/null) < /dev/null) || :";
    let submodules_workaround_win = Step {
        // We can't add git-bash to PATH because this would break the Rust build.
        // Instead we manually spawn the bash with a given command from CMD shell.
        run: Some(format!(r#""c:\Program Files\Git\bin\bash.exe" -c "{}""#, git_bash_command)),
        shell: Some(Shell::Cmd),
        r#if: Some(is_windows_runner()),
        name: Some(
            "Workaround for https://github.com/actions/checkout/issues/590 (Windows)".into(),
        ),
        ..default()
    };
    let submodules_workaround_linux = Step {
        run: Some(git_bash_command.into()),
        shell: Some(Shell::Bash),
        r#if: Some(is_non_windows_runner()),
        name: Some(
            "Workaround for  https://github.com/actions/checkout/issues/590 (non-Windows)".into(),
        ),
        ..default()
    };
    let actual_checkout = Step {
        name: Some("Checking out the repository".into()),
        // FIXME: Check what is wrong with v3. Seemingly Engine Tests fail because there's only a
        //        shallow copy of the repo.
        uses: Some("actions/checkout@v2".into()),
        with: Some(step::Argument::Checkout {
            clean:      Some(false),
            submodules: Some(CheckoutArgumentSubmodules::Recursive),
        }),
        ..default()
    };
    [submodules_workaround_win, submodules_workaround_linux, actual_checkout]
}

pub trait JobArchetype {
    fn id_key_base() -> String {
        std::any::type_name::<Self>().to_kebab_case()
    }

    fn key(os: OS) -> String {
        format!("{}-{}", Self::id_key_base(), os)
    }

    fn job(os: OS) -> Job;

    fn entry(os: OS) -> (String, Job) {
        (Self::key(os), Self::job(os))
    }

    // [Step ID] => [variable names]
    fn outputs() -> BTreeMap<String, Vec<String>> {
        default()
    }

    fn expose_outputs(job: &mut Job) {
        for (step_id, outputs) in Self::outputs() {
            for output in outputs {
                job.expose_output(&step_id, output);
            }
        }
    }
}
