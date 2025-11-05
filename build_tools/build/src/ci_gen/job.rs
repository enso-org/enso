use crate::prelude::*;

use crate::ci_gen::not_default_branch;
use crate::ci_gen::runs_on;
use crate::ci_gen::secret;
use crate::ci_gen::step;
use crate::ci_gen::variables;
use crate::ci_gen::RunStepsBuilder;
use crate::ci_gen::RunnerType;
use crate::ci_gen::RELEASE_CLEANING_POLICY;
use crate::engine;
use crate::ide;
use crate::paths;

use core::panic;
use ide_ci::actions::workflow::definition::cancel_workflow_action;
use ide_ci::actions::workflow::definition::checkout_repo_step;
use ide_ci::actions::workflow::definition::shell;
use ide_ci::actions::workflow::definition::step::Argument;
use ide_ci::actions::workflow::definition::Access;
use ide_ci::actions::workflow::definition::Job;
use ide_ci::actions::workflow::definition::JobArchetype;
use ide_ci::actions::workflow::definition::Permission;
use ide_ci::actions::workflow::definition::RunnerLabel;
use ide_ci::actions::workflow::definition::Shell;
use ide_ci::actions::workflow::definition::Step;
use ide_ci::actions::workflow::definition::Strategy;
use ide_ci::actions::workflow::definition::Target;
use ide_ci::cache::goodie::graalvm;
use ide_ci::convert_case::ToKebabCase;

/// Target runners set (or just a single runner) for a job.
pub trait RunsOn: 'static + Debug {
    /// A strategy that will be used for the job.
    ///
    /// Needs to be customized only for matrix jobs.
    fn strategy(&self) -> Option<Strategy> {
        None
    }

    /// Labels required on the runner to run this job.
    fn runs_on(&self) -> Vec<RunnerLabel>;

    /// A name that will be added to the job name.
    ///
    /// Should not be used if there is per-os matrix.
    fn job_name_suffix(&self) -> Option<String> {
        None
    }
}

impl RunsOn for RunnerLabel {
    fn runs_on(&self) -> Vec<RunnerLabel> {
        vec![*self]
    }

    fn job_name_suffix(&self) -> Option<String> {
        match self {
            RunnerLabel::MacOS => Some("MacOS".to_string()),
            RunnerLabel::Linux => Some("Linux".to_string()),
            RunnerLabel::Windows => Some("Windows".to_string()),
            RunnerLabel::MacOS13 => Some("MacOS13".to_string()),
            RunnerLabel::MacOSLatest => Some("MacOSLatest".to_string()),
            RunnerLabel::LinuxLatest => Some("LinuxLatest".to_string()),
            RunnerLabel::WindowsLatest => Some("WindowsLatest".to_string()),
            // Other labels are not OS-specific, so None.
            RunnerLabel::SelfHosted
            | RunnerLabel::Engine
            | RunnerLabel::X64
            | RunnerLabel::Arm64
            | RunnerLabel::Benchmark
            | RunnerLabel::Metarunner
            | RunnerLabel::MatrixOs => None,
        }
    }
}

impl RunsOn for OS {
    fn runs_on(&self) -> Vec<RunnerLabel> {
        (*self, Arch::X86_64).runs_on()
    }
    fn job_name_suffix(&self) -> Option<String> {
        Some(self.to_string())
    }
}

impl RunsOn for (OS, Arch) {
    fn runs_on(&self) -> Vec<RunnerLabel> {
        match self {
            (os, Arch::X86_64) => runs_on(*os, RunnerType::SelfHosted),
            (OS::MacOS, Arch::AArch64) => {
                let mut ret = runs_on(OS::MacOS, RunnerType::SelfHosted);
                ret.push(RunnerLabel::Arm64);
                ret
            }
            _ => panic!("Unsupported OS/arch combination: {self:?}"),
        }
    }

    fn job_name_suffix(&self) -> Option<String> {
        Some(format!("{}, {}", self.0, self.1))
    }
}

pub fn plain_job(
    runs_on: impl RunsOn,
    name: impl Into<String>,
    command_line: impl Into<String>,
) -> Job {
    RunStepsBuilder::new(command_line).build_job(name, runs_on)
}

/// Pretty print arguments to `./run` that will invoke SBT with the given command.
///
/// Meant to be used together with [`RunStepsBuilder::new`].
///
/// ```
/// use enso_build::ci_gen::job::sbt_command;
/// assert_eq!(sbt_command("verifyLicensePackages"), "backend sbt '--' verifyLicensePackages");
/// ```
pub fn sbt_command(command: impl AsRef<str>) -> String {
    // Note that we put -- in quotes to avoid issues with powershell (which is default on Windows).
    // Otherwise, pwsh would handle `--` by itself, rather than passing it to build script's args.
    // See: https://stackoverflow.com/questions/15780174/powershell-command-line-parameters-and
    format!("backend sbt '--' {}", command.as_ref())
}

/// Expose variables for the GUI build.
pub fn expose_gui_vars(step: Step) -> Step {
    step.with_variable_exposed_as(
        variables::ENSO_CLOUD_ENVIRONMENT,
        ide::web::env::ENSO_IDE_ENVIRONMENT,
    )
    .with_variable_exposed_as(variables::ENSO_CLOUD_API_URL, ide::web::env::ENSO_IDE_API_URL)
    .with_variable_exposed_as(variables::ENSO_CLOUD_CHAT_URL, ide::web::env::ENSO_IDE_CHAT_URL)
    .with_variable_exposed_as(variables::ENSO_CLOUD_SENTRY_DSN, ide::web::env::ENSO_IDE_SENTRY_DSN)
    .with_variable_exposed_as(variables::ENSO_CLOUD_STRIPE_KEY, ide::web::env::ENSO_IDE_STRIPE_KEY)
    .with_variable_exposed_as(
        variables::ENSO_CLOUD_AUTH_ENDPOINT,
        ide::web::env::ENSO_IDE_AUTH_ENDPOINT,
    )
    .with_variable_exposed_as(
        variables::ENSO_CLOUD_COGNITO_USER_POOL_ID,
        ide::web::env::ENSO_IDE_COGNITO_USER_POOL_ID,
    )
    .with_variable_exposed_as(
        variables::ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID,
        ide::web::env::ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID,
    )
    .with_variable_exposed_as(
        variables::ENSO_CLOUD_COGNITO_DOMAIN,
        ide::web::env::ENSO_IDE_COGNITO_DOMAIN,
    )
    .with_variable_exposed_as(
        variables::ENSO_CLOUD_COGNITO_REGION,
        ide::web::env::ENSO_IDE_COGNITO_REGION,
    )
    .with_variable_exposed_as(
        variables::ENSO_CLOUD_GOOGLE_ANALYTICS_TAG,
        ide::web::env::ENSO_IDE_GOOGLE_ANALYTICS_TAG,
    )
    .with_variable_exposed_as(
        variables::ENSO_AG_GRID_LICENSE_KEY,
        ide::web::env::ENSO_IDE_AG_GRID_LICENSE_KEY,
    )
    .with_variable_exposed_as(
        variables::ENSO_MAPBOX_API_TOKEN,
        ide::web::env::ENSO_IDE_MAPBOX_API_TOKEN,
    )
    .with_secret_exposed_as(
        secret::ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID,
        ide::web::env::ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID,
    )
    .with_secret_exposed_as(
        secret::ENSO_IDE_STRAVA_OAUTH_CLIENT_ID,
        ide::web::env::ENSO_IDE_STRAVA_OAUTH_CLIENT_ID,
    )
    .with_secret_exposed_as(
        secret::ENSO_IDE_MS365_OAUTH_CLIENT_ID,
        ide::web::env::ENSO_IDE_MS365_OAUTH_CLIENT_ID,
    )
}

/// Expose variables for debugging purposes.
pub fn expose_debugging_vars(step: Step) -> Step {
    step.with_secret_exposed(secret::SENTRY_AUTH_TOKEN)
        .with_variable_exposed_as(
            variables::ENSO_CLOUD_SENTRY_ORGANIZATION,
            ide::web::env::ENSO_IDE_SENTRY_ORGANIZATION,
        )
        .with_variable_exposed_as(
            variables::ENSO_CLOUD_SENTRY_PROJECT,
            ide::web::env::ENSO_IDE_SENTRY_PROJECT,
        )
}

#[derive(Clone, Copy, Debug)]
pub struct CancelWorkflow;

impl JobArchetype for CancelWorkflow {
    fn job(&self, _target: Target) -> Job {
        Job {
            name: "Cancel Previous Runs".into(),
            // It is important that this particular job runs pretty much everywhere (we use x64,
            // as all currently available GH runners have this label). If we limited it only to
            // our self-hosted machines (as we usually do), it'd be enqueued after other jobs
            // and wouldn't be able to cancel them.
            runs_on: vec![RunnerLabel::LinuxLatest],
            steps: vec![cancel_workflow_action()],
            r#if: Some(not_default_branch()),
            ..default()
        }
        // Necessary permission to cancel a run, as per:
        // https://docs.github.com/en/rest/actions/workflow-runs?apiVersion=2022-11-28#cancel-a-workflow-run
        .with_permission(Permission::Actions, Access::Write)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VerifyLicensePackages;
impl JobArchetype for VerifyLicensePackages {
    fn id_key_base(&self) -> String {
        "license-check".to_string()
    }

    fn job(&self, target: Target) -> Job {
        RunStepsBuilder::new(sbt_command("verifyLicensePackages"))
            .build_job("Verify License Packages", target)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct JvmTests {
    pub graal_edition: graalvm::Edition,
    pub engine_launcher: engine::EngineLauncher,
}

impl JobArchetype for JvmTests {
    fn job(&self, target: Target) -> Job {
        let graal_edition = self.graal_edition;
        let engine_launcher = self.engine_launcher;
        let job_name = format!("JVM Tests ({graal_edition})");
        let mut job = RunStepsBuilder::new("backend test jvm")
            .customize(move |step| {
                let cleanup_engine_distribution =
                    step::cleanup_engine_distribution(engine_launcher);

                let download_engine_distribution =
                    step::download_engine_distribution(target, engine_launcher, graal_edition);

                vec![
                    cleanup_engine_distribution,
                    download_engine_distribution,
                    step::check_engine_distribution(),
                    step::unpack_engine_distribution(),
                    step,
                    step::engine_test_reporter(target, graal_edition),
                ]
            })
            .build_job(job_name, target)
            .with_permission(Permission::Checks, Access::Write);
        match graal_edition {
            graalvm::Edition::Community => {
                job.env(engine::env::GRAAL_EDITION, graalvm::Edition::Community)
            }
            graalvm::Edition::Enterprise => {
                job.env(engine::env::GRAAL_EDITION, graalvm::Edition::Enterprise)
            }
        }
        job
    }

    fn key(&self, (os, arch): Target) -> String {
        format!(
            "{}-{}-{os}-{arch}",
            self.id_key_base(),
            self.graal_edition.to_string().to_kebab_case()
        )
    }
}

fn enable_cloud_tests(step: Step) -> Step {
    step.with_variable_exposed_as(
        secret::ENSO_CLOUD_COGNITO_USER_POOL_ID,
        crate::cloud_tests::env::ci_config::ENSO_CLOUD_COGNITO_USER_POOL_ID,
    )
    .with_variable_exposed_as(
        secret::ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID,
        crate::cloud_tests::env::ci_config::ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID,
    )
    .with_variable_exposed_as(
        secret::ENSO_CLOUD_COGNITO_REGION,
        crate::cloud_tests::env::ci_config::ENSO_CLOUD_COGNITO_REGION,
    )
    .with_secret_exposed_as(
        secret::ENSO_CLOUD_TEST_ACCOUNT_USERNAME,
        crate::cloud_tests::env::ci_config::ENSO_CLOUD_TEST_ACCOUNT_USERNAME,
    )
    .with_secret_exposed_as(
        secret::ENSO_CLOUD_TEST_ACCOUNT_PASSWORD,
        crate::cloud_tests::env::ci_config::ENSO_CLOUD_TEST_ACCOUNT_PASSWORD,
    )
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum StandardLibraryTestsScope {
    CloudRelated,
    StandardLibraryJvm,
    StandardLibraryInNative,
    Microsoft,
}

impl Display for StandardLibraryTestsScope {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            StandardLibraryTestsScope::CloudRelated => write!(f, "std-cloud-related"),
            StandardLibraryTestsScope::StandardLibraryJvm => write!(f, "standard-library"),
            StandardLibraryTestsScope::StandardLibraryInNative => {
                write!(f, "standard-library-in-native")
            }
            StandardLibraryTestsScope::Microsoft => {
                write!(f, "std-microsoft std-mock-dual-microsoft")
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StandardLibraryTests {
    pub graal_edition: graalvm::Edition,
    pub engine_launcher: engine::EngineLauncher,
    pub scope: StandardLibraryTestsScope,
}

impl StandardLibraryTests {
    fn title(&self) -> String {
        let title = match self.scope {
            StandardLibraryTestsScope::StandardLibraryJvm => "Standard Library JVM Tests",
            StandardLibraryTestsScope::StandardLibraryInNative => "Standard Library Native Tests",
            StandardLibraryTestsScope::Microsoft => "Standard Library Microsoft Tests",
            StandardLibraryTestsScope::CloudRelated => "Standard Library Cloud Tests",
        };
        title.to_string()
    }
}

impl JobArchetype for StandardLibraryTests {
    fn id_key_base(&self) -> String {
        "stdlib".to_string()
    }

    fn job(&self, target: Target) -> Job {
        let graal_edition = self.graal_edition;
        let engine_launcher = self.engine_launcher;
        let scope = self.scope;
        let job_name = format!("{job_title} ({graal_edition})", job_title = self.title());
        let run_command = format!("backend test {scope}");
        let heapdump_artifact_name =
            format!("Heap dumps ({}, {}, {})", &self.title(), target.0, target.1);

        let run_steps_builder = RunStepsBuilder::new(run_command).customize(move |step| {
            let cleanup_engine_distribution = step::cleanup_engine_distribution(engine_launcher);

            let download_engine_distribution =
                step::download_engine_distribution(target, engine_launcher, graal_edition);

            let main_step = step
                .with_secret_exposed_as(
                    secret::ENSO_LIB_S3_AWS_REGION,
                    crate::libraries_tests::s3::env::ENSO_LIB_S3_AWS_REGION,
                )
                .with_secret_exposed_as(
                    secret::ENSO_LIB_S3_AWS_ACCESS_KEY_ID,
                    crate::libraries_tests::s3::env::ENSO_LIB_S3_AWS_ACCESS_KEY_ID,
                )
                .with_secret_exposed_as(
                    secret::ENSO_LIB_S3_AWS_SECRET_ACCESS_KEY,
                    crate::libraries_tests::s3::env::ENSO_LIB_S3_AWS_SECRET_ACCESS_KEY,
                );

            let updated_main_step = if scope == StandardLibraryTestsScope::CloudRelated {
                enable_cloud_tests(main_step)
            } else {
                main_step
            };
            let upload_hprof = step::heapdump_upload(heapdump_artifact_name);

            vec![
                cleanup_engine_distribution,
                download_engine_distribution,
                step::check_engine_distribution(),
                step::unpack_engine_distribution(),
                updated_main_step,
                step::stdlib_test_reporter(target, graal_edition),
                upload_hprof,
            ]
        });
        let mut job = build_job_ensuring_cloud_tests_run_on_github(
            run_steps_builder,
            target,
            &job_name,
            self.scope,
        )
        .with_permission(Permission::Checks, Access::Write);
        match graal_edition {
            graalvm::Edition::Community => {
                job.env(engine::env::GRAAL_EDITION, graalvm::Edition::Community)
            }
            graalvm::Edition::Enterprise => {
                job.env(engine::env::GRAAL_EDITION, graalvm::Edition::Enterprise)
            }
        }

        // If running extra cloud tests, enable reporting all tests. These tests run on a nightly
        // schedule, and so the normal test reporter is not available to them. Thus we want to see
        // the full log in the CI to be able to tell which tests have been run.
        if self.scope == StandardLibraryTestsScope::CloudRelated {
            job.env(crate::libraries_tests::env::REPORT_ALL_TESTS, "1");
        }

        job
    }

    fn key(&self, (os, arch): Target) -> String {
        let key = format!(
            "{}-{}-{}-{os}-{arch}",
            self.id_key_base(),
            self.graal_edition.to_string().to_kebab_case(),
            self.scope.to_string().replace(' ', "-"),
        );
        if key.len() >= 100 {
            panic!("Too long CI job key: {:}", key)
        }
        key
    }
}

/// Job that runs Enso lint checks (type checker, later formatting) on the Enso
/// standard libraries and tests.
#[derive(Clone, Copy, Debug)]
pub struct EnsoCodeLintCheck {
    pub graal_edition: graalvm::Edition,
    pub engine_launcher: engine::EngineLauncher,
}

impl JobArchetype for EnsoCodeLintCheck {
    fn job(&self, target: Target) -> Job {
        let graal_edition = self.graal_edition;
        let engine_launcher = self.engine_launcher;
        let mut job = RunStepsBuilder::new("libraries lint")
            .customize(move |step| {
                let check_syntax = Step {
                    name: Some("Check syntax".into()),
                    run: Some("./run libraries check-syntax".into()),
                    ..Default::default()
                };

                let cleanup_engine_distribution =
                    step::cleanup_engine_distribution(engine_launcher);

                let download_engine_distribution =
                    step::download_engine_distribution(target, engine_launcher, graal_edition);

                vec![
                    check_syntax,
                    cleanup_engine_distribution,
                    download_engine_distribution,
                    step::check_engine_distribution(),
                    step::unpack_engine_distribution(),
                    step,
                ]
            })
            .build_job("Enso Code Lint", target);
        job.env(crate::libraries_tests::env::ENSO_LINT_ENABLE_GITHUB_ANNOTATIONS, "true");
        job
    }
}

/// Job that checks if any of stdlib APIs have changed, by building the Enso
/// engine distribution, and running `enso --docs api --in-project <std-lib>`,
/// and comparing it to the API signature files that are already in the VCS.
#[derive(Clone, Copy, Debug)]
pub struct StandardLibraryApiCheck {
    pub graal_edition: graalvm::Edition,
    pub engine_launcher: engine::EngineLauncher,
}

impl JobArchetype for StandardLibraryApiCheck {
    fn job(&self, target: Target) -> Job {
        let job_name = "Standard Library API check";
        let graal_edition = self.graal_edition;
        let engine_launcher = self.engine_launcher;
        let run_command = "backend stdlib-api-check";
        let job = RunStepsBuilder::new(run_command)
            .customize(move |step| {
                let cleanup_engine_distribution =
                    step::cleanup_engine_distribution(engine_launcher);

                let download_engine_distribution =
                    step::download_engine_distribution(target, engine_launcher, graal_edition);

                vec![
                    cleanup_engine_distribution,
                    download_engine_distribution,
                    step::check_engine_distribution(),
                    step::unpack_engine_distribution(),
                    step,
                ]
            })
            .build_job(job_name, target);
        job
    }
}

/// Job that checks if the API of a standard library has changed, and if so,
/// appends a label to the PR.
#[derive(Clone, Debug)]
pub struct StandardLibraryLabelCheck {
    /// Library name to check. WIthout the leading `Standard` prefix
    pub lib_name: String,
}

impl StandardLibraryLabelCheck {
    fn changed_files_step_name(&self) -> String {
        format!("{}-changed-files", self.lib_name)
    }

    fn changed_files_step(&self) -> Step {
        let changed_files_pattern =
            format!("distribution/lib/Standard/{}/**/docs/api/**/**.md", self.lib_name);
        let changed_files_action = "step-security/changed-files@v45".to_string();
        Step {
            id: Some(self.changed_files_step_name()),
            name: Some(self.changed_files_step_name()),
            uses: Some(changed_files_action),
            ..default()
        }
        .with_custom_argument("files", changed_files_pattern)
    }

    fn list_all_changed_files_step(&self) -> Step {
        let run = format!(
            r#"
        if [[ "${{{{ steps.{}.outputs.any_changed }}}}" == "true" ]]; then
            echo "Files changed:"
        fi
        for file in ${{ALL_CHANGED_FILES}}; do
            echo "$file"
        done
        "#,
            self.changed_files_step_name()
        );
        Step {
            name: Some(format!("List all changed files in {}", self.lib_name)),
            run: Some(run),
            ..default()
        }
        .with_env(
            "ALL_CHANGED_FILES",
            "${{ steps.".to_string()
                + &self.changed_files_step_name()
                + ".outputs.all_changed_files }}",
        )
    }

    fn append_label_step(&self) -> Step {
        let label_name = format!("-libs-API-change-{}", self.lib_name);
        let add_label_action = "actions-ecosystem/action-add-labels@v1".to_string();
        Step {
            name: Some(format!("Append {} label", label_name)),
            uses: Some(add_label_action),
            r#if: Some(format!(
                "steps.{}.outputs.any_changed == 'true'",
                self.changed_files_step_name()
            )),
            ..default()
        }
        .with_custom_argument("labels", label_name)
        .with_custom_argument("github_token", "${{ secrets.GITHUB_TOKEN }}")
    }
}

impl JobArchetype for StandardLibraryLabelCheck {
    fn job(&self, target: Target) -> Job {
        if target.0 != OS::Linux {
            panic!("StandardLibraryApiCheck jobs run only on Linux");
        }
        let job_name = format!("{}-change-labels", self.lib_name);
        let steps: Vec<Step> = vec![
            checkout_repo_step(Some(2)),
            self.changed_files_step(),
            self.list_all_changed_files_step(),
            self.append_label_step(),
        ];
        Job { name: job_name, runs_on: vec![RunnerLabel::LinuxLatest], steps, ..default() }
    }

    fn id_key_base(&self) -> String {
        format!("stdlib-api-check-{}", self.lib_name)
    }
}

/** This is a temporary workaround.
 *
 * The Cloud tests preparation requires `aws` CLI to be installed on the machine.
 * The GitHub hosted runners have it, but our self-hosted runners do not.
 * To fix this we either need to modify self-hosted runners to provide the AWS CLI or change the
 * way we prepare the Cloud tests to not require it.
 */
fn build_job_ensuring_cloud_tests_run_on_github(
    run_steps_builder: RunStepsBuilder,
    target: Target,
    job_name: &str,
    scope: StandardLibraryTestsScope,
) -> Job {
    if scope == StandardLibraryTestsScope::CloudRelated {
        if target.0 != OS::Linux {
            panic!("If the Cloud tests are enabled, they require GitHub hosted runner for Cloud auth, so they only run on Linux.");
        }

        run_steps_builder.build_job(job_name, RunnerLabel::LinuxLatest)
    } else {
        run_steps_builder.build_job(job_name, target)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SnowflakeTests {
    pub graal_edition: graalvm::Edition,
    pub engine_launcher: engine::EngineLauncher,
    pub jvm_mode: bool,
}

const GRAAL_EDITION_FOR_EXTRA_TESTS: graalvm::Edition = graalvm::Edition::Community;

impl JobArchetype for SnowflakeTests {
    fn job(&self, target: Target) -> Job {
        if target.0 != OS::Linux {
            panic!("Snowflake tests currently require GitHub hosted runner for Cloud auth, so they only run on Linux.");
        }
        let job_name = "Snowflake Tests";
        let job_name = if self.jvm_mode {
            format!("{job_name} (JVM)")
        } else {
            format!("{job_name} (Native)")
        };
        let graal_edition = self.graal_edition;
        let engine_launcher = self.engine_launcher;
        let run_command = if self.jvm_mode {
            "backend test std-snowflake-jvm"
        } else {
            "backend test std-snowflake"
        };
        let mut job = RunStepsBuilder::new(run_command)
            .customize(move |step| {
                let main_step = step
                    .with_secret_exposed_as(
                        secret::ENSO_SNOWFLAKE_ACCOUNT,
                        crate::libraries_tests::snowflake::env::ENSO_SNOWFLAKE_ACCOUNT,
                    )
                    .with_secret_exposed_as(
                        secret::ENSO_SNOWFLAKE_USER,
                        crate::libraries_tests::snowflake::env::ENSO_SNOWFLAKE_USER,
                    )
                    .with_secret_exposed_as(
                        secret::ENSO_SNOWFLAKE_PASSWORD,
                        crate::libraries_tests::snowflake::env::ENSO_SNOWFLAKE_PASSWORD,
                    )
                    .with_secret_exposed_as(
                        secret::ENSO_SNOWFLAKE_DATABASE,
                        crate::libraries_tests::snowflake::env::ENSO_SNOWFLAKE_DATABASE,
                    )
                    .with_secret_exposed_as(
                        secret::ENSO_SNOWFLAKE_SCHEMA,
                        crate::libraries_tests::snowflake::env::ENSO_SNOWFLAKE_SCHEMA,
                    )
                    .with_secret_exposed_as(
                        secret::ENSO_SNOWFLAKE_WAREHOUSE,
                        crate::libraries_tests::snowflake::env::ENSO_SNOWFLAKE_WAREHOUSE,
                    );

                // Snowflake tests are run only in the 'Extra' job, so it is okay to run it with
                // Enso Cloud as well. They need it to test data link integration.
                let updated_main_step = enable_cloud_tests(main_step);

                let cleanup_engine_distribution =
                    step::cleanup_engine_distribution(engine_launcher);

                let download_engine_distribution =
                    step::download_engine_distribution(target, engine_launcher, graal_edition);

                vec![
                    cleanup_engine_distribution,
                    download_engine_distribution,
                    step::check_engine_distribution(),
                    step::unpack_engine_distribution(),
                    updated_main_step,
                    step::extra_stdlib_test_reporter(target, GRAAL_EDITION_FOR_EXTRA_TESTS),
                ]
            })
            .build_job(job_name, RunnerLabel::LinuxLatest)
            .with_permission(Permission::Checks, Access::Write);
        job.env(engine::env::GRAAL_EDITION, GRAAL_EDITION_FOR_EXTRA_TESTS);
        job.env(crate::libraries_tests::env::REPORT_ALL_TESTS, "1");
        job
    }

    fn key(&self, (os, arch): Target) -> String {
        if self.jvm_mode {
            format!("{}-jvm-{os}-{arch}", self.id_key_base())
        } else {
            format!("{}-native-{os}-{arch}", self.id_key_base())
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WasmLint;

impl JobArchetype for WasmLint {
    fn job(&self, target: Target) -> Job {
        plain_job(target, "Lint", "wasm lint")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NativeTest;

impl JobArchetype for NativeTest {
    fn job(&self, target: Target) -> Job {
        plain_job(target, "Native Rust tests", "wasm test")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct GuiBuild;

impl JobArchetype for GuiBuild {
    fn job(&self, target: Target) -> Job {
        let command: &str = "gui build";
        RunStepsBuilder::new(command)
            .customize(move |step| {
                let mut steps = vec![expose_gui_vars(step)];

                if target.0 == OS::Linux {
                    let upload_gui = step::upload_artifact("Upload gui")
                        .with_custom_argument("name", "gui")
                        .with_custom_argument("path", "dist/gui/");
                    steps.push(upload_gui);
                }

                steps
            })
            .build_job("GUI build", target)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BuildBackend {
    pub engine_launcher: engine::EngineLauncher,
}

impl JobArchetype for BuildBackend {
    fn job(&self, target: Target) -> Job {
        let mut job = RunStepsBuilder::new("backend get")
            .customize(move |step| {
                let mut steps = vec![step];

                if target.0 == OS::Linux {
                    let upload_edition_file = step::upload_artifact("Upload Edition File")
                        .with_custom_argument("name", paths::EDITION_FILE_ARTIFACT_NAME)
                        .with_custom_argument("path", "distribution/editions/*.yaml");
                    steps.push(upload_edition_file);

                    let upload_fbs_schema = step::upload_artifact("Upload fbs-schema")
                        .with_custom_argument("name", "fbs-schema")
                        .with_custom_argument("path", "engine/language-server/src/main/schema/");
                    steps.push(upload_fbs_schema)
                }

                let archive_project_manager = Step {
                    name: Some("Archive project-manager".into()),
                    run: Some("tar -cvf project-manager.tar -C dist/backend .".into()),
                    ..Default::default()
                };
                steps.push(archive_project_manager);

                let upload_project_manager = step::upload_artifact("Upload project-manager")
                    .with_custom_argument("name", format!("project-manager-{}", target.0))
                    .with_custom_argument("path", "project-manager.tar");
                steps.push(upload_project_manager);

                let cleanup = Step {
                    name: Some("Cleanup".into()),
                    run: Some("rm project-manager.tar".into()),
                    ..Default::default()
                };
                steps.push(cleanup);

                steps
            })
            .build_job("Build Backend", target);
        job.env(engine::env::ENSO_LAUNCHER, self.engine_launcher);

        job
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UploadBackend;

impl JobArchetype for UploadBackend {
    fn job(&self, target: Target) -> Job {
        RunStepsBuilder::new("backend upload")
            .cleaning(RELEASE_CLEANING_POLICY)
            .customize(move |step| {
                let mut steps = vec![step];

                if target.0 == OS::Linux {
                    let upload_edition_file = step::upload_artifact("Upload Edition File")
                        .with_custom_argument("name", paths::EDITION_FILE_ARTIFACT_NAME)
                        .with_custom_argument("path", "distribution/editions/*.yaml");
                    steps.push(upload_edition_file);
                }

                steps
            })
            .build_job("Upload Backend", target)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DeployRuntime;

impl JobArchetype for DeployRuntime {
    fn job(&self, target: Target) -> Job {
        RunStepsBuilder::new("release deploy-runtime")
            .customize(|step| {
                vec![step
                    .with_secret_exposed_as(secret::CI_PRIVATE_TOKEN, ide_ci::github::GITHUB_TOKEN)
                    .with_env("ENSO_BUILD_ECR_REPOSITORY", crate::aws::ecr::runtime::NAME)
                    .with_secret_exposed_as(
                        secret::ECR_PUSH_RUNTIME_ACCESS_KEY_ID,
                        "AWS_ACCESS_KEY_ID",
                    )
                    .with_secret_exposed_as(
                        secret::ECR_PUSH_RUNTIME_SECRET_ACCESS_KEY,
                        "AWS_SECRET_ACCESS_KEY",
                    )
                    .with_env("AWS_DEFAULT_REGION", crate::aws::ecr::runtime::REGION)]
            })
            .build_job("Upload Runtime to ECR", target)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DispatchBuildImage;

impl JobArchetype for DispatchBuildImage {
    fn job(&self, target: Target) -> Job {
        RunStepsBuilder::new("release dispatch-build-image")
            .customize(|step| {
                vec![step
                    .with_secret_exposed_as(secret::CI_PRIVATE_TOKEN, ide_ci::github::GITHUB_TOKEN)]
            })
            .build_job("Dispatch Cloud build-image workflow", target)
    }
}

pub fn expose_os_specific_signing_secret(os: OS, step: Step) -> Step {
    match os {
        OS::Windows => step
            .with_secret_exposed_as(secret::WINDOWS_CERT_PATH, &ide::web::env::WIN_CSC_LINK)
            .with_secret_exposed_as(
                secret::WINDOWS_CERT_PASSWORD,
                &ide::web::env::WIN_CSC_KEY_PASSWORD,
            ),
        OS::MacOS => step
            .with_secret_exposed_as(secret::APPLE_CODE_SIGNING_CERT, &ide::web::env::CSC_LINK)
            .with_secret_exposed_as(
                secret::APPLE_CODE_SIGNING_CERT_PASSWORD,
                &ide::web::env::CSC_KEY_PASSWORD,
            )
            .with_secret_exposed_as(secret::APPLE_NOTARIZATION_USERNAME, &ide::web::env::APPLEID)
            .with_secret_exposed_as(
                secret::APPLE_NOTARIZATION_PASSWORD,
                &ide::web::env::APPLEIDPASS,
            )
            .with_secret_exposed_as(secret::APPLE_NOTARIZATION_TEAM_ID, &ide::web::env::APPLETEAMID)
            .with_env(ide::web::env::CSC_IDENTITY_AUTO_DISCOVERY, "true")
            // `CSC_FOR_PULL_REQUEST` can potentially expose sensitive information to third-party,
            // see the comment in the definition of `CSC_FOR_PULL_REQUEST` for more information.
            //
            // In our case, we are safe here, as any PRs from forks do not get the secrets exposed.
            .with_env(ide::web::env::CSC_FOR_PULL_REQUEST, "true"),
        _ => step,
    }
}

/// Whether the artifact is being built for a release build (nightlies, releases)
/// or a development build (PRs).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PackagingTarget {
    /// The artifact is being built for a release build (nightlies, releases).
    Development,
    /// The artifact is being built for a development build (PRs).
    Release,
}

/// Prepares the packaging steps for the given OS.
///
/// This involves:
/// * exposing secrets necessary for code signing and notarization;
/// * exposing variables defining cloud environment for dashboard.
pub fn prepare_packaging_steps(os: OS, step: Step, packaging_target: PackagingTarget) -> Vec<Step> {
    let step = expose_gui_vars(step);
    let step = if packaging_target == PackagingTarget::Release {
        expose_debugging_vars(step)
    } else {
        step
    };
    let step = expose_os_specific_signing_secret(os, step);
    vec![step]
}

#[derive(Clone, Copy, Debug)]
pub struct PackageIde;

impl JobArchetype for PackageIde {
    fn job(&self, target: Target) -> Job {
        RunStepsBuilder::new("ide build --backend-source local --gui-upload-artifact false")
            .fetch_depth(2)
            .customize(move |step| {
                let mut steps = vec![];

                let download_project_manager = step::download_artifact("Download project-manager")
                    .with_custom_argument("name", format!("project-manager-{}", target.0))
                    .with_custom_argument("path", "dist/backend");
                steps.push(download_project_manager);

                let unpack_project_manager = Step {
                    run: Some(
                        "tar -xvf dist/backend/project-manager.tar -C dist/backend
rm dist/backend/project-manager.tar"
                            .into(),
                    ),
                    ..Default::default()
                };
                steps.push(unpack_project_manager);

                let mut packaging_steps =
                    prepare_packaging_steps(target.0, step, PackagingTarget::Development);
                steps.append(&mut packaging_steps);

                let upload_ide = step::upload_artifact("Upload ide")
                    .with_custom_argument("name", format!("ide-{}", target.0))
                    .with_custom_argument(
                        "path",
                        format!("dist/ide/enso-*.{}", target.0.package_extension()),
                    );
                steps.push(upload_ide);

                let test_prepare_step = shell("\
                    mkdir -p app/electron-client/playwright/.auth && \
                    touch app/electron-client/playwright/.auth/user.json && \
                    chmod 600 app/electron-client/playwright/.auth/user.json && \
                    echo \"{\\\"user\\\": \\\"$ENSO_TEST_USER\\\",\\\"password\\\":\\\"$ENSO_TEST_USER_PASSWORD\\\"}\" > app/electron-client/playwright/.auth/user.json\
                    ").with_shell(Shell::Bash).with_secret_exposed_as(
                        secret::ENSO_CLOUD_TEST_ACCOUNT_USERNAME,
                        "ENSO_TEST_USER",
                    )
                    .with_secret_exposed_as(
                        secret::ENSO_CLOUD_TEST_ACCOUNT_PASSWORD,
                        "ENSO_TEST_USER_PASSWORD",
                    ).with_name(
                        "Prepare Package Tests"
                    );
                steps.push(test_prepare_step);

                const TEST_COMMAND: &str = "corepack pnpm -r --filter enso ide-integration-test";
                let test_step = match target.0 {
                    OS::Linux => shell(format!("xvfb-run {TEST_COMMAND}")),
                    OS::MacOS =>
                    // MacOS CI runners are very slow
                        shell(format!("{TEST_COMMAND} --timeout 300000")),
                    _ => shell(TEST_COMMAND),
                };
                let test_step = test_step
                    .with_env("DEBUG", "pw:browser log:")
                    .with_name("Run Package Tests");

                steps.push(test_step);

                let upload_test_traces_step = Step {
                    r#if: Some("failure()".into()),
                    name: Some("Upload Test Traces".into()),
                    uses: Some("actions/upload-artifact@v4".into()),
                    with: Some(Argument::Other(BTreeMap::from_iter([
                        ("name".into(), format!("test-traces-{}-{}", target.0, target.1).into()),
                        ("path".into(), "app/electron-client/test-traces".into()),
                        ("compression-level".into(), 0.into()), // The traces are in zip already.
                    ]))),
                    ..Default::default()
                };
                steps.push(upload_test_traces_step);

                // After the E2E tests run, they create a credentials file in user home directory.
                // If that file is not cleaned up, future runs of our tests may randomly get
                // authenticated into Enso Cloud. We want to run tests as an authenticated
                // user only when we explicitly set that up, not randomly. So we clean the
                // credentials file.
                let cloud_credentials_path = "$HOME/.enso/credentials";
                let cleanup_credentials_step = Step {
                    r#if: Some("always()".into()),
                    name: Some("Remove Credentials File".into()),
                    shell: Some(Shell::Bash),
                    ..shell(format!("rm -f {cloud_credentials_path}"))
                };

                steps.push(cleanup_credentials_step);

                steps
            })
            .build_job("Package New IDE", target)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BuildEngineDistribution {
    pub graal_edition: graalvm::Edition,
    pub engine_launcher: engine::EngineLauncher,
}

impl JobArchetype for BuildEngineDistribution {
    fn job(&self, target: Target) -> Job {
        let engine_launcher = self.engine_launcher;
        let graal_edition = self.graal_edition;
        let job_name =
            format!("Build Engine Distribution ({}) ({})", self.graal_edition, engine_launcher);
        let mut job = RunStepsBuilder::new("backend ci-build-engine-distribution")
            .customize(move |step| {
                let archive_engine_distribution =
                    step::archive_engine_distribution(engine_launcher);

                let upload_engine_distribution =
                    step::upload_engine_distribution(target, engine_launcher, graal_edition);

                let cleanup_archive = Step {
                    name: Some("Cleanup Archive".into()),
                    run: Some("rm built-distribution.tar".into()),
                    ..Default::default()
                };

                vec![step, archive_engine_distribution, upload_engine_distribution, cleanup_archive]
            })
            .build_job(job_name, target);
        job.env(engine::env::ENSO_LAUNCHER, self.engine_launcher);
        match self.graal_edition {
            graalvm::Edition::Community => {
                job.env(engine::env::GRAAL_EDITION, graalvm::Edition::Community)
            }
            graalvm::Edition::Enterprise => {
                job.env(engine::env::GRAAL_EDITION, graalvm::Edition::Enterprise)
            }
        }
        job
    }

    fn key(&self, (os, arch): Target) -> String {
        format!(
            "{}-{}-{os}-{arch}",
            self.id_key_base(),
            self.graal_edition.to_string().to_kebab_case()
        )
    }
}
