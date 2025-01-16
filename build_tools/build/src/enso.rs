use crate::prelude::*;

use crate::cloud_tests;
use crate::engine::StandardLibraryTestsSelection;
use crate::paths::Paths;
use crate::paths::ENSO_ENABLE_ASSERTIONS;
use crate::paths::ENSO_META_TEST_ARGS;
use crate::paths::ENSO_META_TEST_COMMAND;
use crate::paths::ENSO_TEST_ANSI_COLORS;
use crate::postgres;
use crate::postgres::EndpointConfiguration as PostgresEndpointConfiguration;
use crate::postgres::Postgresql;
use crate::sqlserver;
use crate::sqlserver::EndpointConfiguration as SQLServerEndpointConfiguration;
use crate::sqlserver::SQLServer;

use ide_ci::env::accessor::TypedVariable;
use ide_ci::future::AsyncPolicy;
use ide_ci::programs::docker::ContainerId;



#[derive(Copy, Clone, Debug, strum::Display, strum::EnumString)]
pub enum Boolean {
    True,
    False,
}

impl From<bool> for Boolean {
    fn from(value: bool) -> Self {
        if value {
            Self::True
        } else {
            Self::False
        }
    }
}

ide_ci::define_env_var! {
    JAVA_OPTS, String;
    ENSO_BENCHMARK_TEST_DRY_RUN, Boolean;
}

#[derive(Copy, Clone, Debug)]
pub enum IrCaches {
    Yes,
    No,
}

impl IrCaches {
    pub fn flag(self) -> &'static str {
        match self {
            IrCaches::Yes => "--ir-caches",
            IrCaches::No => "--no-ir-caches",
        }
    }
}

impl AsRef<OsStr> for IrCaches {
    fn as_ref(&self) -> &OsStr {
        self.flag().as_ref()
    }
}

#[derive(Copy, Clone, Debug)]
pub struct BenchmarkOptions {
    pub dry_run: bool,
}

#[derive(Clone, Debug)]
pub struct BuiltEnso {
    pub paths: Paths,
}

impl BuiltEnso {
    pub fn wrapper_script_path(&self) -> PathBuf {
        let filename = format!("enso{}", if TARGET_OS == OS::Windows { ".bat" } else { "" });
        self.paths.repo_root.built_distribution.enso_engine_triple.engine_package.bin.join(filename)
    }

    pub async fn run_benchmarks(&self, opt: BenchmarkOptions) -> Result {
        let filename = format!("enso{}", if TARGET_OS == OS::Windows { ".exe" } else { "" });
        let base_working_directory = self.paths.repo_root.test.benchmarks.try_parent()?;
        let enso = self
            .paths
            .repo_root
            .built_distribution
            .enso_engine_triple
            .engine_package
            .bin
            .join(filename);
        let benchmarks = Command::new(&enso)
            .args(["--jvm", "--run", self.paths.repo_root.test.benchmarks.as_str()])
            .current_dir(base_working_directory)
            .set_env(ENSO_BENCHMARK_TEST_DRY_RUN, &Boolean::from(opt.dry_run))?
            .run_ok()
            .await;
        benchmarks
    }

    pub fn run_test(
        &self,
        test_path: impl AsRef<Path>,
        ir_caches: IrCaches,
        environment_overrides: Vec<(String, String)>,
    ) -> Result<Command> {
        let mut command = self.cmd()?;
        let base_working_directory = test_path.try_parent()?;
        command
            .arg(ir_caches)
            .arg("--run")
            .arg(test_path.as_ref())
            .current_dir(base_working_directory)
            // This flag enables assertions in the JVM. Some of our stdlib tests had in the past
            // failed on Graal/Truffle assertions, so we want to have them triggered.
            .set_env(JAVA_OPTS, &ide_ci::programs::java::Option::EnableAssertions.as_ref())?;

        for (k, v) in environment_overrides {
            command.env(k, &v);
        }

        if test_path.as_str().contains("_Internal_") {
            command.arg("--disable-private-check");
        }
        Ok(command)
    }

    pub fn repl(&self) -> Result<Command> {
        let mut command = self.cmd()?;
        command.arg("--repl");
        Ok(command)
    }

    pub async fn run_tests(
        &self,
        ir_caches: IrCaches,
        sbt: &crate::engine::sbt::Context,
        async_policy: AsyncPolicy,
        test_selection: StandardLibraryTestsSelection,
    ) -> Result {
        let paths = &self.paths;
        // Environment for meta-tests. See:
        // https://github.com/enso-org/enso/tree/develop/test/Meta_Test_Suite_Tests
        ENSO_META_TEST_COMMAND.set(&self.wrapper_script_path())?;
        ENSO_META_TEST_ARGS.set(&format!("{} --run", ir_caches.flag()))?;

        ENSO_ENABLE_ASSERTIONS.set("true")?;
        ENSO_TEST_ANSI_COLORS.set("true")?;

        // Prepare Engine Test Environment
        if let Ok(gdoc_key) = std::env::var("GDOC_KEY") {
            let google_api_test_data_dir =
                paths.repo_root.join("test").join("Google_Api_Test").join("data");
            ide_ci::fs::create_dir_if_missing(&google_api_test_data_dir)?;
            ide_ci::fs::write(google_api_test_data_dir.join("secret.json"), gdoc_key)?;
        }

        let std_tests = match &test_selection {
            StandardLibraryTestsSelection::All =>
                crate::paths::discover_standard_library_tests(&paths.repo_root)?,
            StandardLibraryTestsSelection::Selected(only) =>
                only.iter().map(|test| paths.repo_root.test.join(test)).collect(),
        };
        let may_need_postgres = match &test_selection {
            StandardLibraryTestsSelection::All => true,
            StandardLibraryTestsSelection::Selected(only) =>
                only.iter().any(|test| test.contains("Table_Tests")),
        };
        let may_need_sqlserver = match &test_selection {
            StandardLibraryTestsSelection::All => true,
            StandardLibraryTestsSelection::Selected(only) =>
                only.iter().any(|test| test.contains("Microsoft_Tests")),
        };

        let cloud_credentials_file = match cloud_tests::build_auth_config_from_environment() {
            Ok(config) => {
                let file = cloud_tests::prepare_credentials_file(config).await?;
                info!("Enso Cloud authentication (for cloud integration tests) is enabled.");
                Some(file)
            }
            Err(err) => {
                info!("Enso Cloud authentication (for cloud integration tests) is skipped, because of: {}", err);
                None
            }
        };

        let _httpbin = crate::httpbin::get_and_spawn_httpbin_on_free_port(sbt).await?;

        let _postgres = match TARGET_OS {
            OS::Linux if may_need_postgres => {
                let runner_context_string = crate::env::ENSO_RUNNER_CONTAINER_NAME
                    .get_raw()
                    .or_else(|_| ide_ci::actions::env::RUNNER_NAME.get())
                    .unwrap_or_else(|_| Uuid::new_v4().to_string());
                // GH-hosted runners are named like "GitHub Actions 10". Spaces are not allowed in
                // the container name.
                let container_name =
                    format!("postgres-for-{runner_context_string}").replace(' ', "_");
                let config = postgres::Configuration {
                    postgres_container: ContainerId(container_name),
                    database_name:      "enso_test_db".to_string(),
                    user:               "enso_test_user".to_string(),
                    password:           "enso_test_password".to_string(),
                    endpoint:           PostgresEndpointConfiguration::deduce()?,
                    version:            "latest".to_string(),
                };
                let postgres = Postgresql::start(config).await?;
                Some(postgres)
            }
            _ => None,
        };

        let _sqlserver = match TARGET_OS {
            OS::Linux if may_need_sqlserver => {
                let runner_context_string = crate::env::ENSO_RUNNER_CONTAINER_NAME
                    .get_raw()
                    .or_else(|_| ide_ci::actions::env::RUNNER_NAME.get())
                    .unwrap_or_else(|_| Uuid::new_v4().to_string());
                // GH-hosted runners are named like "GitHub Actions 10". Spaces are not allowed in
                // the container name.
                let container_name =
                    format!("sqlserver-for-{runner_context_string}").replace(' ', "_");
                let config = sqlserver::Configuration {
                    sqlserver_container: ContainerId(container_name),
                    database_name:       "tempdb".to_string(),
                    user:                "sa".to_string(),
                    password:            "enso_test_password_<YourStrong@Passw0rd>".to_string(),
                    endpoint:            SQLServerEndpointConfiguration::deduce()?,
                    version:             "2022-latest".to_string(),
                };
                let sqlserver = SQLServer::start(config).await?;
                Some(sqlserver)
            }
            _ => None,
        };

        let mut environment_overrides: Vec<(String, String)> = vec![];
        if let Some(credentials_file) = cloud_credentials_file.as_ref() {
            let path_as_str = credentials_file.path().to_str();
            let path = path_as_str
                .ok_or_else(|| anyhow!("Path to credentials file is not valid UTF-8"))?;
            environment_overrides.push((
                cloud_tests::env::test_controls::ENSO_CLOUD_CREDENTIALS_FILE.name().to_string(),
                path.to_string(),
            ));
            // We do not set ENSO_CLOUD_API_URI - we rely on the default, or any existing overrides.
            environment_overrides.push((
                cloud_tests::env::test_controls::ENSO_RUN_REAL_CLOUD_TEST.name().to_string(),
                "1".to_string(),
            ));
        };

        let futures = std_tests.into_iter().map(|test_path| {
            let command: std::result::Result<Command, anyhow::Error> =
                self.run_test(test_path, ir_caches, environment_overrides.clone());
            async move { command?.run_ok().await }
        });

        // We need to join all the test tasks here, as they require postgres and httpbin alive.
        // Could share them with Arc but then scenario of multiple test runs being run in parallel
        // should be handled, e.g. avoiding port collisions.
        let results = ide_ci::future::join_all(futures, async_policy).await;
        // Only drop the credentials file after all tests have finished.
        drop(cloud_credentials_file);
        let errors = results.into_iter().filter_map(Result::err).collect::<Vec<_>>();
        if errors.is_empty() {
            Ok(())
        } else {
            let summary = errors
                .as_slice()
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(";\n")
                .replace("\n", "%0A");
            println!(
                "::error title=Failed Standard Library Tests::{} test suite(s) failed: {}",
                errors.len(),
                summary
            );
            error!("{} test suite(s) failed.", errors.len());
            for error in &errors {
                error!("{}", error);
            }
            bail!("Standard library tests failed. Details: {:?}.", errors);
        }
    }
}

#[async_trait]
impl Program for BuiltEnso {
    type Command = Command;
    type Version = Version;

    fn executable_name(&self) -> &str {
        ide_ci::platform::DEFAULT_SHELL.executable_name()
    }

    fn cmd(&self) -> Result<Command> {
        ide_ci::platform::DEFAULT_SHELL.run_script(self.wrapper_script_path())
    }

    fn version_string(&self) -> BoxFuture<'static, Result<String>> {
        let command = self.cmd();
        async move { command?.args(["version", "--json", "--only-launcher"]).run_stdout().await }
            .boxed()
    }

    async fn version(&self) -> Result<Version> {
        #[derive(Clone, Debug, Deserialize)]
        struct VersionInfo {
            version: Version,
        }

        let stdout = self.version_string().await?;
        let version = serde_json::from_str::<VersionInfo>(&stdout)?;
        Ok(version.version)
    }
}
