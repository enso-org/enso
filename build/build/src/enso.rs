use crate::prelude::*;

use crate::paths::Paths;
use crate::postgres;
use crate::postgres::EndpointConfiguration;
use crate::postgres::Postgresql;

use ide_ci::env::Variable;
use ide_ci::future::AsyncPolicy;
use ide_ci::programs::docker::ContainerId;



ide_ci::define_env_var! {
    ENSO_JVM_OPTS, String;
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

#[derive(Clone, Debug)]
pub struct BuiltEnso {
    pub paths: Paths,
}

impl BuiltEnso {
    pub fn wrapper_script_path(&self) -> PathBuf {
        self.paths.engine.dir.join("bin").join("enso")
    }

    pub fn run_test(&self, test: impl AsRef<Path>, ir_caches: IrCaches) -> Result<Command> {
        let test_path = self.paths.stdlib_test(test);
        let mut command = self.cmd()?;
        command
            .arg(ir_caches)
            .arg("--run")
            .arg(test_path)
            // This flag enables assertions in the JVM. Some of our stdlib tests had in the past
            // failed on Graal/Truffle assertions, so we want to have them triggered.
            .set_env(ENSO_JVM_OPTS, &ide_ci::programs::java::Option::EnableAssertions.as_ref())?;
        Ok(command)
    }

    pub fn compile_lib(&self, target: impl AsRef<Path>) -> Result<Command> {
        ide_ci::fs::require_exist(&target)?;
        let mut command = self.cmd()?;
        command
            .arg(IrCaches::Yes)
            .args(["--no-compile-dependencies", "--no-global-cache", "--compile"])
            .arg(target.as_ref());
        Ok(command)
    }

    pub async fn run_tests(&self, ir_caches: IrCaches, async_policy: AsyncPolicy) -> Result {
        let paths = &self.paths;
        // Prepare Engine Test Environment
        if let Ok(gdoc_key) = std::env::var("GDOC_KEY") {
            let google_api_test_data_dir =
                paths.repo_root.join("test").join("Google_Api_Test").join("data");
            ide_ci::fs::create_dir_if_missing(&google_api_test_data_dir)?;
            ide_ci::fs::write(google_api_test_data_dir.join("secret.json"), &gdoc_key)?;
        }

        let _httpbin = crate::httpbin::get_and_spawn_httpbin_on_free_port().await?;
        let _postgres = match TARGET_OS {
            OS::Linux => {
                let runner_context_string = crate::env::RunnerContainerName
                    .fetch()
                    .map(|name| name.0)
                    .or_else(|_| ide_ci::actions::env::RUNNER_NAME.get())
                    .unwrap_or_else(|_| Uuid::new_v4().to_string());
                // GH-hosted runners are named like "GitHub Actions 10". Spaces are not allowed in
                // the container name.
                let container_name =
                    iformat!("postgres-for-{runner_context_string}").replace(' ', "_");
                let config = postgres::Configuration {
                    postgres_container: ContainerId(container_name),
                    database_name:      "enso_test_db".to_string(),
                    user:               "enso_test_user".to_string(),
                    password:           "enso_test_password".to_string(),
                    endpoint:           EndpointConfiguration::deduce()?,
                    version:            "latest".to_string(),
                };
                let postgres = Postgresql::start(config).await?;
                Some(postgres)
            }
            _ => None,
        };

        let futures = crate::paths::LIBRARIES_TO_TEST.map(ToString::to_string).map(|test| {
            let command = self.run_test(test, ir_caches);
            async move { command?.run_ok().await }
        });

        // We need to join all the test tasks here, as they require postgres and httpbin alive.
        // Could share them with Arc but then scenario of multiple test runs being run in parallel
        // should be handled, e.g. avoiding port collisions.
        let results = ide_ci::future::join_all(futures, async_policy).await;
        let errors = results.into_iter().filter_map(Result::err).collect::<Vec<_>>();
        if errors.is_empty() {
            Ok(())
        } else {
            error!("{} test suit(s) failed.", errors.len());
            for error in &errors {
                error!("{}", error);
            }
            bail!("Standard library tests failed. Details: {:?}.", errors);
        }
    }
}

#[async_trait]
impl Program for BuiltEnso {
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
