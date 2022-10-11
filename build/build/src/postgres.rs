use crate::prelude::*;

use ide_ci::env::new::RawVariable;
use ide_ci::env::new::TypedVariable;
use ide_ci::get_free_port;
use ide_ci::programs::docker::ContainerId;
use ide_ci::programs::docker::ImageId;
use ide_ci::programs::docker::Network;
use ide_ci::programs::docker::RunOptions;
use ide_ci::programs::Docker;
use std::process::Stdio;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncRead;
use tokio::io::BufReader;
use tokio::process::Child;



/// Port used by Postgres in its container.
const POSTGRES_CONTAINER_DEFAULT_PORT: u16 = 5432;

/// Environment variables used to configure the Postgres container.
pub mod env {

    pub mod container {
        ide_ci::define_env_var! {
            POSTGRES_DB, String;
            POSTGRES_USER, String;
            POSTGRES_PASSWORD, String;
        }
    }
    pub mod tests {
        ide_ci::define_env_var! {
            ENSO_DATABASE_TEST_DB_NAME, String;
            ENSO_DATABASE_TEST_HOST, String;
            ENSO_DATABASE_TEST_DB_USER, String;
            ENSO_DATABASE_TEST_DB_PASSWORD, String;
        }
    }
}

#[derive(Clone, Debug)]
pub enum EndpointConfiguration {
    /// Used when the Postgres container is started directly from host (rather than Docker
    /// container). In such case the Postgres will be exposed to host network on a given port.
    Host { port: u16 },
    /// Used when Postgres is spawned from a container. In such case it will be spawned in a owning
    /// container's network on the default port.
    Container { owner: ContainerId },
}

impl EndpointConfiguration {
    /// Tries to deduce what endpoint should be used for a spawned Postgres service.
    pub fn deduce() -> Result<Self> {
        if let Ok(container_name) = std::env::var("ENSO_RUNNER_CONTAINER_NAME") {
            debug!("Assuming that I am in the Docker container named {container_name}.");
            Ok(Self::Container { owner: ContainerId(container_name) })
        } else {
            // If we are running on the bare machine (i.e. not in container), we spawn postgres
            // and expose it on a free host port. Then we can directly consume.
            let port = if port_check::is_local_port_free(POSTGRES_CONTAINER_DEFAULT_PORT) {
                // Prefer the usual port.
                POSTGRES_CONTAINER_DEFAULT_PORT
            } else {
                get_free_port()?
            };
            Ok(Self::Host { port })
        }
    }
}

#[derive(Clone, Debug)]
pub struct Configuration {
    pub postgres_container: ContainerId,
    pub database_name:      String,
    pub user:               String,
    pub password:           String,
    pub endpoint:           EndpointConfiguration,
    pub version:            String,
}

impl Configuration {
    pub fn image_id(&self) -> ImageId {
        ImageId(format!("postgres:{}", &self.version))
    }

    pub fn set_enso_test_env(&self) -> Result {
        env::tests::ENSO_DATABASE_TEST_DB_NAME.set(&self.database_name)?;
        env::tests::ENSO_DATABASE_TEST_HOST.set(match &self.endpoint {
            EndpointConfiguration::Host { port } => format!("localhost:{port}"),
            EndpointConfiguration::Container { .. } =>
                format!("localhost:{POSTGRES_CONTAINER_DEFAULT_PORT}"),
        })?;
        env::tests::ENSO_DATABASE_TEST_DB_USER.set(&self.user)?;
        env::tests::ENSO_DATABASE_TEST_DB_PASSWORD.set(&self.password)?;
        Ok(())
    }

    pub fn clear_enso_test_env(&self) {
        env::tests::ENSO_DATABASE_TEST_DB_NAME.remove();
        env::tests::ENSO_DATABASE_TEST_HOST.remove();
        env::tests::ENSO_DATABASE_TEST_DB_USER.remove();
        env::tests::ENSO_DATABASE_TEST_DB_PASSWORD.remove();
    }

    pub async fn cleanup(&self) -> Result {
        Docker.remove_container(&self.postgres_container, true).await
    }
}

/// Retrieve input from asynchronous reader line by line and feed them into the given function.
pub async fn process_lines<R: AsyncRead + Unpin>(reader: R, f: impl Fn(String)) -> Result<R> {
    debug!("Started line processor.");
    let mut reader = BufReader::new(reader);
    let mut line = String::new();
    while reader.read_line(&mut line).await? != 0 {
        f(std::mem::take(&mut line));
    }
    Ok(reader.into_inner())
}

pub async fn process_lines_until<R: AsyncRead + Unpin>(
    reader: R,
    f: &impl Fn(&str) -> bool,
) -> Result<R> {
    let mut reader = BufReader::new(reader);
    let mut line = String::new();
    loop {
        let bytes_read = reader.read_line(&mut line).await?;
        ensure!(bytes_read != 0, "Postgresql container closed without being ready!");
        if f(&line) {
            break;
        }
        line.clear();
    }
    Ok(reader.into_inner())
}

#[derive(Debug)]
pub struct PostgresContainer {
    _docker_run: Child,
    config:      Configuration,
}

impl Drop for PostgresContainer {
    fn drop(&mut self) {
        self.config.clear_enso_test_env();

        debug!("Will remove the postgres container");
        let cleanup_future = self.config.cleanup();
        if let Err(e) = futures::executor::block_on(cleanup_future) {
            debug!(
                "Failed to kill the Postgres container named {}: {}",
                self.config.postgres_container, e
            );
        } else {
            debug!("Postgres container killed.");
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Postgresql;

impl Postgresql {
    pub async fn start(config: Configuration) -> Result<PostgresContainer> {
        // Attempt cleanup in case previous script run crashed in the middle of this.
        // Otherwise, postgres container names could collide.
        let _ = config.cleanup().await;

        let mut opts = RunOptions::new(config.image_id());
        opts.env(&env::container::POSTGRES_DB, &*config.database_name)?;
        opts.env(&env::container::POSTGRES_USER, &*config.user)?;
        opts.env(&env::container::POSTGRES_PASSWORD, &*config.password)?;
        match &config.endpoint {
            EndpointConfiguration::Host { port } => {
                opts.publish_port(*port, POSTGRES_CONTAINER_DEFAULT_PORT);
            }
            EndpointConfiguration::Container { owner } => {
                opts.network = Some(Network::Container(owner.clone()));
            }
        }
        opts.sig_proxy = Some(true);
        opts.name = Some(config.postgres_container.to_string());

        let mut cmd = Docker.run_cmd(&opts)?;
        cmd.stderr(Stdio::piped());
        cmd.kill_on_drop(true);
        let mut child = cmd.spawn().anyhow_err()?;
        let stderr = child
            .stderr
            .ok_or_else(|| anyhow!("Failed to access standard output of the spawned process!"))?;

        // Wait until container is ready.
        let check_line = |line: &str| {
            debug!("ERR: {}", line);
            line.contains("database system is ready to accept connections")
        };
        let stderr = process_lines_until(stderr, &check_line).await?;

        // Put back stream we've been reading and pack the whole thing back for the caller.
        child.stderr = Some(stderr);

        config.set_enso_test_env()?;
        Ok(PostgresContainer { _docker_run: child, config })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[ignore]
    async fn start_postgres() -> Result {
        let config = Configuration {
            postgres_container: ContainerId("something".into()),
            endpoint:           EndpointConfiguration::deduce()?,
            version:            "latest".into(),
            user:               "test".into(),
            password:           "test".into(),
            database_name:      "test".into(),
        };
        let child = Postgresql::start(config).await?;
        // drop(child);
        std::mem::forget(child);
        Ok(())
    }

    #[tokio::test]
    #[ignore]
    async fn test_postgres() -> Result {
        // let config = Configuration {
        //     postgres_container: ContainerId("something".into()),
        //     endpoint:           EndpointConfiguration::deduce()?,
        //     version:            "latest".into(),
        //     user:               "test".into(),
        //     password:           "test".into(),
        //     database_name:      "test".into(),
        // };
        // let child = Postgresql::start(config).await?;
        // std::mem::forget(child);
        // // let mut httpbin = get_and_spawn_httpbin_on_free_port().await?;
        // Command::new("cmd")
        //     .args(["/c",
        // "H:\\NBO\\enso2\\built-distribution\\enso-engine-0.2.32-SNAPSHOT-windows-amd64\\enso-0.2.
        // 32-SNAPSHOT\\bin\\enso", "--no-ir-caches", "--run",
        // "H:\\NBO\\enso2\\test\\Database_Tests"]).run_ok().await?; httpbin.process.kill().
        // await?;
        Ok(())
    }
}
