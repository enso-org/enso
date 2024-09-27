use crate::prelude::*;

use ide_ci::env::accessor::RawVariable;
use ide_ci::env::accessor::TypedVariable;
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



/// Port used by SQLServer in its container.
const SQLSERVER_CONTAINER_DEFAULT_PORT: u16 = 1433;

/// Environment variables used to configure the SQLServer container.
pub mod env {

    pub mod container {
        ide_ci::define_env_var! {
            ACCEPT_EULA, String;
            MSSQL_SA_PASSWORD, String;
        }
    }
    pub mod tests {
        ide_ci::define_env_var! {
            ENSO_SQLSERVER_DATABASE, String;
            ENSO_SQLSERVER_HOST, String;
            ENSO_SQLSERVER_PORT, String;
            ENSO_SQLSERVER_USER, String;
            ENSO_SQLSERVER_PASSWORD, String;
        }
    }
}

#[derive(Clone, Debug)]
pub enum EndpointConfiguration {
    /// Used when the SQLServer container is started directly from host (rather than Docker
    /// container). In such case the SQLServer will be exposed to host network on a given port.
    Host { port: u16 },
    /// Used when SQLServer is spawned from a container. In such case it will be spawned in a
    /// owning container's network on the default port.
    Container { owner: ContainerId },
}

impl EndpointConfiguration {
    /// Tries to deduce what endpoint should be used for a spawned SQLServer service.
    pub fn deduce() -> Result<Self> {
        if let Ok(container_name) = crate::env::ENSO_RUNNER_CONTAINER_NAME.get() {
            debug!("Assuming that I am in the Docker container named {container_name}.");
            Ok(Self::Container { owner: container_name })
        } else {
            // If we are running on the bare machine (i.e. not in container), we spawn SQLServer
            // and expose it on a free host port. Then we can directly consume.
            let port = if port_check::is_local_port_free(SQLSERVER_CONTAINER_DEFAULT_PORT) {
                // Prefer the usual port.
                SQLSERVER_CONTAINER_DEFAULT_PORT
            } else {
                get_free_port()?
            };
            Ok(Self::Host { port })
        }
    }
}

#[derive(Clone, Debug)]
pub struct Configuration {
    pub sqlserver_container: ContainerId,
    pub database_name:       String,
    pub user:                String,
    pub password:            String,
    pub endpoint:            EndpointConfiguration,
    pub version:             String,
}

impl Configuration {
    pub fn image_id(&self) -> ImageId {
        ImageId(format!("mcr.microsoft.com/mssql/server:{}", &self.version))
    }

    pub fn set_enso_test_env(&self) -> Result {
        env::tests::ENSO_SQLSERVER_DATABASE.set(&self.database_name)?;
        env::tests::ENSO_SQLSERVER_HOST.set("localhost")?;
        env::tests::ENSO_SQLSERVER_PORT.set(&match &self.endpoint {
            EndpointConfiguration::Host { port } => port.to_string(),
            EndpointConfiguration::Container { .. } => SQLSERVER_CONTAINER_DEFAULT_PORT.to_string(),
        })?;
        env::tests::ENSO_SQLSERVER_USER.set(&self.user)?;
        env::tests::ENSO_SQLSERVER_PASSWORD.set(&self.password)?;
        Ok(())
    }

    pub fn clear_enso_test_env(&self) {
        env::tests::ENSO_SQLSERVER_DATABASE.remove();
        env::tests::ENSO_SQLSERVER_HOST.remove();
        env::tests::ENSO_SQLSERVER_PORT.remove();
        env::tests::ENSO_SQLSERVER_USER.remove();
        env::tests::ENSO_SQLSERVER_PASSWORD.remove();
    }

    pub async fn cleanup(&self) -> Result {
        Docker.remove_container(&self.sqlserver_container, true).await
    }
}

pub async fn process_lines_until<R: AsyncRead + Unpin>(
    reader: R,
    f: &impl Fn(&str) -> bool,
) -> Result<R> {
    let mut reader = BufReader::new(reader);
    let mut line = String::new();
    loop {
        let bytes_read = reader.read_line(&mut line).await?;
        ensure!(bytes_read != 0, "SQLServer container closed without being ready!");
        if f(&line) {
            break;
        }
        line.clear();
    }
    Ok(reader.into_inner())
}

#[derive(Debug)]
pub struct SQLServerContainer {
    _docker_run: Child,
    config:      Configuration,
}

impl Drop for SQLServerContainer {
    fn drop(&mut self) {
        self.config.clear_enso_test_env();

        debug!("Will remove the SQLServer container");
        let cleanup_future = self.config.cleanup();
        if let Err(e) = futures::executor::block_on(cleanup_future) {
            debug!(
                "Failed to kill the SQLServer container named {}: {}",
                self.config.sqlserver_container, e
            );
        } else {
            debug!("SQLServer container killed.");
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SQLServer;

impl SQLServer {
    pub async fn start(config: Configuration) -> Result<SQLServerContainer> {
        // Attempt cleanup in case previous script run crashed in the middle of this.
        // Otherwise, SQLServer container names could collide.
        let _ = config.cleanup().await;

        let mut opts = RunOptions::new(config.image_id());
        opts.env(&env::container::ACCEPT_EULA, "Y")?;
        opts.env(&env::container::MSSQL_SA_PASSWORD, &*config.password)?;
        match &config.endpoint {
            EndpointConfiguration::Host { port } => {
                opts.publish_port(*port, SQLSERVER_CONTAINER_DEFAULT_PORT);
            }
            EndpointConfiguration::Container { owner } => {
                opts.network = Some(Network::Container(owner.clone()));
            }
        }
        opts.sig_proxy = Some(true);
        opts.name = Some(config.sqlserver_container.to_string());
        let mut cmd = Docker.run_cmd(&opts)?;
        cmd.stdout(Stdio::piped());
        cmd.kill_on_drop(true);
        let mut child = cmd.spawn()?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| anyhow!("Failed to access standard output of the spawned process!"))?;

        // Wait until container is ready.
        let check_line = |line: &str| {
            debug!("SQLSERVER_LOG: {}", line.trim_end().trim_start());
            line.contains("SQL Server is now ready for client connections")
        };
        let stdout = process_lines_until(stdout, &check_line).await?;
        // Put back stream we've been reading and pack the whole thing back for the caller.
        child.stdout = Some(stdout);
        config.set_enso_test_env()?;
        Ok(SQLServerContainer { _docker_run: child, config })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn start_sqlserver() -> Result {
        let config = Configuration {
            sqlserver_container: ContainerId("something".into()),
            endpoint:            EndpointConfiguration::deduce()?,
            version:             "2022-latest".into(),
            user:                "test".into(),
            password:            "<YourStrong@Passw0rd>".into(),
            database_name:       "test".into(),
        };
        let child = SQLServer::start(config).await?;
        // drop(child);
        std::mem::forget(child);
        Ok(())
    }
}
