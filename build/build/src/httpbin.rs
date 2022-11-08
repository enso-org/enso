use crate::prelude::*;

use ide_ci::programs::Go;
use tokio::process::Child;



pub mod env {
    use super::*;

    ide_ci::define_env_var! {
        /// Environment variable that stores URL under which spawned httpbin server is available.
        ENSO_HTTP_TEST_HTTPBIN_URL, Url;
    }
}

#[derive(Debug)]
pub struct Spawned {
    pub process: Child,
    pub url:     Url,
}

pub async fn get_and_spawn_httpbin(port: u16) -> Result<Spawned> {
    Go.cmd()?
        .args(["install", "-v", "github.com/ahmetb/go-httpbin/cmd/httpbin@latest"])
        .run_ok()
        .await?;
    let gopath = Go.cmd()?.args(["env", "GOPATH"]).run_stdout().await?;
    let gopath = gopath.trim();
    let gopath = PathBuf::from(gopath); // be careful of trailing newline!
    let program = gopath.join("bin").join("httpbin");
    debug!("Will spawn {}", program.display());
    let process = Command::new(program) // TODO? wrap in Program?
        .args(["-host", &format!(":{port}")])
        .kill_on_drop(true)
        .spawn_intercepting()
        .anyhow_err()?;

    let url_string = format!("http://localhost:{port}");
    let url = Url::parse(&url_string)?;
    env::ENSO_HTTP_TEST_HTTPBIN_URL.set(&url)?;
    Ok(Spawned { url, process })
}

impl Drop for Spawned {
    fn drop(&mut self) {
        debug!("Dropping the httpbin wrapper.");
        env::ENSO_HTTP_TEST_HTTPBIN_URL.remove();
    }
}

pub async fn get_and_spawn_httpbin_on_free_port() -> Result<Spawned> {
    get_and_spawn_httpbin(ide_ci::get_free_port()?).await
}

#[cfg(test)]
mod tests {
    use crate::project::ProcessWrapper;

    use super::*;


    #[tokio::test]
    #[ignore]
    async fn spawn() -> Result {
        let mut spawned = get_and_spawn_httpbin_on_free_port().await?;
        dbg!(&spawned);
        spawned.process.wait_ok().await?;
        Ok(())
    }
}
