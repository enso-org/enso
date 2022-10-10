use crate::prelude::*;

use ide_ci::env::Variable;
use ide_ci::programs::Go;
use tokio::process::Child;



pub mod env {
    /// Environment variable that stores URL under which spawned httpbin server is available.
    #[derive(Clone, Copy, Debug)]
    pub struct Url;
    impl ide_ci::env::Variable for Url {
        const NAME: &'static str = "ENSO_HTTP_TEST_HTTPBIN_URL";
        type Value = url::Url;
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
    env::Url.set(&url);
    Ok(Spawned { url, process })
}

impl Drop for Spawned {
    fn drop(&mut self) {
        debug!("Dropping the httpbin wrapper.");
        env::Url.remove();
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
