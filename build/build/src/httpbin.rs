use crate::prelude::*;

use ide_ci::extensions::child::ChildExt;
use tokio::process::Child;



pub mod env {
    use super::*;

    ide_ci::define_env_var! {
        /// Environment variable that stores URL under which spawned httpbin server is available.
        ENSO_HTTP_TEST_HTTPBIN_URL, Url;
    }
}

/// Handle to the spawned httpbin server.
///
/// It kills the process when dropped.
#[derive(Debug)]
pub struct Spawned {
    pub process: Child,
    pub url:     Url,
}

pub async fn get_and_spawn_httpbin(
    sbt: &crate::engine::sbt::Context,
    port: u16,
) -> Result<Spawned> {
    let process = sbt
        .command()?
        .arg(format!("http-test-helper/run localhost {port}"))
        .kill_on_drop(true)
        .spawn()?;

    let url_string = format!("http://localhost:{port}");
    let url = Url::parse(&url_string)?;
    env::ENSO_HTTP_TEST_HTTPBIN_URL.set(&url)?;
    Ok(Spawned { url, process })
}

impl Drop for Spawned {
    fn drop(&mut self) {
        debug!("Dropping the httpbin wrapper.");
        env::ENSO_HTTP_TEST_HTTPBIN_URL.remove();
        self.process.kill_subtree();
    }
}

pub async fn get_and_spawn_httpbin_on_free_port(
    sbt: &crate::engine::sbt::Context,
) -> Result<Spawned> {
    get_and_spawn_httpbin(sbt, ide_ci::get_free_port()?).await
}


#[cfg(test)]
mod tests {
    use ide_ci::cache;
    use ide_ci::env::current_dir;
    use std::env::set_current_dir;

    use super::*;


    #[tokio::test]
    #[ignore]
    async fn spawn() -> Result {
        setup_logging()?;
        set_current_dir(r"H:\NBO\enso5")?;
        let cache = cache::Cache::new_default().await?;
        cache::goodie::sbt::Sbt.install_if_missing(&cache).await?;

        let sbt = crate::engine::sbt::Context {
            repo_root:         current_dir()?,
            system_properties: vec![],
        };

        let spawned = get_and_spawn_httpbin_on_free_port(&sbt).await?;
        std::thread::sleep(std::time::Duration::from_secs(20));
        dbg!(&spawned);


        Ok(())
    }
}
