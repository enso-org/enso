use crate::prelude::*;

use crate::cache;
use crate::env::known::PATH;
use crate::programs;



const DOWNLOAD_URL_TEXT: &str = "https://github.com/sbt/sbt/releases/download/v1.5.5/sbt-1.5.5.tgz";

crate::define_env_var! {
    SBT_HOME, PathBuf;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub struct Sbt;

impl cache::Goodie for Sbt {
    fn url(&self) -> BoxFuture<'static, Result<Url>> {
        ready(Url::parse(DOWNLOAD_URL_TEXT).anyhow_err()).boxed()
    }

    fn is_active(&self) -> BoxFuture<'static, Result<bool>> {
        ready(Ok(programs::Sbt.lookup().is_ok())).boxed()
    }

    fn activation_env_changes(&self, package_path: &Path) -> Result<Vec<crate::env::Modification>> {
        let sbt_home = package_path.join("sbt");
        // Yeah, it is needed. Sbt will fail, if not set.
        Ok(vec![
            crate::env::Modification::set(&SBT_HOME, &sbt_home)?,
            crate::env::Modification::prepend_path(&PATH, sbt_home.join("bin")),
        ])
    }
}
