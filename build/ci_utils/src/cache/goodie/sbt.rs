use crate::prelude::*;

use crate::cache::goodie;
use crate::cache::Cache;
use crate::env::known::PATH;
use crate::programs;



const DOWNLOAD_URL_TEXT: &str = "https://github.com/sbt/sbt/releases/download/v1.5.5/sbt-1.5.5.tgz";

crate::define_env_var! {
    SBT_HOME, PathBuf;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub struct Sbt;

impl Goodie for Sbt {
    fn get(&self, cache: &Cache) -> BoxFuture<'static, Result<PathBuf>> {
        goodie::download_try_url(Url::from_str(DOWNLOAD_URL_TEXT), cache)
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
