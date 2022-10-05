use crate::prelude::*;

use crate::cache;
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

    fn activate(&self, package_path: PathBuf) -> Result {
        let sbt_home = package_path.join("sbt");
        // Yeah, it is needed. Sbt will fail, if not set.
        SBT_HOME.set(&sbt_home)?;
        crate::env::prepend_to_path(sbt_home.join("bin"))?;
        Ok(())
    }
}
