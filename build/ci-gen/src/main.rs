// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use crate::prelude::*;

use enso_build::paths::generated::RepoRootGithub;
use enso_build::repo::deduce_repository_path;



pub mod prelude {
    pub use enso_build::prelude::*;
    pub use enso_build_shader_tools::prelude::*;
}


#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let repo_root = deduce_repository_path()?;
    let workflows_dir = RepoRootGithub::new_under(repo_root).workflows;
    workflows_dir
        .shader_tools_yml
        .write_as_yaml(&enso_build_shader_tools::ci::generate_workflow())?;
    enso_build::ci_gen::generate(&workflows_dir)?;
    Ok(())
}
