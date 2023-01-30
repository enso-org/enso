// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use crate::prelude::*;

use enso_build::paths::generated::RepoRootGithub;
use enso_build::repo::deduce_repository_path;
use ide_ci::actions::workflow::definition::WorkflowToWrite;



pub mod prelude {
    pub use enso_build::prelude::*;
    pub use enso_build_shader_tools::prelude::*;
}

/// Generate the comment that is at the top of each generated workflow file.
fn preamble(source: &str) -> String {
    format!(
        "# This file is auto-generated. Do not edit it manually!\n\
            # Edit the {source} module instead and run `cargo run --package {}`.",
        env!("CARGO_PKG_NAME")
    )
}

#[tokio::main]
async fn main() -> Result {
    setup_logging()?;
    let repo_root = deduce_repository_path()?;
    let workflows_dir = RepoRootGithub::new_under(&repo_root).workflows;
    let mut workflows = enso_build::ci_gen::generate(&workflows_dir)?;
    workflows.push(enso_build_shader_tools::ci::generate_workflow(&workflows_dir.shader_tools_yml));

    for WorkflowToWrite { source, path, workflow } in workflows {
        let preamble = preamble(&source);
        let yaml = serde_yaml::to_string(&workflow)?;
        let contents = format!("{preamble}\n\n{yaml}");
        ide_ci::fs::tokio::write(path, contents).await?;
    }

    warn!("Remember to run formatter on the generated files!");
    Ok(())
}
