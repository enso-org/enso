use crate::prelude::*;

use enso_build::paths::generated::RepoRootGithub;
use enso_build::repo::deduce_repository_path;
use ide_ci::actions::workflow::definition::WorkflowToWrite;



pub mod prelude {
    pub use enso_build::prelude::*;
}

/// Generate the comment that is at the top of each generated workflow file.
fn preamble(source: &str) -> String {
    // To make output consistent across platforms.
    let source = source.replace('\\', "/");
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
    let workflows = enso_build::ci_gen::generate(&workflows_dir)?;

    for WorkflowToWrite { source, path, workflow } in workflows {
        let preamble = preamble(&source);
        let yaml = serde_yaml::to_string(&workflow)?;
        let contents = format!("{preamble}\n\n{yaml}");
        ide_ci::fs::tokio::write(path, contents).await?;
    }

    // Ensure that generated files are properly formatted.
    enso_build::web::install(&repo_root).await?;
    enso_build::web::run_script(&repo_root, enso_build::web::Script::FormatWorkflows).await?;

    Ok(())
}
