use enso_build_cli::prelude::*;

// use clap::Parser;
// use enso_build::context::BuildContext;
// use enso_build::paths::TargetTriple;
// use enso_build::project::Engine;
// use enso_build::setup_octocrab;
// use enso_build::version;
// use enso_build_cli::arg::Source;
// use enso_build_cli::Processor;
// use ide_ci::cache::Cache;
// use ide_ci::models::config::RepoContext;
//
// #[derive(Clone, Debug, Parser)]
// pub struct Cli {
//     #[clap(flatten)]
//     engine_src: Source<Engine>,
// }

#[tokio::main]
pub async fn main() -> Result {
    // let cli = Cli::parse();
    // let octocrab = setup_octocrab().await?,;
    // let triple =TargetTriple::new(version::deduce_versions());
    //
    // let processor = Processor {
    //     context: BuildContext {
    //         cache: Cache::new_default(),
    //         remote_repo: RepoContext::from_str("enso-org/enso"),
    //         octocrab: setup_octocrab().await?,
    //         triple: TargetTriple::new(version::deduce_versions())
    //     }
    // }
    Ok(())
}
