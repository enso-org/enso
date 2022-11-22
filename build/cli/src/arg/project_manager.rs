// use enso_build::prelude::*;
//
// use crate::arg::Source;
// use crate::source_args_hlp;
// use enso_build::project::project_manager::ProjectManager;
//
// use clap::Args;
//
// source_args_hlp!(ProjectManager, "project-manager", BuildInput);
//
// #[derive(Args, Clone, Debug, PartialEq)]
// pub struct BuildInput {}
//
// #[derive(Args, Clone, Debug)]
// pub struct Target {
//     #[clap(flatten)]
//     pub source: Source<ProjectManager>,
// }
