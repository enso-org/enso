// use enso_build::prelude::*;
//
// use crate::arg::Source;
// use crate::source_args_hlp;
// use clap::Args;
// // use enso_build::project::engine::Engine;
//
// source_args_hlp!(Engine, "engine", BuildInput);
//
// #[derive(Args, Clone, Debug, PartialEq)]
// pub struct BuildInput {}
//
// #[derive(Args, Clone, Debug)]
// pub struct Target {
//     #[clap(flatten)]
//     pub source: Source<Engine>,
// }
