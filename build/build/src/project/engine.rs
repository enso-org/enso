// use crate::prelude::*;
//
// use crate::engine::BuildConfigurationFlags;
// use crate::project::Context;
// use crate::project::IsArtifact;
// use crate::project::IsTarget;
//
// use ide_ci::goodie::GoodieDatabase;
// use ide_ci::ok_ready_boxed;
//
// pub use crate::project::backend::BuildInput;
// use crate::source::BuildTargetJob;
// use crate::source::WithDestination;
//
// #[derive(Clone, Debug)]
// pub struct Artifact {
//     pub root: PathBuf,
// }
//
// impl AsRef<Path> for Artifact {
//     fn as_ref(&self) -> &Path {
//         &self.root
//     }
// }
//
// impl IsArtifact for Artifact {}
//
//
// #[derive(Clone, Copy, Debug, PartialEq)]
// pub struct Engine;
//
// impl IsTarget for Engine {
//     type BuildInput = BuildInput;
//     type Artifact = Artifact;
//
//     fn artifact_name(&self) -> String {
//         "Enso Engine".into()
//     }
//
//     fn adapt_artifact(self, path: impl AsRef<Path>) -> BoxFuture<'static, Result<Self::Artifact>>
// {         ok_ready_boxed(Artifact { root: path.as_ref().into() })
//     }
//
//     fn build_internal(
//         &self,
//         context: Context,
//         job: BuildTargetJob<Self>,
//     ) -> BoxFuture<'static, Result<Self::Artifact>> {
//         let WithDestination { inner, destination } = job;
//         let this = self.clone();
//         async move {
//             let paths = crate::paths::Paths::new_versions(&inner.repo_root, inner.versions)?;
//             let context = crate::engine::context::RunContext {
//                 operation: crate::engine::Operation::Build,
//                 goodies: GoodieDatabase::new()?,
//                 config: BuildConfigurationFlags {
//                     clean_repo: false,
//                     build_engine_package: true,
//                     ..crate::engine::NIGHTLY
//                 }
//                 .into(),
//                 inner: context,
//                 paths,
//             };
//             let artifacts = context.build().await?;
//             let engine_distribution =
//                 artifacts.packages.engine.context("Missing Engine Distribution!")?;
//             ide_ci::fs::mirror_directory(&engine_distribution.dir, &destination).await?;
//             this.adapt_artifact(destination).await
//         }
//         .boxed()
//     }
// }
